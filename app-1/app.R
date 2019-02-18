#https://www.kba.de/DE/Statistik/Fahrzeuge/Neuzulassungen/MonatlicheNeuzulassungen/monatl_neuzulassungen_node.html

library(shiny)
library(tidyverse)
library(reshape2)
library(zoo)
library(readxl)
library(stringi)
library(DT)
library(scales)
library(ggrepel)



# Definitions -------------------------------------------------------------


monthStart <- function(x){
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

# Load data ---------------------------------------------------------------

#setwd("~/806 Projekte R/shiny/tutorial/app-1")
#setwd("C:/Users/bened/OneDrive/Arbeit/Lernen/hersteller_dashboard/fahrzeugverkauf_dashboard/app-1/")

files <- list.files("data", pattern = "fz*")
full_dat_kba <- data_frame()
full_dat_hersteller <- data_frame()
for (f in files){
  year <- substr(f, 6, 9)
  month <- substr(f, 11, 12)
  filenam <- paste0("data/fz10_", year, "_", month, "_xlsx.xlsx")
  filenam_old <- paste0("data/fz10_", year, "_", month, "_xls.xls")
  
  if(year > 2018){
    dat_curr_month <- read_xlsx(filenam, sheet = 2, skip = 8)
  }
  else if (year == 2018 & month == 12){
    dat_curr_month <- read_xlsx(filenam, sheet = 2, skip = 8)
  }
  else {
    dat_curr_month <- read_xls(filenam_old, sheet = 2, skip = 8)
  }
  
  
  names(dat_curr_month) <- c("marke", "modellreihe", "gesamt", "gesamt_", "gesamt_anteil",
                             "diesel", "diesel_", "diesel_anteil",
                             "hybrid", "hybrid_", "hybrid_anteil",
                             "elektro", "elektro_", "elektro_anteil",
                             "allrad", "allrad_", "allrad_anteil",
                             "cabrio", "cabrio_", "cabrio_anteil")
  dat_curr_clean <- dat_curr_month %>%
    select(-modellreihe, -gesamt_, -gesamt_anteil, -diesel_, -diesel_anteil, -hybrid_, -hybrid_anteil, -elektro_, -elektro_anteil, -allrad_, -allrad_anteil, -cabrio_, -cabrio_anteil) %>%
    filter(grepl("ZUSAMMEN", marke)) %>%
    mutate(date = paste0(year, "-", month)) %>%
    mutate(date = as.yearmon(date)) %>%
    melt(id = c("marke", "date")) %>%
    mutate(marke = gsub(" ZUSAMMEN", "", marke)) %>%
    rename(hersteller = marke, typ = variable)
  
  dat_curr_hersteller <- dat_curr_month %>%
    select(-gesamt_, -gesamt_anteil, -diesel_, -diesel_anteil, -hybrid_, -hybrid_anteil, -elektro_, -elektro_anteil, -allrad_, -allrad_anteil, -cabrio_, -cabrio_anteil) %>%
    filter(!(is.na(marke) & is.na(modellreihe))) %>%
    mutate(marke = na.locf(marke), na.rm = T) %>%
    filter(!grepl("ZUSAMMEN", marke)) %>%
    mutate(date = paste0(year, "-", month)) %>%
    mutate(date = as.yearmon(date)) %>%
    melt(id = c("marke", "modellreihe", "date")) %>%
    rename(hersteller = marke, typ = variable)
  
  
  full_dat_kba <- rbind(full_dat_kba, dat_curr_clean)
  full_dat_hersteller <- rbind(full_dat_hersteller, dat_curr_hersteller)
}


full_dat_kba <- full_dat_kba %>%
  mutate(value = as.numeric(value)) %>%
  mutate(date = as.Date(date))

full_dat_hersteller <- full_dat_hersteller %>%
  mutate(value = as.numeric(value)) %>%
  mutate(modellreihe = as.factor(modellreihe)) %>%
  mutate(date = as.Date(date))

hersteller_vect <- c("Alfa Romeo" = "Alfa Romeo",
                     "Audi" = "Audi",
                     "Bentley" = "Bentley",
                     "BMW" = "BMW",
                     "Cadillac" = "Cadillac",
                     "Chevrolet" = "Chevrolet",
                     "Citroen" = "Citroen",
                     "Dacia" = "Dacia",
                     "DS" = "DS",
                     "Ferrari" = "Ferrari",
                     "Fiat" = "Fiat",
                     "Ford" = "Ford",
                     "Honda" = "Honda",
                     "Hyundai" = "Hyundai",
                     "Infiniti" = "Infiniti",
                     "Iveco" = "Iveco",
                     "Jaguar" = "Jaguar",
                     "Jeep" = "Jeep",
                     "Kia" = "Kia",
                     "Lada" = "Lada",
                     "Lamborghini" = "Lamborghini",
                     "Land Rover" = "Land Rover",
                     "Lexus" = "Lexus",
                     "Maserati" = "Maserati",
                     "Mazda" = "Mazda",
                     "Mercedes" = "Mercedes",
                     "Mini" = "Mini",
                     "Mitsubishi" = "Mitsubishi",
                     "Nissan" = "Nissan",
                     "Opel" = "Opel",
                     "Peugeot" = "Peugeot",
                     "Porsche" = "Porsche",
                     "Renault" = "Renault",
                     "Seat" = "Seat",
                     "Skoda" = "Skoda",
                     "Smart" = "Smart",
                     "Ssangyong" = "Ssangyong",
                     "Subaru" = "Subaru",
                     "Suzuki" = "Suzuki",
                     "Tesla" = "Tesla",
                     "Toyota" = "Toyota",
                     "Volvo" = "Volvo",
                     "VW" = "VW",
                     "Lotus" = "Lotus",
                     "Rolls Royce" = "Rolls Royce",
                     "Aston Martin" = "Aston Martin",
                     "Alpine" = "Alpine",
                     "Morgan" = "Morgan")



# UI ----------------------------------------------------------------------


ui <- navbarPage("Fahrzeugverkaeufe",
                 
                 
                 tabPanel("Deutschland",
                          tags$head(tags$style(".checkbox-inline {margin: 0 !important;}")),
                          
                          titlePanel("Fahrzeugverkaeufe nach Hersteller"),
                          
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         helpText("Auswahl der anzuzeigenden Fahrzeughersteller, Antriebstypen und Zeitraum"),
                                         
                                         fluidRow(
                                           column(12, align = "left",
                                                  checkboxGroupInput("herstellerCheckbox",
                                                                     h3("Hersteller"),
                                                                     inline = T,
                                                                     choices = hersteller_vect,
                                                                     selected = list("Mercedes", "BMW", "Audi"))
                                           )
                                         ),
                                         
                                         fluidRow(
                                           column(5, align = "left",
                                                  
                                                  selectInput("typAuswahl",
                                                              h3("Typ"),
                                                              choices = list("Gesamt" = "gesamt", "Diesel" = "diesel", "Hybrid" = "hybrid",
                                                                             "Elektro" = "elektro", "Allrad" = "allrad", "Cabrio" = "cabrio"), selected = "gesamt")
                                           )
                                         ),
                                         
                                         fluidRow(
                                           column(10, align = "left",
                                                  
                                                  sliderInput("zeitraum",
                                                              h3("Zeitraum"),
                                                              min = min(full_dat_kba$date), max = max(full_dat_kba$date),
                                                              value = c(min(full_dat_kba$date), max(full_dat_kba$date)),
                                                              timeFormat = "%b %Y")
                                           )
                                         )
                            ),
                            
                            mainPanel(
                              plotOutput("plot"),
                              
                              h3("Details:"),
                              
                              column(11,
                                     dataTableOutput('table'))
                              
                              
                            )
                          )
                 ),
                 
                 tabPanel("Deutschland Details",
                          tags$head(tags$style(".radio-inline {margin: 0 !important;}")),
                          
                          
                          titlePanel("Fahrzeugmodelle Detailansicht"),
                          
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         helpText("Auswahl der anzuzeigenden Fahrzeughersteller, Antriebstypen und Zeitraum"),
                                         
                                         fluidRow(
                                           column(12, align = "left",
                                                  radioButtons("herstellerRadio",
                                                               h3("Hersteller"),
                                                               inline = T,
                                                               choices = hersteller_vect,
                                                               selected = "Mercedes")
                                           )
                                         ),
                                         
                                         fluidRow(
                                           column(5, align = "left",
                                                  
                                                  selectInput("herstellerTypAuswahl",
                                                              h3("Typ"),
                                                              choices = list("Gesamt" = "gesamt", "Diesel" = "diesel", "Hybrid" = "hybrid",
                                                                             "Elektro" = "elektro", "Allrad" = "allrad", "Cabrio" = "cabrio"), selected = "gesamt")
                                           ),
                                           column(5,
                                                  selectInput("herstellerTopzahl",
                                                              h3("Anzeigen"),
                                                              choices = list("Alle" = "50", "Top 5" = "5", "Top 10" = "10"), selected = "50")
                                                  
                                           )
                                         ),
                                         
                                         fluidRow(
                                           column(10, align = "left",
                                                  
                                                  sliderInput("herstellerZeitraum",
                                                              h3("Zeitraum"),
                                                              min = min(full_dat_kba$date), max = max(full_dat_kba$date),
                                                              value = c(min(full_dat_kba$date), max(full_dat_kba$date)),
                                                              timeFormat = "%b %Y")
                                           )
                                         )
                            ),
                            mainPanel(
                              plotOutput("herstellerPlot"),
                              h3("Details:"),
                              dataTableOutput('herstellerTable'),
                              p("")
                            )
                            
                            
                          )
                          
                 ),
                 
                 tabPanel("Infos",
                          
                          h3("Allgemeine Informationen"),
                          p("Die Daten stammen vom Kraftfahrzeugbundesamt und sind daher komplett oeffentlich."),
                          p("Ich gebe keine Garantie auf Richtigkeit der Daten oder Darstellungen oder auf Verfuegbarkeit dieser Webanwendung."),
                          uiOutput("githubLink"),
                          p("Dies ist ein privates Projekt und hat daher keinen Zusammenhang mit meinem Arbeitgeber, der Daimler AG.")
                 )
                 
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  plotData <- reactive({
    full_dat_kba %>%
      filter(hersteller %in% toupper(input$herstellerCheckbox), typ == input$typAuswahl,
             date >= input$zeitraum[1], date <= input$zeitraum[2])
    
  })
  
  herstellerData <- reactive({
    full_dat_hersteller %>%
      filter(hersteller == toupper(input$herstellerRadio), typ == input$herstellerTypAuswahl,
             date >= input$herstellerZeitraum[1], date <= input$herstellerZeitraum[2],
             !is.na(modellreihe), !is.na(value))
  })
  
  modellreiheTop <- reactive({
    herstellerData() %>%
      group_by(modellreihe) %>%
      summarize(s = sum(value, na.rm = T)) %>%
      arrange(desc(s)) %>%
      filter(!is.na(modellreihe)) %>%
      mutate(modellreihe = as.character(modellreihe)) %>%
      head(as.numeric(input$herstellerTopzahl)) %>%
      pull(modellreihe)
  })
  
  output$herstellerPlot <- renderPlot({
    dat <- herstellerData() %>%
      filter(modellreihe %in% modellreiheTop()) %>%
      group_by(modellreihe) %>%
      mutate(maxdate = max(date)) %>%
      filter(date == maxdate)
    
    plot_title = paste0("Neufahrzeugzulassungen der Hersteller pro Monat (Deutschland) - ", toupper(input$herstellerTypAuswahl))
    herstellerData() %>%
      filter(modellreihe %in% modellreiheTop()) %>%
      ggplot(aes(x = date, y = value, col = modellreihe)) + geom_line() + geom_point() +
      geom_text_repel(data = dat, aes(label = modellreihe)) +
      ggtitle(plot_title) +
      labs(x = "Monat", y = "Anzahl zugelassener Fahrzeuge", colour = "Hersteller") +
      theme(plot.title = element_text(size = 30), axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            legend.title = element_text(size = 16), legend.text = element_text(size = 14)) +
      scale_x_date(date_labels = "%b %Y", date_minor_breaks = "1 month")
  })
  
  output$herstellerTable <- renderDataTable({
    
    datatable(
      herstellerData() %>%
        filter(modellreihe %in% modellreiheTop()) %>%
        select(-hersteller) %>%
        filter(!is.na(value)) %>%
        dcast(modellreihe ~ date) %>%
        rename(Modellreihe = modellreihe),
      options = list(dom = 'lt')
    )
  })
  
  
  output$plot <- renderPlot({
    
    
    
    plot_title = paste0("Neufahrzeugzulassungen der Hersteller pro Monat (Deutschland) - ", toupper(input$typAuswahl))
    
    ggplot(plotData(), aes(x = date, y = value, col = hersteller)) +
      geom_line(size = 1.2) + geom_point(size = 4) +
      ggtitle(plot_title) +
      labs(x = "Monat", y = "Anzahl zugelassener Fahrzeuge", colour = "Hersteller") +
      theme(plot.title = element_text(size = 30), axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            legend.title = element_text(size = 16), legend.text = element_text(size = 14)) +
      scale_x_date(date_labels = "%b %Y", date_minor_breaks = "1 month")
  })
  
  output$table <- renderDataTable({
    
    datatable(
      plotData() %>%
        select(-typ) %>%
        dcast(hersteller ~ date) %>%
        rename(Hersteller = hersteller),
      options = list(dom = 'lt')
    )
    
    
    
  })
  url <- a("Github", href = "https://github.com/BeneHa/fahrzeugverkauf_dashboard")
  output$githubLink <- renderUI({
    tagList("Link zum Quellcode:", url)
  })
}



# Run ---------------------------------------------------------------------


shinyApp(ui = ui, server = server)