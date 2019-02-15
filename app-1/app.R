#https://www.kba.de/DE/Statistik/Fahrzeuge/Neuzulassungen/MonatlicheNeuzulassungen/monatl_neuzulassungen_node.html

library(shiny)
library(tidyverse)
library(reshape2)
library(zoo)
library(readxl)
library(stringi)
library(DT)
library(ggiraph)
library(scales)
#setwd("~/806 Projekte R/shiny/tutorial/app-1")
#setwd("C:/Users/bened/OneDrive/Arbeit/Lernen/hersteller_dashboard/app-1")



# Definitions -------------------------------------------------------------


monthStart <- function(x){
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

# Load data ---------------------------------------------------------------


files <- list.files("data", pattern = "fz*")
full_dat_kba <- data_frame()
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
  
  full_dat_kba <- rbind(full_dat_kba, dat_curr_clean)
}


full_dat_kba <- full_dat_kba %>%
  mutate(value = as.numeric(value)) %>%
  mutate(date = as.Date(date))





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
                               choices = c("Alfa Romeo" = "Alfa Romeo",
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
                                              "Morgan" = "Morgan"
                                              ),
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
  
  tabPanel("Seite 2", 
           
    h1("Deutschland")
  ),
  
  tabPanel("Infos",

    h3("Allgemeine Informationen"),
    p("Die Daten stammen vom Kraftfahrzeugbundesamt und sind daher komplett oeffentlich."),
    p("Ich gebe keine Garantie auf Richtigkeit der Daten oder Darstellungen oder auf Verfuegbarkeit dieser Webanwendung."),
    p("Der Quellcode ist auf github.com/beneha zu finden."),
    p("Dies ist ein privates Projekt und hat daher keinen Zusammenhang mit meinem Arbeitgeber, der Daimler AG.")
  )

)


# Server ------------------------------------------------------------------

server <- function(input, output) { 
  output$plot <- renderPlot({
    
    dat <- full_dat_kba %>%
      filter(hersteller %in% toupper(input$herstellerCheckbox), typ == input$typAuswahl,
             date >= input$zeitraum[1], date <= input$zeitraum[2])
    
    plot_title = paste0("Neufahrzeugzulassungen der Hersteller pro Monat (Deutschland) - ", toupper(input$typAuswahl))
    
    ggplot(dat, aes(x = date, y = value, col = hersteller)) +
      geom_line_interactive(size = 1.2) + geom_point(size = 4) +
      ggtitle(plot_title) +
      labs(x = "Monat", y = "Anzahl zugelassener Fahrzeuge", colour = "Hersteller") +
      theme(plot.title = element_text(size = 30), axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            legend.title = element_text(size = 16), legend.text = element_text(size = 14)) +
      scale_x_date(date_labels = "%b %Y", date_minor_breaks = "1 month")
  })
  
  output$table <- renderDataTable({
    
    datatable(
      full_dat_kba %>%
        filter(hersteller %in% toupper(input$herstellerCheckbox), typ == input$typAuswahl,
               date >= input$zeitraum[1], date <= input$zeitraum[2]) %>%
        select(-typ) %>%
        dcast(hersteller ~ date) %>%
        rename(Hersteller = hersteller),
      options = list(dom = 't')
    )
    
    
    
  })
}



# Run ---------------------------------------------------------------------


shinyApp(ui = ui, server = server)



