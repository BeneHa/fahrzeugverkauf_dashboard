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
library(shiny.i18n)

# Definitions -------------------------------------------------------------

#Translations
translator <- Translator$new(translation_csvs_path = "translations/")
translator$set_translation_language("de")

monthStart <- function(x){
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

# Load data ---------------------------------------------------------------

setwd("~/806 Projekte R/shiny/tutorial/app-1")
#setwd("C:/Users/bened/OneDrive/Arbeit/Lernen/hersteller_dashboard/fahrzeugverkauf_dashboard/app-1/")

files <- list.files("data", pattern = "fz*")
full_dat_kba <- data_frame()
full_dat_hersteller <- data_frame()

#Dateien einlesen, ins long format bringen und untereinander haengen
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


ui <- navbarPage(translator$t("Vehicle sales"),
                 
                 
                 tabPanel(translator$t("Germany overview"),
                          tags$head(tags$style(".checkbox-inline {margin: 0 !important;}")),
                          
                          titlePanel(translator$t("Vehicle sales by manufacturer")),
                          
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         helpText(translator$t("Selection of manufacturer / engine type / time slice")),
                                         
                                         fluidRow(
                                           column(12, align = "left",
                                                  checkboxGroupInput("herstellerCheckbox",
                                                                     h3(translator$t("Manufacturer")),
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
                                                              h3(translator$t("Time slice")),
                                                              min = min(full_dat_kba$date), max = max(full_dat_kba$date),
                                                              value = c(min(full_dat_kba$date), max(full_dat_kba$date)),
                                                              timeFormat = "%b %Y")
                                           )
                                         )
                            ),
                            
                            mainPanel(
                              plotOutput("overviewPlot"),
                              
                              h3("Details:"),
                              
                              column(11,
                                     dataTableOutput('overviewTable')),
                              
                              column(11, dataTableOutput('overviewTableCached'))
                              
                              
                            )
                          )
                 ),
                 
                 tabPanel(translator$t("Details Germany"),
                          tags$head(tags$style(".radio-inline {margin: 0 !important;}")),
                          
                          
                          titlePanel(translator$t("Detailed view of vehicle model series")),
                          
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         helpText(translator$t("Selection of manufacturer / engine type / time slice")),
                                         
                                         fluidRow(
                                           column(12, align = "left",
                                                  radioButtons("herstellerRadio",
                                                               h3(translator$t("Manufacturer")),
                                                               inline = T,
                                                               choices = hersteller_vect,
                                                               selected = "Mercedes")
                                           )
                                         ),
                                         
                                         fluidRow(
                                           column(5, align = "left",
                                                  
                                                  selectInput("herstellerTypAuswahl",
                                                              h3(translator$t("Type")),
                                                              choices = list("Gesamt" = "gesamt", "Diesel" = "diesel", "Hybrid" = "hybrid",
                                                                             "Elektro" = "elektro", "Allrad" = "allrad", "Cabrio" = "cabrio"), selected = "gesamt")
                                           ),
                                           column(5,
                                                  selectInput("herstellerTopzahl",
                                                              h3(translator$t("Show:")),
                                                              choices = list("Alle" = "50", "Top 5" = "5", "Top 10" = "10"), selected = "50")
                                                  
                                           )
                                         ),
                                         
                                         fluidRow(
                                           column(10, align = "left",
                                                  
                                                  sliderInput("herstellerZeitraum",
                                                              h3(translator$t("Time slice")),
                                                              min = min(full_dat_kba$date), max = max(full_dat_kba$date),
                                                              value = c(min(full_dat_kba$date), max(full_dat_kba$date)),
                                                              timeFormat = "%b %Y")
                                           )
                                         )
                            ),
                            mainPanel(
                              plotOutput("detailPlot"),
                              h3("Details:"),
                              dataTableOutput('detailTable'),
                              p("")
                            )
                            
                            
                          )
                          
                 ),
                 
                 tabPanel("Infos",
                          
                          h3(translator$t("General information")),
                          p(translator$t("All data comes from the Feder Motor Transport Authority Germany and are completely public.")),
                          p(translator$t("I do not give warranty for correctness of the data or the visualization or accessibility of this tool.")),
                          uiOutput("githubLink"),
                          p(translator$t("This is a private project and not related to my employer (Daimler AG)."))
                 )
                 
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  #Data for overview page
  overviewData <- reactive({
    full_dat_kba_filtered <- full_dat_kba %>%
      filter(hersteller %in% toupper(input$herstellerCheckbox), typ == input$typAuswahl,
             date >= input$zeitraum[1], date <= input$zeitraum[2])
    
    validate(
      need(nrow(full_dat_kba_filtered) > 0, translator$t("Please select at least one manufacturer."))
    )
    full_dat_kba_filtered
    
  })
  
  #Data for details page
  detailData <- reactive({
    full_dat_hersteller_filtered <- full_dat_hersteller %>%
      filter(hersteller == toupper(input$herstellerRadio), typ == input$herstellerTypAuswahl,
             date >= input$herstellerZeitraum[1], date <= input$herstellerZeitraum[2],
             !is.na(modellreihe), !is.na(value))
    
    validate(
      need(nrow(full_dat_hersteller_filtered) > 0, translator$t("No data available. Please select another engine type for this manufacturer."))
    )
    full_dat_hersteller_filtered
  })
  
  #Vector of names of top model series for the current manufacturer based on number of user input
  modellreiheTop <- reactive({
    detailData() %>%
      group_by(modellreihe) %>%
      summarize(s = sum(value, na.rm = T)) %>%
      arrange(desc(s)) %>%
      filter(!is.na(modellreihe)) %>%
      mutate(modellreihe = as.character(modellreihe)) %>%
      head(as.numeric(input$herstellerTopzahl)) %>%
      pull(modellreihe)
  })
  
  #Plot of manufacturer details
  output$detailPlot <- renderPlot({
    dat <- detailData() %>%
      filter(modellreihe %in% modellreiheTop()) %>%
      group_by(modellreihe) %>%
      mutate(maxdate = max(date)) %>%
      filter(date == maxdate)
    
    plot_title = paste0(translator$t("Vehicle registrations per manufacturer per month (Germany) - "), toupper(input$herstellerTypAuswahl))
    detailData() %>%
      filter(modellreihe %in% modellreiheTop()) %>%
      ggplot(aes(x = date, y = value, col = modellreihe)) + geom_line() + geom_point() +
      geom_text_repel(data = dat, aes(label = modellreihe)) +
      ggtitle(plot_title) +
      labs(x = translator$t("Month"), y = translator$t("Number of registered vehicles"), colour = translator$t("Manufacturer")) +
      theme(plot.title = element_text(size = 30), axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            legend.title = element_text(size = 16), legend.text = element_text(size = 14)) +
      scale_x_date(date_labels = "%b %Y", date_minor_breaks = "1 month")
  })
  
  #Table of manufacturer details
  output$detailTable <- renderDataTable({
      
    datatable(
      detailData() %>%
        filter(modellreihe %in% modellreiheTop()) %>%
        select(-hersteller) %>%
        filter(!is.na(value)) %>%
        dcast(modellreihe ~ date) %>%
        rename(Modellreihe = modellreihe),
      options = list(dom = 'lt')
    )
  })
  
  #Manufacturer overview plot
  output$overviewPlot <- renderPlot({
    
    plot_title = paste0(translator$t("Vehicle registrations per manufacturer per month (Germany) - "), toupper(input$herstellerTypAuswahl))
    
    ggplot(overviewData(), aes(x = date, y = value, col = hersteller)) +
      geom_line(size = 1.2) + geom_point(size = 4) +
      ggtitle(plot_title) +
      labs(x = translator$t("Month"), y = translator$t("Number of registered vehicles"), colour = translator$t("Manufacturer")) +
      theme(plot.title = element_text(size = 30), axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            legend.title = element_text(size = 16), legend.text = element_text(size = 14)) +
      scale_x_date(date_labels = "%b %Y", date_minor_breaks = "1 month")
  })
  
  output$overviewPlotCached <- renderCachedPlot({
    ggplot(overviewData(), aes(x = date, y = value, col = hersteller)) + geom_line()
  }, cacheKeyExpr = { overviewData() })
  
  #Manufacturer overview table
  output$overviewTable <- renderDataTable({
    
    datatable(
      overviewData() %>%
        select(-typ) %>%
        dcast(hersteller ~ date) %>%
        rename(Hersteller = hersteller),
      options = list(dom = 'lt')
    )

  })
  url <- a("Github", href = "https://github.com/BeneHa/fahrzeugverkauf_dashboard")
  output$githubLink <- renderUI({
    tagList(translator$t("Link to source code:"), url)
  })
}



# Run ---------------------------------------------------------------------


shinyApp(ui = ui, server = server)
