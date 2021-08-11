
library(shinydashboard)
library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(shinyWidgets)
library(glue)
library(scales)
library(viridis)

# load data ---------------------------------------------------------------

df <- read_rds("data_out/ess_data.rds")
poly <- st_read("data_out/lga.gpkg")

initial_poly <- left_join(poly, df %>% filter(year == "2016"),
                           by = c("lga_name" = "lga")) %>% 
    filter(!is.na(year))

# modules -----------------------------------------------------------------

list.files("modules") %>% 
    purrr::map(~source(paste0("modules/", .)))

# ui section --------------------------------------------------------------


ui <- dashboardPage(
    dashboardHeader(title = "Employment self sufficiency"),
    dashboardSidebar(
        selectInput("year",
                    "Select Census year",
                    choices = c("2011", "2016"),
                    selected = "2016")
        
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            
            box(width = 12,
                tags$p("Employment self-sufficency is the proportion of local jobs are filled by local residents.",
                       "Local jobs are those within the same municipality."),
                tags$a(href="https://www.abs.gov.au/census", target="_blank",
                       "Data source: ABS, Census of Population and Housing, 2011 and 2016.")
            )

        ),
        
        fluidRow(
            map_ui("map")
        )
    )
)


# server section ----------------------------------------------------------


server <- function(input, output) {
    
    # reactive year
    ryear <- reactive(input$year)
    
    # call map module
    map_server("map",
               ryear)

}

shinyApp(ui, server)