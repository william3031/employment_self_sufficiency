
# map module
map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map")) %>% 
      addSpinner()
  )
}

map_server <- function(id, selected_year) {
  moduleServer(id, function(input, output, session) {
    
    # initial values ------------------
    pal <- colorNumeric(c("red", "blue"), domain = initial_poly$ess)
    
    initial_poly <- inner_join(poly, df %>% filter(year == "2016"),
                               by = c("lga_name" = "lga"))

    initial_label_text <- glue("<b>LGA:</b> {initial_poly$lga_name}</br>",
                               "<b>Year:</b> {initial_poly$year}</br>",
                               "<b>Local jobs:</b> {comma(initial_poly$local_jobs, accuracy = 1)}</br>",
                               "<b>Total jobs:</b> {comma(initial_poly$total_jobs, accuracy = 1)}</br>",
                               "<b>ESS:</b> {initial_poly$ess}") %>% lapply(htmltools::HTML)
    
    # initial map --------------------------------
    output$map <- renderLeaflet(
      
      # initial map
      leaflet() %>% 
        addProviderTiles("Stamen.TonerHybrid") %>% 
        
        # layer
        addPolygons(data = initial_poly, 
                    fillColor =  ~pal(ess),
                    fillOpacity = 0.5,
                    weight = 2,
                    stroke = TRUE,
                    color = "white", 
                    opacity = 1,
                    layerId = ~lga_name,
                    label = initial_label_text,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list("color" = "black", "font-weight" = "normal", padding = "3px 8px"),
                                                clickable = TRUE,
                                                textsize = "12px"),
                    highlightOptions = highlightOptions(color = "aqua",
                                                        weight = 1.5,
                                                        opacity = 0.5,
                                                        bringToFront = TRUE)) %>% 
        # initial legend
        addLegend(pal = pal,
                  data = initial_poly,
                  values = ~ess,
                  opacity = 0.7,
                  title = "Employment </br> Self Sufficiency",
                  position = "topright")

    )
    
    # updated poly ------------------------
    updated_poly <- reactive({
      
        inner_join(poly, df %>% filter(year == selected_year()),
                   by = c("lga_name" = "lga"))
        })
    
    updated_label_text <- reactive({
      
      glue("<b>LGA:</b> {updated_poly()$lga_name}</br>",
           "<b>Year:</b> {updated_poly()$year}</br>",
           "<b>Local jobs:</b> {comma(updated_poly()$local_jobs, accuracy = 1)}</br>",
           "<b>Total jobs:</b> {comma(updated_poly()$total_jobs, accuracy = 1)}</br>",
           "<b>ESS:</b> {updated_poly()$ess}") %>% lapply(htmltools::HTML)
    })
    
    # observe -------------------------------
    observe({
      
      leafletProxy("map") %>% 
        clearShapes() %>% 
        clearControls() %>% 
        addPolygons(data = updated_poly(), 
                    fillColor =  ~pal(ess),
                    fillOpacity = 0.5,
                    weight = 2,
                    stroke = TRUE,
                    color = "white", 
                    opacity = 1,
                    layerId = ~lga_name,
                    label = updated_label_text(),
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list("color" = "black", "font-weight" = "normal", padding = "3px 8px"),
                                                clickable = TRUE,
                                                textsize = "12px"),
                    highlightOptions = highlightOptions(color = "aqua",
                                                        weight = 1.5,
                                                        opacity = 0.5,
                                                        bringToFront = TRUE)) %>% 

        addLegend(pal = pal,
                  data = updated_poly(),
                  values = ~ess,
                  opacity = 0.7,
                  title = "Employment </br> Self Sufficiency",
                  position = "topright")
        
        
    })
    
  })
}