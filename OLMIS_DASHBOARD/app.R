library(shiny)
library(DT)
library(jsonlite)
library(dplyr)
library(shinythemes)
library(leaflet)
library(htmltools)
region_list<- jsonlite::fromJSON("http://dev.datahub.ph/olmis/API/?type=reference&list=regions")
regional_data<- jsonlite::fromJSON("http://dev.datahub.ph/olmis/API/?type=cases&fid=1&year=2019&location=regions")
provincial_data <- jsonlite::fromJSON("http://dev.datahub.ph/olmis/API/?type=cases&fid=1&year=2019&location=provinces")

ui <- navbarPage(theme = shinytheme("sandstone"),"OLMIS Dashboard",
                 tabPanel("Summary",
                          sidebarLayout(
                              sidebarPanel(
                                  h4("Map of Malaria Cases"),                              
                                  selectInput("input_year", "Select Year", choices = 2019:2020),
                                  width = 2
                              ),
                              mainPanel(
                                  h4("Total Detected and Reported Cases in the Philippines By Region", textOutput("selected_year")),
                                  hr(),
                                  leafletOutput("mymap"),
                                  br(),
                                  hr(),
                                  DT::dataTableOutput("table")       
                              )
                          )
                 ),
                 tabPanel("Diagnostic and Cases",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("input_year2", "Select Year", choices = 2019:2020),
                                  selectInput("input_region", "Select Region", choices = region_list$region_name),
                                  width = 3
                              ),
                              mainPanel(
                                  h4("Total Detected and Reported Cases in the Philippines By Region,Province", textOutput("selected_year_1")),                             
                                  leafletOutput("mymap2"),
                                  DT::dataTableOutput("regional"),
                                  br(),
                                  hr(),
                                  DT::dataTableOutput("provincial")
                              )
                          )
                 ),
                 tabPanel("Vector Control",
                          sidebarLayout(
                              sidebarPanel(
                                  actionButton("myButton4", "Display All"),
                                  hr(),
                                  actionButton("myButton3", "Filter"),
                                  width = 3
                              ),
                              mainPanel(
                                  DT::dataTableOutput("vector")
                              )
                          )
                 )
                 
)
server <- function(input, output) {
    
    regional_data_new <- filter(regional_data, period == 2019)
    provincial_data_new <- filter(provincial_data, period == 2019)
    output$selected_year <- renderText({ 
        "2019"
    })
    output$mymap <- renderLeaflet({
        leaflet() %>% setView(lng = 121.7740173, lat = 12.8797207, zoom = 3) %>% addTiles()
        leaflet(data = regional_data_new) %>% addTiles() %>%
            addCircleMarkers(~long, ~lat, popup = ~as.character(html), label = ~as.character(region_name),
                             radius = ~ifelse(regional_data_new$Microscopy_Total_Positive > 100, regional_data_new$Microscopy_Total_Positive * 0.1,(regional_data_new$Microscopy_Total_Positive + 1) * 1),
                             color = "red",
                             stroke = FALSE, fillOpacity = 0.5
            )  %>% addCircleMarkers(~long, ~lat, popup = ~as.character(html), label = ~as.character(region_name),
                                    radius = ~ifelse(regional_data_new$RDT_Total_Positive > 100, regional_data_new$RDT_Total_Positive * 0.1, (regional_data_new$RDT_Total_Positive + 1) * 1),
                                    color = "yellow",
                                    stroke = FALSE, fillOpacity = 0.5
            )
    })
    output$mymap2 <- renderLeaflet({
        leaflet() %>% setView(lng = 121.7740173, lat = 12.8797207, zoom = 3) %>% addTiles()
        leaflet(data = regional_data_new) %>% addTiles() %>%
            addCircleMarkers(~long, ~lat, popup = ~as.character(html), label = ~as.character(region_name),
                             radius = ~ifelse(regional_data_new$Microscopy_Total_Positive > 100, regional_data_new$Microscopy_Total_Positive * 0.1,(regional_data_new$Microscopy_Total_Positive + 1) * 1),
                             color = "red",
                             stroke = FALSE, fillOpacity = 0.5
            )  %>% addCircleMarkers(~long, ~lat, popup = ~as.character(html), label = ~as.character(region_name),
                                    radius = ~ifelse(regional_data_new$RDT_Total_Positive > 100, regional_data_new$RDT_Total_Positive * 0.1, (regional_data_new$RDT_Total_Positive + 1) * 1),
                                    color = "yellow",
                                    stroke = FALSE, fillOpacity = 0.5
            )
    })
    observeEvent(input$input_year,{
        regional_data_new <- filter(regional_data, period == input$input_year)
        output$selected_year<- renderText({ 
            input$input_year
        })
        output$mymap <- renderLeaflet({
            leaflet() %>% setView(lng = 121.7740173, lat = 12.8797207, zoom = 3) %>% addTiles()
            leaflet(data = regional_data_new) %>% addTiles() %>%
                addCircleMarkers(~long, ~lat, popup = ~as.character(html), label = ~as.character(region_name),
                                 radius = ~ifelse(regional_data_new$Microscopy_Total_Positive > 100, regional_data_new$Microscopy_Total_Positive * 0.1,regional_data_new$Microscopy_Total_Positive * 1 ),
                                 color = "red",
                                 stroke = FALSE, fillOpacity = 0.5
                )  %>% addCircleMarkers(~long + 0.1, ~lat + 0.1, popup = ~as.character(html), label = ~as.character(region_name),
                                        radius = ~ifelse(regional_data_new$RDT_Total_Positive > 100, regional_data_new$RDT_Total_Positive * 0.1, regional_data_new$RDT_Total_Positive * 1),
                                        color = "yellow",
                                        stroke = FALSE, fillOpacity = 0.5
                )
        })
        
        output$table <- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
                                                          selection = "none",options = list(paging=FALSE, searching=FALSE,columnDefs = list(list(visible=FALSE, targets=c(1,3,4,5,6,7,8,9,10,11,14,15,16,17,18,19,20,23,24,25,26))),scrollX=TRUE, scrollCollapse=TRUE),{
                                                              regional_data_new
                                                          }))
    })
    
    
    observeEvent(input$input_year2,{
        regional_data_new <- filter(regional_data, period == input$input_year)
        output$selected_year_1<- renderText({ 
            input$input_year2
        })
        
        output$regional <- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
                                                             selection = "none",options = list(paging=FALSE, searching=FALSE, columnDefs = list(list(visible=FALSE, targets=c(1,3,4,5,6,25,26))),scrollX=TRUE, scrollCollapse=TRUE),{
                                                                 regional_data_new
                                                             }))
        
        output$provincial<- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
                                                              selection = "none",options = list(paging=FALSE, columnDefs = list(list(visible=FALSE, targets=c(1,2,4,5,26,27,28,29))),scrollX=TRUE, scrollCollapse=TRUE),{
                                                                  provincial_data_new
                                                              }))
        
    })
    observeEvent(input$input_region,{
        provincial_data_new <- filter(provincial_data, region_name== input$input_region)
        #output$selected_year_1<- renderText({ 
        #  input$input_year2
        # })
        
        output$regional <- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
                                                             selection = "none",options = list(paging=FALSE, searching=FALSE, columnDefs = list(list(visible=FALSE, targets=c(1,3,4,5,6,25,26))),scrollX=TRUE, scrollCollapse=TRUE),{
                                                                 regional_data_new
                                                             }))
        
        output$provincial<- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
                                                              selection = "none",options = list(paging=FALSE, columnDefs = list(list(visible=FALSE, targets=c(1,2,4,5,26,27,28,29))),scrollX=TRUE, scrollCollapse=TRUE),{
                                                                  provincial_data_new
                                                              }))
        
    })
    output$table <- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
                                                      selection = "none",options = list(paging=FALSE, searching=FALSE,columnDefs = list(list(visible=FALSE, targets=c(1,3,4,5,6,7,8,9,10,11,14,15,16,17,18,19,20,23,24,25,26))),scrollX=TRUE, scrollCollapse=TRUE),{
                                                          regional_data_new
                                                      }))
    
    
    output$regional <- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
                                                         selection = "none",options = list(paging=FALSE, searching=FALSE, columnDefs = list(list(visible=FALSE, targets=c(1,3,4,5,6,25,26))),scrollX=TRUE, scrollCollapse=TRUE),{
                                                             regional_data_new
                                                         }))
    output$provincial<- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
                                                          selection = "none",options = list(paging=FALSE, columnDefs = list(list(visible=FALSE, targets=c(1,2,4,5,26,27,28,29))),scrollX=TRUE, scrollCollapse=TRUE),{
                                                              provincial_data_new
                                                          }))
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

