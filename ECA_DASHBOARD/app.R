#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(jsonlite)
library(dplyr)
library(shinythemes)
library(leaflet)
ecamm <- jsonlite::fromJSON("http://ecamm.datahub.ph/API/?type=ecamm")
participants <- jsonlite::fromJSON("http://ecamm.datahub.ph/API/?type=participants")
country <- jsonlite::fromJSON("http://ecamm.datahub.ph/API/?type=country")
map <- jsonlite::fromJSON("http://ecamm.datahub.ph/API/?type=map")
country_list = country$country;
# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("sandstone"),"ECAMM Dashboard",
                 tabPanel("Summary",
                          sidebarLayout(
                              
                              sidebarPanel(
                                  h4("What is ECAMM?"),
                                  p("The WHO External Competence Assessment of Malaria Microscopists (ECAMM) objectively and formally assesses microscopists competence in parasite detection, species identification and parasite quantification. At the end of each assessment, microscopists who achieve competency Level 1 or Level 2 are given certification while microscopists who achieve competency Level 3 or 4 are given a certificate of participation. Certificate of competency is valid for 3 years. "),
                                  h4(""),
                                  em("For information regarding the site, please email: Glenda Gonzales (gonzalesg@who.int)"),
                                  width = 4
                              ),
                              mainPanel(
                                  h4("Distribution of Level 1 and Level 2 Microscopists"),
                                  hr(),
                                  leafletOutput("mymap"),
                                  hr(),
                                  tableOutput("table")
                              )
                          )
                          
                 ),
                 tabPanel("ECAMM",
                          
                          sidebarLayout(
                              
                              sidebarPanel(
                                  actionButton("myButton2", "Display All"),
                                  hr(),
                                  selectInput("input1", "Select Year", choices = 2020:2009),
                                  radioButtons("input2", "Choose Region", choices = c("AFRO","EMRO","SEARO","WPRO")),
                                  actionButton("myButton", "Filter"),
                                  width = 3
                              ),
                              mainPanel(
                                  DT::dataTableOutput("ecamm")
                              )
                          )
                 ),
                 tabPanel("Microscopists",
                          sidebarLayout(
                              
                              sidebarPanel(
                                  actionButton("myButton4", "Display All"),
                                  hr(),
                                  selectInput("country_input","Select Nationality", choices = country_list),
                                  selectInput("input3", "Select Year", choices = 2020:2009),
                                  radioButtons("input4", "Choose Level", choices = c("Level 1","Level 2")),
                                  actionButton("myButton3", "Filter"),
                                  width = 3
                              ),
                              mainPanel(
                                  DT::dataTableOutput("participants")
                              )
                          )
                 ),
                 tabPanel("Country",
                          sidebarLayout(
                              
                              sidebarPanel(
                                  h2("Country Profile"),
                                  hr(),
                                  selectInput("country_input_1","Select Country", choices = country_list),
                                  actionButton("country_button", "Country Profile"),
                                  width = 3
                              ),
                              mainPanel(
                                  h2(textOutput("selected_country")),
                                  p("This section displays the list of ECAMM conducted and the microscopists that has a valid Level 1 or Level 2 certificate."),
                                  hr(),
                                  h4("Distribution of Level 1 and 2 Microscopists"),
                                  leafletOutput("mymap2"),
                                  hr(),
                                  #strong("Details for ",textOutput("selected_country")),
                                  h5("List of ECAMMs conducted"),
                                  DT::dataTableOutput("country_eca"),
                                  hr(),
                                  h5("List of microscopists with valid certificates, Level 1 and 2"),
                                  DT::dataTableOutput("country_participants")
                                  
                              )
                          )
                 )
                 
)





server <- function(input, output) {
    #-27.467331464 153.02333324, 12.8797207, 121.7740173
    output$mymap <- renderLeaflet({
        mapdata <- filter(map,certificate_status == "valid")
        leaflet() %>% setView(lng = 121.7740173, lat = 12.8797207, zoom = 3) %>% addTiles()
        #pal <- colorFactor(c("navy", "brown"), domain = c("Level 1", "Level 2"))
        leafIcons <- icons(
            iconUrl = ifelse(mapdata$ecamm_level == "Level 1",
                             "http://ecamm.datahub.ph/images/number_1.png",
                             "http://ecamm.datahub.ph/images/number_2.png"
            ),
            iconWidth = 20, iconHeight = 20,
            iconAnchorX = 0, iconAnchorY = 0)
        html_legend <- "<img src='http://ecamm.datahub.ph/images/number_1.png'>Level 1<br/>
<img src='http://ecamm.datahub.ph/images/number_2.png'>Level 2"
        
        leaflet(data = mapdata) %>% addTiles() %>%
            addMarkers(~long, ~lat, icon = leafIcons,  popup = ~as.character(assignment), label = ~as.character(ecamm_level), clusterOptions = markerClusterOptions()) %>% addControl(html = html_legend, position = "topright")
        # leaflet(data = mapdata) %>% addTiles() %>%
        #   addCircleMarkers(~long, ~lat, popup = ~as.character(assignment), label = ~as.character(ecamm_level),
        #                   radius = ~ifelse(ecamm_level == "Level 2", 6, 7),
        #                   color = ~pal(ecamm_level),
        #                   stroke = FALSE, fillOpacity = 0.5,
        #                   clusterOptions = markerClusterOptions())
    })
    
    output$mymap2 <- renderLeaflet({
        leaflet() %>% setView(lng = 134.489563, lat = -25.734968, zoom = 4) %>% addTiles()
    })
    output$selected_country <- renderText({ 
        "AUSTRALIA"
    })
    
    
    output$ecamm <- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
                                                      filter = "top", selection = "none",options = list(columnDefs = list(list(visible=FALSE, targets=c(1)))),{
                                                          ecamm
                                                      }))
    #output$ecammdata <- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
    #                                                       filter = "top", selection = "single", {
    #  ecamm
    #}))
    output$participants <- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
                                                             filter = "top", selection = "single",options = list(columnDefs = list(list(visible=FALSE, targets=c(1,7))),scrollX=TRUE, scrollCollapse=TRUE),{
                                                                 participants
                                                             }))
    country_eca_data <- filter(ecamm, country == "AUSTRALIA")
    output$country_eca <- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
                                                            selection = "single",options = list(columnDefs = list(list(visible=FALSE, targets=c(1,7))),scrollX=TRUE, scrollCollapse=TRUE),{
                                                                country_eca_data
                                                            }))
    
    country_participant_data <- filter(participants, nationality == "AUSTRALIA", certificate_status == "valid")
    output$country_participants <- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
                                                                     selection = "single",options = list(columnDefs = list(list(visible=FALSE, targets=c(1,7))),scrollX=TRUE, scrollCollapse=TRUE),{
                                                                         country_participant_data
                                                                     }))
    
    observeEvent(input$myButton, {
        f_df <- filter(ecamm, year == input$input1, region == input$input2)
        output$ecamm <- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
                                                          filter = "top", selection = "single",options = list(columnDefs = list(list(visible=FALSE, targets=c(1)))),{
                                                              f_df
                                                          }))
        
        
    })
    observeEvent(input$myButton2, {
        #f_df <- filter(ecamm, year == input$input1, region == input$input2)
        output$ecamm <- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
                                                          filter = "top", selection = "single",options = list(columnDefs = list(list(visible=FALSE, targets=c(1)))),{
                                                              ecamm
                                                          }))
        
        
    })
    observeEvent(input$myButton3, {
        fp_df <- filter(participants, ecamm_year == input$input3, ecamm_level == input$input4, nationality == input$country_input)
        output$participants <- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
                                                                 filter = "top", selection = "single",options = list(columnDefs = list(list(visible=FALSE, targets=c(1,7))),scrollX=TRUE, scrollCollapse=TRUE),{
                                                                     fp_df
                                                                 }))
        
        
    })
    observeEvent(input$myButton4, {
        #fp_df <- filter(participants, year == input$input3, level == input$input4)
        output$participants <- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
                                                                 filter = "top", selection = "single",options = list(columnDefs = list(list(visible=FALSE, targets=c(1,7))),scrollX=TRUE, scrollCollapse=TRUE),{
                                                                     participants
                                                                 }))
        
        
    })
    observeEvent(input$country_button, {
        country_eca_data <- filter(ecamm, country == input$country_input_1)
        output$country_eca <- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
                                                                selection = "single",options = list(columnDefs = list(list(visible=FALSE, targets=c(1,7))),scrollX=TRUE, scrollCollapse=TRUE),{
                                                                    country_eca_data
                                                                }))
        
        
        country_participant_data <- filter(participants, nationality == input$country_input_1, certificate_status == "valid")
        map_participant_data <- filter(map, nationality == input$country_input_1, certificate_status == "valid")
        #longvalue <- summarise(map_participant_data,sum(long))
        #View(longvalue)
        #View(country_participant_data)
        output$country_participants <- DT::renderDataTable(DT::datatable(class = "display nowrap compact",
                                                                         selection = "single",options = list(columnDefs = list(list(visible=FALSE, targets=c(1,7))),scrollX=TRUE, scrollCollapse=TRUE),{
                                                                             country_participant_data
                                                                         }))
        output$selected_country <- renderText({ 
            input$country_input_1
        })
        lng1 <- filter(country, country == input$country_input_1) %>% select(lng)
        lat1 <- filter(country, country == input$country_input_1) %>% select(lat)
        zoom1 <- filter(country, country == input$country_input_1) %>% select(zoom)
        
        output$mymap2 <- renderLeaflet({
            leaflet() %>% setView(lng = lng1$lng, lat = lat1$lat, zoom = zoom1$zoom) %>% addTiles()
            pal <- colorFactor(c("navy", "green"), domain = c("Level 1", "Level 2"))
            leaflet(data = map_participant_data) %>% addTiles() %>%
                addCircleMarkers(~long, ~lat, popup = ~as.character(assignment), label = ~as.character(ecamm_level),
                                 radius = ~ifelse(ecamm_level == "Level 2", 6, 7),
                                 color = ~pal(ecamm_level),
                                 stroke = FALSE, fillOpacity = 0.5)
        })
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

