# Install the packages to R

install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("tidyverse")
install.packages("readr")
install.packages("maps")
install.packages("tidyr")
install.packages("leaflet")
install.packages("plotly")
install.packages("dplyr")
install.packages("shiny")
install.packages("wordcloud")
install.packages("highcharter")
install.packages("shinydashboard")

#load the installed R libraries

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(leaflet)
library(highcharter)
library(plotly)
library(ggplot2)
library(readr)
library(wordcloud)
library(RColorBrewer)

#Reading Zomato CSV data to R
zomato_data <- read.csv("ZomatoData.csv")


#Filter out the Data with only the columns required.
zomato <- zomato_data %>% select(1, 2, 3, 4, 5, 8, 9, 10, 11, 12, 17, 18, 19, 22)

#Filter out the longitude and latitude columns and remove those which have zeros in the rows
zomato <- zomato %>% filter(Latitude !=0, Longitude != 0)


# Factor the column Price range 
zomato$Price.range <- factor(zomato$Price.range, levels = c(1,2,3,4), labels = c("Easily Affordable", "Affordable", 
                                                                                 "Expensive", "Highly Expensive"), ordered = TRUE)


#Displaying the component UI in Dashboard of Shiny

ui <- dashboardPage(skin = "red",
                    
          #Title of header in dashboard                  
          dashboardHeader(title = "Zomato Restaurants", titleWidth = 215),
          
          #Layout is defined for sidebar
          dashboardSidebar(
            sidebarMenu(
              # Items in Menu for Side Bar Menu
              menuItem("Welcome", tabName = "Welcome", icon = icon("home", lib = "font-awesome")),
              menuItem("Restaurants & Cuisines", tabName = "cuisines", icon = icon("store", lib = "font-awesome")),
              menuItem("Country", tabName = "country", icon = icon("fas fa-map-marker-alt", lib = "font-awesome")),
              menuItem("City", tabName = "city", icon = icon("fas fa-map-marker-alt", lib = "font-awesome")) 
            )
          ),
          
          #Layout of Main Page which is Dashboard Body
          dashboardBody(
            tabItems(
              
              #Tab 1 
              tabItem(tabName = "Welcome", 
                      #Header on welcome page
                      strong(em(h1(align = 'center', "Identify your Favourite Cuisines & Restaurants"))), 
                      tags$head(tags$style('h1 {color:blue;}')),
                      align = 'center', img(src = "https://image.freepik.com/free-photo/restaurant-hall-with-red-brick-walls-wooden-tables-pipes-ceiling_140725-8504.jpg", width= 700, height = 420),
                      p("Welcome to Zomato's World to discover your favourite foods and restaurants. You will learn a lot from the menus and learn about the different cuisines and cities using this 
          application for ordering food or eating at the restaurants. Apps like Zomato allow us to accomplish this goal effectively. Zomato, as we all know, is one of the world 's
          basic online food ordering applications.", style = "font-family: 'times'; font-size:20px") # Content in the paragraph under the image
              ),
              
              #Tab 2
              tabItem(tabName = "cuisines", 
                      #Giving out two headers for two boxes
                      fluidRow(
                        #Giving headers for the boxes
                        box(title = "Restaurants ranged in various Countries", status = "primary", solidHeader = TRUE, width = 13, height = 520,
                            leafletOutput(outputId = "worldmap"), strong(p("*** Tap upon on Circle Markers to view the restaurant details. ***", style = "font-family: 'times'; font-size:16px")))
                      ),
                      
                      fluidRow(
                        #Second Header for Tree Map
                        box(title = "A Tree map with all the available cuisines", status = "info", solidHeader = TRUE, width = 13, height = 490, 
                            highchartOutput("treeMap"), strong(p("*** Hover across a tile to discover the number of restaurants where the variety of cuisine is served. ***", 
                                                                 style = "font-family: 'times'; font-size:16px")))
                      )
              ),
              
              #Tab 3
              tabItem(tabName = "country",
                      fluidRow(
                        #defining the value boxes in the row
                        valueBoxOutput("country"),
                        valueBoxOutput("cost"),
                        valueBoxOutput("rating")
                      ),
                      fluidRow(
                        #Showing the layout of the column.
                        column(width = 4,  
                               
                               box(title = "Select from the Options below", status = "primary", solidHeader = TRUE, height = 535, width = NULL,
                                   strong("*** Note : Select a Country from the list to know about their Restaurants ***"),  
                                   uiOutput("Select.Country"),
                                   uiOutput("avg.rating"),
                                   uiOutput("avg.price"),
                                   strong(p("*** Note: Hover over the values shown on the bars on the bar chart or the bar to know the Name of the restaurant
                              and its related information.", br(), br(), "*** Note : The Average Price is equal to the currency of the selected Country.",
                                            br(), "Example: Price is in INR if the selected country is INDIA"))                                                   
                               ),
                               #Word Cloud for Types of Cuisines in the country selected
                               box(title = "A Word cloud of Types of Cuisines in the Country selected", height = 505, status = "info", 
                                   solidHeader = TRUE, width = NULL, plotOutput(outputId = "wordcloud"))
                        ),
                        column(width = 8,
                               box(status = "info", width = NULL, solidHeader = FALSE, height = 510, plotlyOutput("price_plot")),
                               box(status = "info", width = NULL, solidHeader = FALSE, height = 510, plotlyOutput("rating_plot"))
                        ))
              ),
              
              #Tab 4
              tabItem(tabName = "city",
                      
                      fluidRow(
                        #Giving out values for rows
                        valueBoxOutput("no_of_rest"),
                        valueBoxOutput("avg_price"),
                        valueBoxOutput("agg_rating")
                      ),
                      
                      fluidRow(
                        #Giving the Layout of the Column
                        column(width = 4,  
                               #Giving the content of box in the column
                               box(title = "Select from the Options below", status="primary", width = NULL, solidHeader = TRUE, height = 870,                        
                                   strong("*** Note : Select a Country and City to know about their restaurants ***"),                            
                                   uiOutput("Country1"),
                                   uiOutput("city1"),
                                   uiOutput("Aggregate.rating1"),
                                   uiOutput("Avg.price1"),
                                   strong(p("*** Note : Hover over the values shown on the bars on the bar chart or the bar to know the Name of the restaurant
                              and its related information.", br(), br(), "*** Note : The Average Price is equal to the currency of the selected Country.",
                                            br(), "Example: Price is in INR if the selected country is INDIA"))                                                    
                               )
                        ),
                        column(width = 8,
                               box(status = "info", width = NULL, solidHeader = FALSE, plotlyOutput("city_price")),
                               box(status = "info", width = NULL, solidHeader = FALSE, plotlyOutput("city_rating"))
                        )
                      )
                    )
                  )
                )
              )

  #The server of UI is defined here
  server <- function(input, output) {
    
    #Restaurants & Cuisines Tab
  
    #Output on world map
    output$worldmap <- renderLeaflet({
      zomato = zomato %>% mutate(Popup.info = paste("<h3 style = 'color: blue'> Restaurant Information </h3>", 
                                                    "<b>Name of the Restaurant: </b>", Restaurant.Name, "</b>", "<br>", "<b>Address: </b>", Address, "<br>", "<b>Country: </b>", Country, "<br>", 
                                                    "<b>City: </b>", City, "<br>", "<b>Cuisines: </b>", Cuisines, "<br>", "<b>Aggregate Rating: </b>", Aggregate.rating))
      
      # Color of the restaurants on map
      paint <- colorRampPalette(brewer.pal(9, 'Reds'))(length(zomato))
      pal <- colorFactor(palette = 'Reds', zomato$Country)

      leaflet(zomato)%>%
        addTiles()%>%
        addProviderTiles("Esri.NatGeoWorldMap")%>%
        addCircleMarkers(data = zomato, lng = ~Longitude , lat = ~Latitude, radius = 3, popup = ~Popup.info, color = ~pal(Country))%>%
        addLegend(position = "bottomright", # position of the legend on map
                  pal = pal, # pallet object in which various colors are given
                  values = zomato$Country, # To use the column values to get the color pallet object
                  title = "Countries", # To give the Title of the legend Country
                  opacity = 1)
    })
    
    
    # Tree map 
    
    #output of tree map in Restaurants & Cuisines Menu
    output$treeMap <- renderHighchart ({ 
      cuisines <- zomato %>%
        select(Country, Cuisines) %>%
        mutate(Cuisines=strsplit(as.character(Cuisines), ", ")) %>%
        unnest(Cuisines) %>%
        group_by(Cuisines) %>%
        summarise(Counts = n()) %>%
        arrange(desc(Counts))
      
      hchart(cuisines, "treemap", hcaes(x = Cuisines, value = Counts, color = Counts))%>%
        hc_tooltip(pointFormat = "<b>Type of Cuisine </b>: {point.Cuisines} <br> <b> Total Number of Restaurants </b>: {point.Counts}")
      
      
    })
    
    # Word cloud 
    
    #output of word cloud in Country Menu
    output$wordcloud <- renderPlot ({ 
      
      zomato_temp <- filter(zomato, Country == input$Select.Country)
      cuisines = c()
      
      #A loop for having all the cuisines 
      for(cuisine in zomato_temp$Cuisines) {
        list.x = trimws(unlist(strsplit(cuisine, ",")))
        cuisines = c(cuisines, list.x)
      }
      clist = unique(cuisines)
      flist = c()
      for(c in clist) {
        list.x = length(which(cuisines == c))
        flist = c(flist, list.x)
      }
      wordcloud(clist, flist, random.order = TRUE, use.r.layout = FALSE, scale = c(2.5,.5), rot.per = 0.35, random.color = FALSE,
                 colors = brewer.pal(8, "Dark2"))
      
    })
    
    
    #output of Total Number of Restaurants in Country Menu
    output$country <- renderValueBox({     
      zomato_temp <- filter(zomato, Country == input$Select.Country)     
      Rest <- nrow(zomato_temp)          
      
      valueBox(       
        formatC(Rest, format = "d", big.mark = ',')       
        ,paste('Number of Restaurants in this Country')       
        ,icon = icon("menu-hamburger",lib='glyphicon')       
        ,color = "purple")   
    }) 
    
    
    #output of Average Price of the Restaurants in Country menu
    output$cost <- renderValueBox({     
      zomato_temp <- filter(zomato, Country == input$Select.Country)     
      Avg.cost <- mean(zomato_temp$Average.Cost.for.two)          
      
      valueBox(       
        formatC(Avg.cost, format = "f", big.mark = ',', digits = 2)       
        ,'Average Price of Restaurants in this Country'
        ,icon = icon("coins",lib='font-awesome')
        ,color = "blue")   
      
    })
    
    
    
    #output of Aggregate Rating of Restaurants in Country menu
    output$rating <- renderValueBox({          
      zomato_temp <- filter(zomato, Country == input$Select.Country)     
      avg.rating <- mean(zomato_temp$Aggregate.rating)          
      
      valueBox(       
        formatC(avg.rating, format="f", big.mark=',', digits = 1)       
        ,paste('Average Rating of Restaurants in this Country')       
        ,icon = icon("star",lib='glyphicon')       
        ,color = "yellow")        
    }) 
    
    
    #output of List for Country selection in Country Menu
    output$Select.Country <- renderUI({     
      selectInput("Select.Country", "Select a Country",                
                  choices = as.character(unique(zomato$Country)),               
                  selected = "Singapore")   
    })   
    
    
    #output of Slider for Aggregate rating range in Country Menu
    output$avg.rating <- renderUI({     
     sliderInput(inputId  = "avg.rating", 
                label = "Aggregate Rating Range", 
                 min = min(zomato$Aggregate.rating),
                  max = max(zomato$Aggregate.rating),
                  value = c(min,max)
      )
    })
    
    
    #output of slider for price range in Country Menu
    output$avg.price <- renderUI({     
      sliderInput(inputId  = "avg.price", label = "Price Range", 
                  min = min(zomato$Average.Cost.for.two),
                  max = max(zomato$Average.Cost.for.two),
                  pre = "$",
                  step = 80000,
                  sep = ',',
                  value = c(min, max)
      )
      
    })
    
    
    
    #output of bar plot for Average Price of the Restaurants in Country Menu
    output$price_plot <- renderPlotly({ 
      zomato_temp <- filter(zomato, Country == input$Select.Country)
      #zomato_temp <- filter(zomato_temp, Price.range == input$avg.price)
      #zomato_temp <- filter(zomato_temp, Aggregate.rating == input$avg.rating)
      text <- paste("</br>Name of Restaurant: ", zomato_temp$Restaurant.Name,
                    "</br>Address: ", zomato_temp$Address, 
                    "</br>State: ", zomato_temp$City,
                    "</br>types of Cuisines: ", zomato_temp$Cuisines, 
                    "</br> rating" , zomato_temp$Aggregate.rating,
                     fill = 'black')
      
      plot3 <- ggplot(zomato_temp, aes(x = Restaurant.Name, y = Average.Cost.for.two,                                  
                                      text = paste("</br>Restaurant Name: ", Restaurant.Name, 
                                                   "</br>Address: ", Address, 
                                                   "</br>City: ", City,  
                                                   "</br>Cuisines: ", Cuisines 
                                                 ))) +       
        labs (title = "Restaurants Price Range", x = "Restaurant", y = "Average Price for Two Customers") + 
        geom_bar(stat = 'identity', aes(fill = Country)) +
        geom_text(aes(label = Average.Cost.for.two), vjust = 0) +
        scale_fill_manual(values = "purple") +
        theme(axis.text.x = element_blank(),
              legend.position = "none",
              panel.grid.minor.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              plot.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "white"))
      
      plot3temp <- ggplotly(plot3, tooltip = c("text"))               
      plot3temp 
      
    })
    
    
    #output of bar plot on Aggregate Rating of Restaurants in Country Menu
    output$rating_plot <- renderPlotly({     
      zomato_temp <- filter(zomato, Country == input$Select.Country)
      #zomato_temp <- filter(zomato_temp, Price.range == input$avg.price)
      #zomato_temp <- filter(zomato_temp, Aggregate.rating == input$avg.rating)
      
      text <- paste("</br>Restaurant Name: ", zomato_temp$Restaurant.Name,
                    "</br>Address: ",zomato_temp$Address,
                    "</br>State: ", zomato_temp$City,  
                    "</br>Cuisines: ", zomato_temp$Cuisines, fill = 'black')     
      
      plot4 <- ggplot(zomato_temp, aes(x = Restaurant.Name, y = Aggregate.rating, text = paste("</br>Restaurant Name: ", Restaurant.Name, 
                                                                                                 "</br>Address: ", Address, 
                                                                                                 "</br>City: ", City,  
                                                                                                 "</br>Cuisines: ", Cuisines))) +        
        labs (title = "Aggregate Ratings of Restaurants", x = "Restaurant", y = "Aggregate Ratings") +
        geom_bar(stat = 'identity', aes(fill = Country)) +
        geom_text(aes(label = Aggregate.rating), vjust=0) +
        scale_fill_manual(values = "cyan") +
        theme(axis.text.x=element_blank(),
              legend.position = "none",
              panel.grid.minor.y = element_blank(),
              panel.grid.major.y = element_blank(), 
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              plot.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "white"))
      
      plot4temp <- ggplotly(plot4, tooltip = c("text"))     
      plot4temp    
      
    })    
    
    
    
    # City Menu Charts
    
    
    #output for Selecting a Country in City Menu
    output$Country1 <- renderUI({     
      selectInput("Country1", "Select a Country: ",                
                  choices = as.character(unique(zomato$Country)),               
                  selected = "India")   
    })   
    
    
    #output for Selecting a City in City Menu
    output$city1 <- renderUI({     
      selectInput("city1", "Select a City: ",
                  choices = (zomato[zomato$Country == input$Country1, "City"]),
                  selected = (zomato[zomato$Country == input$Country1, "City"][0]))
    })
    
    
    
    #output for Aggregate Rating range in City Menu
    output$Aggregate.rating1 <- renderUI({     
      sliderInput("Aggregate.rating1", "Aggregate Rating Range:", 
                  min = min(zomato$Aggregate.rating),
                  max = max(zomato$Aggregate.rating),
                  value = c(min, max)
      )
      
    })
    
    
    #output for Price range in City Menu
    output$Avg.price1 <- renderUI({     
      sliderInput("Avg.price1", "Price Range:", 
                  min = min(zomato$Average.Cost.for.two),
                  max = max(zomato$Average.Cost.for.two),
                  value = c(min, max)
      )
      
    })
    
    #output for Total Number of Restaurants in City Menu
    output$no_of_rest <- renderValueBox({     
      zomato_temp <- filter(zomato, City == input$city1) 
      zomato_temp <- filter(zomato, Country == input$Country1)     
      Rest <- nrow(zomato_temp)          
      
      valueBox(       
        formatC(Rest, format="d", big.mark=',')       
        ,paste('Number of Restaurants in the City Selected')       
        ,icon = icon("stats",lib='glyphicon')       
        ,color = "purple")   
    }) 
    
    
    
    #output for Average Price of all Restaurants in City Menu
    output$avg_price <- renderValueBox({     
      zomato_temp <- filter(zomato, City == input$city1)  
      zomato_temp <- filter(zomato, Country == input$Country1)     
      Avg.cost <- mean(zomato_temp$Average.Cost.for.two)          
      
      valueBox(       
        formatC(Avg.cost, format="f", big.mark=',',digits = 2)       
        ,paste('Average Price of Restaurants of the City Selected')
        ,icon = icon("coins",lib='font-awesome')
        ,color = "blue")   
      
    })
    
    
    
    #output for Average Aggregate Ratings of Restaurants in City Menu
    output$agg_rating <- renderValueBox({      
      zomato_temp <- filter(zomato, City == input$city1) 
      zomato_temp <- filter(zomato, Country == input$Country1)     
      Agger.rating <- mean(zomato_temp$Aggregate.rating)          
      
      valueBox(       
        formatC(Agger.rating, format="f", big.mark=',', digits = 1)       
        ,paste('Average Rating of Restaurants in City Selected')       
        ,icon = icon("star",lib='glyphicon')       
        ,color = "yellow")        
    }) 
    
    
    #output for Bar Plot which Shows the Average Price of Restaurants in City Menu
    output$city_price <- renderPlotly({     
      zomato_temp <- filter(zomato, Country == input$Country1)     
      zomato_temp <- filter(zomato, City == input$city1)
      #zomato_temp <- filter(zomato,  Aggregate.rating == input$Aggregate.rating)
      #zomato_temp <- filter(zomato,  Average.rating == input$Avg.rating)
      
      text <- paste("</br>Restaurant Name: ", zomato_temp$Restaurant.Name,
                    "</br>Address: ", zomato_temp$Address,
                    "</br>Cuisines: ", zomato_temp$Cuisines, fill = 'black')
      
      plot1 <- ggplot(zomato_temp, aes(x = Restaurant.Name, y = Average.Cost.for.two,                                  
                                      text=paste("</br>Restaurant Name: ", Restaurant.Name, "</br>Address: ", Address, "</br>Cuisines: ", Cuisines))) +       
        labs (title = "Restaurants Price Range", x = "Restaurant", y = "Average Price for Two Customers") + 
        geom_bar(stat='identity', aes(fill = Country)) +
        geom_text(aes(label = Average.Cost.for.two), vjust = 0) +
        scale_fill_manual(values = "cyan") +
        theme(axis.text.x = element_blank(),
              legend.position = "none",
              panel.grid.minor.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              plot.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "white"))
      
      plot1temp <- ggplotly(plot1, tooltip = c("text"))               
      plot1temp
      
    })
    
    
    #output for Bar Plot which Shows Aggregate Ratings of the Restaurants in City Menu
    output$city_rating <- renderPlotly({     
      zomato_temp <- filter(zomato, Country == input$Country1)
      zomato_temp <- filter(zomato, City == input$city1)
      
      text <- paste("</br>Restaurant Name: ", zomato_temp$Restaurant.Name, "</br>Address: ",zomato_temp$Address, 
                    "</br>Cuisines: ", zomato_temp$Cuisines, fill = 'black')          
      
      
      plot2 <- ggplot(zomato_temp, aes(x = Restaurant.Name, y = Aggregate.rating, text=paste("</br>Restaurant Name: ", Restaurant.Name, "</br>Address: ", Address, "</br>Types of Cuisines: ", Cuisines)))+        
        labs (title = "Aggregate Rating of Restaurant", x = "Restaurant", y = "Aggregate Rating") +
        geom_bar(stat = 'identity', aes(fill = Country)) +
        geom_text(aes(label = Aggregate.rating), vjust = 0) +
        scale_fill_manual(values = "purple") +
        theme(axis.text.x=element_blank(),
              legend.position = "none",
              panel.grid.minor.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              plot.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "white"))
      
      plot2temp <- ggplotly(plot2, tooltip=c("text"))     
      plot2temp       
    })    
  }
  
  #Final call for the Shiny App
  shinyApp(ui, server)