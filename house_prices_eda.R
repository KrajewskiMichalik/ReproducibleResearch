houses <- readRDS("data/houses.rds")

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggpubr)
library(ggridges)
library(corrplot)
library(leaflet)
library(htmltools)

options(scipen=999)

ui <- dashboardPage(
  
  dashboardHeader(title = "House Sales in King County, USA", titleWidth = 350),
  
  ## Sidebar content
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("info-circle")),
      menuItem("Dataset", tabName = "data", icon = icon("database")),
      menuItem("Target variable", tabName = "target", icon = icon("crosshairs")),
      menuItem("Independent variables", tabName = "regressors", icon = icon("lightbulb")),
      menuItem("Map", tabName = "map", icon = icon("map-marker-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      # dashboard content
      tabItem(
        tabName = "introduction",
        
        
        tabsetPanel(
          tabPanel("Welcome page",br(),
                   h1(strong("House Sales in King County, USA - Exploratory Data Analysis"), align = "center", style = "font-size:300%"), br(), br(),
                   htmlOutput("picture", align = "center"), br(), br(), br(), br(), br(),
                   h3("Prepared by: Krajewski Maksymilian & Michalik Dominik", align = "right"),
          ),
          tabPanel("Introduction",br(),
                   h1(strong("General information"), align = "center", style = "font-size:250%"), br(),
                   br(),
                   h3(HTML("<ul><li>Kaggle repository as a source of data</li></ul>")),
                   h3(HTML("<ul><li>Dataset consists of sale prices of houses located in King County (state of Washington) with characteristic of each property</li></ul>")),
                   h3(HTML("<ul><li>Data has been pre-processed before providing to Shiny (exclusion of one incorrect observations, removal of duplicates, inclusion of one additional variable from external data source)</li></ul>")),
                   h3(HTML("<ul><li>Finally 21 435 observations and 17 variables with no missing values</li></ul>"))
          ))
        
        
      ),
      # dashboard content
      tabItem(tabName = "data",
              h1(strong("Dataset details"), align = "center", style = "font-size:250%"), br(), tags$hr(style="border-color: grey;"), br(),
              sidebarLayout(
                sidebarPanel(
                  checkboxGroupInput("show_vars", "Variables to show:",
                                     names(houses), selected = names(houses)[c(1:6, 12, 17)]),
                  width = 2
                ),
                mainPanel(
                  DT::dataTableOutput("mytable1"),
                  width = 10, br(), br(),
                  div(style="float:right", downloadButton("downloadData", strong("Download"), style="padding:25px; font-size:150%"))
                )
              )
      ),
      # dashboard content
      tabItem(tabName = "target",
              h1(strong("Target variable - house price"), align = "center", style = "font-size:250%"), br(), tags$hr(style="border-color: grey;"), br(),
              sidebarLayout(
                sidebarPanel(
                  radioButtons("target_types", "Type of graph", choices = c("Density plot", "Histogram", "Both"), selected = "Density plot"),
                  # checkboxGroupInput("target_types", "Type of graph", c("Density plot", "Histogram"),
                  #                    selected = "Density plot"),
                  # br(),
                  # p(strong("Type of graph")),
                  # actionButton("if_dens", "Density plot", width = '49%'),
                  # actionButton("if_hist", "Histogram", width = '49%'), br(),
                  # actionButton("if_both", "Both", width = '98%'),
                  
                  conditionalPanel(
                    condition = "input.target_types == 'Histogram' || input.target_types == 'Both'", br(),
                    numericInput("target_bindt", "Bin width", 50000, min = 10000, max = 150000, step = 10000)
                  ),
                  
                  conditionalPanel(
                    condition = "input.location_target == 'All'", br(),
                    sliderInput("slider_target", "Price range", min(houses$price), max(houses$price), value = c(min(houses$price), max(houses$price)), step = 100000, pre = "$"), br()
                  ),
                  
                  conditionalPanel(
                    condition = "input.location_target == 'Select city'", br(),
                    #display dynamic UI
                    uiOutput("slider"), br()
                  ),
                  
                  radioButtons("location_target", "Location", choices = c("All", "Select city"), selected = "All"),
                  
                  conditionalPanel(
                    condition = "input.location_target == 'Select city'",
                    selectInput("target_city", "City", choices = unique(sort(houses$city), incomparables = FALSE))
                  ),
                  
                  conditionalPanel(
                    condition = "input.target_types == 'Density plot' && input.location_target == 'Select city'", br(),
                    actionButton("add_dens", "Compare by city", width = '100%', icon("info-circle"), 
                                 style="color: #2e6da4; border-color: #2e6da4"), br(), br()
                  ), 
                  
                  br(),
                  colourpicker::colourInput("target_color", "Choose color", value="#FF3333", showColour = "background"),
                  br(),
                  
                  actionButton("target_stats", "Show statistics", width = '100%', icon("info-circle"), 
                               style="color: #2e6da4; border-color: #2e6da4"), br(), br(),
                  
                  conditionalPanel(
                    condition = "input.target_stats == 1", br(),
                    verbatimTextOutput("target_summary")
                  ),
                  
                  width = 2
                  
                ),
                
                mainPanel(
                  
                  conditionalPanel(
                    condition = "input.target_types == 'Density plot' || input.target_types == 'Both'",
                    plotOutput("density_target", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.target_types == 'Both'", 
                    tags$hr(width = '120%', style="border-color: grey;"), br(), 
                    plotOutput("barplot_target", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.target_types == 'Histogram'", 
                    plotOutput("barplot_target3", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.add_dens == 1 && input.target_types == 'Density plot' && input.location_target == 'Select city'",
                    tags$hr(width = '120%', style="border-color: grey;"), br(), 
                    plotOutput("density_cities", height = '700px', width = '120%')
                  )
                )
              )
      ),
      # dashboard content
      tabItem(tabName = "regressors",
              h1(strong("Indepentend variables"), align = "center", style = "font-size:250%"), br(), tags$hr(style="border-color: grey;"), br(),
              sidebarLayout(
                sidebarPanel(
                  radioButtons("regressors_types", "Variable type", choices = c("Continous", "Discrete"), selected = "Discrete"), br(), 
                  
                  conditionalPanel(
                    condition = "input.regressors_types == 'Continous'",
                    selectInput("regressors_con_list", "Variable", choices = c("sqft_living", "sqft_lot", "lat & long"))
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_types == 'Discrete'",
                    selectInput("regressors_num_list", "Variable", choices = c("bedrooms", "bathrooms", "floors", "waterfront", "view", "condition", "grade", "house_renov", "basement"))
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_con_list == 'sqft_lot'", br(),
                    sliderInput("slider_lot", "Land area range", min(houses$sqft_lot), max(houses$sqft_lot), value = c(min(houses$sqft_lot), max(houses$sqft_lot)), step = 10000), br()
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_con_list != 'lat & long'", 
                    checkboxInput("regressors_if_relation", "Show relationship between variable and house price")
                  ),
                  
                  br(),
                  colourpicker::colourInput("regressors_hist_color", "Choose histogram color", value="#99CCFF", showColour = "background"),
                  
                  conditionalPanel(
                    condition = "input.regressors_types == 'Continous' && input.regressors_if_relation == 1",
                    br(),
                    colourpicker::colourInput("regressors_rel_color", "Choose scatter plot color", value="#FF3333", showColour = "background")
                  ),
                  
                  br(), actionButton("regressors_stats", "Show statistics", width = '100%', icon("info-circle"), 
                                     style="color: #2e6da4; border-color: #2e6da4"), br(),
                  
                  conditionalPanel(
                    condition = "input.regressors_stats == 1 && input.regressors_con_list == 'sqft_living' && input.regressors_types == 'Continous'", br(),
                    verbatimTextOutput("reg_stat_sqft_living")
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_stats == 1 && input.regressors_con_list == 'sqft_lot' && input.regressors_types == 'Continous'", br(),
                    verbatimTextOutput("reg_stat_sqft_lot")
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_stats == 1 && input.regressors_con_list == 'lat & long' && input.regressors_types == 'Continous'", br(),
                    verbatimTextOutput("reg_stat_lat_long")
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_stats == 1 && input.regressors_num_list == 'bathrooms' && input.regressors_types == 'Discrete'", br(),
                    verbatimTextOutput("reg_stat_bathrooms")
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_stats == 1 && input.regressors_num_list == 'bedrooms' && input.regressors_types == 'Discrete'", br(),
                    verbatimTextOutput("reg_stat_bedrooms")
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_stats == 1 && input.regressors_num_list == 'condition' && input.regressors_types == 'Discrete'", br(),
                    verbatimTextOutput("reg_stat_condition")
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_stats == 1 && input.regressors_num_list == 'floors' && input.regressors_types == 'Discrete'", br(),
                    verbatimTextOutput("reg_stat_floors")
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_stats == 1 && input.regressors_num_list == 'view' && input.regressors_types == 'Discrete'", br(),
                    verbatimTextOutput("reg_stat_view")
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_stats == 1 && input.regressors_num_list == 'grade' && input.regressors_types == 'Discrete'", br(),
                    verbatimTextOutput("reg_stat_grade")
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_stats == 1 && input.regressors_num_list == 'waterfront' && input.regressors_types == 'Discrete'", br(),
                    verbatimTextOutput("reg_stat_waterfront")
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_stats == 1 && input.regressors_num_list == 'basement' && input.regressors_types == 'Discrete'", br(),
                    verbatimTextOutput("reg_stat_basement")
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_stats == 1 && input.regressors_num_list == 'house_renov' && input.regressors_types == 'Discrete'", br(),
                    verbatimTextOutput("reg_stat_house_renov")
                  ),
                  
                  br(), checkboxInput("regressors_if_corr", "Show correlation plot"),
                  
                  width = 2
                ),
                
                mainPanel(
                  
                  conditionalPanel(
                    condition = "input.regressors_num_list == 'bedrooms' && input.regressors_types == 'Discrete' && input.regressors_if_corr == 0",
                    plotOutput("barplot_bedrooms", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_num_list == 'bedrooms' && input.regressors_types == 'Discrete' && input.regressors_if_relation == 1 && input.regressors_if_corr == 0",
                    br(), tags$hr(width = '120%', style="border-color: grey;"), br(), 
                    plotOutput("relation_bedrooms", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_num_list == 'bathrooms' && input.regressors_types == 'Discrete' && input.regressors_if_corr == 0",
                    plotOutput("barplot_bathrooms", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_num_list == 'bathrooms' && input.regressors_types == 'Discrete' && input.regressors_if_relation == 1 && input.regressors_if_corr == 0",
                    br(), tags$hr(width = '120%', style="border-color: grey;"), br(), 
                    plotOutput("relation_bathrooms", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_num_list == 'floors' && input.regressors_types == 'Discrete' && input.regressors_if_corr == 0",
                    plotOutput("barplot_floors", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_num_list == 'floors' && input.regressors_types == 'Discrete' && input.regressors_if_relation == 1 && input.regressors_if_corr == 0",
                    br(), tags$hr(width = '120%', style="border-color: grey;"), br(), 
                    plotOutput("relation_floors", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_num_list == 'waterfront' && input.regressors_types == 'Discrete' && input.regressors_if_corr == 0",
                    plotOutput("barplot_waterfront", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_num_list == 'waterfront' && input.regressors_types == 'Discrete' && input.regressors_if_relation == 1 && input.regressors_if_corr == 0",
                    br(), tags$hr(width = '120%', style="border-color: grey;"), br(), 
                    plotOutput("relation_waterfront", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_num_list == 'view' && input.regressors_types == 'Discrete' && input.regressors_if_corr == 0",
                    plotOutput("barplot_view", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_num_list == 'view' && input.regressors_types == 'Discrete' && input.regressors_if_relation == 1 && input.regressors_if_corr == 0",
                    br(), tags$hr(width = '120%', style="border-color: grey;"), br(), 
                    plotOutput("relation_view", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_num_list == 'condition' && input.regressors_types == 'Discrete' && input.regressors_if_corr == 0",
                    plotOutput("barplot_condition", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_num_list == 'condition' && input.regressors_types == 'Discrete' && input.regressors_if_relation == 1 && input.regressors_if_corr == 0",
                    br(), tags$hr(width = '120%', style="border-color: grey;"), br(), 
                    plotOutput("relation_condition", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_num_list == 'grade' && input.regressors_types == 'Discrete' && input.regressors_if_corr == 0",
                    plotOutput("barplot_grade", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_num_list == 'grade' && input.regressors_types == 'Discrete' && input.regressors_if_relation == 1 && input.regressors_if_corr == 0",
                    br(), tags$hr(width = '120%', style="border-color: grey;"), br(), 
                    plotOutput("relation_grade", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_num_list == 'house_renov' && input.regressors_types == 'Discrete' && input.regressors_if_corr == 0",
                    plotOutput("barplot_house_renov", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_num_list == 'house_renov' && input.regressors_types == 'Discrete' && input.regressors_if_relation == 1 && input.regressors_if_corr == 0",
                    br(), tags$hr(width = '120%', style="border-color: grey;"), br(), 
                    plotOutput("relation_house_renov", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_num_list == 'basement' && input.regressors_types == 'Discrete' && input.regressors_if_corr == 0",
                    plotOutput("barplot_basement", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_num_list == 'basement' && input.regressors_types == 'Discrete' && input.regressors_if_relation == 1 && input.regressors_if_corr == 0",
                    br(), tags$hr(width = '120%', style="border-color: grey;"), br(), 
                    plotOutput("relation_basement", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_if_corr == 1",
                    plotOutput("regressors_corr_plot", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_con_list == 'sqft_living' && input.regressors_types == 'Continous' && input.regressors_if_corr == 0",
                    plotOutput("barplot_sqft_living", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_con_list == 'sqft_living' && input.regressors_types == 'Continous' && input.regressors_if_relation == 1 && input.regressors_if_corr == 0",
                    br(), tags$hr(width = '120%', style="border-color: grey;"), br(), 
                    plotOutput("relation_sqft_living", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_con_list == 'sqft_lot' && input.regressors_types == 'Continous' && input.regressors_if_corr == 0",
                    plotOutput("barplot_sqft_lot", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_con_list == 'sqft_lot' && input.regressors_types == 'Continous' && input.regressors_if_relation == 1 && input.regressors_if_corr == 0",
                    br(), tags$hr(width = '120%', style="border-color: grey;"), br(), 
                    plotOutput("relation_sqft_lot", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_con_list == 'lat & long' && input.regressors_types == 'Continous' && input.regressors_if_corr == 0",
                    plotOutput("barplot_lat", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_con_list == 'lat & long' && input.regressors_types == 'Continous' && input.regressors_if_corr == 0",
                    br(), tags$hr(width = '120%', style="border-color: grey;"), br(), 
                    plotOutput("barplot_long", height = '500px', width = '120%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.regressors_con_list == 'lat & long' && input.regressors_types == 'Continous' && input.regressors_if_corr == 0",
                    br(), tags$hr(width = '120%', style="border-color: grey;"), br(), 
                    plotOutput("vis_coordinates", height = '500px', width = '120%')
                  )
                  
                  
                )
              )
      ),
      # dashboard content
      tabItem(tabName = "map",
              
              leafletOutput("map_interactive", height="950px"), 
              
              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            draggable = TRUE, top = 80, left = "auto", right = 20, bottom = "auto",
                            width = 200, height = "auto", cursor = c("inherit"),
                            
                            h2(strong("Filter"), align = "center"),br(),
                            
                            sliderInput("legend_slider1", "Price", min(houses$price), max(houses$price), value = c(min(houses$price), max(houses$price)), step = 100000, pre = "$"),
                            sliderInput("legend_slider2", "Living area", min(houses$sqft_living), max(houses$sqft_living), value = c(min(houses$sqft_living), max(houses$sqft_living)), step = 100),
                            sliderInput("legend_slider3", "Bedrooms", min(houses$bedrooms), max(houses$bedrooms), value = c(min(houses$bedrooms), max(houses$bedrooms)), step = 1),
                            sliderInput("legend_slider4", "Bathrooms", min(houses$bathrooms), max(houses$bathrooms), value = c(min(houses$bathrooms), max(houses$bathrooms)), step = 1),
                            checkboxInput("legend_check1", "Overlooking the waterfront"),
                            checkboxInput("legend_check2", "Has a basement"),
                            checkboxInput("legend_check3", "Search by ID"),
                            conditionalPanel(
                              condition = "input.legend_check3 == 1",
                              textInput("legend_search", value = 5419000050,label = NULL, placeholder = "Transaction ID"), br()
                            ),
                            style = "background-color:rgba(128, 128, 128, 0.25);"
              )
              
      )
    )
  )
)

server <- function(input, output) {
  
  src = "https://storage.googleapis.com/kaggle-datasets-images/128/270/d149695d1f9a97ec54cf673be6430ad7/dataset-original.jpg"
  output$picture <- renderText({c('<img src="',src,'" width=1000>')}) 
  
  output$mytable1 <- DT::renderDataTable(
    DT::datatable(
      data <- houses[, input$show_vars, drop = FALSE],
      options = list(orderClasses = TRUE, lengthMenu = c(5, 10, 25, 50, 100), pageLength = 10,
                     searching = FALSE, scrollX = TRUE)
    ))
  
  data_download <- houses
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("houses-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_download, file)
    }
  )
  
  output$slider <- renderUI({
    
    filtered_target <-
      houses %>%
      filter(
        if (input$location_target == 'Select city') {
          price >= input$slider_target[1]
          price <= input$slider_target[2]
          city == input$target_city
        }
        else {
          price >= input$slider_target[1]
          price <= input$slider_target[2]
        }
      )
    
    sliderInput("inSlider", "Price range", min=min(filtered_target$price), max=max(filtered_target$price), value = c(min(filtered_target$price), max(filtered_target$price)), step = 100000, pre = "$")
  })
  
  output$target_summary <- renderPrint({
    if (input$location_target == 'Select city') {
      
      filtered_target3 <-
        houses %>%
        filter(
          price >= input$slider_target[1],
          price <= input$slider_target[2],
          city == input$target_city
        )
      
      t(t(summary(filtered_target3$price)))
      
    }
    
    else {
      
      filtered_target4 <-
        houses %>%
        filter(
          price >= input$slider_target[1],
          price <= input$slider_target[2],
        )
      
      t(t(summary(filtered_target4$price)))
      
    }
  })
  
  
  output$density_target <- renderPlot({
    
    if (input$location_target == 'Select city') {
      
      filtered_target1 <-
        houses %>%
        filter(
          price >= input$slider_target[1],
          price <= input$slider_target[2],
          city == input$target_city
        )
      
      ggdensity(filtered_target1, x = "price", color = "#808080", fill = input$target_color[1]) +
        scale_x_continuous(labels = scales::dollar, limits=c(input$inSlider[1],input$inSlider[2])) +
        labs(title = "House price distribution - density plot", x = "House price", y = "Density") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
    
    else {
      filtered_target2 <-
        houses %>%
        filter(
          price >= input$slider_target[1],
          price <= input$slider_target[2],
        )
      
      ggdensity(filtered_target2, x = "price", color = "#808080", fill = input$target_color[1]) +
        scale_x_continuous(labels = scales::dollar, limits=c(input$slider_target[1],input$slider_target[2])) +
        labs(title = "House price distribution - density plot", x = "House price", y = "Density") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
    }
  }, bg="transparent")
  
  
  output$density_cities <- renderPlot({
    
    ggplot(houses, aes(x = price, y = reorder(city, price))) +
      scale_x_continuous(labels = scales::dollar, limits=c(0,2000000)) +
      geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
      scale_fill_gradientn(colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF")) +
      labs(title = "Distribution of house price depending on city", x = "House price", y = "City") +
      theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
            axis.title=element_text(size=16), axis.text=element_text(size=12),
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank()
      )
    
    
  }, bg="transparent")
  
  
  output$barplot_target <- renderPlot({
    
    if (input$location_target == 'Select city') {
      
      filtered_target1 <-
        houses %>%
        filter(
          price >= input$slider_target[1],
          price <= input$slider_target[2],
          city == input$target_city
        )
      
      gghistogram(filtered_target1, x = "price", binwidth = input$target_bindt, color = "#808080", fill = input$target_color[1]) +
        scale_x_continuous(labels = scales::dollar, limits=c(input$inSlider[1],input$inSlider[2])) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "House price distribution - histogram", x = "House price", y = "Number of houses") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
    
    else {
      filtered_target2 <-
        houses %>%
        filter(
          price >= input$slider_target[1],
          price <= input$slider_target[2],
        )
      
      gghistogram(filtered_target2, x = "price", binwidth = input$target_bindt, color = "#808080", fill = input$target_color[1]) +
        scale_x_continuous(labels = scales::dollar, limits=c(input$slider_target[1],input$slider_target[2])) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "House price distribution - histogram", x = "House price", y = "Number of houses") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$barplot_target3 <- renderPlot({
    
    if (input$location_target == 'Select city') {
      
      filtered_target1 <-
        houses %>%
        filter(
          price >= input$slider_target[1],
          price <= input$slider_target[2],
          city == input$target_city
        )
      
      gghistogram(filtered_target1, x = "price", binwidth = input$target_bindt, color = "#808080", fill = input$target_color[1]) +
        scale_x_continuous(labels = scales::dollar, limits=c(input$inSlider[1],input$inSlider[2])) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "House price distribution - histogram", x = "House price", y = "Number of houses") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
    
    else {
      filtered_target2 <-
        houses %>%
        filter(
          price >= input$slider_target[1],
          price <= input$slider_target[2],
        )
      
      gghistogram(filtered_target2, x = "price", binwidth = input$target_bindt, color = "#808080", fill = input$target_color[1]) +
        scale_x_continuous(labels = scales::dollar, limits=c(input$slider_target[1],input$slider_target[2])) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "House price distribution - histogram", x = "House price", y = "Number of houses") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$reg_stat_sqft_living <- renderPrint({
    if (input$regressors_con_list == 'sqft_living') {
      
      t(t(summary(houses$sqft_living)))
      
    }
  })
  
  output$reg_stat_sqft_lot <- renderPrint({
    if (input$regressors_con_list == 'sqft_lot') {
      
      t(t(summary(houses$sqft_lot)))
      
    }
  })
  
  output$reg_stat_lat_long <- renderPrint({
    if (input$regressors_con_list == 'lat & long') {
      
      t(t(summary(houses[,c(13,14)])))
      
    }
  })
  
  output$reg_stat_bedrooms <- renderPrint({
    if (input$regressors_num_list == 'bedrooms') {
      
      print(as.data.frame(ftable(houses$bedrooms)), row.names = FALSE)
      
    }
  })
  
  output$reg_stat_bathrooms <- renderPrint({
    if (input$regressors_num_list == 'bathrooms') {
      
      print(as.data.frame(ftable(houses$bathrooms)), row.names = FALSE)
      
    }
  })
  
  output$reg_stat_floors <- renderPrint({
    if (input$regressors_num_list == 'floors') {
      
      print(as.data.frame(ftable(houses$floors)), row.names = FALSE)
      
    }
  })
  
  output$reg_stat_waterfront <- renderPrint({
    if (input$regressors_num_list == 'waterfront') {
      
      print(as.data.frame(ftable(houses$waterfront)), row.names = FALSE)
      
    }
  })
  
  output$reg_stat_view <- renderPrint({
    if (input$regressors_num_list == 'view') {
      
      print(as.data.frame(ftable(houses$view)), row.names = FALSE)
      
    }
  })
  
  output$reg_stat_condition <- renderPrint({
    if (input$regressors_num_list == 'condition') {
      
      print(as.data.frame(ftable(houses$condition)), row.names = FALSE)
      
    }
  })
  
  output$reg_stat_grade <- renderPrint({
    if (input$regressors_num_list == 'grade') {
      
      print(as.data.frame(ftable(houses$grade)), row.names = FALSE)
      
    }
  })
  
  output$reg_stat_house_renov <- renderPrint({
    if (input$regressors_num_list == 'house_renov') {
      
      print(as.data.frame(ftable(houses$house_renov)), row.names = FALSE)
      
    }
  })
  
  output$reg_stat_basement <- renderPrint({
    if (input$regressors_num_list == 'basement') {
      
      print(as.data.frame(ftable(houses$basement)), row.names = FALSE)
      
    }
  })
  
  output$barplot_bedrooms <- renderPlot({
    
    if (input$regressors_num_list == 'bedrooms' && input$regressors_types == 'Discrete') {
      
      ggplot(houses, aes(x = as.factor(bedrooms))) + 
        geom_bar(color = "#808080", fill = input$regressors_hist_color[1]) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Distribution of bedrooms", x = "Number of bedrooms", y = "Number of houses") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$relation_bedrooms <- renderPlot({
    
    if (input$regressors_num_list == 'bedrooms' && input$regressors_types == 'Discrete' && input$regressors_if_relation == 1) {
      
      ggplot(houses, aes(x=as.factor(bedrooms), y=price, fill=as.factor(bedrooms))) + 
        geom_boxplot(alpha=0.5) + 
        theme(legend.position="none") +
        labs(title = "Relationship between number of bedrooms and house price", x = "Number of bedrooms", y = "House price") +
        scale_y_continuous(labels = scales::dollar) +  
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  
  output$barplot_bathrooms <- renderPlot({
    
    if (input$regressors_num_list == 'bathrooms' && input$regressors_types == 'Discrete') {
      
      ggplot(houses, aes(x = as.factor(bathrooms))) + 
        geom_bar(color = "#808080", fill = input$regressors_hist_color[1]) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Distribution of bathrooms", x = "Number of bathrooms", y = "Number of houses") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$relation_bathrooms <- renderPlot({
    
    if (input$regressors_num_list == 'bathrooms' && input$regressors_types == 'Discrete' && input$regressors_if_relation == 1) {
      
      ggplot(houses, aes(x=as.factor(bathrooms), y=price, fill=as.factor(bathrooms))) + 
        geom_boxplot(alpha=0.5) + 
        theme(legend.position="none") +
        labs(title = "Relationship between number of bathrooms and house price", x = "Number of bathrooms", y = "House price") +
        scale_y_continuous(labels = scales::dollar) +  
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$barplot_floors <- renderPlot({
    
    if (input$regressors_num_list == 'floors' && input$regressors_types == 'Discrete') {
      
      ggplot(houses, aes(x = as.factor(floors))) + 
        geom_bar(color = "#808080", fill = input$regressors_hist_color[1]) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Distribution of floors", x = "Number of floors", y = "Number of houses") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$relation_floors <- renderPlot({
    
    if (input$regressors_num_list == 'floors' && input$regressors_types == 'Discrete' && input$regressors_if_relation == 1) {
      
      ggplot(houses, aes(x=as.factor(floors), y=price, fill=as.factor(floors))) + 
        geom_boxplot(alpha=0.5) + 
        theme(legend.position="none") +
        labs(title = "Relationship between number of floors and house price", x = "Number of floors", y = "House price") +
        scale_y_continuous(labels = scales::dollar) +  
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$barplot_waterfront <- renderPlot({
    
    if (input$regressors_num_list == 'waterfront' && input$regressors_types == 'Discrete') {
      
      ggplot(houses, aes(x = as.factor(waterfront))) + 
        geom_bar(color = "#808080", fill = input$regressors_hist_color[1]) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Does the house overlooking the waterfront?", x = "Waterfront", y = "Number of houses") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$relation_waterfront <- renderPlot({
    
    if (input$regressors_num_list == 'waterfront' && input$regressors_types == 'Discrete' && input$regressors_if_relation == 1) {
      
      ggplot(houses, aes(x=as.factor(waterfront), y=price, fill=as.factor(waterfront))) + 
        geom_boxplot(alpha=0.5) + 
        theme(legend.position="none") +
        labs(title = "Relationship between waterfront and house price", x = "Waterfront", y = "House price") +
        scale_y_continuous(labels = scales::dollar) +  
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$barplot_view <- renderPlot({
    
    if (input$regressors_num_list == 'view' && input$regressors_types == 'Discrete') {
      
      ggplot(houses, aes(x = as.factor(view))) + 
        geom_bar(color = "#808080", fill = input$regressors_hist_color[1]) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Distribution of view index", x = "View", y = "Number of houses") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$relation_view <- renderPlot({
    
    if (input$regressors_num_list == 'view' && input$regressors_types == 'Discrete' && input$regressors_if_relation == 1) {
      
      ggplot(houses, aes(x=as.factor(view), y=price, fill=as.factor(view))) + 
        geom_boxplot(alpha=0.5) + 
        theme(legend.position="none") +
        labs(title = "Relationship between view index and house price", x = "View", y = "House price") +
        scale_y_continuous(labels = scales::dollar) +  
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$barplot_condition <- renderPlot({
    
    if (input$regressors_num_list == 'condition' && input$regressors_types == 'Discrete') {
      
      ggplot(houses, aes(x = as.factor(condition))) + 
        geom_bar(color = "#808080", fill = input$regressors_hist_color[1]) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Distribution of condition index", x = "Condition index", y = "Number of houses") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$relation_condition <- renderPlot({
    
    if (input$regressors_num_list == 'condition' && input$regressors_types == 'Discrete' && input$regressors_if_relation == 1) {
      
      ggplot(houses, aes(x=as.factor(condition), y=price, fill=as.factor(condition))) + 
        geom_boxplot(alpha=0.5) + 
        theme(legend.position="none") +
        labs(title = "Relationship between condition index and house price", x = "Condition index", y = "House price") +
        scale_y_continuous(labels = scales::dollar) +  
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$barplot_grade <- renderPlot({
    
    if (input$regressors_num_list == 'grade' && input$regressors_types == 'Discrete') {
      
      ggplot(houses, aes(x = as.factor(grade))) + 
        geom_bar(color = "#808080", fill = input$regressors_hist_color[1]) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Distribution of grades", x = "Grade", y = "Number of houses") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$relation_grade <- renderPlot({
    
    if (input$regressors_num_list == 'grade' && input$regressors_types == 'Discrete' && input$regressors_if_relation == 1) {
      
      ggplot(houses, aes(x=as.factor(grade), y=price, fill=as.factor(grade))) + 
        geom_boxplot(alpha=0.5) + 
        theme(legend.position="none") +
        labs(title = "Relationship between grades and house price", x = "Grade", y = "House price") +
        scale_y_continuous(labels = scales::dollar) +  
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$barplot_house_renov <- renderPlot({
    
    if (input$regressors_num_list == 'house_renov' && input$regressors_types == 'Discrete') {
      
      ggplot(houses, aes(x = as.factor(house_renov))) + 
        geom_bar(color = "#808080", fill = input$regressors_hist_color[1]) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Has the house been renovated", x = "Renovated", y = "Number of houses") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$relation_house_renov <- renderPlot({
    
    if (input$regressors_num_list == 'house_renov' && input$regressors_types == 'Discrete' && input$regressors_if_relation == 1) {
      
      ggplot(houses, aes(x=as.factor(house_renov), y=price, fill=as.factor(house_renov))) + 
        geom_boxplot(alpha=0.5) + 
        theme(legend.position="none") +
        labs(title = "Relationship between renovated and house price", x = "Renovated", y = "House price") +
        scale_y_continuous(labels = scales::dollar) +  
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$barplot_basement <- renderPlot({
    
    if (input$regressors_num_list == 'basement' && input$regressors_types == 'Discrete') {
      
      ggplot(houses, aes(x = as.factor(basement))) + 
        geom_bar(color = "#808080", fill = input$regressors_hist_color[1]) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Does the house have a basement", x = "Basement", y = "Number of houses") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$relation_basement <- renderPlot({
    
    if (input$regressors_num_list == 'basement' && input$regressors_types == 'Discrete' && input$regressors_if_relation == 1) {
      
      ggplot(houses, aes(x=as.factor(basement), y=price, fill=as.factor(basement))) + 
        geom_boxplot(alpha=0.5) + 
        theme(legend.position="none") +
        labs(title = "Relationship between basement and house price", x = "Basement", y = "House price") +
        scale_y_continuous(labels = scales::dollar) +  
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="transparent")
  
  output$regressors_corr_plot <- renderPlot({
    
    if (input$regressors_if_corr == 1) {
      
      corrplot(cor(houses[,c(3:16)]), bg="#F0F0F0", tl.cex=1.5, cl.cex=1.25) 
      
    }
  })
  
  
  output$barplot_sqft_living <- renderPlot({
    
    if (input$regressors_con_list == 'sqft_living' && input$regressors_types == 'Continous') {
      
      ggplot(houses, aes(x = sqft_living)) + 
        geom_histogram(binwidth = 300, color = "#808080", fill = input$regressors_hist_color[1]) +
        geom_vline(aes(xintercept=mean(sqft_living)), color="#FF3333", linetype="dashed", size=1) +
        scale_x_continuous(labels = scales::comma) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Living area (sqft) distribution", x = "Living area", y = "Number of houses") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
    }
  }, bg="transparent")
  
  output$relation_sqft_living <- renderPlot({
    
    if (input$regressors_con_list == 'sqft_living' && input$regressors_types == 'Continous' && input$regressors_if_relation == 1) {
      
      ggplot(houses, aes(x=sqft_living, y=price)) + 
        geom_point(shape = 19, size = 2, alpha = .3, color=input$regressors_rel_color[1]) +
        geom_smooth(method='auto',formula=y~x) +
        scale_x_continuous(labels = scales::comma) +
        scale_y_continuous(labels = scales::dollar) +
        labs(title = "Relationship between living area and house price", x = "Living area", y = "House price") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
    }
  }, bg="transparent")
  
  output$barplot_sqft_lot <- renderPlot({
    
    if (input$regressors_con_list == 'sqft_lot' && input$regressors_types == 'Continous') {
      
      filtered_lot1 <-
        houses %>%
        filter(
          sqft_lot >= input$slider_lot[1],
          sqft_lot <= input$slider_lot[2],
        )
      
      ggplot(filtered_lot1, aes(x = sqft_lot)) + 
        geom_histogram(binwidth = 10000, color = "#808080", fill = input$regressors_hist_color[1]) +
        geom_vline(aes(xintercept=mean(sqft_lot)), color="#FF3333", linetype="dashed", size=1) +
        scale_x_continuous(labels = scales::comma, limits=c(input$slider_lot[1],input$slider_lot[2])) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Land area (sqft) distribution", x = "Land area", y = "Number of houses") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
    }
  }, bg="transparent")
  
  output$relation_sqft_lot <- renderPlot({
    
    if (input$regressors_con_list == 'sqft_lot' && input$regressors_types == 'Continous' && input$regressors_if_relation == 1) {
      
      filtered_lot2 <-
        houses %>%
        filter(
          sqft_lot >= input$slider_lot[1],
          sqft_lot <= input$slider_lot[2],
        )
      
      ggplot(filtered_lot2, aes(x=sqft_lot, y=price)) + 
        geom_point(shape = 19, size = 2, alpha = .3, color=input$regressors_rel_color[1]) +
        geom_smooth(method='auto',formula=y~x) +
        scale_x_continuous(labels = scales::comma, limits=c(input$slider_lot[1],input$slider_lot[2])) +
        scale_y_continuous(labels = scales::dollar) +
        labs(title = "Relationship between land area and house price", x = "Land area", y = "House price") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
    }
  }, bg="transparent")
  
  output$barplot_lat <- renderPlot({
    
    if (input$regressors_con_list == 'lat & long' && input$regressors_types == 'Continous') {
      
      ggplot(houses, aes(x = lat)) + 
        geom_histogram(binwidth = 0.01, color = "#808080", fill = input$regressors_hist_color[1]) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Houses location (North/South)", x = "Latutide", y = "Number of houses") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
    }
  }, bg="transparent")
  
  output$barplot_long <- renderPlot({
    
    if (input$regressors_con_list == 'lat & long' && input$regressors_types == 'Continous') {
      
      ggplot(houses, aes(x = long)) + 
        geom_histogram(binwidth = 0.01, color = "#808080", fill = input$regressors_hist_color[1]) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Houses location (East/West)", x = "Longitude", y = "Number of houses") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
    }
  }, bg="transparent")
  
  output$vis_coordinates <- renderPlot({
    
    if (input$regressors_con_list == 'lat & long' && input$regressors_types == 'Continous') {
      
      houses1 <- houses
      houses1$density1 <- fields::interp.surface(
        MASS::kde2d(houses1$long, houses1$lat), houses1[,c("long", "lat")])
      
      ggplot(houses1, aes(x=long, y=lat, color = density1)) + 
        geom_point(shape = 19, size = 2, alpha = .3) +
        labs(title = "Houses location (coordinates)", x = "Longitude", y = "Latitude") +
        scale_color_gradient(low = "#0091ff", high = "#f0650e") +
        theme(plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none",
              axis.title=element_text(size=16), axis.text=element_text(size=12),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank()
        )
      
    }
  }, bg="#E0E0E0")
  
  
  map_points <- reactive({
    
    if(input$legend_check1 == 1 && input$legend_check2 == 1 && input$legend_check3 == 1){
      
      filtered_map <-
        houses %>%
        filter(
          id == input$legend_search,
          price >= input$legend_slider1[1],
          price <= input$legend_slider1[2],
          sqft_living >= input$legend_slider2[1],
          sqft_living <= input$legend_slider2[2],
          bedrooms >= input$legend_slider3[1],
          bedrooms <= input$legend_slider3[2],
          bathrooms >= input$legend_slider4[1],
          bathrooms <= input$legend_slider4[2],
          waterfront == input$legend_check1,
          basement == input$legend_check2
        )
      
    }
    else if(input$legend_check1 == 1 && input$legend_check2 == 0 && input$legend_check3 == 1){
      
      filtered_map <-
        houses %>%
        filter(
          id == input$legend_search,
          price >= input$legend_slider1[1],
          price <= input$legend_slider1[2],
          sqft_living >= input$legend_slider2[1],
          sqft_living <= input$legend_slider2[2],
          bedrooms >= input$legend_slider3[1],
          bedrooms <= input$legend_slider3[2],
          bathrooms >= input$legend_slider4[1],
          bathrooms <= input$legend_slider4[2],
          waterfront == input$legend_check1
        )
      
    }
    
    else if(input$legend_check1 == 0 && input$legend_check2 == 1 && input$legend_check3 == 1){
      
      filtered_map <-
        houses %>%
        filter(
          id == input$legend_search,
          price >= input$legend_slider1[1],
          price <= input$legend_slider1[2],
          sqft_living >= input$legend_slider2[1],
          sqft_living <= input$legend_slider2[2],
          bedrooms >= input$legend_slider3[1],
          bedrooms <= input$legend_slider3[2],
          bathrooms >= input$legend_slider4[1],
          bathrooms <= input$legend_slider4[2],
          basement == input$legend_check2
        )
      
    }
    
    else if(input$legend_check1 == 0 && input$legend_check2 == 0 && input$legend_check3 == 1){
      
      filtered_map <-
        houses %>%
        filter(
          id == input$legend_search,
          price >= input$legend_slider1[1],
          price <= input$legend_slider1[2],
          sqft_living >= input$legend_slider2[1],
          sqft_living <= input$legend_slider2[2],
          bedrooms >= input$legend_slider3[1],
          bedrooms <= input$legend_slider3[2],
          bathrooms >= input$legend_slider4[1],
          bathrooms <= input$legend_slider4[2]
        )
      
    }
    
    else if(input$legend_check1 == 1 && input$legend_check2 == 1 && input$legend_check3 == 0){
      
      filtered_map <-
        houses %>%
        filter(
          price >= input$legend_slider1[1],
          price <= input$legend_slider1[2],
          sqft_living >= input$legend_slider2[1],
          sqft_living <= input$legend_slider2[2],
          bedrooms >= input$legend_slider3[1],
          bedrooms <= input$legend_slider3[2],
          bathrooms >= input$legend_slider4[1],
          bathrooms <= input$legend_slider4[2],
          waterfront == input$legend_check1,
          basement == input$legend_check2
        )
      
    }
    else if(input$legend_check1 == 1 && input$legend_check2 == 0 && input$legend_check3 == 0){
      
      filtered_map <-
        houses %>%
        filter(
          price >= input$legend_slider1[1],
          price <= input$legend_slider1[2],
          sqft_living >= input$legend_slider2[1],
          sqft_living <= input$legend_slider2[2],
          bedrooms >= input$legend_slider3[1],
          bedrooms <= input$legend_slider3[2],
          bathrooms >= input$legend_slider4[1],
          bathrooms <= input$legend_slider4[2],
          waterfront == input$legend_check1
        )
      
    }
    
    else if(input$legend_check1 == 0 && input$legend_check2 == 1 && input$legend_check3 == 0){
      
      filtered_map <-
        houses %>%
        filter(
          price >= input$legend_slider1[1],
          price <= input$legend_slider1[2],
          sqft_living >= input$legend_slider2[1],
          sqft_living <= input$legend_slider2[2],
          bedrooms >= input$legend_slider3[1],
          bedrooms <= input$legend_slider3[2],
          bathrooms >= input$legend_slider4[1],
          bathrooms <= input$legend_slider4[2],
          basement == input$legend_check2
        )
      
    }
    
    else{
      
      filtered_map <-
        houses %>%
        filter(
          price >= input$legend_slider1[1],
          price <= input$legend_slider1[2],
          sqft_living >= input$legend_slider2[1],
          sqft_living <= input$legend_slider2[2],
          bedrooms >= input$legend_slider3[1],
          bedrooms <= input$legend_slider3[2],
          bathrooms >= input$legend_slider4[1],
          bathrooms <= input$legend_slider4[2]
        )
    }
    
    
  })
  
  leafIcons <- icons(
    iconUrl = "https://image.flaticon.com/icons/png/512/63/63813.png",
    iconWidth = 30, iconHeight = 30
  )
  
  output$map_interactive <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = map_points(), 
                 popup = ~as.character(paste(paste("Price: &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp$", format(price, big.mark=" "), sep=""), 
                                             paste("Living area:&nbsp&nbsp&nbsp&nbsp", format(sqft_living, big.mark=" "), " sqft", sep=""),
                                             paste("Land area:&nbsp&nbsp&nbsp&nbsp&nbsp", format(sqft_lot, big.mark=" "), " sqft", sep=""),
                                             paste("Bedrooms:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp", bedrooms, sep=""),
                                             paste("Bathrooms:&nbsp&nbsp&nbsp&nbsp&nbsp", bathrooms, sep=""),
                                             paste("Build year: &nbsp&nbsp&nbsp&nbsp&nbsp", yr_built, sep=""), sep="<br/>")),
                 label = ~htmlEscape(as.character(paste("Transaction ID: ", id, sep=""))),
                 labelOptions = labelOptions(textsize = "13px"),
                 icon = leafIcons,
                 clusterOptions = markerClusterOptions())
    
  })
  
  
}



shinyApp(ui, server)
