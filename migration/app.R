library(shiny)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(scales)
library(shinyalert)

migration <- read.csv("data/migration.csv")
reason <- read.csv("data/reason.csv")
type_vec <- as.character(unique(migration$migration_type))

# reasons list to filter on later
reasons <- list(family = c("change_in_marital_status", "to_establish_own_household", "other_family_reason"),
                employment = c("new_job_or_job_transfer", "to_look_for_work_or_lost_job",  "to_be_closer_to_work_easier_commute", "retired", "other_job_related_reason"),
                housing = c("wanted_to_own_home_not_rent", "wanted_new_or_better_home_apartment", "wanted_better_neighborhood_less_crime", "wanted_cheaper_housing", "foreclosure_eviction_5", "other_housing_reason"),
                other = c("to_attend_or_leave_college", "change_of_climate", "health_reasons", "natural_disaster_8", "other_reasons"))



##Dashboard Header-------------
header <- dashboardHeader(title = "Migration Explorer")

##Dashboard Sidebar----------------
sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Introduction", tabName = "introduction"),
      menuItem("Net Migration", tabName = "net_migration"),
      menuItem("Reason for Move", tabName = "reason")
    )
)

##Dashboard Body-------------------
body <- dashboardBody(
    tabItems(
    ##Introduction Tab---------------  
    tabItem(tabName = "introduction",
            includeHTML("custom/introduction.html"),
            includeCSS("custom/custom.css")
    ),
    ##Net_migration Tab---------------
    tabItem(tabName = "net_migration",
      fluidRow(
        tabsetPanel(
            tabPanel("Overall trends",
                     headerPanel(h4("Timeline for US Internal Migration Trends")),
                     # column-based layout 
                     column(width = 9,
                      box(width = NULL, status = "primary", solidHeader = TRUE,
                        plotOutput(outputId = "lineplot",
                                   height = 600)),
         
                      box(width = NULL, status = "primary",  solidHeader = TRUE, collapsible = TRUE, 
                         DT::dataTableOutput(outputId = "table"))),
                     
                     column(width = 3,
                      box(width = NULL, status = "warning", solidHeader = TRUE,
                         checkboxGroupInput(inputId = "migration_type",
                                            label = "Select migration type",
                                            choices = type_vec,
                                            select = type_vec),
                        
                         checkboxGroupInput(inputId = "region",
                                            label = "Select region(s)",
                                            choices = c(
                                              "midwest", "northeast", "south", "west"),
                                            select = c("midwest", "northeast", "south", "west")),
               
                         numericInput(inputId = "start_year",
                                      label = "Select start year between 1981 - 2018",
                                      value = "1981",
                                      min = 1981,
                                      max = 2019),
                         numericInput(inputId = "end_year",
                                      label = "Select end year between 1982 - 2019",
                                      value = "2019",
                                      min = 1981, 
                                      max = 2019),
                         p(class = "text-muted",
                           tags$div(
                             HTML("<strong>Note: </strong><br>Net internal = Inmigrants + Outmigrants<br>",
                                  "Net Migration = Net internal + Movers from abroad"))),
                         useShinyalert(),  # Set up shinyalert
                         actionButton(inputId = "getdata",
                                      label = strong("Pull Data"))))),
          
                        
               tabPanel("Distribution",
                        
                      headerPanel(h4("Distribution of Net Population Flows by Migration Type and by Region")),
                      headerPanel(strong(h6("Aggregated from 1981 - 2019"))),
                                 
                        fluidRow(
                          # Column-based layout 
                          column(width = 6,
                                 box(width = NULL, height = 150, status = "warning", solidHeader = TRUE,
                                     selectInput(inputId  = "region_choice",
                                                 label    = "Select a region",
                                                 choices  = c("midwest", "northeast", "south", "west")),
                                     hr(),
                                     actionButton(inputId = "getdata_box",
                                                  label = strong("Pull Data"))),

                                box(width = NULL, 
                                    HTML("<center><strong><h6>Inmigrants in South and West have the largest 3rd quartile,<br>
                                       Outmigrants in Midwest and Northeast the largest</h6></strong></center>"),
                            
                                    status = "primary", collapsible = TRUE,
                                    plotlyOutput(outputId = "boxplot", height = 500)),
                          ),
                          column(width = 5,
                                 box(width = NULL, height = 150, status = "warning", solidHeader = TRUE,
                                     # Input: Slider for the number of bins ----
                                     sliderInput(inputId = "bins",
                                                 label = "Number of bins:",
                                                 min = 30,
                                                 max = 150,
                                                 value = 100)),
                       
                                box(width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                    plotOutput(outputId = "hist", height = 500))
                              
                             
                          ) # column 
                        ) # fluidRow 
               ), # tabPanel "Distribution"
            
            
            tabPanel("Correlation",
                     headerPanel(h4("Explore correlation between migration type and region!")),
                     box(width = 3, status = "warning", solidHeader = TRUE,
                         numericInput(inputId = "year",
                                      label = "Choose a year to zoom in",
                                      value = 2018,
                                      min = 1981),
                         hr(),
                         useShinyalert(),  # Set up shinyalert
                         actionButton(inputId = "getdata_heatmap",
                                      label = strong("Pull Data"))
                         
                         ),
                     box(width = 9, status = "primary", solidHeader = TRUE,
                         plotOutput(outputId = "heatmap",
                                    height = 450)))
                 
        ) # tabsetPanel 
      ) # fluid row 
    ), # tab item net 
    ##Reason for Move Tab-------------------------------
    tabItem(tabName = "reason",
       fluidRow(
          tabsetPanel(
              tabPanel("Aggregate", 
                       headerPanel(h4("Interactive Bar Plot of Aggregate Reasons")),
                       column(width = 9,
                              box(strong("hover over for year and agg. proportion"),
                              hr(),
                              width = NULL, status = "primary", solidHeader = TRUE,
                              plotlyOutput(outputId = 'agg_barplot',
                                          height = 500))
                       ), # column
                       column(width = 3,
                              fluidRow(
                                # A static valueBox -- display mean of each aggregate category (over the years)
                                valueBox(width = NULL, paste(46.14, "%"), "Housing (on average)", color = "yellow", icon = icon("bed")),
                                valueBox(width = NULL, paste(27.93, "%"), "Family (on average)", icon = icon("user-friends")),
                                valueBox(width = NULL, paste(18.65, "%"), "Employment (on average)", icon = icon("briefcase")),
                                valueBox(width = NULL, paste(7.3, "%"), "Other (on average)", icon = icon("align-justify"))
      
                              ),
                       ) #column
              ), #tabPanel "Aggregate"

              tabPanel("Breakdown",
                       headerPanel(h4("Explore the subcategories makeup of each mega-reason!")),
                       column(width = 9,
                       box(width = NULL, status = "primary", solidHeader = TRUE,
                       plotOutput(outputId = "breakdown_barplot",
                                  height = 500))),
                       column(width = 3,
                       box(width = NULL, status = "warning",
                           selectInput(inputId  = "breakdown_choice",
                                      label    = "Select a reason",
                                      choices  = names(reasons))))
                      )

             ) # tabset Panel
       ) # fluidrow 
    ) # tab reason 
  ) #tab items 
    
)

ui <- dashboardPage(
  skin = "yellow",
  header,
  sidebar,
  body
)


server <- function(input, output) {
    
    ##Server code for "Reason for Move" Tab-------------------------
    agg_reason_long <- reactive({
      reason %>%
        pivot_longer(cols = family:other,
                     names_to = "aggregate_reasons", values_to = "agg_prop")
    })
    
    output$agg_barplot <- renderPlotly({
      p <- ggplot(agg_reason_long(), 
                aes(x=reorder(period, period), 
                    y=agg_prop, 
                    fill=aggregate_reasons,
                    text = paste('Year:', period,
                                 '<br>Agg. prop: ', agg_prop,
                                 '<br>Agg. reason: ', aggregate_reasons))) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_brewer(palette = "Blues") + 
      coord_flip() + 
      labs(y = "Proportion (%)", 
           x = "Year",
           title = "Housing is the most common reason to relocate",
           subtitle = "1998 - 2018, US Census") +
      theme_bw()
    
    ggplotly(p, tooltip = c("text"))
   })
    
    output$breakdown_barplot <- renderPlot(
        reason %>%
        pivot_longer(cols = reasons[[input$breakdown_choice]], 
                     names_to = "related_reasons", values_to = "prop") %>%
       
        ggplot(mapping = aes(x = reorder(period, prop),
                             y = prop,
                             fill = related_reasons)) +
          geom_bar(stat = "identity") + 
          scale_fill_brewer(palette = "Blues") +
          coord_flip() + 
          geom_text(aes(label = paste(prop, "%")), 
                    size = 3, 
                    position = position_stack(vjust = 0.5)) + 
          labs(y = "Proportion (%)", 
               x = "Year",
               fill = "Specific Reasons") + 
          theme_bw(base_size = 14) 
    ) 
    
    ## Server code for "Net Migration" Tab--------------------------
    ## "Overall trends" tab item under "Net Migration" Tab----------
    observeEvent(input$getdata, {
      if (!(input$start_year >= 1981 & input$start_year <= 2019 & 
            input$end_year >= 1981 & input$end_year <= 2019)){
        shinyalert(title = "Oh no!",
                   text = "invalid year range: must be 1981 - 2019",
                   type = "error",
                   closeOnEsc = TRUE, 
                   closeOnClickOutside = TRUE)
      }
    })
    
    long_data <- reactive({input$getdata
      migration  %>%
        filter(migration_type %in% isolate(input$migration_type),
               year >= isolate(input$start_year),
               year <= isolate(input$end_year)) %>%
        pivot_longer(cols = northeast:west, 
                     names_to = "region", values_to = "population_count") %>%
        filter(region %in% isolate(input$region))
      
    })


    output$lineplot <- renderPlot({
       plot <- ggplot(long_data(), mapping = aes(x = year, y = population_count, 
                                                 color = factor(migration_type))) +
        geom_line(alpha = 0.7) +
        geom_point(alpha = 0.7) + 
        facet_wrap(~region) + 
        labs(x = "Year", y = "Population Count (in thousands)",
             color = "Migration Type") +
        theme_bw(base_size = 18) + 
        scale_color_manual(values = c("#d8b365", "#a1d76a", "#256D7B", "#fc8d59", "#99d8c9")) 
       
       plot + theme(axis.text.x = element_text(angle = 30, size = 10, vjust = 0.6),
                    axis.text.y = element_text(size=10),
                    axis.title.y = element_text(size=14),
                    axis.title.x = element_text(size=14),
                    legend.title = element_text(size=12),
                    legend.text = element_text(size=10, face="bold"),
                    legend.position = "bottom") 
        
    })


    output$table <- DT::renderDataTable ({
       long_data() %>%
        pivot_wider(names_from = region, values_from = population_count) %>%
        DT::datatable()
    })
    
    ## "Distribution" tab item under "Net Migration" --------------------
    output$hist <- renderPlot({
      p <- ggplot(long_data(), aes(x = population_count, fill = region)) + 
        geom_histogram(binwidth = input$bins, alpha = 0.7) + 
        theme_bw(base_size = 15) + 
        scale_fill_manual(values = c("#808000", "#002D72", "#187bcd", "#9ACD32")) + 
        labs(x = "Population (in thousands)",
             y = "Count",
             fill = "Region",
             title = "Symmetric, unimodal or bimodal distribution",
             subtitle = "Histogram of Net Population Flows") + 
        geom_vline(aes(xintercept = mean(population_count)),color ='red',linetype = 2, size = 1.25) + 
        annotate("text", y = -10, x = 0, label = "mean population",
                 color = "red", alpha = 0.7, size = 6) 
    
      p + theme(legend.position = "bottom")
    })
    
    
    long_data_box <- reactive({input$getdata_box 
      migration %>%
        pivot_longer(cols = northeast:west, 
                     names_to = "region", values_to = "population_count") %>%
        filter(region == isolate(input$region_choice)) 
    })
    
    output$boxplot <- renderPlotly ({
      p <- ggplot(long_data_box(), 
                  mapping = aes(x = reorder(migration_type, population_count, quantile, .75), 
                                y = population_count, 
                                fill = region)) + 
        geom_boxplot(alpha = 0.7, fill = "#2a9df4",
                     show.legend = FALSE) + 
        labs(x = "Migration Type (ordered by Q3)",
             y = "Population Count (in thousands)"
             ) + 
        theme_bw(base_size = 12)

      ggplotly(p + theme(axis.text.x = element_text(angle = 15, size = 8, hjust = 2),
                         legend.position = "none",
                         plot.title = element_text(size=12)))
      
    })
    
    ## "Correlation" tab item under "Net Migration" Tab-----------------------
    observeEvent(input$getdata_heatmap, {
      if (!(input$year >= 1981 & input$year <= 2019)){
        shinyalert(title = "Oh no!",
                   text = "invalid year: must be 1981 - 2019",
                   type = "error",
                   closeOnEsc = TRUE, 
                   closeOnClickOutside = TRUE)
      }
    })
    
    long_data_heatmap <- reactive({input$getdata_heatmap
      migration %>%
        pivot_longer(cols = northeast:west, 
                     names_to = "region", values_to = "population_count") %>%
        filter(year == isolate(input$year))
    })
    
    
    output$heatmap <- renderPlot ({
      ggplot(long_data_heatmap(), 
             mapping = aes(x = region, y = migration_type, fill = population_count)) +
        geom_raster() + theme_bw(base_size = 16) + 
        labs(x = "Region",
             y = "Migration Type",
             fill = "Population Count",
             title = "Into South, Out of Northeast and Midwest",
             subtitle = "Net population flows according to region and migration type")
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
