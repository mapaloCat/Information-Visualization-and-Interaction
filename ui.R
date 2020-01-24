# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(shinycssloaders)

sidebar = dashboardSidebar(
  sidebarMenu(
    style = "position: fixed; overflow: visible;",
    menuItem("Welcome!", tabName = "welcome", icon = icon("globe")),
    menuItem("Explore Europe dataset", tabName = "explore", icon = icon("fas fa-database"),
             menuSubItem("Dataset", tabName = "dataset"),
             menuSubItem("Data Exploration", tabName = "data_exploration")),
    menuItem("Data analysis", tabName = "analysis", icon = icon("fas fa-search"),
             menuSubItem("Principal Component Analysis", tabName = "pca"),
             menuSubItem("Clustering", tabName = "clustering"))
  )
  
)
body = dashboardBody(
  tags$head(includeCSS('www/style.css')),
  tags$head(tags$style(HTML('
                            .content-wrapper, .right-side {background-color: white;}
                            '))),
  tabItems( 
    tabItem(tabName = "welcome",
            includeMarkdown("www/welcome.md")
    ),
    tabItem(tabName = "dataset",
            fluidRow(
              DT::dataTableOutput('dTable')
            )
    ),
    tabItem(tabName = "data_exploration",
            div(
              tabsetPanel(
                type="tabs",
                tabPanel(p(icon("line-chart"), "Map Visualization"),
                         fluidRow(
                           column(6,
                                  selectInput("variable_mapSimple_1", "Attribute:",
                                              choices = c("None","Area", "GDP", "Inflation",
                                                          "Life expectancy", "Military", "Population growth",
                                                          "Unemployment"))
                           )
                         ),
                         fluidRow(tags$style(type = "text/css", "#map_simple_1 {height: calc(100vh - 80px) !important;}"),
                                  leafletOutput("map_simple_1", height = "100%"))
                ),
                tabPanel(p(icon("pie-chart"), "Pie and Bar Chart Visualization"),
                         fluidRow(
                           column(4,
                                  selectInput("variable_pie_chart", "Attribute:",
                                              choices = c("None","Area", "GDP", "Inflation",
                                                          "Life expectancy", "Military", "Population growth",
                                                          "Unemployment")), align = "center", offset = 4
                           )
                         ),
                         fluidRow(
                           column(6, box(
                             status = "primary",
                             width = "12",
                             solidHeader = T,
                             plotlyOutput("pie_chart_static")
                           )),
                           column(6, box(
                             status = "primary",
                             width = "12",
                             solidHeader = T,
                             plotlyOutput("bar_chart_static")
                           ))
                         ),
                         fluidRow(
                           column(2,
                                  selectInput("country_1", "Country 1:",
                                              choices = c("Austria","Belgium", "Bulgaria", "Croatia", "Czech Republic",
                                                          "Denmark", "Estonia", "Finland", "Germany", "Greece", "Hungary",
                                                          "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
                                                          "Netherlands", "Norway", "Poland", "Portugal", "Slovakia",
                                                          "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom")), offset = 1
                           ),
                           column(2, uiOutput("country_2")),
                           column(2, uiOutput("country_3")),
                           column(2, uiOutput("country_4")),
                           column(2, uiOutput("country_5"))
                         ),
                         fluidRow(
                           # column(6, box(
                           #   status = "primary",
                           #   width = "12",
                           #   solidHeader = T,
                           #   plotlyOutput("pie_chart_reactive")
                           # )),
                           column(6, box(
                             status = "primary",
                             width = "12",
                             solidHeader = T,
                             plotlyOutput("bar_chart_reactive")
                           ), align = "center", offset = 3)
                         )
                ),
                tabPanel(p(icon("area-chart"), "Radar Chart Visualization"),
                         fluidRow(
                           column(12,h3("Radar Chart visualization of the percentage of the average of each variable per Country"), align = "center")
                           ),
                         fluidRow(
                           column(3,
                                  selectInput("country_radar_chart", "Selected Country:",
                                              choices = c("Austria","Belgium", "Bulgaria", "Croatia", "Czech Republic",
                                                          "Denmark", "Estonia", "Finland", "Germany", "Greece", "Hungary",
                                                          "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
                                                          "Netherlands", "Norway", "Poland", "Portugal", "Slovakia",
                                                          "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom")), align = "center", offset = 3
                           ),
                           column(3,
                                  selectInput("country_radar_chart2", "Selected Country to Compare With:",
                                              choices = c("None","Austria","Belgium", "Bulgaria", "Croatia", "Czech Republic",
                                                          "Denmark", "Estonia", "Finland", "Germany", "Greece", "Hungary",
                                                          "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
                                                          "Netherlands", "Norway", "Poland", "Portugal", "Slovakia",
                                                          "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom"), selected = "None"), align = "center"
                           )
                         ),
                         fluidRow(
                           plotlyOutput("radar_chart", height = "100%")
                         )
                         ),
                tabPanel(p(icon("table"), "Scatter Plot Visualization"),
                         fluidRow(
                           column(12,h3("Is there any correlation between the different attributes of the dataset?"), align = "center")
                         ),
                         fluidRow(
                           column(12, box(
                             h4(strong("Looking at all pairwise combinations of sequential attributes in scatterplots using the function splom alongside plotly.")),
                             status = "primary",
                             width = "12",
                             solidHeader = T,
                             withSpinner(plotlyOutput("splom", height = "700"))
                           ), align = "center")
                         ),
                         fluidRow(
                           column(12, box(
                             h4(strong("Scatter plot matrix")),
                             status = "primary",
                             width = "12",
                             solidHeader = T,
                             includeHTML("www/scatter_explanation.md"),
                             withSpinner(plotOutput("ggpairs_graph", height = "700"))
                           ), align = "center")
                         )
                )
              )
            )
			),
    tabItem(tabName = "pca",
            fluidRow(
              box(
                width ="12",
                solidHeader = T,
                status = "primary",
                includeMarkdown("www/pca.md")
              )
            ),
            fluidRow(
              box(h4(strong("Summary of the PCA applied to Europe's dataset")),
                  status = "primary",
                  width ="12",
                  solidHeader = T,
                  dataTableOutput("pca_summary")
              ),
              fluidRow(
                column(7,box(h4(strong("Accumulative variance explained by components")),
                             width = "12",
                             solidHeader = T,
                             status = "primary",
                             sliderInput("variance_slider", "Choose the number of components you want", 1,7,1),
                             plotlyOutput("var_explained", height = "183px"))
                ),
                column(5,
                       box(h4(strong("Percentage of variance explained by each component")),
                           width = "12",
                           solidHeader = T,
                           status = "primary",
                           plotlyOutput("variances_donut", height = "318px")
                       )
                )
              ),
              fluidRow(
                column(7,
                       box(
                        solidHeader = T,
                        status = "primary",
                        width = "12",
                        includeMarkdown("www/pca_explanation.md"),
                        height = "464px")),
                column(5,
                       tags$head(tags$style(HTML("div.box-body-primary {margin-left: -105px;}"))),
                       box(h4(strong("Percentage of variance explained by each dimension")),
                           width = "12",
                           solidHeader = T,
                           status = "primary",
                           plotOutput("var_explained_bars")
                       )
                )
              ),
              fluidRow(
                column(6,
                  box(h4(strong("Variance graph by dimensions")),
                      width = "12",
                      solidHeader = T,
                      status = "primary",
                      numericInput("first_dimension", "Choose one dimension", value = 1, min = 1, max = 7),
                      numericInput("second_dimension", "Choose one dimension", value = 2, min = 1, max = 7),
                      plotOutput("pca_dimensions")
                  )
                ),
                column(6,
                  box(h4(strong("Contribution of variables to dimensions")),
                      width = "12",
                      solidHeader = T,
                      status = "primary",
                      numericInput("dimension_contrib", "Choose one dimension", value = 1, min = 1, max = 7, width = "300px"),
                      plotOutput("pca_contribution", width = "750px", height = "473px")
                  )
                )
              )
                
              
            )
    ),
    tabItem(tabName ="clustering",
            div (
              fluidRow(
                column(
                  6,
                  selectInput(
                    "select_algorithm",
                    "Select the clustering algorithm:",
                    choices = c("None","Hierarchical Clustering (HC)", "Kmeans")
                  )
                ),
                column(
                  6,
                  numericInput(
                    "select_Nclusters",
                    "Select the number of clusters",
                    value = 3,
                    min = 2)
                )
              ),
              fluidRow(
                column(6,
                       checkboxInput("pca_checkbox", 
                                     strong("Do you want to perform PCA before applying the clustering algorithm?"),
                                     value = F)),
                column(6,
                       conditionalPanel(condition = "input.pca_checkbox == 1",
                                        numericInput("pca_nDimensions", strong("Choose a number of components from the PCA (between 1 and 7)"), value = 1, min = 1, max = 7)
                       )
                )
              )
            ),
            div(
              align = "center",
              actionButton(
                "display",
                strong("Display")             
              ),
              actionButton(
                "clear",
                strong("Clear")
              )
            ),
            #div(
              tabsetPanel(
                type="tabs",
                tabPanel("Map clustering",
                         #tags$style(type = "text/css", "#map_cluster {height: calc(100vh - 80px) !important;}"),
                         tags$style(type = "text/css", "#map_cluster {height: calc(400px) !important;}"),
                         leafletOutput("map_cluster", height = "400px")
                ),
                tabPanel("Dimensions clustering", 
                         plotOutput("dimensions", height = "450px")),
                tabPanel("Dendogram clustering (only for HC)",
                         includeMarkdown("www/dendogram_explanation.md"),
                         plotOutput("dendrogram", height = "450px")
                )
              ),
              tabsetPanel(
                type="tabs",
                tabPanel("Clustering validation",
                         includeMarkdown("www/silhouette_explanation.md"),
                         plotOutput("clustering_validation", height = "450px"))
                
              )
            
            #)
    )
  )
  
  )


header =  dashboardHeader(title = "Europe",
                tags$li(a(onclick = "openTab('welcome')",
                          href = NULL,
                          icon("home"),
                          title = "Homepage",
                          style = "cursor: pointer;"),
                        class = "dropdown",
                        tags$script(HTML("
                                         var openTab = function(tabName){
                                         $('a', $('.sidebar')).each(function() {
                                         if(this.getAttribute('data-value') == tabName) {
                                         this.click()
                                         };
                                         });
                                         }")))
)



shinyUI(
  dashboardPage(
    header,
    sidebar,
    body
  )
  
)