library(shiny)
library(ggplot2)
library(grid)
library(rworldmap)
library(ggmap)
library(mapproj)
library(maps)
library(maptools)
library(dplyr)
library(eurostat)
library(sf)
library(tmap)
library(ggplot2)
library(shinyalert)
library(leaflet)
library(factoextra)
library(FactoMineR)
library(knitr)
library(plotly)
library(DT)
library(cluster)
library(fpc)
library(NbClust)
library(DT)
library(corrgram)
library(GGally)
library(stringr)
library(hrbrthemes)
library(heatmaply)

eu = read.csv("europe.csv")
rownames(eu) = eu$Country
eu.datatable = read.csv("europe.csv")

# ADAPT EUROPE DATABASE
eu[nrow(eu)+10,] = NA
eu$Country = as.character(eu$Country)
eu[29,"Country"] = "Albania"
eu[30,"Country"] = "Bosnia and Herzegovina"
eu[31,"Country"] = "Belarus"
eu[32,"Country"] = "France"
eu[33,"Country"] = "Romania"
eu[34,"Country"] = "Montenegro"
eu[35,"Country"] = "Maldova"
eu[36,"Country"] = "North Macedonia"
eu[37,"Country"] = "Kosovo"
eu[38,"Country"] = "Republic of Serbia"
eu$Country = as.factor(eu$Country)


# REPRESENTING DATA THROW MAP VISUALIZATION
suppressPackageStartupMessages(library(sf))
world <- st_as_sf(rnaturalearth::countries110)
europe <- dplyr::filter(world, region_un=="Europe" & name!='Russia')

europe <- europe %>% 
  mutate(sovereignt = str_replace(sovereignt, "Macedonia", "North Macedonia"))

names(europe)[names(europe) == 'sovereignt'] <- 'Country'
europe = merge(europe, eu, by="Country")

# A bounding box for continental Europe.
europe.bbox <- st_polygon(list(
  matrix(c(-25,29,45,29,45,75,-25,75,-25,29),byrow = T,ncol = 2)))

europe.clipped = suppressWarnings(st_intersection(europe, st_sfc(europe.bbox, crs=st_crs(europe))))


shinyServer(function(input, output) {
  
  output$dTable <- DT::renderDataTable(
    DT::datatable(na.omit(eu), options = list(pageLength = 10), rownames = FALSE)
  )
  
  mapSimpleInput_1 = reactive({
    
    switch (input$variable_mapSimple_1,
            "None" = list("None", 0, 100),
            "Area" = list("Area", min(europe.clipped$Area, na.rm = TRUE), max(europe.clipped$Area, na.rm = TRUE)),
            "GDP" = list("GDP", min(europe.clipped$GDP, na.rm = TRUE), max(europe.clipped$GDP, na.rm = TRUE)),
            "Inflation" = list("Inflation", min(europe.clipped$Inflation, na.rm = TRUE), max(europe.clipped$Inflation, na.rm = TRUE)),
            "Life expectancy" = list("Life.expect", min(europe.clipped$Life.expect, na.rm = TRUE), max(europe.clipped$Life.expect, na.rm = TRUE)),
            "Military" = list("Military", min(europe.clipped$Military, na.rm = TRUE), max(europe.clipped$Military, na.rm = TRUE)),
            "Population growth" = list("Pop.growth", min(europe.clipped$Pop.growth, na.rm = TRUE), max(europe.clipped$Pop.growth, na.rm = TRUE)),
            "Unemployment" = list("Unemployment", min(europe.clipped$Unemployment, na.rm = TRUE), max(europe.clipped$Unemployment, na.rm = TRUE))
    )
  })
  
  pieChartVariableInput = reactive({
    
    switch (input$variable_pie_chart,
            "None" = NULL,
            "Area" = europe.clipped$Area,
            "GDP" = europe.clipped$GDP,
            "Inflation" = europe.clipped$Inflation,
            "Life expectancy" = europe.clipped$Life.expect,
            "Military" = europe.clipped$Military,
            "Population growth" = europe.clipped$Pop.growth,
            "Unemployment" = europe.clipped$Unemployment
    )
  })
  
  
  # output$slider_mapSimple_1 <- renderUI({
  #   x <- mapSimpleInput_1()[[2]]
  #   y <- mapSimpleInput_1()[[3]]
  #   sliderInput("reactiveSlider", "Range of interest:", min=x, max=y, value=(x+(y-x)/2))
  # })
  
  
  output$map_simple_1 = renderLeaflet(
    # map_function(mapSimpleInput_1(), input$reactiveSlider)
    map_function(mapSimpleInput_1())
  )
  
  
  output$country_2 <- renderUI({
    
    options = c("Austria","Belgium", "Bulgaria", "Croatia", "Czech Republic",
                "Denmark", "Estonia", "Finland", "Germany", "Greece", "Hungary",
                "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
                "Netherlands", "Norway", "Poland", "Portugal", "Slovakia",
                "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom")
    
    remove = as.character(input$country_1)
    options <- options[! options %in% remove]
    
    selectInput("country_2_pick", "Country 2:", choices = options)
    
  })
  
  output$country_3 <- renderUI({
    
    options = c("Austria","Belgium", "Bulgaria", "Croatia", "Czech Republic",
                "Denmark", "Estonia", "Finland", "Germany", "Greece", "Hungary",
                "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
                "Netherlands", "Norway", "Poland", "Portugal", "Slovakia",
                "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom")
    
    remove = c(as.character(input$country_1), as.character(input$country_2_pick)) 
    options <- options[! options %in% remove]
    
    selectInput("country_3_pick", "Country 3:", choices = options)
    
  })
  
  output$country_4 <- renderUI({
    
    options = c("Austria","Belgium", "Bulgaria", "Croatia", "Czech Republic",
                "Denmark", "Estonia", "Finland", "Germany", "Greece", "Hungary",
                "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
                "Netherlands", "Norway", "Poland", "Portugal", "Slovakia",
                "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom")

    remove = c(as.character(input$country_1), as.character(input$country_2_pick), as.character(input$country_3_pick))
    options <- options[! options %in% remove]
    
    selectInput("country_4_pick", "Country 4:", choices = options)

  })
  
  output$country_5 <- renderUI({
    
    options = c("Austria","Belgium", "Bulgaria", "Croatia", "Czech Republic",
                "Denmark", "Estonia", "Finland", "Germany", "Greece", "Hungary",
                "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
                "Netherlands", "Norway", "Poland", "Portugal", "Slovakia",
                "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom")
    
    remove = c(as.character(input$country_1), as.character(input$country_2_pick), as.character(input$country_3_pick), as.character(input$country_4_pick))
    options <- options[! options %in% remove]
    
    selectInput("country_5_pick", "Country 5:", choices = options)
    
  })
  
  data <- reactive({
    # selected_countries = sort(c(input$country_1, input$country_2_pick, input$country_3_pick, input$country_4_pick, input$country_5_pick))
    selected_countries = sort(c(input$checkGroupCountries_barChart_1, input$checkGroupCountries_barChart_2))
    ind = which(europe.clipped$Country %in% selected_countries)
    x = pieChartVariableInput()[ind]
    x = c(x, mean(pieChartVariableInput(), na.rm = TRUE))
    y = c(selected_countries, "Average")
    
    if(length(x)==length(y)){
      data <- data.frame(
        "Country" = y,
        "Variable" = x
      )
    }
    else{
      data <- data.frame(
        "Country" = y,
        "Variable" = c(0,0,0,0,0,0)
      )
    }
    
    return(data)
  })
  
  output$pie_chart_static <- renderPlotly({
    
    variable = input$variable_pie_chart
    if(variable!="None"){
      plot_ly(europe.clipped, labels = ~Country, values = ~pieChartVariableInput()) %>%
        add_pie(hole = 0.6) %>%
        layout(title = as.character(variable),  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
  })
  
  output$heatmap <- renderPlotly({
    
    # Matrix format
    mat <- read.csv("europe.csv")
    rownames(mat) <- mat[,1]
    mat <- mat %>% dplyr::select(-Country, -Area)
    mat <- as.matrix(mat)
    mat <- scale(mat)
    
    p <- heatmaply(mat, 
                   dendrogram = "none",
                   xlab = "", ylab = "", 
                   main = "",
                   scale = "column",
                   margins = c(60,100,40,20),
                   grid_color = "white",
                   grid_width = 0.00001,
                   titleX = FALSE,
                   hide_colorbar = FALSE,
                   branches_lwd = 0.1,
                   label_names = c("Country", "Feature", "Normalized Value"),
                   fontsize_row = 5, fontsize_col = 5,
                   labCol = colnames(mat),
                   labRow = rownames(mat),
                   heatmap_layers = theme(axis.line=element_blank())
    )
    
    return(p)
  })
  
  output$bar_chart_static <- renderPlotly({
    
    variable = input$variable_pie_chart
    
    if(variable!="None"){
      temp <- data.frame(
        "Country" = europe.clipped$Country,
        "Variable" = pieChartVariableInput(),
        stringsAsFactors = FALSE
      )
      temp = temp[complete.cases(temp), ]
      temp$Country <- factor(temp$Country, levels = unique(temp$Country)[order(temp$Variable, decreasing = F)])
      plot_ly(temp, x = ~Variable, y = ~Country, type = "bar", orientation = 'h')%>%
        add_lines(x = mean(pieChartVariableInput(), na.rm = T),
                  line = list(color = 'red'),
                  name = "Average", showlegend = F) %>%
        layout(
          xaxis = list(
            title = as.character(variable)
          ))
      
    }
    
  })
  
  output$pie_chart_reactive <- renderPlotly({
    
    variable = input$variable_pie_chart
    
    if(variable!="None"){
      plot_ly(data(), labels = ~Country, values = ~Variable, type = 'pie') %>%
        layout(title = as.character(variable),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
  })
  
  output$bar_chart_reactive <- renderPlotly({
    
    variable = input$variable_pie_chart
    
    if(variable!="None"){
      temp = data()
      temp = temp[-which(temp$Country=="Average"), ]
      temp$Country <- factor(temp$Country, levels = unique(temp$Country)[order(temp$Variable, decreasing = F)])
      plot_ly(temp, x = ~Variable, y = ~Country, type = "bar", orientation = 'h')%>%
        add_lines(x = mean(pieChartVariableInput(), na.rm = T),
                  line = list(color = 'red'),
                  name = "Average", showlegend = F) %>%
        layout(
          xaxis = list(
            title = as.character(variable)
          ))
      
    }
    
  })
  
  output$radar_chart <- renderPlotly({
    
    selected_country = input$country_radar_chart
    temp = europe.clipped[which(europe.clipped$Country %in% selected_country),]
    temp_vector = c(
      temp$GDP/mean(europe.clipped$GDP, na.rm = TRUE),
      temp$Inflation/mean(europe.clipped$Inflation, na.rm = TRUE),
      temp$Life.expect/mean(europe.clipped$Life.expect, na.rm = TRUE),
      temp$Military/mean(europe.clipped$Military, na.rm = TRUE),
      # temp$Pop.growth/mean(europe.clipped$Pop.growth, na.rm = TRUE),
      temp$Unemployment/mean(europe.clipped$Unemployment, na.rm = TRUE)
    )*100
    
    if(input$country_radar_chart2=="None"){
      plot_ly(
        type = 'scatterpolar',
        r = temp_vector,
        # theta = c("GDP" , "Inflation" , "Life Expectancy" , "Military" , "Population Growth", "Unemployment"),
        theta = c("GDP" , "Inflation" , "Life Expectancy" , "Military" , "Unemployment"),
        mode = 'markers',
        fill = 'toself',
        fillcolor = 'rgba(168, 216, 234, 0.5)'
      ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,max(temp_vector))
            )
          ),
          showlegend = F
        )
    }else{
      
      selected_country2 = input$country_radar_chart2
      temp2 = europe.clipped[which(europe.clipped$Country %in% selected_country2),]
      temp_vector2 = c(
        temp2$GDP/mean(europe.clipped$GDP, na.rm = TRUE),
        temp2$Inflation/mean(europe.clipped$Inflation, na.rm = TRUE),
        temp2$Life.expect/mean(europe.clipped$Life.expect, na.rm = TRUE),
        temp2$Military/mean(europe.clipped$Military, na.rm = TRUE),
        # temp2$Pop.growth/mean(europe.clipped$Pop.growth, na.rm = TRUE),
        temp2$Unemployment/mean(europe.clipped$Unemployment, na.rm = TRUE)
      )*100
      
      plot_ly(
        type = 'scatterpolar',
        mode = 'markers',
        fill = 'toself'
      ) %>%
        add_trace(
          r = temp_vector,
          # theta = c("GDP" , "Inflation" , "Life Expectancy" , "Military" , "Population Growth", "Unemployment"),
          theta = c("GDP" , "Inflation" , "Life Expectancy" , "Military", "Unemployment"),
          name = as.character(input$country_radar_chart),
          fillcolor = 'rgba(168, 216, 234, 0.5)'
        ) %>%
        add_trace(
          r = temp_vector2,
          # theta = c("GDP" , "Inflation" , "Life Expectancy" , "Military" , "Population Growth", "Unemployment"),
          theta = c("GDP" , "Inflation" , "Life Expectancy" , "Military", "Unemployment"),
          name = as.character(input$country_radar_chart2),
          fillcolor = 'rgba(255, 212, 96, 0.5)'
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,max(temp_vector, temp_vector2))
            )
          )
        )
    }
  })
  
  output$lollipop_plot <- renderPlot({
    
    if(input$country_radar_chart2=="None"){
      
      selected_country = input$country_radar_chart
      temp = europe.clipped[which(europe.clipped$Country %in% selected_country),]
      temp_vector = c(
        temp$GDP/mean(europe.clipped$GDP, na.rm = TRUE),
        temp$Inflation/mean(europe.clipped$Inflation, na.rm = TRUE),
        temp$Life.expect/mean(europe.clipped$Life.expect, na.rm = TRUE),
        temp$Military/mean(europe.clipped$Military, na.rm = TRUE),
        # temp$Pop.growth/mean(europe.clipped$Pop.growth, na.rm = TRUE),
        temp$Unemployment/mean(europe.clipped$Unemployment, na.rm = TRUE)
      )*100
      temp_vector <- as.data.frame(t(temp_vector))
      # colnames(temp_vector) <- c("GDP" , "Inflation" , "Life Expectancy" , "Military" , "Population Growth", "Unemployment")
      colnames(temp_vector) <- c("GDP" , "Inflation" , "Life Expectancy" , "Military" , "Unemployment")
      
      # Barplot
      p <- temp_vector %>% t() %>% as.data.frame() %>% add_rownames() %>% arrange(V1) %>% mutate(rowname=factor(rowname, rowname)) %>%
        ggplot( aes(x=rowname, y=V1)) +
        geom_segment( aes(x=rowname ,xend=rowname, y=0, yend=V1), color="grey") +
        geom_point(size=5, color="#A8D8EA", alpha = 1) +
        scale_y_continuous(breaks = seq(0, max(temp_vector)+10, by = 10)) +
        coord_flip() +
        theme_ipsum() +
        theme(
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text = element_text( size=48 ),
          legend.position="none"
        ) +
        # ylim(0,max(temp_vector)+10) +
        ylab("Percentage %") +
        xlab("")
      
      return(p)
      
    }else{
      
      selected_country = input$country_radar_chart
      temp = europe.clipped[which(europe.clipped$Country %in% selected_country),]
      temp_vector = c(
        temp$GDP/mean(europe.clipped$GDP, na.rm = TRUE),
        temp$Inflation/mean(europe.clipped$Inflation, na.rm = TRUE),
        temp$Life.expect/mean(europe.clipped$Life.expect, na.rm = TRUE),
        temp$Military/mean(europe.clipped$Military, na.rm = TRUE),
        # temp$Pop.growth/mean(europe.clipped$Pop.growth, na.rm = TRUE),
        temp$Unemployment/mean(europe.clipped$Unemployment, na.rm = TRUE)
      )*100
      
      selected_country2 = input$country_radar_chart2
      temp2 = europe.clipped[which(europe.clipped$Country %in% selected_country2),]
      temp_vector2 = c(
        temp2$GDP/mean(europe.clipped$GDP, na.rm = TRUE),
        temp2$Inflation/mean(europe.clipped$Inflation, na.rm = TRUE),
        temp2$Life.expect/mean(europe.clipped$Life.expect, na.rm = TRUE),
        temp2$Military/mean(europe.clipped$Military, na.rm = TRUE),
        # temp2$Pop.growth/mean(europe.clipped$Pop.growth, na.rm = TRUE),
        temp2$Unemployment/mean(europe.clipped$Unemployment, na.rm = TRUE)
      )*100
      
      data <- as.data.frame(rbind(temp_vector, temp_vector2))
      # colnames(data) <- c("GDP" , "Inflation" , "Life Expectancy" , "Military" , "Population Growth", "Unemployment")
      colnames(data) <- c("GDP" , "Inflation" , "Life Expectancy" , "Military" , "Unemployment")
      row.names(data) <- c("V1", "V2")
      
      # Barplot
      p <- data %>% t() %>% as.data.frame() %>% add_rownames() %>% arrange(V1) %>% mutate(rowname=factor(rowname, rowname)) %>%
        ggplot( aes(x=rowname, y=V1)) +
        geom_segment( aes(x=rowname ,xend=rowname, y=V2, yend=V1), color="grey") +
        geom_point(size=5, color="#A8D8EA", alpha = 1) +
        geom_point(aes(y=V2), size=5, color='#FFD460', alpha = 1) +
        scale_y_continuous(breaks = seq(0, max(data)+10, by = 10)) +
        coord_flip() +
        theme_ipsum() +
        theme(
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text = element_text( size=48 )
        ) +
        # ylim(0,max(data)+10) +
        ylab("Percentage %") +
        xlab("")
      
      return(p)
    }
    
    
  })
  
  output$splom <- renderPlotly({
    axis = list(showline=FALSE,
                zeroline=FALSE,
                gridcolor='#ffff',
                ticklen=4,
                titlefont=list(size=13))
    
    p <- eu[1:28,] %>%
      plot_ly() %>%
      add_trace(
        type = 'splom',
        dimensions = list(
          list(label='GDP', values=~GDP),
          list(label='Inflation', values=~Inflation),
          list(label='Life Expectancy', values=~Life.expect),
          list(label='Military', values=~Military),
          list(label='Population Growth', values=~Pop.growth),
          list(label='Unemployment', values=~Unemployment)
        ),
        text=~Country,
        marker = list(
          color = 'rgb(255,0,0)',
          size = 7,
          line = list(
            width = 1,
            color = 'rgb(230,230,230)'
          )
        )
      ) %>%
      layout(
        title= 'Europe Stats',
        hovermode='closest',
        dragmode= 'select',
        plot_bgcolor='rgba(240,240,240, 0.95)',
        xaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4, titlefont=list(size=13)),
        yaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4, titlefont=list(size=13)),
        xaxis2=axis,
        xaxis3=axis,
        xaxis4=axis,
        xaxis5=axis,
        yaxis2=axis,
        yaxis3=axis,
        yaxis4=axis,
        yaxis5=axis
      )
    return(p %>% style(diagonal = list(visible = F), showupperhalf = F))
  })
  
  output$corrgram_pie <- renderPlot({
    corrgram(eu[1:28,-c(1,2)], order=TRUE,
             lower.panel=panel.shade, upper.panel=panel.pie,
             diag.panel=panel.minmax, text.panel=panel.txt)
  })
  
  output$corrgram_pts <- renderPlot({
    corrgram(eu[1:28,-c(1,2)], order=TRUE,
             lower.panel=panel.shade, upper.panel=panel.pts,
             diag.panel=panel.minmax, text.panel=panel.txt)
  })
  
  output$ggpairs_graph <- renderPlot({
    # ggpairs(eu[1:28,-c(1,2)])
    ggpairs(eu[1:28,-c(1,2)], lower = list(continuous = ggpairs_fn))
  })
  
  
  
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(europe.clipped))
  )
  
  output$ggplot_toggle_points <- renderPlot({
    # Plot the kept and excluded points as two separate data sets
    keep    <- europe.clipped[ vals$keeprows, , drop = FALSE]
    exclude <- europe.clipped[!vals$keeprows, , drop = FALSE]
    
    
    ggplot(keep, aes_string(input$radio_ggplot1, input$radio_ggplot2)) + 
      geom_point() +
      geom_smooth(method = lm, fullrange = TRUE, color = "blue", fill = "blue") +
      geom_smooth(method = loess, fullrange = TRUE, color = "red", fill = "red") +
      geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25)
      # coord_cartesian(xlim = c(1.5, 5.5), ylim = c(5,35))
  })
  
  selected_keepX <- reactive({
    keep <- europe.clipped[ vals$keeprows, , drop = FALSE]
    
    keepX <- switch (input$radio_ggplot1,
                    "Area" = keep$Area,
                    "GDP" = keep$GDP,
                    "Inflation" = keep$Inflation,
                    "Life.expect" = keep$Life.expect,
                    "Military" = keep$Military,
                    "Pop.growth" = keep$Pop.growth,
                    "Unemployment" = keep$Unemployment
            )
    return(keepX)
    
  })
  
  selected_keepY <- reactive({
    keep <- europe.clipped[ vals$keeprows, , drop = FALSE]
    
    keepY <- switch (input$radio_ggplot2,
                     "Area" = keep$Area,
                     "GDP" = keep$GDP,
                     "Inflation" = keep$Inflation,
                     "Life.expect" = keep$Life.expect,
                     "Military" = keep$Military,
                     "Pop.growth" = keep$Pop.growth,
                     "Unemployment" = keep$Unemployment
    )
    return(keepY)
    
  })
  
  
  output$boxplotX_toggle <- renderPlotly({
    
    yaxis <- as.character(input$radio_ggplot1)
    boxplot_title <- paste("Box Plot for", yaxis, sep = " ")

    plot_ly(y = ~selected_keepX(), type="box", name = "All countries", line = list(color = 'rgb(51,102,0)')) %>%
      layout(title = boxplot_title,
             yaxis = list(
               title = as.character(yaxis)
             ))
    
  })
  
  output$boxplotY_toggle <- renderPlotly({
    
    yaxis <- as.character(input$radio_ggplot2)
    boxplot_title <- paste("Box Plot for", yaxis, sep = " ")

    plot_ly(y = ~selected_keepY(), type="box", name = "All countries", line = list(color = 'rgb(204,102,0)')) %>%
      layout(title = boxplot_title,
        yaxis = list(
          title = as.character(yaxis)
        ))
    
  })
  
  # Toggle points that are clicked
  observeEvent(input$toggle_points_click, {
    res <- nearPoints(europe.clipped, input$toggle_points_click, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(europe.clipped, input$toggle_points_brush, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(europe.clipped))
  })
  
  
  
  select_algorithm = eventReactive(input$display,{
    switch(input$select_algorithm,
           "None"= "None",
           "Hierarchical Clustering (HC)" = "HC",
           "Kmeans" = "Kmeans"
    )
  })
  
  
  select_nclusters = eventReactive(input$display,{
    input$select_Nclusters
  })
  
  select_npca_dimensions = eventReactive(input$display,{
    input$pca_nDimensions
  })
  
  select_pca_checkbox = eventReactive(input$display,{
    input$pca_checkbox
  })
  
  observeEvent(input$display,{
    output$map_cluster = renderLeaflet({
      cluster_function(select_algorithm(), select_nclusters(), "map", select_pca_checkbox(), select_npca_dimensions())
    })
  })
  observeEvent(input$display,{
    output$dimensions = renderPlot({
      cluster_function(select_algorithm(),select_nclusters(), "dimensions", select_pca_checkbox(), select_npca_dimensions())
    })
  })
  observeEvent(input$display,{
    output$dendrogram = renderPlot({
      cluster_function(select_algorithm(),select_nclusters(), "dendrogram", select_pca_checkbox(), select_npca_dimensions())
    })
  })
  
  observeEvent(input$display,{
    output$clustering_validation = renderPlot({
      cluster_function(select_algorithm(),select_nclusters(), "validation", select_pca_checkbox(), select_npca_dimensions())
    })
  })
  
  observeEvent(input$display,{
    output$pairs_plot_clustering = renderPlotly({
      cluster_function(select_algorithm(),select_nclusters(), "pairs_plot", select_pca_checkbox(), select_npca_dimensions())
    })
  })
  
  observeEvent(input$clear, {
    output$map_cluster = renderText({
      NULL
    })
  })
  observeEvent(input$clear, {
    output$dimensions = renderText({
      NULL
    })
  })
  observeEvent(input$clear, {
    output$dendrogram = renderText({
      NULL
    })
  })
  observeEvent(input$clear, {
    output$clustering_validation = renderText({
      NULL
    })
  })
  observeEvent(input$clear, {
    output$pairs_plot_clustering = renderPlotly({
      plotly_empty() %>% 
        config(staticPlot = TRUE)
    })
  })
  
  output$pca_summary = renderDataTable({
    pca_function()
  })
  
  output$variances_donut = renderPlotly({
    variance_donut_function()
  })
  
  output$var_explained = renderPlotly({
    variance_explained_function(input$variance_slider)
  })
  
  output$pca_dimensions = renderPlot({
    pca_dimensions_function(input$first_dimension, input$second_dimension)
  })
  
  output$var_explained_bars = renderPlot({
    variance_explained_bars_function()
  })
  
  output$pca_contribution_1 = renderPlot({
    variance_contribution_function(input$first_dimension)
  })
  
  output$pca_contribution_2 = renderPlot({
    variance_contribution_function(input$second_dimension)
  })
  
})

cluster_function = function(algorithm, nclusters, graph, pca, nDimensions) {
  
  if (nclusters<2) {
    showModal(modalDialog(
      title = h2(strong("Oooops!")),
      "The number of clusters selected have to be between 2 and 28. Please try again :)."
    ))  
  } else if(nclusters>28) {
    showModal(modalDialog(
      title = h2(strong("Oooops!")),
      "The number of clusters selected have to be between 2 and 28. Please try again :)."
    ))  
  } else if (algorithm == "None"){
    showModal(modalDialog(
      title = h2(strong("Oooops!")),
      "You forgot to select an algorithm. Please try again :)."
    ))  
  } else if(algorithm == "Kmeans" && pca==F){
    
    if (graph == "validation") {
      eu.validation = read.csv("europe.csv")
      rownames(eu.validation) = eu.validation$Country
      eu.validation$Country = NULL
      eu.validation = as.data.frame(scale(eu.validation))
      km.validation = eclust(eu.validation, FUNcluster = "kmeans", k = nclusters, graph = F, seed = 1)
      return(fviz_silhouette(km.validation, main = ""))
      } else if(graph == "dimensions") {
        eu.dim= read.csv("europe.csv")
        rownames(eu.dim) = eu.dim$Country
        eu.dim$Country = NULL
        eu.scaled = as.data.frame(scale(eu.dim))
        km.validation = eclust(eu.scaled, FUNcluster = "kmeans", k = nclusters, graph = F, seed = 1)
        return(fviz_cluster(km.validation, main = ""))
      } else if(graph == "dendrogram") {
        showModal(modalDialog(
          title = h2(strong("Oooops!")),
          "You can not display a dendrogram with Kmeans algorithm. Please try again :)."
        ))
      } else if(graph == "map") {
        eu.rows= read.csv("europe.csv")
        rownames(eu.rows) = eu.rows$Country
        eu.rows$Country = NULL
        eu.scaled = as.data.frame(scale(eu.rows))
        set.seed(111)
        km = kmeans(eu.scaled,nclusters)
        km$cluster
        c2 = data.frame(km$cluster)
        colnames(c2) = c("cluster")
        c2$Country = rownames(c2)
        c2[nrow(c2)+10,] = NA
        c2[29,"Country"] = "Albania"
        c2[30,"Country"] = "Bosnia and Herzegovina"
        c2[31,"Country"] = "Belarus"
        c2[32,"Country"] = "France"
        c2[33,"Country"] = "Romania"
        c2[34,"Country"] = "Montenegro"
        c2[35,"Country"] = "Moldova"
        c2[36,"Country"] = "North Macedonia"
        c2[37,"Country"] = "Kosovo"
        c2[38,"Country"] = "Republic of Serbia"
        c2$Country = as.factor(c2$Country)
        c2$cluster = as.factor(c2$cluster)
        europe.clipped_Kmeans_map = merge(europe.clipped, c2, by = "Country")
        return(tmap_leaflet(tm_shape(europe.clipped_Kmeans_map) + tm_polygons("cluster", id="Country", pallete="Greens") +
                              tmap_mode("view")))
      } else if(graph=="pairs_plot") {
        eu.rows= read.csv("europe.csv")
        rownames(eu.rows) = eu.rows$Country
        eu.orig = eu.rows
        eu.rows$Country = NULL
        eu.scaled = as.data.frame(scale(eu.rows))
        set.seed(111)
        km = kmeans(eu.scaled,nclusters)
        c2 = data.frame(km$cluster)
        colnames(c2) = c("cluster")
        c2$Country = rownames(c2)
        c2$Country = as.factor(c2$Country)
        c2$cluster = as.factor(c2$cluster)
        eu.kmeans = merge(eu.orig, c2, by = "Country")
        pm <- GGally::ggpairs(eu.kmeans[,-c(1,2)], aes(color = cluster))
        return(pm)
      }
      else {
        hist(rnorm(100))
      }
    } else if (algorithm == "HC" && pca == F) {
        if (graph == "validation") {
          eu.validation= read.csv("europe.csv")
          rownames(eu.validation) = eu.validation$Country
          eu.validation$Country = NULL
          eu.validation = as.data.frame(scale(eu.validation))
          hc.validation = eclust(eu.validation, FUNcluster = "hclust", k = nclusters, hc_metric = "euclidean", hc_method = "ward.D2")
          return(fviz_silhouette(hc.validation, main = ""))
        } else if (graph == "dimensions"){
            eu.dim = read.csv("europe.csv")
            rownames(eu.dim) = eu.dim$Country
            eu.dim$Country = NULL
            eu.dim = as.data.frame(scale(eu.dim))
            hc.dim = eclust(eu.dim, FUNcluster = "hclust", k = nclusters, hc_metric = "euclidean", hc_method = "ward.D2")
            return(fviz_cluster(hc.dim, main = ""))
          } else if(graph == "dendrogram") {
              eu.rows= read.csv("europe.csv")
              rownames(eu.rows) = eu.rows$Country
              eu.rows$Country = NULL
              eu.scaled = as.data.frame(scale(eu.rows))
              hc.dend = eclust(eu.scaled, FUNcluster = "hclust", k = nclusters, hc_metric = "euclidean", hc_method = "ward.D2")
              return(fviz_dend(hc.dend, main = ""))
            } else if (graph == "map"){
                eu.rows= read.csv("europe.csv")
                rownames(eu.rows) = eu.rows$Country
                eu.rows$Country = NULL
                eu.scaled = as.data.frame(scale(eu.rows))
                hierarchical.cluster.ward.D2 = hclust(dist(eu.scaled, method = "euclidean"),method = "ward.D2")
                c1 = data.frame(cutree(hierarchical.cluster.ward.D2, nclusters))
                c1[nrow(c1)+10,] = NA
                colnames(c1) = c("cluster")
                c1$Country = rownames(c1)
                c1[29,"Country"] = "Albania"
                c1[30,"Country"] = "Bosnia and Herzegovina"
                c1[31,"Country"] = "Belarus"
                c1[32,"Country"] = "France"
                c1[33,"Country"] = "Romania"
                c1[34,"Country"] = "Montenegro"
                c1[35,"Country"] = "Moldova"
                c1[36,"Country"] = "North Macedonia"
                c1[37,"Country"] = "Kosovo"
                c1[38,"Country"] = "Republic of Serbia"
                c1$Country = as.factor(c1$Country)
                c1$cluster = as.factor(c1$cluster)
                europe.clipped_HC_map = merge(europe.clipped, c1, by = "Country")
                return(tmap_leaflet(tm_shape(europe.clipped_HC_map) + tm_polygons("cluster", id="Country", pallete="Greens") +
                                      tmap_mode("view")))
            } else if (graph=="pairs_plot"){
                eu.rows= read.csv("europe.csv")
                rownames(eu.rows) = eu.rows$Country
                eu.orig = eu.rows
                eu.rows$Country = NULL
                eu.scaled = as.data.frame(scale(eu.rows))
                hierarchical.cluster.ward.D2 = hclust(dist(eu.scaled, method = "euclidean"),method = "ward.D2")
                c1 = data.frame(cutree(hierarchical.cluster.ward.D2, nclusters))
                colnames(c1) = c("cluster")
                c1$Country = rownames(c1)
                c1$Country = as.factor(c1$Country)
                c1$cluster = as.factor(c1$cluster)
                eu.HC = merge(eu.orig, c1, by = "Country")
                pm <- GGally::ggpairs(eu.HC[,-c(1,2)], aes(color = cluster))
                return(pm)
          }
      else {
            hist(rnorm(100))
          }
    } else if(algorithm == "Kmeans" && pca==T){
      if (graph == "validation") {
        eu = read.csv("europe.csv")
        rownames(eu) = eu$Country
        eu.pca = PCA(eu, quali.sup=1, ncp=9, scale.unit = T, graph = F)
        eu.pca = data.frame(eu.pca$ind$coord)
        km.validation = eclust(eu.pca[,1:nDimensions,drop=F], FUNcluster = "kmeans", k = nclusters, seed = 1)
        return(fviz_silhouette(km.validation, main = ""))
      } else if (graph == "dimensions") {
        eu.a = read.csv("europe.csv")
        rownames(eu.a) = eu.a$Country
        eu.a$Country = NULL
        eu.pca_map = PCA(eu.a,ncp=9, scale.unit = T, graph = F)
        eu.pca_map.a = data.frame(eu.pca_map$ind$coord)
        set.seed(1)
        km.a = kmeans(eu.pca_map.a[,1:nDimensions],nclusters)
        return(fviz_cluster(km.a, eu.pca_map.a, main = ""))
        } else if(graph == "dendrogram") {
          showModal(modalDialog(
            title = h2(strong("Oooops!")),
            "You can not display a dendrogram with Kmeans algorithm. Please try again :)."
          ))
        } else if(graph == "map") {
          eu = read.csv("europe.csv")
          rownames(eu) = eu$Country
          eu.pca_map = PCA(eu, quali.sup=1, ncp=9, scale.unit = T, graph = F)
          eu.pca_map = data.frame(eu.pca_map$ind$coord)
          if (nDimensions == 1){
            eu.pca_map = eu.pca_map[,1:1, drop=F]
            set.seed(1)
            km = kmeans(eu.pca_map, nclusters)
          } else {
            set.seed(1)
            km = kmeans(eu.pca_map[,1:nDimensions],nclusters)
          }
          c2 = data.frame(km$cluster)
          colnames(c2) = c("cluster")
          c2$Country = eu$Country
          c2$Country = as.character(c2$Country)
          c2[nrow(c2)+10,] = NA
          c2[29,"Country"] = "Albania"
          c2[30,"Country"] = "Bosnia and Herzegovina"
          c2[31,"Country"] = "Belarus"
          c2[32,"Country"] = "France"
          c2[33,"Country"] = "Romania"
          c2[34,"Country"] = "Montenegro"
          c2[35,"Country"] = "Moldova"
          c2[36,"Country"] = "North Macedonia"
          c2[37,"Country"] = "Kosovo"
          c2[38,"Country"] = "Republic of Serbia"
          c2$Country = as.factor(c2$Country)
          c2$cluster = as.factor(c2$cluster)
          europe.clipped_PCA_Kmeans_map = merge(europe.clipped, c2, by = "Country")
          return(tmap_leaflet(tm_shape(europe.clipped_PCA_Kmeans_map) + tm_polygons("cluster", id="Country", pallete="Greens") +
                                tmap_mode("view")))
        } else if (graph=="pairs_plot"){
            eu = read.csv("europe.csv")
            rownames(eu) = eu$Country
            eu.orig = eu
            eu.pca_map = PCA(eu, quali.sup=1, ncp=9, scale.unit = T, graph = F)
            eu.pca_map = data.frame(eu.pca_map$ind$coord)
            if (nDimensions == 1){
              eu.pca_map = eu.pca_map[,1:1, drop=F]
              set.seed(1)
              km = kmeans(eu.pca_map, nclusters)
            } else {
              set.seed(1)
              km = kmeans(eu.pca_map[,1:nDimensions],nclusters)
            }
            c2 = data.frame(km$cluster)
            colnames(c2) = c("cluster")
            c2$Country = eu$Country
            c2$Country = as.character(c2$Country)
            c2$Country = as.factor(c2$Country)
            c2$cluster = as.factor(c2$cluster)
            eu_PCA_Kmeans = merge(eu.orig, c2, by = "Country")
            pm <- GGally::ggpairs(eu_PCA_Kmeans[,-c(1,2)], aes(color = cluster))
            return(pm) 
        }
        else {
          hist(0)
        }
    } else if (algorithm == "HC" && pca == T) {
      if (graph == "validation") {
        eu = read.csv("europe.csv")
        eu.HC_pca = PCA(eu, quali.sup=1, ncp=9, scale.unit = T, graph = F)
        eu.HC_pca = data.frame(eu.HC_pca$ind$coord)
        hc.PCA_validation = eclust(eu.HC_pca[,1:nDimensions, drop=F], FUNcluster = "hclust", k =nclusters, hc_metric = "euclidean", hc_method = "ward.D2", seed = 1)
        fviz_silhouette(hc.PCA_validation, main = "")
      } else if (graph == "dimensions"){
          eu = read.csv("europe.csv")
          rownames(eu) = eu$Country
          eu.HC_pca_dimensions = PCA(eu, quali.sup=1, ncp=9, scale.unit = T, graph = F)
          eu.HC_pca_dimensions = data.frame(eu.HC_pca_dimensions$ind$coord)
          hierarchical.cluster.ward.D2_dimensions = hclust(dist(eu.HC_pca_dimensions[,1:nDimensions, drop = F], method = "euclidean"), method = "ward.D2")
          return(fviz_cluster(list(data = eu.HC_pca_dimensions , cluster = cutree(hierarchical.cluster.ward.D2_dimensions, nclusters), main = "")))
          
        } else if(graph == "dendrogram") {
          eu = read.csv("europe.csv")
          rownames(eu) = eu$Country
          eu.HC_pca_dendogram = PCA(eu, quali.sup=1, ncp=9, scale.unit = T, graph = F)
          eu.HC_pca_dendogram = data.frame(eu.HC_pca_dendogram$ind$coord)
          hc.dend = eclust(eu.HC_pca_dendogram, FUNcluster = "hclust", k = nclusters, hc_metric = "euclidean", hc_method = "ward.D2")
          return(fviz_dend(hc.dend, main =""))
        } else if (graph == "map") {
            eu = read.csv("europe.csv")
            rownames(eu) = eu$Country
            eu.HC_pca_map = PCA(eu, quali.sup=1, ncp=9, scale.unit = T, graph = F)
            eu.HC_pca_map = data.frame(eu.HC_pca_map$ind$coord)
          if (nDimensions == 1){
            eu.HC_pca_map = eu.HC_pca_map[,1:1, drop=F]
            hierarchical.cluster.ward.D2_map= hclust(dist(eu.HC_pca_map, method = "euclidean"),method = "ward.D2")
          } else {
              hierarchical.cluster.ward.D2_map= hclust(dist(eu.HC_pca_map[,1:nDimensions], method = "euclidean"),method = "ward.D2")
          }
            c1 = data.frame(cutree(hierarchical.cluster.ward.D2_map, nclusters))
            colnames(c1) = c("cluster")
            c1$Country = eu$Country
            c1$Country = as.character(c1$Country)
            c1[nrow(c1)+10,] = NA
            c1[29,"Country"] = "Albania"
            c1[30,"Country"] = "Bosnia and Herzegovina"
            c1[31,"Country"] = "Belarus"
            c1[32,"Country"] = "France"
            c1[33,"Country"] = "Romania"
            c1[34,"Country"] = "Montenegro"
            c1[35,"Country"] = "Moldova"
            c1[36,"Country"] = "North Macedonia"
            c1[37,"Country"] = "Kosovo"
            c1[38,"Country"] = "Republic of Serbia"
            c1$Country = as.factor(c1$Country)
            c1$cluster = as.factor(c1$cluster)
            europe.clipped_PCA_HC_map = merge(europe.clipped, c1, by = "Country")
            return(tmap_leaflet(tm_shape(europe.clipped_PCA_HC_map) + tm_polygons("cluster", id="Country", pallete="Greens") +
                                  tmap_mode("view")))
          
        } else if(graph=="pairs_plot"){
            eu = read.csv("europe.csv")
            rownames(eu) = eu$Country
            eu.orig = eu
            eu.HC_pca_map = PCA(eu, quali.sup=1, ncp=9, scale.unit = T, graph = F)
            eu.HC_pca_map = data.frame(eu.HC_pca_map$ind$coord)
            if (nDimensions == 1){
              eu.HC_pca_map = eu.HC_pca_map[,1:1, drop=F]
              hierarchical.cluster.ward.D2_map= hclust(dist(eu.HC_pca_map, method = "euclidean"),method = "ward.D2")
            } else {
              hierarchical.cluster.ward.D2_map= hclust(dist(eu.HC_pca_map[,1:nDimensions], method = "euclidean"),method = "ward.D2")
            }
            c1 = data.frame(cutree(hierarchical.cluster.ward.D2_map, nclusters))
            colnames(c1) = c("cluster")
            c1$Country = eu$Country
            c1$Country = as.character(c1$Country)
            c1$Country = as.factor(c1$Country)
            c1$cluster = as.factor(c1$cluster)
            eu.PCA_HC = merge(eu.orig, c1, by = "Country")
            pm <- GGally::ggpairs(eu.PCA_HC[,-c(1,2)], aes(color = cluster))
            return(pm) 
        }
      else {
          hist(0)
        }
        
      } 
}

map_function = function(variable) {
  
  # min <- variable[[2]]
  # max <- variable[[3]]
  # breaks = sort(c(min, sliderValue, max))
  
  if(variable[[1]] == "None") {
    
  } else {
    # tmap_leaflet(tm_shape(europe.clipped) + tm_polygons(variable[[1]], id=variable[[1]], breaks = breaks) + 
    #                tmap_mode("view"))
    tmap_leaflet(tm_shape(europe.clipped) + tm_polygons(variable[[1]], id="Country", style = "cont", palette = "YlOrRd") + 
                   tmap_mode("view"))
  }
}

# map_function_clustering = function() {
#   tmap_leaflet(tm_shape(europe.clipped) + tm_polygons(variable, id=variable, pallete="Greens") + 
#                  tmap_mode("view"))
# }

pca_function = function() {
  eu = read.csv("europe.csv")
  eu.pca_map = PCA(eu, quali.sup=1, ncp=9, scale.unit = T, graph = F)
  eig = data.frame(eu.pca_map$eig)
  is.num = sapply(eig, is.numeric)
  eig[is.num] = lapply(eig[is.num], round, 2)
  colnames(eig) = c("Eigenvalues", "Percentage of variance", "Cumulative percentage of variance")
  return(eig)
}

variance_donut_function = function() {
  eu = read.csv("europe.csv")
  eu.pca = PCA(eu, quali.sup=1, ncp=8, scale.unit = T, graph = F)
  variance_pca = data.frame(eu.pca$eig)
  variance_pca$components = rownames(variance_pca)
  is.num = sapply(variance_pca, is.numeric)
  variance_pca[is.num] = lapply(variance_pca[is.num], round, 2)
  p_pca = plot_ly(labels= variance_pca$components , values = variance_pca$percentage.of.variance)%>%
    add_pie(hole = 0.75) %>%
    layout(title = "",  showlegend = T,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  return(p_pca)
}

variance_explained_function = function(ncomp) {
  eu = read.csv("europe.csv")
  eu.pca = PCA(eu, quali.sup=1, ncp=9, scale.unit = T, graph = F)
  variance_pca = data.frame(eu.pca$eig)
  variance_pca$components = rownames(variance_pca)
  variance_pca$not_explained = 100-variance_pca$cumulative.percentage.of.variance
  is.num = sapply(variance_pca, is.numeric)
  variance_pca[is.num] = lapply(variance_pca[is.num], round, 2)
  variance_pca$number_comp = c("1 comp   ", 
                               "2 comps  ",
                               "3 comps  ", 
                               "4 comps  ", 
                               "5 comps  ", 
                               "6 comps  ",
                               "7 comps  ")
  p <- plot_ly(variance_pca, x = ~cumulative.percentage.of.variance[ncomp], y = ~number_comp[ncomp], type = 'bar',
               orientation = 'h', 
               name = "Explained",
               height= 150, width = 630) %>%
    add_trace(x = ~not_explained[ncomp], name = "Not explained") %>%
    layout(barmode = 'stack',
           xaxis = list(title = "Percentage (%) of variance explained"),
           yaxis = list(title = ""))
  return(p)
  
}

pca_dimensions_function = function(dim1, dim2) {
  eu = read.csv("europe.csv")
  eu.pca = PCA(eu, quali.sup=1, ncp=8, scale.unit = T, graph = F)
  if(is.na(dim1)|| is.na(dim2)) {
    #return(fviz_pca_var(eu.pca, axes = c(1,2)))
    return(fviz_pca_var(eu.pca, axes = c(1,2), col.var = "contrib",
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
    ))
  }else {
    #return(fviz_pca_var(eu.pca, axes = c(dim1,dim2)))
    return(fviz_pca_var(eu.pca, axes = c(dim1,dim2), col.var = "contrib",
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
    ))
  }
}

variance_explained_bars_function = function() {
  eu = read.csv("europe.csv")
  eu.pca = PCA(eu, quali.sup=1, ncp=8, scale.unit = T, graph = F)
  return(fviz_eig(eu.pca))
}

variance_contribution_function = function (dim) {
  eu = read.csv("europe.csv")
  eu.pca = PCA(eu, quali.sup=1, ncp=8, scale.unit = T, graph = F)
  if(is.na(dim)) {
    return(fviz_contrib(eu.pca, choice = "var", axes = 1, top = 7))
  } else {
    return(fviz_contrib(eu.pca, choice = "var", axes = dim, top = 7))
  }
}

ggpairs_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}



