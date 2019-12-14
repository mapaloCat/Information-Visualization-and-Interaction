eu = read.csv("C:/Users/Panagiotis/Documents/EIT Digital Masterschool/UPM/1st semester/Big Data/Practical Work - Visualization/bd2018_2019/big_data_shiny/europe.csv")

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
library(devtools)
library(FactoMineR)
library(factoextra)
library(plotly)
library(fmsb)



adaptDataset = function(){
  
}
  
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
eu[36,"Country"] = "Macedonia"
eu[37,"Country"] = "Kosovo"
eu[38,"Country"] = "Republic of Serbia"
eu$Country = as.factor(eu$Country)

head(eu)

# REPRESENTING DATA THROW MAP VISUALIZATION
suppressPackageStartupMessages(library(sf))
world <- st_as_sf(rnaturalearth::countries110)
europe <- dplyr::filter(world, region_un=="Europe" & name!='Russia')

names(europe)[names(europe) == 'sovereignt'] <- 'Country'

head(eu)

europe = merge(europe, eu, by="Country")

# A bounding box for continental Europe.
europe.bbox <- st_polygon(list(
  matrix(c(-25,29,45,29,45,75,-25,75,-25,29),byrow = T,ncol = 2)))

europe.clipped = suppressWarnings(st_intersection(europe, st_sfc(europe.bbox, crs=st_crs(europe))))


# REPRESENTING SOME VARIABLES THROUGH MAP VISUALIZATION 
map_simple_function = function(v_input) {
  ggplot(europe.clipped, aes(fill=v_input)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") 
}
head(europe.clipped)

ggplot(europe.clipped, aes(fill=Military)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") 

tm_shape(europe.clipped) + 
  tm_polygons("Military", id = "Military", pallete = "Greens")
tmap_mode("view")
tmap_last()

ggplot(europe.clipped, aes(fill=Life.expect)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") 

ggplot(europe.clipped, aes(fill=GDP)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") 


#################################################
min <- 3
max <- 4.3
var <- europe.clipped$Military 
color <- "black"
legend.title <- "Military"

# generate vector of fill colors for map
shades <- colorRampPalette(c("white", color))(100)

# constrain gradient to percents that occur between min and max
var <- pmax(var, min)
var <- pmin(var, max)
percents <- as.integer(cut(var, 100, 
                           include.lowest = TRUE, ordered = TRUE))
fills <- shades[percents]

# plot choropleth map
# map(europe.clipped$Country, fill = TRUE, col = fills,
#     resolution = 0, lty = 0, projection = "polyconic",
#     myborder = 0, mar = c(0,0,0,0))
ggplot(europe.clipped, aes(fill=fills)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14")

#################################################

min <- 2
max <- 4.3
range <- (max-min)
breaks = c(0, min+range/2, 4.3)
tm_shape(europe.clipped) + 
  tm_polygons("Military", id = "Country", breaks = breaks)
tmap_mode("view")
tmap_last()

####################################################

selected_countries = sort(c("Greece", "Italy", "Spain", "Austria", "Germany"))
ind = which(europe.clipped$Country %in% selected_countries)
x = europe.clipped$Military
x = x[ind]
x = c(x, mean(europe.clipped$Military, na.rm = TRUE))

data <- data.frame(
  "Country" = c(selected_countries, "Average"),
  "Military" = x
)

p <- plot_ly(data, labels = ~Country, values = ~Military, type = 'pie') %>%
  layout(title = 'example',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p

t = as.symbol("Military")
p <- plot_ly(europe.clipped, labels = ~Country, values = ~t) %>%
  add_pie(hole = 0.6) %>%
  layout(title = t,  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p
###################################################

####### Radar Plot ###############################

country = "Greece"
temp = europe.clipped[which(europe.clipped$Country %in% country),]
temp_vector = c(
  temp$GDP/mean(europe.clipped$GDP, na.rm = TRUE),
  temp$Inflation/mean(europe.clipped$Inflation, na.rm = TRUE),
  temp$Life.expect/mean(europe.clipped$Life.expect, na.rm = TRUE),
  temp$Military/mean(europe.clipped$Military, na.rm = TRUE),
  temp$Pop.growth/mean(europe.clipped$Pop.growth, na.rm = TRUE),
  temp$Unemployment/mean(europe.clipped$Unemployment, na.rm = TRUE)
)*100

country2 = "Italy"
temp2 = europe.clipped[which(europe.clipped$Country %in% country2),]
temp_vector2 = c(
  temp2$GDP/mean(europe.clipped$GDP, na.rm = TRUE),
  temp2$Inflation/mean(europe.clipped$Inflation, na.rm = TRUE),
  temp2$Life.expect/mean(europe.clipped$Life.expect, na.rm = TRUE),
  temp2$Military/mean(europe.clipped$Military, na.rm = TRUE),
  temp2$Pop.growth/mean(europe.clipped$Pop.growth, na.rm = TRUE),
  temp2$Unemployment/mean(europe.clipped$Unemployment, na.rm = TRUE)
)*100

p <- plot_ly(
  type = 'scatterpolar',
  r = temp_vector,
  theta = c("GDP" , "Inflation" , "Life Expectancy" , "Military" , "Population Growth", "Unemployment"),
  mode = 'markers',
  fill = 'toself'
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
p

plot_ly(
  type = 'scatterpolar',
  mode = 'markers',
  fill = 'toself'
) %>%
  add_trace(
    r = temp_vector,
    theta = c("GDP" , "Inflation" , "Life Expectancy" , "Military" , "Population Growth", "Unemployment"),
    name = as.character(country)
  ) %>%
  add_trace(
    r = temp_vector2,
    theta = c("GDP" , "Inflation" , "Life Expectancy" , "Military" , "Population Growth", "Unemployment"),
    name = as.character(country2)
  ) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,max(temp_vector2))
      )
    )
  )
#####################################################

############### Bubble Chart #########################
plot_ly(eu[1:28,], x = ~GDP, y = ~Military, text = ~Country, type = 'scatter', mode = 'markers', size = ~Area, sizes = c(10, 50),
          marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'Gender Gap in Earnings per University',
         xaxis = list(showgrid = FALSE,
                      range = c(min(eu[1:28,]$GDP), max(eu[1:28,]$GDP))),
         yaxis = list(showgrid = FALSE,
                      range = c(min(eu[1:28,]$Military), max(eu[1:28,]$Military))))

eu = read.csv("C:/Users/Panagiotis/Documents/EIT Digital Masterschool/UPM/1st semester/Big Data/Practical Work - Visualization/bd2018_2019/big_data_shiny/europe.csv")

############ correlation graphs ##############################################
library(corrgram)
corrgram(eu[1:28,-c(1,2)], order=TRUE,
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

corrgram(eu[1:28,-c(1,2)], order=TRUE,
         lower.panel=panel.shade, upper.panel=panel.pts,
         diag.panel=panel.minmax, text.panel=panel.txt)


############## SPLOM ##########################################################
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
    title= 'Europe Data Set',
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
p %>% style(diagonal = list(visible = F), showupperhalf = F)

#########################################################################################
# Built upon ggplot2, GGally provides templates
# for combining plots into a matrix through the ggpairs function.
# Such a matrix of plots can be useful for quickly exploring the
# relationships between multiple columns of data in a data frame.
# The lower and upper arguments to the ggpairs function specifies
# the type of plot or data in each position of the lower or upper diagonal
# of the matrix, respectively.
# For continuous X and Y data, one can specify the smooth option to
# include a regression line.
# To include more than one regression line in a plot (or to customize the
#                                                     plot in any way beyond one of the predefined types), one simply
# defines a function that returns the desired plot.
# The function can then be included in the list provided to upper or
# lower.
ggpairs_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

g = ggpairs(eu[1:28,-c(1,2)], lower = list(continuous = ggpairs_fn))
g








