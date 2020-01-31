<link href="styleMD.css" rel="stylesheet"></link>

# Explore Europe with the application
The shiny application is work of:

<div class="center-block row" >

<div class=col-sm-6>
  <img class="img-circle" src="bart.jpg" width="200" height = "200"/>
  <p> Bart van Rooijen </br>(<a href = "mailto:bart@student.tue.nl">b.w.m.v.rooijen@student.tue.nl</a>)</p>
</div>
<div>
  <img class="img-circle" src="panas.png" width="200" height = "200"/> 
  <p> Panagiotis Michalopoulos </br>(<a href="mailto:p.michalopoulos1@student.tue.nl">p.michalopoulos1@student.tue.nl</a>)</p>
</div>

</div> 

## About the app
This Shiny application offers a friendly interface to users that want to explore and analyse Europe Stats dataset. The application includes various features that are divided into the following tabs:  

*  **Explore Europe Stats Dataset**: This tab allows the user to explore the different variables of the dataset and study how those variables are related to each other. It is divided into two subtabs:
 * **Dataset**: The dataset is presented in the form of a table.
 * **Data exploration**: This tab includes several exploratory graphs that can be studied to understand the nature of the dataset and the correlation between the different atttributes.
 
* **Data analysis**: This tab is dedicated to data analysis tasks. First, it comes with a brief explanation about what PCA is, alongside different visualization graphs to give a better understanding of the effects of applying PCA to Europe Stats dataset. PCA is followed by the option to perform two different clustering algorithms: Hierrachical clustering and Kmeans. In addition to multiple visualization graphs that depict the results of clustering, a Silhoutte graph is present to validate the peformance of the applied clustering algorithm.
 

## About the dataset
The Europe Stats Dataset contains statistics of several european countries and it can be effectively analyzed by means of hierarchical clustering. It includes the following variables:  

* **Country**:(char) A list of 28 european countries. 
*  **Area**:(numeric) It represents the area of each country in km².
*  **GDP**:(numeric) Stands for **Gross Domestic Product** in Euros.
*  **Inflation**:(numeric) It represents the inflation of each country.
*  **Life.expect**:(numeric) It represents the average life expectency of each country in years.
*  **Military**:(numeric) It represents the military power of each country as a percentage of their GDP.
*  **Pop.growth**:(numeric) It represents the population growth of each country compared to the previous year.
*  **Unemployment**:(numeric) It represents the unemployment rate of each country.

## Packages and libraries
The application is built upon functions of different R packages. This is the complete list of packages that have been used:

* shiny
* shinydashoard
* grid
* rworldmap
* ggmap
* mapproj
* maps
* maptools
* dplyr
* eurostat
* sf
* tmap
* ggplot2
* shinyalert
* leaflet
* factoextra
* FactoMineR
* knitr
* plotly
* cluster
* fpc
* NbClust
* DT
* corrgram
* GGally
* shinycssloaders
* stringr
* hrbrthemes
* heatmaply


Execute the following command to successfully install a new package in your R environment:

```
install.packages("name of the package")
```

Then you can call any package library by executing:   

```
library(name of the packages)
```  



