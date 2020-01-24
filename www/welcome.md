<link href="styleMD.css" rel="stylesheet"></link>

# Together we explore Europe! 
The shiny application is work of:

<div class="center-block row" >

<div class=col-sm-6>
  <img class="img-circle" src="bart.jpg" width="120" height = "120"/>
  <p> Bart van Rooijen </br>(<a href = "mailto:bart@student.tue.nl">bartie@student.tue.nl</a>)</p>
</div>
<div>
  <img class="img-circle" src="panas.png" width="120" height = "120"/> 
  <p> Panagiotis Michalopoulos </br>(<a href="mailto:p.michalopoulos1@student.tue.nl">p.michalopoulos1@student.tue.nl</a>)</p>
</div>

</div> 

## About the app
This Shiny application offers a friendly interface so users can explore and analyse Europe´s dataset. The different features are divided into the following tabs:  

*  **Explore Europe dataset**: This tab allows the user to explore the different variables of the dataset as well as the relationship between themselves. It is conformed by three subtabs:
 * **Dataset**: Here you can find a table that provides features like filtering by value, ordering the variables, costume queries and much more.
 * **Data exploration**: In this tabs allow the user to visualize and compare the different values of the variables by country.
 
* **Data analysis**: This tab is dedicated to the funcionalities of data analysis against Europe's dataset. It is divided between the following subtabs:
  	 * **Principal Component Analysis (PCA)**: This tab includes a brief explanation about what PCA is. As well as different visualization graphs to obtain a deeper understanding of the results of applying PCA to Europe's dataset.
  	 * **Clustering**: This tab includes the possibility of perform two different clusterin algorithms: Hierrachical clustering and Kmeans. In addition it includes different graps options to visualize the results and the Silhoutte graph to validate the peformance of the clustering for each combination. Lastly, it also includes the possibility of performing PCA before the clustering algorithm with the desire number of dimensions selected by the user.
 

## About the dataset
The Europe's dataset contains several statistics on european countries and can be effectively analyzed by means of hierarchical clustering. It is conformed by the following variables:  

* **Country**:(numeric) A list of 28 countries from Europe. A curiosity of this data set is that France is not included. 
*  **Area**:(Int) It represents the area of each country in km².
*  **GDP**:(Int) Stands for **Gross Domestic Product** and the is express in Euros.
*  **Inflation**:(numeric) It represents the inflation of each of the countries.
*  **Life.expect**:(numeric) It represents the average life expectency of each country in years.
*  **Military**:(numeric) It represents the military power of each country as a percentage of their GDP.
*  **Pop.growth**:(numeric) It represents the population growth of each country compared to the previous year.
*  **Unemployment**:(numeric) It represents the unemployment rate of each country.

## Packages and libraries
The application has leveraged functions of different R packages. Here is the complete list of packages that have been used:

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


Execute the following command to successfully install a new package in your R environment:

```
install.packages("name of the package")
```

Then you can call any package library by executing:   

```
library(name of the packages)
```  



