<link href="styleMD.css" rel="stylesheet"></link>
# Welcome to Europe's Shiny app! 
The authors of this shiny app  are:

<div class="center-block row" >

<div class=col-sm-6>
<img class="img-circle" src="roberto.png" width="95" height = "100"/>
<p> Roberto Llop Cardenal </br>(<a href = "mailto:robertollopcardenal@gmail.com">robertollopcardenal@gmail.com</a>)</p>
</div>
  <div>
  <img class="img-circle" src="panas.png" width="90" height = "100"/> 
  <p> Panagiotis Michalopoulos </br>(<a href="mailto:panagi.michalopoulos@gmail.com">panagi.michalopoulos@gmail.com</a>)</p>
  </div>
</div> 

* The code and resources for this Shiny app can be found in [**this repository**](https://github.com/rllopcar/big_data_shiny.git)

## About Europe's Shiny app
This Shiny applications provides a friendly interface to let the users explore and analyse Europe´s dataset. The different features are separated by the following tabs:  

*  **Explore Europe dataset**: This tab allows the user to explore the different variables of the dataset as well as the relationship between themselves. It is conformed by three subtabs:
 * **Dataset**: Here you can find a table that provides features like filtering by value, ordering the variables, costume queries and much more.
 * **Data exploration**: In this tabs allow the user to visualize and compare the different values of the variables by country.
 
* **Data analysis**: This tab is dedicated to the funcionalities of data analysis against Europe's dataset. It is divided between the following subtabs:
  	 * **Principal Component Analysis (PCA)**: This tab includes a brief explanation about what PCA is. As well as different visualization graphs to obtain a deeper understanding of the results of applying PCA to Europe's dataset.
  	 * **Clustering**: This tab includes the possibility of perform two different clusterin algorithms: Hierrachical clustering and Kmeans. In addition it includes different graps options to visualize the results and the Silhoutte graph to validate the peformance of the clustering for each combination. Lastly, it also includes the possibility of performing PCA before the clustering algorithm with the desire number of dimensions selected by the user.
 

## About Europe's dataset
It contains several statistics on european countries and can be effectively analyzed by means of hierarchical clustering. It is conformed by the following variables:  

* **Country**:(numeric) A list of 28 countries from Europe. A curiosity of this data set is that France is not included. 
*  **Area**:(Int) It represents the area of each country in km².
*  **GDP**:(Int) Stands for **Gross Domestic Product** and the is express in Euros.
*  **Inflation**:(numeric) It represents the inflation of each of the countries.
*  **Life.expect**:(numeric) It represents the average life expectency of each country and it is measure in years.
*  **Military**:(numeric) 
*  **Pop.growth**:(numeric) It represents the population growth regarding last year population of the country.
*  **Unemployment**:(numeric) It is represents the percentage of the population of each country that is unemploy.

## Packages and libraries
Europe's Shiny app uses many functions from a large quantity of packages. So in order for this app to work the next packages have to be install:

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


This packages are install like the following:

```
install.packages("name of the package")
```

And to add them to the environment:  

```
library(name of the packages)
```  



