# ReproducibleResearch

### Welcome in our project: House Sales in King County, USA - Exploratory Data Analysis

#### OUR APPLICATION WORKS IN TWO PLACES:
##### Incomplete version on AWS: http://3.18.106.13:3838/project/
###### (this is due to some problems with two librabries (ggpubr, leaflet) and support with R 3.4.4, which is available on AWS Ubuntu 18.04)
##### Full version on RShinyServer: 


##### How to run (on AWS):
- run Ubuntu Server on AWS
- add Port 3838 as a role
- create key pair
- connect to you machine with private key
- install Shiny Server as folow: https://rstudio.com/products/shiny/download-server/ubuntu/
- sent to machine your files or clone repository 
- find and port for ShinyServer /path to your project as: (IPv4 Public IP):3838/path, example: http://3.18.106.13:3838/project

##### Basic info about project:
- Kaggle repository as a source of data
- Dataset consists of sale prices of houses located in King County (state of Washington) with characteristic of each property
- Data has been pre-processed before providing to Shiny (exclusion of one incorrect observations, removal of duplicates, inclusion of one additional variable from external data source)
- Finally 21 435 observations and 17 variables with no missing values


#### What can You find in app:

###### Dataset:

it is a place where data frames is avaible to see.

###### Target variable:

more information about target variable, density plot, histogram. Can be filtered by Location.

###### Independent variable:

more information about independent varaibles, basicly You can check distribution of all varaibles. 

###### Map:

there is place where data are visualized on map.

