library("rvest")
library("stringr")
library("ggplot2")
library("ggmap")
library("tidyr")
library("viridis")
library("broom")
library("maptools")
library("caret")
library("dplyr")
library("plyr")
library("scales")
library("data.table")


setwd("C:/Users/Frederik/Documents/scrape af boligdata/")

if(!exists(addr)){
  addr = read.csv("adresser.csv", encoding = "UTF-8", stringsAsFactors = F)
}

source("Functions_Århus.R")



#######################################################################################################################
#######################################################################################################################
###
###           SCRAPING BOLIGA.DK DATA
###
###           TOC:
###             1) Actually getting the data
###             2) Graphical exploration
###             6) Modelling house prices
###                 6.1) functions for data prep
###                       6.1.1) f: prepper - subset by min max, and remove NA's
###                       6.1.2) f: dummies - generate variables nboorhood and tradetype dummies
###                 6.2) Train control 
###                 6.3) Linear models 
###                       6.3.1) m: ordinary OLS
###                       6.3.2) m: LinReg on PCA components
###                 6.4) m: k-NN 
###                 6.5) m: Bayesian regularized neural net 
###                 6.6) m: Random Forest
###                 6.7) m: xgBoost
###                 6.8) Comparing models
###                       6.6.1) Graphical comparison
###             7) Suggestions for improvement
###
### f: "function (inputs)", m: "model"
#######################################################################################################################
#######################################################################################################################

#######################################################################################################################
###         1) FÆRDIGT PROGRAM
###             scraper.singlepage - scrapes a single page
###             pagelooper(S,M) - scrapes resultats from page S to m
###             store.payload - loop through every page by running pagelooper on increments of the total number of pages
###             cleaner(data) - Cleans the dataset for further work
###             geodata.appender(data, zone) - adds lat/lon to the dataset. zone should be added for sufficient precision in geolocating.
###             geodata.offline(data) - mathces lon/lat/zip from a database of all copenhagen addresses (get them from AWS). Note 
###             area.matcher(data)  - decides in which area of copenhagen a given address is using a shapefile of the city
###             trade.volume(data) - 
###             prepper (data, min, max, omit) -  
###             dummies(data) -
###             var.dropper(dat, keep_lonlat) - 
###
###             link.part needs to be changes to get a different area of the country - for now it's set to copenhagen
###
link.part = "http://www.boliga.dk/salg/resultater?so=1&type=Ejerlejlighed&kom=751&fraPostnr=&tilPostnr=&gade=&min=&max=&byggetMin=&byggetMax=&minRooms=&maxRooms=&minSize=&maxSize=&minsaledate=1992&maxsaledate=today&kode=&sort=omregnings_dato-a&p="
###
M = 410# set max page-number for link generator
###
#######################################################################################################################
# Run this to actually get some data

link.list = list.updater(M)   # DONT CHANGE THIS LINE, KØR FØRST

data = pagelooper(1,4)            # S,M angiver min og max sidetal der skal loopes over.
data_out = store.payload(data, 404, 4)

rm(data)

anyDuplicated(data)

data_clean = cleaner(data_out)        # Renser datasættet, (tager output fra pagelooper som argument!)

data_geo = geodata.offline(data_clean)                         # brug evt geodata.offline istedet!
Århus <- data_geo
save(Århus, file ="Århus.Rda")

data_fin = area.matcher(data_geo)[[1]]    #area.matcher finder bykvarter for hver observation
bydel = area.matcher(data_geo)[[2]]    

rm(data_clean)
rm(addr)




#######################################################################################################################
###         6) MODELLING 
###             as default all models are run with repeated CV
###
#######################################################################################################################

###---------------------------------------------
###        6.1) data preparer - subsets data with/without NA's and in range [min,max]
###---------------------------------------------

#***********  6.1.1) prepper- subset data *************************

prepper = function(data, min, max, omit){
  # omit NA's if omit == T
  if(omit == T) {
    data = na.omit(subset(data, buysum < max & buysum > min))
  } else if(omit == F) {
    data = subset(data, buysum < max & buysum > min)
  } else {
    stop("omit must be TRUE or FALSE")
  }
  return(data)
}

