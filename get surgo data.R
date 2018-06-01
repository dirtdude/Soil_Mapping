# ######This script is largely borrowed from scott devine https://github.com/smdevine/GreenWater and is for querying the national soil data base using soilDB
# # in this example, I extract the area sysmbols for all of California first
# In further queries, this will exlcude the statsgo information implicitly, since the area symbol will have CA in it. 

# for a better understanding of soil data access see https://sdmdataaccess.sc.egov.usda.gov/documents/SoilDataAccessQueryGuide.pdf

install.packages("httr", dep=TRUE)
install.packages("soilDB", dep=TRUE)
install.packages("rgdal", dep = TRUE)
install.packages("raster", dep = TRUE)
install.packages("rgeos", dep = TRUE)

library(soilDB)
library(sp)
library(rgdal)
library(plyr)
library(raster)
library(rgeos)

#Get all the soil survey areas in california
Ca_areas<-SDA_query("SELECT areasymbol FROM sacatalog WHERE areasymbol LIKE 'CA%' ")

#check data 
head(Ca_areas)

# Note using sacatalog instead of satabularver I think specifies the soil survey area with a simple one to one realtionship. But they area to do the same thing.  

#write to local as a .csv
setwd("C:/Users/stuwi/Dropbox/Phosphorus Mapping Project/Code Repository")
write.csv(Ca_areas, file = 'Califonia_Soil_Survey_Area_symbols.csv', row.names = FALSE)

#now get the mapunit data 
# This is probably unnecessary but specifying the areasymbol
areasysm<-Ca_areas$areasymbol

query_mapunit <- function(x) {
  SDA_query(paste0("SELECT
                   component.mukey, cokey, comppct_r, compname
                   FROM legend
                   INNER JOIN mapunit ON mapunit.lkey = legend.lkey
                   INNER JOIN component ON component.mukey = mapunit.mukey
                   WHERE legend.areasymbol = '", x, "'"))
  
}

# apply to all of california 
mu_data <- do.call(rbind, lapply(areasysm, query_mapunit))

head(mu_data)

#Write to csv

write.csv(mu_data, file = 'Califonia_Map_unit_data.csv', row.names = FALSE)

#funciton to get the horizon data we want 
query_horizon <- function(x) { #this will not return cokey NAs
  print(x)
  SDA_query(paste0("SELECT mu.mukey, comp.cokey, ch.chkey, hzname, hzdept_r, hzdepb_r, ec_r, claytotal_r, silttotal_r, sandtotal_r, cec7_r,om_r, awc_r, ksat_r
                   FROM legend l
                   INNER JOIN mapunit mu ON mu.lkey = l.lkey
                   INNER JOIN component comp ON comp.mukey = mu.mukey
                   LEFT OUTER JOIN chorizon ch on ch.cokey = comp.cokey
                   
                   WHERE l.areasymbol = '", x, "'"))
}


# apply to all of california 

horizon_data <- do.call(rbind, lapply(areasysm, query_horizon))

dim(horizon_data)

write.csv(horizon_data, file = 'Califonia_Horizon_data.csv', row.names = FALSE)


