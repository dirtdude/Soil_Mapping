#-----------------------------------------------------------------------
# 
#         This R code is for loading rasters, manipulating the rasters to get in the same CRS, same extent, 
#         cropped as need (CONUS data for Prisim), and resamlped so that cell size, extent and CRS all add up. 
#         The rasters are stacked, and the Lodi point data is used to extract point covariables from the raster stack
#         These covariabels are cbind to the variable of intereset, say Olsen P, that was field measured point
#         data. Then, a randomForest is constructed, with the variabel of interest, say Olsen P, and the stack of covariates. 
#         There is no model validation, partly becasue the model validation sucks.  The main thrust of this portion of 
#         the project was proof of concept that I could brin in diverse sources, and runa  stack and extract with many 
#         different rasters from differnt sources, and extrat the informaiotn from the rasters using The 
#         field measured point data. 
#         
#         Future work includes going back to the data extraction and amalgamation from SSURGO.There is missing data, and the 
#         method I used to slice the data may not be what I want.  Also, I ran a component weighted average, ie weighting the 
#         values in the map unit, which is what the data in  this prject, by the component percent in that mapunit. 
#         I believe that this introduced errors. Now that I have proved to myself that I can get to the end, and introduce
#         many raster sources, extract, model and predict, I can go back and fine tune the steps. 
#         
# 




library(rgdal)
library(raster)
library(maptools)
library(reshape)




# first get the point data 
setwd("C:/Users/dirtdude/Documents/ABT 182/Final Project/Shapefiles")


lodi_points<-shapefile("Lodi_Points_Good.shp")

# we want to set all the rasters to be in the sdame CRS as the point data to facilitate the raster extract
# create a crs object form lodi_points
crs<-crs(lodi_points)

# now get all the rasters

# I chose not to do this as a loop, becasue differnt things need to be done to different rasters
# If i read into one big object, I would have to parse the object to get out DEM and prisim data, and soils,
# manipulate and restack. 

#change wd to Raster folder

setwd("C:/Users/dirtdude/Documents/ABT 182/Final Project/Rasters")

DEM<-raster("sac_sj_dem.tif") 
CLAY_r<-raster("claywts.tif")
ph_r<-raster("pHwts.tif")
CEC_r<-raster("CECwts.tif")
Om_r<-raster("OMwts.tif")
precip<-raster("PRISM_ppt_30yr_normal_800mM2_annual_bil.bil")
geol<-raster("ptype.tif")


# Now I'll stack the soils data

soils<-stack(CLAY_r, ph_r, CEC_r, Om_r )

# now reproject to lodi_points

soil_n <- projectRaster(soils, crs=crs)

# test to make sure we are all ale to stack and extract
vars<-extract(soil_n, lodi_points) # good

# now lets get teh DEM in the same CRS, and build the stack of terrain attributes

# now reproject to lodi_points
DEM <- projectRaster(DEM, crs=crs)

# build terrain attributes
?terrain

slope <- terrain(DEM, opt='slope')
aspect <- terrain(DEM, opt='aspect')
roughness<-terrain(DEM, opt='roughness')
TPI<-terrain(DEM, opt="TPI")

# ok for future, this ^ should probably be in a projected CRS


# stack the terrain attributes
terrain<-stack(slope, aspect, roughness, TPI )

#test

vars<-extract(terrain, lodi_points)
# ok we are good to this point

# build geology
# reproject geology

geol<-projectRaster(geol, crs=crs)


# stack?

test<-stack(terrain, soil_n)# different extents

# ok so the geology layer has the smallest extent, I guess from the crop
# done in arcpy. So, we will get all the rasters in the extent of the geology raster

# ok so the geology layer has the smallest extent, and the DEm has the most finest dimensions
# my approach is to get the stack in the extent of the Geology layer, and the dimenrison of the DEM
f2 <- function(raster) {
  
  raster<-resample(raster, DEM, method="bilinear")
  extent(raster)<- extent(geol)
  return(raster)

}


# "apply" the funtion to the stacks etc
soils<-f2(soil_n)
geol<-f2(geol)
terrain<-f2(terrain)

var<-stack(terrain, geol, soils) # looks good!

# now a test extract

vars<-extract(var, lodi_points)
# looks good

# now we need to process the PRISIM data which is CONUS data so we need to crop it

# lets crop it to the DEM

precip<- crop(precip, extent(DEM))


# now apply the function 

precip<-f2(precip)

# Now...drum roll please... Lets build the entire stack!

covariables<-stack(terrain, geol, soils, precip)

# now extract the field data

Lodi_variables <- extract(covariables, lodi_points)

# get teh extent of the covariables or use in the predict later

ext<-extent(covariables)
class(Lodi_variables)

#-------------------------------------------------------------------------------------------------------
# 
# 
#         Ok now we have a matrix of all the raster covariables!
#   
#         The next section will be to combine the data with the field measured properties, and throw a model at it
#         then predict and write to kml, to see if model "passes the smell test", eg looks ok with what we know about
#         soils
# 
# 
# 
# 
# ---------------------------------------------------------------------------------------------------------

# load the packages
library(party)
library(randomForest)
library (tree)

# make the point data into a Data frame
lodidf<-data.frame(lodi_points)

# creat the frame work for the model for ONE outcome varaibel, or variabel we wish to model
# in this case we will do Olsen extractable phosphorous 

P_model<-cbind(lodidf["Olsn_wt"], Lodi_variables)

# now build a randomForest

Olsen_RF<-randomForest(Olsn_wt~., data=P_model)

summary(Olsen_RF)
importance(Olsen_RF)
varImpPlot (Olsen_RF )

Olsen_Tree<-(predict(covariables, Olsen_RF, ext=ext))
plot(Olsen_Tree)

KML(Olsen_Tree, file='Available_Phosphorous.kml')


# open in google earth and enjoy!



