#Quantifying ETr Using EEFlux Data. 
#Date: 09 April 2017 

#------------- Background and Objective ------------
#Background: Quantifying the amount of ET in agricultural fields can be costly and time-intensive. FAO standards that allow farmers
#to quantify ET without in-situ ET measurements include using a "reference ET" (ETr/ETo) of a well-watered, thriving crop. ETr = alfalfa
#ETo = short-grass (commonly used in California). The ET of a given crop is then estimated using a "crop coefficient" (Kc) which accocunts 
#for biophysical parameters of the given crop type. The EEFlux model uses Landsat imagery to calculate the surface-energy balance and actual ET 
#and incorporates the crop coefficient method, where 24-hr ETa = ETr * ETrF (ETrF is known as the "fraction of reference ET) and is synonymous 
#with the Kc. EEFlux provides a daily ETrF map which is useful to understand ET relative to the reference across a landscape. 

#Objective: EEFlux SEB estimates rely on the amount of incoming Rn and consequently the ETr. I will determine ETr, using 24-hr ETrF 
#and ET maps. Then, I will compare the pixel values to ground-observed CIMIS ETo values recorded across the study area.


#------------- Script ------------------------------

#Identify map year
map.year <- "2010"

#List files of interest 
ETrF.fold <- paste0("F:/EEFlux/P39R37/",map.year,"/ETrF/Clipped/")
ETrF.files <- list.files(ETrF.fold, patt=".tif$|.img$", all.files = TRUE, full.names = TRUE)
ETrF.files <- ETrF.files[grep("_me",ETrF.files)] #uses images with matching extent. "_me"

ET.fold <- paste0("F:/EEFlux/P39R37/",map.year,"/ET/InterpDaily_Maps/")
ET.files <- list.files(ET.fold, patt=".tif$|.img$", all.files = TRUE, full.names = TRUE)
ET.files <- ET.files[grep("_me",ET.files)]

#Create ETr files (outfile names)
ETr.files <- paste0("F:/EEFlux/P39R37/",map.year,"/",substr(ET.files,42,52))
extension(ETr.files) <- ".img"

#Clipping Loop. This will save in your output directory. 
for (f in 1:length(ET.files)) {
  ETrF.r <- raster(ETrF.files[f])
  ET.r <- raster(ET.files[f])
  rw <- writeRaster(rm, ETr.files[f], overwrite=TRUE)
}

#Compare to weather data.
