#Background: EEFlux provides ET and ETrF maps using 30 m Landsat resolution in the Google Earth Engine playground. 
#However, Landsat 7 images have gaps that EEFlux did not remove in its beta version. The pixels within these gaps have a value
#of 0. Although the striping is not in the image center, about 22% of image data is lost. Missing data is interpolated and images 
#are "gap-filled". Before interpolation, we must clip out the "false zeroes" in EEFlux maps. 

#Objectives: I will use the pre-downloaded gap mask (gm) files from Landsat, convert them to a shapefile, 
#and clip out the gaps in the ET maps, using the converted shp gm. 
#GM - refers to Gap Mask. EE - refers to EEFlux ET maps. 

#Note: As with other scripts, you will notice that these folder naming conventions are structured and contain a sub-dir with 
#the year of interest.

#Note: EEFlux maps and Landsat Gap Masks must already be downloaded. 

#-----------------------------------------------------------------------------------


#require MapTools
require(rgdal)
require(rgeos)
require(raster)

########################### Begin code ##############################

#enter the year of study
Code.Year <- 2015

#------------ Clipping Gap Mask Polygons to the Gaps. ------------
#Isolate all the poly .shp in your gm folders
GM.pn <- paste0("F:/Landsat/P39R37/Gap Masks/",Code.Year) 
GM.directories <- dir(GM.pn,patt=".tif$|.TIF$",recursive = TRUE,full.names = TRUE,include.dirs = TRUE) #the "|" allows dir() to search for more than one pattern
GM.directories <- GM.directories[grep("clipB4",GM.directories)]
GM.directories <- GM.directories[grep("gap_mask",GM.directories)]
GM.directories


#Clip gap mask files to a smaller AOI for faster processing.
GM.AOI.pn <- "F:\\Landsat\\P39R37"
GM.AOI.fn <- "SubsetAOI"
GM.AOI <- readOGR(GM.AOI.pn,GM.AOI.fn)
GM.AOI.ext <- extent(GM.AOI)

gm.outfiles.pn <- paste0(substr(GM.directories,0,46))
gm.outfiles.pn
gm.outfiles.fn <- paste0("clip",substr(GM.directories,92,93))
gm.outfiles.fn
gm.outfiles <- paste0(gm.outfiles.pn,"/",gm.outfiles.fn)
extension(gm.outfiles) <- "tif" 
gm.outfiles 

for (i in 2:length(GM.directories)) {
  GM.r <- raster(GM.directories[i])
  GM.clip <- crop(GM.r, GM.AOI.ext)
  writeRaster(GM.clip,gm.outfiles[i],overwrite=TRUE)
  
  print(paste(i, "of",length(GM.directories)))
  flush.console()
}  

#Work in a GIS to convert to shp. (R code to do this crashes as of Jan 23, 2017)
#After conversion, I save to integer file in ArcMap to convert raster to poly.




#Bring in your tiff-to-shp converted files
GM.shp.directories <- dir(GM.pn,patt=".shp",recursive = TRUE,full.names = TRUE,include.dirs = TRUE)
GM.shp.directories

gm.shp.outfiles.pn <- paste0(substr(GM.shp.directories,0,46))
gm.shp.outfiles.pn
gm.shp.outfiles.fn <- paste0("stripebuff",substr(GM.shp.directories,48,53))
gm.shp.outfiles.fn

gm.shp.outfiles <- paste0(gm.shp.outfiles.pn,"/",gm.shp.outfiles.fn)
extension(gm.shp.outfiles) <- "shp" 
gm.shp.outfiles


#Create buffer around the shapefiles (mine is set to 15 m all around).
gm.dsn <- substr(GM.shp.directories,0,46)
gm.lyr <- substr(GM.shp.directories,48,53) #this might account for the "_int" extension. change if necessary. 

for (i in 1:length(gm.shp.outfiles)) { 
  GM.shp_ss <- readOGR(gm.dsn[i],gm.lyr[i])
  GM.shp_stripe <- GM.shp_ss[GM.shp_ss$GRIDCODE == 0,]
  
  GM.shp_stripe_15m <- rgeos::gBuffer(GM.shp_stripe,width=15,byid=TRUE,quadsegs = 15)
  GM.shp_stripe_15m <- SpatialPolygonsDataFrame(GM.shp_stripe_15m,data = GM.shp_stripe@data) 
  
  writeOGR(GM.shp_stripe_15m,gm.shp.outfiles.pn[i],gm.shp.outfiles.fn[i],driver = "ESRI Shapefile", overwrite_layer = TRUE)
  
  print(paste(i, "of",length(gm.shp.outfiles)))
  flush.console()
  }





##### Gap removal in ET maps ######
#--------- Bring in your EEFlux clipped (study area) images -----------
EE.pn <- paste0("F:/EEFlux/P39R37/",Code.Year,"/ET/Clipped") #I've renamed my images to "YYYY_DD_MM_Sensor" where sensors = L5, L7, or L8
EE.directories <- dir(EE.pn,patt=".tif$|.img$",full.names=TRUE,recursive = FALSE)
EE.directories <- EE.directories[grep("_L5",EE.directories,invert = TRUE)]
EE.directories <- EE.directories[grep("_L8",EE.directories,invert = TRUE)] #we only want to destripe L7 images!
EE.directories

#create filenames for the EEFlux maps that you will destripe
#ds = destriped EEFlux maps
ds.outfiles.fold <- paste0("F:/EEFlux/P39R37/",Code.Year,"/ET/DestripedImages/")
ds.outfiles.fold
ds.outfiles.fn <- substr(EE.directories,34,43)
ds.outfiles.fn
ds.outfiles <- paste0(ds.outfiles.fold,ds.outfiles.fn)
extension(ds.outfiles) <- "tif" 
ds.outfiles

#call in your buffered polygons  
gm.stripe.pn <- GM.pn
gm.stripe.pn
gm.stripe.directories <- dir(gm.stripe.pn,patt=".shp",recursive = TRUE,full.names = TRUE,include.dirs = TRUE)
gm.stripe.directories <- gm.stripe.directories[grep("stripebuff",gm.stripe.directories,invert=FALSE)] #call to the striped shp only.
gm.stripe.directories

gm.stripe.dsn <- substr(gm.stripe.directories,0,46)
gm.stripe.lyr <- substr(gm.stripe.directories,48,63)



#--------- Erase the gaps -------- #
#TEST 
gm.test <- readOGR(gm.stripe.dsn[1],gm.stripe.lyr[1])
#mask the gaps using your gm data
ee.gm.test <- mask(raster(EE.directories[1]),gm.test,inverse = TRUE)
plot(ee.gm.test)
writeRaster(ee.gm.test, ds.outfiles[1], overwrite=TRUE)

#CREATE LOOP TO CLIP OUT GAPPED AREAS
for (i in 1:length(EE.directories)){
  gm.shp <- readOGR(gm.stripe.dsn[i],gm.stripe.lyr[i])
  ee.rast <- raster(EE.directories[i])
  ee.mask <- mask(ee.rast,gm.shp,inverse = TRUE)
  writeRaster(ee.mask,ds.outfiles[i],overwrite=TRUE)
  
  print(paste(i,"of",length(ds.outfiles)))
  flush.console()
}


#---------- Create point shapefiles using your destriped EEflux ET maps ------- 
pnt.shp.outfiles.fold <- paste0(substr(ds.outfiles,0,17),Code.Year,substr(ds.outfiles,22,41),"DestripedPoints/")
pnt.shp.outfiles.fn <- ds.outfiles.fn
pnt.shp.outfiles <- paste0(pnt.shp.outfiles.fold,pnt.shp.outfiles.fn)
extension(pnt.shp.outfiles) <- "shp"

ee.shp.dsn <- substr(pnt.shp.outfiles.fold,0,56)
ee.shp.lyr <- pnt.shp.outfiles.fn

#Test a single image before running loop
test.raspoint <- rasterToPoints(raster(ds.outfiles[1]),spatial = TRUE)
writeOGR(obj = test.raspoint, dsn = ee.shp.dsn[1], layer = ee.shp.lyr[1], driver = "ESRI Shapefile")

#CREATE LOOP FOR RASTER TO POINT CONVERSION
for (i in 2:length(ds.outfiles)){
  pnt.rast <- rasterToPoints(raster(ds.outfiles[i]),spatial = TRUE)
  writeOGR(obj = pnt.rast, dsn = ee.shp.dsn[i], layer = ee.shp.lyr[i], driver = "ESRI Shapefile")
  
  print(paste(i, "of", length(pnt.shp.outfiles)))
  flush.console()
}









####### for the files that are messed up for whatever reason, do them manually here... #####
GM.20131115 <- readOGR("F:\\Landsat\\P39R37\\Gap Masks\\2013\\1115\\gap_mask","stripebuffclipb4")
EE.20131115 <- raster("F:\\EEFlux\\P39R37\\2013\\ET\\Clipped\\2013_11_15_ET_L7.tif")
ee.gm.20131115 <- mask(EE.20131115,GM.20131115,inverse = TRUE)
writeRaster(ee.gm.20131115, "F:/EEFlux/P39R37/2013/ET/DestripedImages/2013_11_15.tif", overwrite=TRUE)


EE.20160108.raspoint <- rasterToPoints(raster("F:/EEFlux/P39R37/2016/ET/DestripedImages/2016_01_08.img"),spatial = TRUE)
writeOGR(obj = EE.20160108.raspoint, dsn = "F:\\EEFlux\\P39R37\\2016\\ET\\DestripedImages\\DestripedPoints", layer = "0108", driver = "ESRI Shapefile")
