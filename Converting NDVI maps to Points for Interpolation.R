#Converting Landsat NDVI maps to points. 

#Background: Landsat 7 maps have gaps due to a scan line error. Gaps are interpolated in a GIS. 
#Objective: To convert L7 NDVI maps to point shapefiles for future interpolation.
#Note: Maps should be clipped to AOI for faster processing. 

#################   Begin Code  ##########################
require(rgdal)

#---------- Identify the clipped NDVI maps of interest ----------------
#Note: NDVI maps are clipped to the entire IV (using ET map 2012_12_30.img)
Code.Year <- "2014"
NDVI.pn <- paste0("F:/Landsat/P39R37_NDVI/",Code.Year,"/ImperialValleyCrops") #Your clipped IV outline NDVI maps
NDVI.directories <- dir(NDVI.pn, patt=".tif$|.img$", full.names = TRUE, recursive = FALSE)
NDVI.directories <- NDVI.directories[grep("_L7",NDVI.directories)] #only use L7 images
NDVI.directories

#---------- Create point shapefiles ------- 
NDVI.shp.outfiles.fold <- paste0("F:/Landsat/P39R37_NDVI/",Code.Year,"/DestripedPoints/")
NDVI.shp.outfiles.fn <- substr(NDVI.directories,49,52)

NDVI.shp.outfiles <- paste0(NDVI.shp.outfiles.fold,NDVI.shp.outfiles.fn)
extension(NDVI.shp.outfiles) <- "shp"
NDVI.shp.outfiles

NDVI.shp.dsn <- substr(NDVI.shp.outfiles.fold,0,43) #this if for you loop below.
NDVI.shp.lyr <- NDVI.shp.outfiles.fn

#Test a single image before running loop
test.raspoint <- rasterToPoints(raster(NDVI.directories[1]),spatial = TRUE)
writeOGR(obj = test.raspoint, dsn = NDVI.shp.dsn, layer = NDVI.shp.lyr[1], driver = "ESRI Shapefile")

#CREATE LOOP FOR RASTER TO POINT CONVERSION
for (i in 2:length(NDVI.directories)){
  pnt.rast <- rasterToPoints(raster(NDVI.directories[i]),spatial = TRUE)
  writeOGR(obj = pnt.rast, dsn = NDVI.shp.dsn, layer = NDVI.shp.lyr[i], driver = "ESRI Shapefile")
  
  print(paste(i, "of", length(NDVI.directories)))
  flush.console()
}
