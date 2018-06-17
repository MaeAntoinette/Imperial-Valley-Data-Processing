# Objective: Process Landsat raster image for multi-annual interpolation. 

# Workflow: 
# 1) Clip raster images to area of interest (AOI). 
# 2) One pixel interpolation test. 
# 3) Run interpolation for all years. 

# Notes: 
# a) if ETM+ images are used, they will need to be "destriped" and interpolated. 
# b) images should be organized in folders by year. 
# c) images should have same naming convention. 
# d) interpolation of the beginning and end images will require images from the previous and next years. (i.e. 2010 analysis will require 2009 December and 2011 January images).

# required tools. 
library(rgdal)
library(maptools)
library(htmlTable)
library(plyr)
library(dplyr)

######### 1. Clip images #########

# bring in your AOI .shp.
AOI.shp <- readOGR()
plot(AOI.shp)

# set AOI extent
AOI.ext <- extent(AOI.shp)

# navigate to folder with images that will be clipped.
images.folder <- 
  
# list all images of interest.
images.filename <- dir(images.folder, patt = "", recursive = TRUE, full.names = TRUE, include.dirs = TRUE)

# include only .tif files.
images.filename  <- dir(images.filename, patt = "", recursive = TRUE, full.names = TRUE, include.dirs = TRUE)

# create output file names and place in output folder using existing file name of input images.
output.folder <- ""
output.filename <- paste0(substr(images.filename,xx,xx), "_Clipped")
output.pathname <- paste0(output.folder,output.filename)

# set file type to output images
extension(output.pathname) - "tif"

# create a loop to clip raster images and save to output directory with new filenames.
for (m in 1:length(images.filename)) {
  r <- raster(images.filename[m])
  rc <- crop(r, AOI.ext)
  rm <- mask(rc, AOI.shp)
  rw <- writeRaster(rm, output.pathname[m], overwrite = TRUE)
  
  print(paste(m, "of", length(output.pathname)))
  flush.console()
}

######### 2a. One pixel test - setting variables for interpolation #########
# separate files by year. add necessary lines of code for years. example used here is 2010. 

# create a raster stack of clipped images by year.
# create stacks by year accordingly.
clipped.images.2010 <- list.files(output.pathname, patt = ".tif$", full.names = TRUE)
stopifnot(length(image.stack)>0)

# stack images by year accordingly
clipped.images.2010.stack <- stack(clipped.images.2010)
plot(clipped.images.stack)

# create a dataframe (df) for all pixels in images by year.
# columns: x, y, z (pixel value for all images used in stack) 
clipped.2010.df <- data.frame(rasterToPoints(clipped.images.2010.stack, fun = NULL))

# merge x and y (east and north) columns to make on unique column.
clipped.2010.df$UTM <- paste(clipped.2010.df$x, clipped.2010.df$y, sep = "_")

# remove the separate eastings and northings.
drop.coord <- c("x", "y")
clipped.2010.df <- clipped.2010.df[,!(names(clipped.2010.df) %in% drop.coord)]
        
# column headers for all images in stack will be given arbitrary names. rename them using the actual image file names.
clipped.2010.list <- list.files(clipped.images.2010, patt = ".tif$", full.names = FALSE)
# extract the date in YYYY-MM-DD format from clipped.2010.list. these will be the column headers. 
clipped.2010.list.sub <- gsub()
# apply extracted dates to df column headers. 
colnames(clipped.2010.df)[1:(length(clipped.2010.df)-1)] <- clipped.2010.list.sub
        
        
        
######### 2b. One pixel test - DF for interpolated values #########
# This will be used to determine interpolation values for each pixel.

# create a df for interpolated values of full dates, months, and years. 
df.dates <- seq.Date(as.Date.character("2010-01-01"),as.Date.character("2015-12-31"), by="day")

IV.int.timeline <- data.frame(df.dates)
names(IV.int.timeline) <- "Date"
IV.int.timeline$Month <- as.numeric(format(strptime(IV.int.timeline$Date,format="%Y-%m-%d"),"%m"))

# add column for months
monthly.breaks <- seq.Date(as.Date.character("2010-01-01"),as.Date.character("2016-01-01"), by="month")
monthly.breaks
IV.int.timeline$Monthly.Break <- cut(IV.int.timeline$Date,monthly.breaks)

# add column for years
annual.breaks <- seq.Date(as.Date.character("2010-01-01"),as.Date.character("2016-01-01"), by="year")
annual.breaks
IV.int.timeline$Annual.Break <- cut(IV.int.timeline$Date,annual.breaks)

# ensure your Date column is classified as "date" 
class(IV.int.timeline$Date) 
  
######### 2c. One pixel test - interpolation  #########
#Identify the pixel that you want
#Each row represents one pixel
pixel.1 <- ET.2010.df[1,(1:(length(ET.2010.df)-1))]
pixel.1
pixel.1.t <- t(pixel.1)
plot(pixel.1.t)
pixel.data <- data.frame(ET.2010.list.sub,pixel.1.t) #your dates are factors.
names(pixel.data) <- c("Date","Pixel.Value") #convert to date class.
pixel.data$Date <- as.Date.character(as.character(pixel.data$Date),"%Y-%m-%d")

#Merge with the interpolate timeline. 
pixel.data.temp <- merge(IV.int.timeline,pixel.data,by="Date",all=TRUE)
pixel.data.temp$Pixel.Value[pixel.data.temp$Pixel.Value < 0] <- 0  #if pix value is < 0 make 0

date.int <- pixel.data.temp$Date
pix.val.int <- pixel.data.temp$Pixel.Value
pix.val.xout <- df.dates

pixel.data.temp.int <- approx(date.int,pix.val.int,xout=pix.val.xout)
pixel.data.temp.int <- data.frame(pixel.data.temp.int)
names(pixel.data.temp.int) <- c("Date","Pixel.Value")

plot(pixel.data.temp.int$Date,pixel.data.temp.int$Pixel.Value,type="l")
#Yay. That works.
  
  
######### 3a. Interpolating all years - Setting variables.  #########

# create raster stack of clipped images by year.
ET.2010.stack <-  list.files(xx, patt = ".tif$", full.names = TRUE)
# ET.2011.stack etc...


# identify the analysis year that you want to interpolate.
Analysis.Year <- 2010

# acquire images end of previous year and beginning of post year.
Previous.Year <- as.character(Analysis.Year - 1)
Post.Year <- as.character(Analysis.Year + 1)

ET.previous.year <- list.files(FOLDERNAME, patt = ".tif$", full.names = TRUE)
ET.previous.year <- last(ET.previous.year)
ET.post.year <- list.files(FOLDERNAME, patt = ".tif$", full.names = TRUE)
ET.post.year <- last(rev(ET.post.year))


# stack your year of interest plus the previous and post year images. 
year.files.complete <- c(Previous.Year, ET.2010.stack, Post.Year)
ET.stack <- stack(year.files.complete)

# create a df of all data from your image stack. 
ET.stack.df <- data.frame(rasterToPoints(ET.stack, fun = NULL))

# check if your coordinates appear right (are they in UTM?)
head(ET.stack.df)

# create labels for column headers
ET.list.sub <- gsub("_", "-", substr(year.files.complete,xx,xx))

# apply labels to ET stack df
colnames(ET.stack.df)[3:(length(ET.stack.df))] <- ET.list.sub
colnames(ET.stack.df)

# identify pixel info, characteristics 
pixel.count <- length(ET.stack.df$x) # how many pixels analyzed?
image.count <- length(ET.stack.df) - 2 # how many images are in stack
  
  
 ######### 3b. Interpolating all years - Running loop.  #########
  system.time(
    for (i in 1:length(pixel.count)) {
      
      ################ One pixel analysis
      #Begin one pixel analysis.
      UTM.Easting <- ET.stack.df$x[i]                              #will use for column header name in script below.
      UTM.Northing <- ET.stack.df$y[i]
      
      pixel.val <- ET.stack.df[i,(3:length(ET.stack.df))]                #aquire values:
      pixel.val                                                          #pixel i (row i), column i through n (=/= UTM col)
      
      pixel.val.t <- t(pixel.val)                               #transpose/flip your dataset into vec.
      pixel.data.df <- data.frame(ET.list.sub,pixel.val.t)      #creates df with your transp vec date and value at the 
      #date for pixel i. 
      #this is dependent of [i] && iteration.
      
      names(pixel.data.df) <- c("Date","Pixel.Value")           #column headers in your new df.
      #convert dates in df to date format
      pixel.data.df$Date <- as.Date.character(as.character(pixel.data.df$Date),"%Y-%m-%d")
      
      #Merge with the interpolate timeline created in Interpolation Timeline DF (line 255-ish)
      pixel.data.merge <- merge(IV.int.timeline,pixel.data.df,by="Date",all=TRUE) #make sure IV.int.timeline$Date
      #is classified as "Date" prior.
      #all=TRUE merges mismatching length.
      #Account for negative values. Make 0. 
      pixel.data.merge$Pixel.Value[pixel.data.merge$Pixel.Value < 0] <- 0  #if pix is < 0 make 0
      
      
      #Arguments for approx function.
      date.int <- pixel.data.merge$Date                #X argument - determines x axis of interp.
      pix.val.int <- pixel.data.merge$Pixel.Value      #Y argument - determines y axis (values to be interp)
      pix.val.xout <- df.dates                         #xout argument - determines start to end dates of interp to take place.
      
      pixel.data.int.vec <- approx(date.int,pix.val.int,xout=pix.val.xout) #vector of interpolated values.
      #vector of 2 variables (date and value)
      pixel.daily.data.df <- data.frame(pixel.data.int.vec)                  #create a df of the 2 variables.
      names(pixel.daily.data.df) <- c("Date","Pixel.Value")                  #name the 2 columns
      
      
      #Create a monthly df of the values per coordinate. 
      #This requires summing per month. 
      #Save output as file. 
      
      pixel.monthly.data.df <- aggregate(pixel.daily.data.df$Pixel.Value, by=list(IV.int.timeline$Monthly.Break), FUN=sum)
      pixel.monthly.data.df$Group.1 <- as.Date.character(as.character(pixel.monthly.data.df$Group.1)) #reclassify to date. Date.
      pixel.monthly.data.df$x <- as.numeric(as.character(pixel.monthly.data.df$x))                    #reclassify to numeric. Pixel value.
      
      
      #Make a new df that has columns for UTM coord and monthly ET sums 
      pixel.monthly.df = data.frame(x=UTM.Easting,y=UTM.Northing, 
                                    Jan=pixel.monthly.data.df[grep(paste0(Analysis.Year,"-01-01"), pixel.monthly.data.df$Group.1),2],
                                    Feb=pixel.monthly.data.df[grep(paste0(Analysis.Year,"-02-01"), pixel.monthly.data.df$Group.1),2],
                                    Mar=pixel.monthly.data.df[grep(paste0(Analysis.Year,"-03-01"), pixel.monthly.data.df$Group.1),2],
                                    Apr=pixel.monthly.data.df[grep(paste0(Analysis.Year,"-04-01"), pixel.monthly.data.df$Group.1),2],
                                    May=pixel.monthly.data.df[grep(paste0(Analysis.Year,"-05-01"), pixel.monthly.data.df$Group.1),2],
                                    Jun=pixel.monthly.data.df[grep(paste0(Analysis.Year,"-06-01"), pixel.monthly.data.df$Group.1),2],
                                    Jul=pixel.monthly.data.df[grep(paste0(Analysis.Year,"-07-01"), pixel.monthly.data.df$Group.1),2],
                                    Aug=pixel.monthly.data.df[grep(paste0(Analysis.Year,"-08-01"), pixel.monthly.data.df$Group.1),2],
                                    Sep=pixel.monthly.data.df[grep(paste0(Analysis.Year,"-09-01"), pixel.monthly.data.df$Group.1),2],
                                    Oct=pixel.monthly.data.df[grep(paste0(Analysis.Year,"-10-01"), pixel.monthly.data.df$Group.1),2],
                                    Nov=pixel.monthly.data.df[grep(paste0(Analysis.Year,"-11-01"), pixel.monthly.data.df$Group.1),2],
                                    Dec=pixel.monthly.data.df[grep(paste0(Analysis.Year,"-12-01"), pixel.monthly.data.df$Group.1),2])
      
      #code above states: "From monthly aggregated data, 
      #find January column (row 1 column 1 stated within grep)
      #and then list its month sum (row 1 column 2)". Do this for each month of a year. 
      
      #Add the analysis year to the colnames. Cannot use paste function in setting df code above. 
      colnames(pixel.monthly.df)[3:(length(pixel.monthly.df))] <- paste0(colnames(pixel.monthly.df[3:14]),".",Analysis.Year)   #renames Monthly headers.
      
      #If-else statement to create a built-on df that shows monthly pixel value sums per UTM coord.
      #If df exists, rbind will build on df with additional rows of data.
      if(exists("scene.monthly.value")){
        scene.monthly.value = rbind(scene.monthly.value,pixel.monthly.df) #take scene.monthly.df iteration and bind pixel.monthly.value
      } else {
        scene.monthly.value = pixel.monthly.df
        #if it doesn't exist, the scene.monthly.value = the first iteration of 
        #pixel.monthly.df above. ie: pixel 1 to pixel n
        
      }
      
      print(paste("Your interpolation for pixel" ,i, "of" ,pixel.count,"is now finished!"))
      flush.console()
      
    }
  )
  #Print an update.
  print(paste("Your ET file for", Analysis.Year, "is completed, Mae. Yay."))
  print(paste("Make sure to save your text file!!!!"))
  
  write.table(scene.monthly.value,file = "",row.names=FALSE)
  
  print(paste("Erase the -scene.monthly.value- file and restart the loop again for new file. Now removing your scene.monthly.value"))
  rm(scene.monthly.value)
  
  
  
  