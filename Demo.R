##############################################################################
# title         : Demo.R;
# purpose       : create R raster stack of CRU CL 2.0 data and do some GIS work
#               : with it for demonstration purposes;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Laguna, PI May 2015;
# inputs        : CRU CL2.0 Climate data;
#               : GADM Philippines RData file;
# outputs       : GGPLOT2 and GoogleEarth maps of minimum temperatures <18 ºC
#               : in the Philippines in May;
# remarks 1     : Demo of GIS in R;
# remarks 2     : This script requires a broadband internet connection to
#               : download data;
##############################################################################

#### Libraries ####
library(raster)
library(ggplot2)
library(maptools)
library(plotKML)
library(RColorBrewer)
##### End Libraries ####

##### Download and read CRU data files ####
## create a temp file and directory for downloading files
tf <- tempfile()
## mean monthly diurnal temperature range ####
# Download data into tempfile created previously
download.file("http://www.cru.uea.ac.uk/cru/data/hrg/tmc/grid_10min_dtr.dat.gz", tf)
# Use header, colClasses and nrows to speed input into R
dtr <- read.table(tf, header = FALSE, colClasses = "numeric", nrows = 566262) 

## mean monthly temperature ####
download.file("http://www.cru.uea.ac.uk/cru/data/hrg/tmc/grid_10min_tmp.dat.gz", tf)
# Use header, colClasses and nrows to speed input into R
tmp <- read.table(tf, header = FALSE, colClasses = "numeric", nrows = 566262) 

##### Fetch GADM RData File for Philippines #####
# getData() function is part of raster package
PHL <- getData(country = "PHL", level = 0)

#### calculate tmax and tmin from tmp and dtr (see: http://www.cru.uea.ac.uk/cru/data/hrg/tmc/readme.txt) #####
tmx <- tmp[, c(3:14)]+(0.5*dtr[, c(3:14)])
tmx <- cbind(tmp[, 1:2], tmx)
tmn <- tmp[, c(3:14)]-(0.5*dtr[, c(3:14)])
tmn <- cbind(tmp[, c(1:2)], tmn)

##### column names and later layer names for raster stack objects ####
months <- c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')

##### GIS work ####
## set up a raster object to use as the basis for converting CRU data to raster objects at 10 arc minute resolution ####
wrld <- raster(nrows = 900, ncols = 2160, ymn = -60, ymx = 90, xmn = -180, xmx = 180)

## Create raster objects using cellFromXY and generate a raster stack
## create.stack takes pre, tmp, tmn and tmx and creates a raster object stack of 12 month data
create.stack <- function(wvar, xy, wrld, months){
  x <- wrld
  cells <- cellFromXY(x, wvar[, c(2, 1)])
  for(i in 3:14){
    x[cells] <- wvar[, i]
    if(i == 3){y <- x} else y <- stack(y, x)
  }
  names(y) <- months
  return(y)
  rm(x)
}

# Use the create.stack function to create a raster stack of minimum temperature
tmn.stack <- create.stack(tmn, xy, wrld, months)

## Plot May min temps
plot(tmn.stack[[5]])

## Crop raster stacks using PHL object
tmn.stack <- crop(tmn.stack, PHL)
## Plot resulting layer for January
plot(tmn.stack[[5]])# still has a bit of Borneo

## Mask raster
tmn.stack <- mask(tmn.stack, PHL)
# Plot resulting layer for May
plot(tmn.stack[[5]]) # that fixes it

## Where are temperatures the coolest in the Philippines in May?
tmn.stack[tmn.stack > 18] <- NA

#### Visualize using ggplot2 ####

## Plotting the original PHL RData file takes too long, this helps
PHL.thinned <- thinnedSpatialPoly(PHL, tolerance = 0.02)

## Use fortify to turn the RData object into something ggplot understands
PHL.df <- fortify(PHL.thinned)

## Turn raster layer into a data frame
May <- data.frame(rasterToPoints(tmn.stack[[5]]))

## Cut for plotting
#### Cut data for mapping ####
May$cuts <- as.factor(cut(May$may,
                           include.lowest = TRUE,
                           breaks = seq(14, 18, by = 1)))


#### Enough already! On with the show. ####
ggplot(May, aes(x = x, y = y, fill = cuts, colour = cuts)) +
  geom_polygon(data = PHL.df, aes(x = long, y = lat, group = group),
               colour = "#333333", fill = "#333333") +
  geom_tile(size = 0.4) + # eliminates lines between the cell
  scale_colour_brewer(type = "seq",
                      palette = "YlOrRd",
                      name = "Degrees") +
  scale_fill_brewer(type = "seq",
                    palette = "YlOrRd",
                    name = "Degrees") +
  labs(x = "Longitude",
       y = "Latitude") +
  ggtitle("Average Minimum May Temperatures Less Than 18 Degrees C for 1961-2000") +
  coord_map("cylindrical") # set map projection

#### Plot this on Google Earth ####
# Use the RColorBrewer package to create a new palette for plotting
my.palette <- colorRampPalette(brewer.pal(length(seq(14, 18, by = 1)),
                                "YlOrRd"), space = "Lab")

# Take the fifth layer, May, of the raster stack object and create a new SpatialPixelsDataframe
# for cutting and plotting as factors
May.KML <- as(tmn.stack[[5]], "SpatialPixelsDataFrame")
# When you change from a raster object to SpatialPixelsDataFrame, you need to reproject for GoogleEarth
May.KML <- reproject(May.KML)
# Cut the data creating a column of factors
May.KML$cuts <- cut(May.KML$may,
                    breaks = seq(14, 18, by = 1),
                           include.lowest = TRUE)

# Use plotKML function to plot SpatialPixelsDataFrame and open in GoogleEarth
plotKML(May.KML["cuts"],
        subfolder.name = "May Temps Below 18º C",
        layer.name = "May Temps Below 18º C",
        raster_name = "May.png",
        colour_scale = my.palette(length(levels(May$cuts))))

#eos
