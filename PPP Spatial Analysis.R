require(rgdal)
require(sp)
require(maps)
require(spdep)
require(maptools)
require(rgeos)
require(dplyr)
require(ggplot2)
require(gpclib)
require(RColorBrewer)
require(spatstat)
require(MASS)
require(geostatsp)
require(spgrass6)
require(PtProcess)
require(raster)
# I forgot which specific libraries these commands use so I just loaded all my spatial and plotting libraries


##### LOAD AND AGGREGATE PPP DATA #####

# PPP data downloaded from https://sba.app.box.com/s/tvb0v5i57oa8gc6b5dcm9cyw7y2ms6pp
# https://www.sba.gov/funding-programs/loans/coronavirus-relief-options/paycheck-protection-program
dat1 <- read.csv2(file = "/Users/ryanarellano/Downloads/All Data by State/California/PPP Data up to 150k - CA.csv", header = TRUE, sep = "," )

# need to aggregate data by zip code regions using dplyr
zip.aggregate.dat.1 <- dat1 %>%
  group_by(Zip) %>%
  summarize(Total_LoanAmount = sum(as.numeric(LoanAmount)),
            Count_Loans = n(), 
            Total_JobsRetained = sum(as.numeric(JobsRetained))
            )

# force data type match for later shapefile
zip.aggregate.dat.1$Zip <- as.factor(zip.aggregate.dat.1$Zip)

###### END LOAD AND AGGREGATE PPP DATA ####




###### SHAPEFILE CREATION AND EDITING #####

# before reading in OGR file you need to append the aggregate zip code data via sql/dplr join functions
# examples
# https://stackoverflow.com/questions/19791210/r-ggplot2-merge-with-shapefile-and-csv-data-to-fill-polygons
# https://gis.stackexchange.com/questions/110183/join-csv-file-to-shapefile

# TIGER shapefiles were downloaded from https://www2.census.gov/geo/tiger/TIGER2019/ZCTA5/ 
zipbounds1 <- readOGR( dsn = "/Users/ryanarellano/Downloads/tl_2019_us_zcta510", layer = "tl_2019_us_zcta510")

# grab unique california zip codes
ca.zl <- unique(dat1$Zip) # gets list of unique zip codes from PPP data
ca.zl <- ca.zl[!(ca.zl %in% sort(ca.zl, decreasing = F)[1:18] )] # removes lower bad zip codes (puerto rico etc)
ca.zl <- ca.zl[!(ca.zl %in% sort(ca.zl, decreasing = T)[1:9] )] # removes upper bad zip codes (oregon washington)


# subsets CA zipcode shapefiles
zb1 <- subset(zipbounds1, (zipbounds1$ZCTA5CE10) %in% ca.zl ) 

#spatial join aggregate table data and shapefile data
zb1@data <- left_join(zb1@data, zip.aggregate.dat.1, by = c("ZCTA5CE10" = "Zip")) # do i only merge with @data file, or parent shapefile??? ... it looks like merging the files seemed to coerce them both shp and @data zip values to characters

##### END SHAPEFILE CREATION AND EDITING ######



##### BEGIN PLOT CONSTRUCTION #####

#o1 <- as.owin(zb1) # may need to set custom owin to only focus on CA... need to set CRS

pal2 <- brewer.pal(9, "OrRd") # sets color palatte for legend
spplot2 <- spplot(zb1, "Total_LoanAmount", main="Total Loan Amounts", col.regions=pal2, cuts=8) # stores plot code

# Generate and save plot in separate file (it excecutes way faster this way) ###
pdf(file = "/Users/ryanarellano/Desktop/R Plots/CA_Loan_Map_HiDef.pdf",   # The directory you want to save the file in
    width = 500, # The width of the plot in inches
    height = 500) # The height of the plot in inches

# Step 2: Create the plot with R code
spplot2

# Step 3: Run dev.off() to create the file!
dev.off()


# NOTES: To fix legend and title font size, review crime mapping final plots code

#### END PLOT CONSTRUCTION ####
