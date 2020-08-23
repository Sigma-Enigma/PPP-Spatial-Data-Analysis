require(rgdal)
require(sp)
require(maps)
require(maptools)
require(rgeos)
require(dplyr)
require(ggplot2)
require(gpclib)
require(RColorBrewer)
require(spatstat)
require(MASS)
require(raster)
require(tigris) # great library that makes DLing shapefiles and doing geo_joins easy!
require(shiny)
require(shinydashboard)
require(leaflet)


# I forgot which specific libraries these commands use so I just loaded all my spatial and plotting libraries


##### LOAD AND AGGREGATE PPP DATA #####

# PPP data downloaded from https://sba.app.box.com/s/tvb0v5i57oa8gc6b5dcm9cyw7y2ms6pp
# https://www.sba.gov/funding-programs/loans/coronavirus-relief-options/paycheck-protection-program
dat1 <- read.csv2(file = "/Users/ryanarellano/Downloads/All Data by State/California/PPP Data up to 150k - CA.csv", header = TRUE, sep = "," )

# fix label of blank BusinessType response
levels(dat1$BusinessType)[1] <- "Unanswered"

dat1$DateApproved <- as.Date(dat1$DateApproved, format = "%m/%d/%Y")
dat1$LoanAmount <- as.numeric(dat1$LoanAmount)
dat1$JobsRetained <- as.numeric(dat1$JobsRetained)


# need to aggregate data by zip code regions using dplyr
zip.aggregate.dat.1 <- dat1 %>%
  group_by(Zip) %>%
  summarize(Total_LoanAmount = sum(as.numeric(LoanAmount)),
            Count_Loans = n(), 
            Total_JobsRetained = sum(as.numeric(JobsRetained)),
            Min_Approval_Date = min(DateApproved),
            Median_Approval_Date = median(DateApproved),
            Max_Approval_Date = max(DateApproved)
            # proportion of each business type
            )


# NAICS code (industry ) group them and label them
# make dropdowns for more granular NAICS code levels
# sliding day scale to see how data changed by day (or by week)
# see if you can show that some areas were greatly serviced earlier on as opposed to other areas (how to visualize this)


# sum( dat1$LoanAmount[ with(dat1$LoanAmount, dat1$BusinessType == "Unanswered" ), ]  )

# Simplify Business Type Groupings into these factors
# independent contractor + self employed, LLC and LLP, Coporation, Subchapter S Corp, Sole Prop, Non-profits, Others


# do separate analysis on the top 50 common lenders
# they amount of loans they did
# the proportion of total loans
# the aggregate sum of loans

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

zb1@data$Total_LoanAmount <- zb1@data$Total_LoanAmount / 1000000 # values down to millions

##### END SHAPEFILE CREATION AND EDITING ######



##### BEGIN PLOT CONSTRUCTION #####

# o1 <- as.owin(zb1) # may need to set custom owin to only focus on CA... need to set CRS

pal2 <- brewer.pal(7, "OrRd") # sets color palatte for legend
# layout.labels(obj, labels = TRUE, plot = FALSE)
#
# layout.scalebar(obj, corner = c(0.05, 0.95), scale = 1,
#                 labels = c(0, scale), height = 0.05,
#                 pos = 3, ..., plot = FALSE)
# 
# sb <- layout.scalebar(obj, corner = c(0.05, 0.95), scale = 1,
#                       labels = c(0, scale), height = 0.05,
#                       pos = 3, ..., plot = FALSE)




options(scipen = 10) # makes plotting less likely to use scientific notation (bigger range needed)

spplot2 <- spplot(zb1, "Total_LoanAmount", main = list(label = "Total Loan Amounts, in Millions", cex = 1), col.regions=pal2, cuts= 6, labels = list(cex = 15), par.settings = list(fontsize=list(text=100)) ) # stores plot code


# Generate and save plot in separate file (it excecutes way faster this way) ###
pdf(file = "/Users/ryanarellano/Desktop/R Plots/CA_Loan_Map_Test.pdf",   # The directory you want to save the file in
    width = 100, # The width of the plot in inches
    height = 100) # The height of the plot in inches

# Step 2: Create the plot with R code
spplot2

# Step 3: Run dev.off() to create the file!
dev.off()

options(scipen = 0) # sets scientific switch setting to default value

# NOTES: To fix legend and title font size, review crime mapping final plots code
?spplot

#### END PLOT CONSTRUCTION ####




# things to investigate
# By Zip-Region
# Top 5 jobs saved/ Loan Amount
# Bottom 5 Jobs Saved / Loan Amount

# Integrate other spatial data?

# Total count of loans under $1000
# Total count of loans under $100


# Tabular analysis
# Jobs Saved/Loan Amount ratio by industry
# Jobs Saved/Loan Amount ratio by business type


# Simplify Business Type Groupings into these factors
# independent contractor + self employed, LLC and LLP, Coporation, Subchapter S Corp, Sole Prop, Non-profits, Others

# NAICS code (industry ) group them and label them
# make dropdowns for more granular NAICS code levels
# sliding day scale to see how data changed by day (or by week)
# see if you can show that some areas were greatly serviced earlier on as opposed to other areas (how to visualize this)


# Count of each group that were approved for loans by first 7 days, then by first 4 weeks

# do separate analysis on the top 50 common lenders
# they amount of loans they did
# the proportion of total loans
# the aggregate sum of loans




