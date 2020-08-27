require(rgdal)
require(sp)
require(maptools)
require(dplyr)
require(gpclib)
require(RColorBrewer)
require(ggplot2)
require(reactable)
# require(tigris) # great library that makes DLing shapefiles and doing geo_joins easy!



# I forgot which specific libraries these commands use so I just loaded all my spatial and plotting libraries


##### LOAD AND AGGREGATE PPP DATA #####

# PPP data downloaded from https://sba.app.box.com/s/tvb0v5i57oa8gc6b5dcm9cyw7y2ms6pp
# https://www.sba.gov/funding-programs/loans/coronavirus-relief-options/paycheck-protection-program
dat1 <- read.csv2(file = "/Users/ryanarellano/Desktop/All Data by State/California/PPP Data up to 150k - CA.csv", header = TRUE, sep = "," )

# fix label of blank BusinessType response
levels(dat1$BusinessType)[1] <- "Unanswered"

dat1$DateApproved <- as.Date(dat1$DateApproved, format = "%m/%d/%Y")
dat1$LoanAmount <- as.numeric(as.character(dat1$LoanAmount))
dat1$JobsRetained <- as.numeric(as.character(dat1$JobsRetained))


# need to aggregate data by zip code regions using dplyr
zip.aggregate.dat.1 <- dat1 %>%
  group_by(Zip) %>%
  summarize(Total_LoanAmount = sum(as.numeric(LoanAmount), na.rm = TRUE),
            Count_Loans = n(), 
            Total_JobsRetained = sum(as.numeric(JobsRetained), na.rm = TRUE),
            Min_Approval_Date = min(DateApproved),
            Median_Approval_Date = median(DateApproved),
            Max_Approval_Date = max(DateApproved)
            # proportion of each business type
            )

# do other aggregate tables by industry,
# then do aggregate table by industry and date approved (by day for first 7 days, also by week for first 4 weeks)



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

zb1@data$Total_LoanAmount <- zb1@data$Total_LoanAmount / 1000000 # converts value to millions

zb1@data$Jobs_Retained_Per_10k_Lent <- zb1@data$Total_JobsRetained / (zb1@data$Total_LoanAmount*100) # converts value back to every 10k dollars

##### END SHAPEFILE CREATION AND EDITING ######



##### BEGIN PLOT CONSTRUCTION #####

pal2 <- brewer.pal(8, "OrRd") # sets color palatte for legend
options(scipen = 10) # makes plotting less likely to use scientific notation (bigger range needed)
spplot2 <- spplot(zb1, "Total_LoanAmount", main = list(label = "Total Loan Amounts, in Millions", cex = 1), col.regions=pal2, at = c(0,2.5, 5,10,20,40,60,80,100), labels = list(cex = 15), par.settings = list(fontsize=list(text=100)) ) # stores plot code; ######### !!!!!make values in these ranges factors, order and then plot them!!!!! #########


# Generate and save plot in separate file (it excecutes way faster this way) ###
pdf(file = "/Users/ryanarellano/Desktop/R Plots/CA_Loan_Map_Test.pdf",   # The directory you want to save the file in
    width = 100, # The width of the plot in inches
    height = 100) # The height of the plot in inches

# Step 2: Create the plot with R code
spplot2

# Step 3: Run dev.off() to create the file!
dev.off()



a <- ggplot(data = zb1@data, aes(zb1@data$Jobs_Retained_Per_10k_Lent) ) 

a + geom_histogram( aes( y=..count..), col = "cyan4", size = 0.75, fill = "cyan3", alpha = 0.4, breaks = seq(from=0, to=40, length.out = 41) ) + geom_density( aes(y = ..count..), col="indianred2", fill = "indianred2", size =0.5, alpha = 0.1, adjust = 1) + scale_x_continuous(breaks=seq(from=0, to=40, length.out = 21), labels = seq(from=0, to=40, length.out = 21), limits = c(0,40)) + labs(title = " Histogram: Count of Zip Regions Retaining X Jobs Per $10,000 USD Loaned", y = "Y = Count of Zip Regions", x = " X = # of Jobs Retained Per $10,000 Loaned") #note density does not need to be adjusted because full range of values is used for calculation

a + geom_histogram( aes( y=..count..), col = "cyan4", size = 0.75, fill = "cyan3", alpha = 0.4, breaks = seq(from=0, to=4, length.out = 41) ) + geom_density( aes(y = ..count..*.1), col="indianred2", fill = "indianred2", size =0.5, alpha = 0.1, adjust = .5) + scale_x_continuous(breaks=seq(from=0, to=4, length.out = 21), labels = seq(from=0, to=4, length.out = 21), limits = c(0,4)) + labs(title = " Histogram (Truncated): Count of Zip Regions Retaining X Jobs Per $10,000 USD Loaned", y = "Y = Count of Zip Regions", x = " X = # of Jobs Retained Per $10,000 Loaned")



b <- ggplot(data = zb1@data, aes(zb1@data$Total_LoanAmount) ) 

b + geom_histogram( aes( y=..count..), col = "cyan4", size = 0.75, fill = "cyan3", alpha = 0.4, breaks = seq(from=0, to=100, length.out = 41) ) + scale_x_continuous(breaks=seq(from=0, to=100, length.out = 21), labels = seq(from=0, to=100, length.out = 21), limits = c(0,100)) + geom_density( aes(y = ..count..*2.5), col="indianred2", fill = "indianred2", size =0.5, alpha = 0.1, adjust = 0.25) + labs(title = "Histogram: Count of Zip Regions with X Total Loaned in Millions USD", y = "Y = Count of Zip Regions", x = " X = # Total Loaned in Millions USD")

b + geom_histogram( aes( y=..count..), col = "cyan4", size = 0.75, fill = "cyan3", alpha = 0.4, breaks = seq(from=0, to=25, length.out = 41) ) + scale_x_continuous(breaks=seq(from=0, to=25, length.out = 21), labels = seq(from=0, to=25, length.out = 21), limits = c(0,25)) + geom_density( aes(y = ..count..*.65), col="indianred2", fill = "indianred2", size =0.5, alpha = 0.1, adjust = 0.125) + labs(title = "Histogram (Truncated): Count of Zip Regions with X Total Loaned in Millions USD", y = "Y = Count of Zip Regions", x = " X = # Total Loaned in Millions USD")


zb1@data <- zb1@data %>% mutate(category=cut(Jobs_Retained_Per_10k_Lent, breaks=c(-.001, 0.001, 0.4, 0.8, 1.2, 1.6, 2, 4, 8, 40), labels=c("=0", "0-0.4", "0.4-0.8", "0.8-1.2", "1.2-1.6", "1.6-2", "2-4", "4-8", "40+") ) )
zb1@data$category <- as.numeric(zb1@data$category)
zb1$category <- as.numeric(zb1$category)

pal3 <- c("#bdbdbd", brewer.pal(7, "OrRd")) # sets color palatte for legend
spplot3 <- spplot(zb1, "category", main = list(label = "Jobs Retained Per $10,000 Lent", cex = 1), col.regions=pal3, labels = list(cex = 15), par.settings = list(fontsize=list(text=100)), at = c(1,2,3,4,5,6,7,8,9), names.attr = as.character(c("=0", "0-0.4", "0.4-0.8", "0.8-1.2", "1.2-1.6", "1.6-2", "2-4", "4-8", "40+")) ) # set labels after # also check out "formula" parameter

# Generate and save plot in separate file (it excecutes way faster this way) ###
pdf(file = "/Users/ryanarellano/Desktop/R Plots/CA_Loan_Map_Job_Per_10K_Lent.pdf",   # The directory you want to save the file in
    width = 100, # The width of the plot in inches
    height = 100) # The height of the plot in inches

# Step 2: Create the plot with R code
spplot3

# Step 3: Run dev.off() to create the file!
dev.off()


# second plot with jobs saved to mills spent ratio
pal4 <- c("#bdbdbd", brewer.pal(9, "OrRd")) # sets color palatte for legend
#breakpoints <- as.numeric(quantile(zb1@data$Jobs_Retained_Per_10k_Lent, probs = seq(from=0, to=1, length.out = 11)))
spplot4 <- spplot(zb1, "Jobs_Retained_Per_10k_Lent", main = list(label = "Jobs Retained Per $10,000 Lent", cex = 1), col.regions=pal4, at = c(-.001, 0.001, 0.4, 0.8, 1.2, 1.6, 2, 4, 8, 40), labels = list(cex = 15), par.settings = list(fontsize=list(text=100))) # stores plot code
# get legend to show character values at set ranges between, perhaps transform to factors?? Yes do this
# set factor breaks from c(0, 0.000001, 0.4, 0.8, 1.2, 1.6, 2, 4, 8, 40) c(0, >0, >0.4, >0.8, >1.2, >1.6, >2, >4, >8, <40)
# is this plot off??

# Generate and save plot in separate file (it excecutes way faster this way) ###
pdf(file = "/Users/ryanarellano/Desktop/R Plots/CA_Loan_Map_Job_Per_10K_Lent_OLD.pdf",   # The directory you want to save the file in
    width = 100, # The width of the plot in inches
    height = 100) # The height of the plot in inches

# Step 2: Create the plot with R code
spplot4

# Step 3: Run dev.off() to create the file!
dev.off()


# NOTES: To fix legend cut points review crime mapping final plots code

#### END PLOT CONSTRUCTION ####


options(scipen = 0) # sets scientific switch setting to default value



# things to investigate
# By Zip-Region
# Top 5 jobs saved/ Loan Amount
# Bottom 5 Jobs Saved / Loan Amount

# Integrate other spatial data?

# Total count of loans under $1000
# Total count of loans under $100


# Tabular analysis
# Loans, Loan Amount, Jobs Saved, Jobs Saved/Loan Amount ratio by industry
# Jobs Saved/Loan Amount ratio by business type


dat2 <- dat1

# try to combine levels using dplyr notation
f <-  levels(dat1$BusinessType) %>%
  fct_collapse( "Other" = c( "Cooperative", "Employee Stock Ownership Plan(ESOP)", "Joint Venture", "Rollover as Business Start-Ups (ROB", "Tenant in Common", "Trust", "Professional Association") ) %>%
  fct_collapse( "Non-Profit Organization" = c("Non-Profit Organization", "Non-Profit Childcare Center") )
            # proportion of each business type

levels(dat2$BusinessType) <- f



zip.aggregate.2 <- dat2 %>%
  group_by(Lender) %>%
  summarize(Total_LoanAmount = sum(as.numeric(LoanAmount), na.rm = TRUE),
            Count_Loans = n(),
            Total_JobsRetained = sum(as.numeric(JobsRetained), na.rm = TRUE)
  )

zip.aggregate.2$Total_LoanAmount_In_Millions <- zip.aggregate.2$Total_LoanAmount / 1000000
zip.aggregate.2$Jobs_Retained_Per_10k_Lent <- (zip.aggregate.2$Total_JobsRetained / zip.aggregate.2$Total_LoanAmount) *10000


zip.aggregate.3 <- dat2 %>%
  group_by(BusinessType) %>%
  summarize(Total_LoanAmount = sum(as.numeric(LoanAmount), na.rm = TRUE),
            Count_Loans = n(), 
            Total_JobsRetained = sum(as.numeric(JobsRetained), na.rm = TRUE)
  )

zip.aggregate.3$Total_LoanAmount_In_Millions <- zip.aggregate.3$Total_LoanAmount / 1000000
zip.aggregate.3$Jobs_Retained_Per_10k_Lent <- (zip.aggregate.3$Total_JobsRetained / zip.aggregate.3$Total_LoanAmount) *10000



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



# interesting articles
#https://www.marketwatch.com/story/over-500000-businesses-got-ppp-loans-but-are-listed-as-retaining-zero-jobs-treasury-department-data-show-2020-07-08#:~:text=A%20MarketWatch%20analysis%20of%20the,%245%20million%20and%20%2410%20million.

# https://www.statmethods.net/RiA/lattice.pdf

# https://www.magesblog.com/post/2012-12-04-changing-colours-and-legends-in-lattice/

# https://blog.rstudio.com/2020/04/08/great-looking-tables-gt-0-2/
