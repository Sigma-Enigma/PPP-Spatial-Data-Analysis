# PPP Data Cleaning and Merging 
# feed the output of this file to the dashboard script

library(rgdal)
library(dplyr)


# !!!! ADD POPULATION AND ECONOMIC DATA TO SERVE AS A CONTROL VARIABLE FOR LENDING!!! Otherwise effect of lending is largely a function of population/# of businesses/orgs of the region !!!!


##### LOAD AND AGGREGATE PPP DATA #####

# PPP data downloaded from https://sba.app.box.com/s/tvb0v5i57oa8gc6b5dcm9cyw7y2ms6pp
# https://www.sba.gov/funding-programs/loans/coronavirus-relief-options/paycheck-protection-program
dat1 <- read.csv2(file = "Data/All Data by State/California/PPP Data up to 150k - CA.csv", header = TRUE, sep = "," )

# fix label of blank BusinessType response
levels(dat1$BusinessType)[1] <- "Unanswered"

# fixing column data formats

dat1$DateApproved <- as.Date(dat1$DateApproved, format = "%m/%d/%Y")
dat1$LoanAmount <- as.numeric(as.character(dat1$LoanAmount))
dat1$JobsRetained <- as.numeric(as.character(dat1$JobsRetained))
dat1$IndustryNumber <- substr(as.character(dat1$NAICSCode), start = 1, stop = 2)

# cleaning bad zips not in california 
dat1 <- subset(dat1, subset = (Zip > 89119 & Zip < 96214) )

# changing zip data format to match with shapefile zip format later
dat1$Zip <- as.character(dat1$Zip)

# TIGER shapefiles were downloaded from https://www2.census.gov/geo/tiger/TIGER2019/ZCTA5/ 
zipbounds1 <- readOGR( dsn = "Data/tl_2019_us_zcta510", layer = "tl_2019_us_zcta510")

# grab unique california zip codes
ca.zl <- unique(dat1$Zip) # gets list of unique zip codes from PPP data
ca.zl <- ca.zl[!(ca.zl %in% sort(ca.zl, decreasing = F)[1:18] )] # removes lower bad zip codes (puerto rico etc)
ca.zl <- ca.zl[!(ca.zl %in% sort(ca.zl, decreasing = T)[1:9] )] # removes upper bad zip codes (oregon washington)

# subsets CA zipcode shapefiles to remove shapefile components not in ca.zl vector
zb1 <- subset(zipbounds1, (zipbounds1$ZCTA5CE10) %in% ca.zl ) 
rm(zipbounds1)

FinalCaZipList <- as.character(unique(zb1$ZCTA5CE10))

# download 2-6 digit 2017 NAICS Code File 
#https://www.census.gov/eos/www/naics/downloadables/downloadables.html
NaicsCodeData <- read.csv2( file = "Data/2-6 digit_2017_Codes.csv", header = TRUE, sep = ",")
NaicsCodeDataClean <- NaicsCodeData[which(nchar(as.character(NaicsCodeData$X2017.NAICS.US...Code)) == 2),]
NaicsCodeDataClean <- NaicsCodeDataClean[,2:3]
names(NaicsCodeDataClean) <- c("IndustryNumber", "IndustryName")
NaicsCodeDataClean$IndustryNumber <- as.character(NaicsCodeDataClean$IndustryNumber)
NaicsCodeDataClean$IndustryName <- as.character(NaicsCodeDataClean$IndustryName)
rm(NaicsCodeData)

# Add missing NAICS industry rows
NaicsCodeDataClean <- NaicsCodeDataClean %>% add_row( IndustryNumber = "99", IndustryName = "Unclassified", .after = 17) 
NaicsCodeDataClean <- NaicsCodeDataClean %>% add_row( IndustryNumber = "49", IndustryName = "Transportation & Warehousing", .after = 5)
NaicsCodeDataClean <- NaicsCodeDataClean %>% add_row( IndustryNumber = "48", IndustryName = "Transportation & Warehousing", .after = 5)
NaicsCodeDataClean <- NaicsCodeDataClean %>% add_row( IndustryNumber = "45", IndustryName = "Retail Trade", .after = 5)
NaicsCodeDataClean <- NaicsCodeDataClean %>% add_row( IndustryNumber = "44", IndustryName = "Retail Trade", .after = 5)
NaicsCodeDataClean <- NaicsCodeDataClean %>% add_row( IndustryNumber = "33", IndustryName = "Manufacturing", .after = 4)
NaicsCodeDataClean <- NaicsCodeDataClean %>% add_row( IndustryNumber = "32", IndustryName = "Manufacturing", .after = 4)
NaicsCodeDataClean <- NaicsCodeDataClean %>% add_row( IndustryNumber = "31", IndustryName = "Manufacturing", .after = 4)
NaicsCodeDataClean <- NaicsCodeDataClean %>% add_row( IndustryNumber = NA, IndustryName = "Data Not Available", .after = 25)


# Zip code census data
# https://github.com/Ro-Data/Ro-Census-Summaries-By-Zipcode 
CaDemographicData <- read.delim2( file = "Data/census_demo.txt", header = TRUE, sep = "\t")
CaDemographicData <- subset(CaDemographicData, ZCTA5 %in% FinalCaZipList)
CaDemographicData <- CaDemographicData[,1:2]
names(CaDemographicData) <- c("ZCTA5", "TotalPopulation")
CaDemographicData$ZCTA5 <- as.character(CaDemographicData$ZCTA5)
CaDemographicData$TotalPopulation <- as.numeric(CaDemographicData$TotalPopulation)

CaEconomicData <- read.delim2( file = "Data/census_econ.txt", header = TRUE, sep = "\t")
CaEconomicData <- subset(CaEconomicData, ZCTA5 %in% FinalCaZipList)
CaEconomicData <- CaEconomicData[,1:3]
names(CaEconomicData) <- c("ZCTA5", "PopulationOver16", "EmployedPopulationOver16")
CaEconomicData$ZCTA5 <- as.character(CaEconomicData$ZCTA5)
CaEconomicData$PopulationOver16 <- as.numeric(CaEconomicData$PopulationOver16)
CaEconomicData$EmployedPopulationOver16 <- as.numeric(CaEconomicData$EmployedPopulationOver16)

# Zip code organizational data
# https://www.census.gov/data/datasets/2018/econ/cbp/2018-cbp.html

CaOrganizationData <- read.delim2( file = "Data/zbp18totals.txt", header = TRUE, sep = ",")
CaOrganizationData <- subset(CaOrganizationData, zip %in% FinalCaZipList) # note missing about 31 zip code regions,
CaOrganizationData <- CaOrganizationData[,-2]

# adjust factor labels
 
library(plyr)
for(i in c(2,4,6)) {
  CaOrganizationData[,i] <- revalue(CaOrganizationData[,i], c("G"="0-2% error", "H"="2-5% error", "J"=">=5% error"))
}
names(CaOrganizationData) <- c("zip", "employmentError", "employment", "quarter1ProfitError", "quarter1Profit", "annualPayrollError", "annualPayrollInThousands", "establishments", "city", "state", "county")
# adjust numeric mode
for(i in c(3,5,7,8)) {
  CaOrganizationData[,i] <- as.numeric(CaOrganizationData[,i])
}
# adjust character values
for(i in c(1,2,4,6,9,10,11)){
CaOrganizationData[,i] <- as.character(CaOrganizationData[,i])
}
rm(i)

detach("package:plyr", unload=TRUE)

MergedPppData <- left_join(x = dat1, y = NaicsCodeDataClean, by = "IndustryNumber")
MergedPppData <- left_join(x = MergedPppData, y = CaDemographicData, by = c("Zip" = "ZCTA5"))
MergedPppData <- left_join(x = MergedPppData, y = CaEconomicData, by = c("Zip" = "ZCTA5"))
MergedPppData <- left_join(x = MergedPppData, y = CaOrganizationData, by = c("Zip" = "zip"))


# CLEAN UP VARIABLE NAMES AND REDUNDANT CODE BEFORE PUBLICATION
# need to aggregate data by zip code regions using dplyr

zip.aggregate.dat.1 <- MergedPppData %>%
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
# !!!!!! then do aggregate table by  date approved (by day for first 7 days, also by week for first 4 weeks) !!!!!! #

# force data type match for later shapefile
zip.aggregate.dat.1$Zip <- as.factor(zip.aggregate.dat.1$Zip)

###### END LOAD AND AGGREGATE PPP DATA ####


###### SHAPEFILE CREATION AND EDITING #####
# before reading in OGR file you need to append the aggregate zip code data via sql/dplr join functions
# examples
# https://stackoverflow.com/questions/19791210/r-ggplot2-merge-with-shapefile-and-csv-data-to-fill-polygons
# https://gis.stackexchange.com/questions/110183/join-csv-file-to-shapefile

#spatial join aggregate table data and shapefile data
zb1@data <- left_join(zb1@data, zip.aggregate.dat.1, by = c("ZCTA5CE10" = "Zip")) # do i only merge with @data file, or parent shapefile??? ... it looks like merging the files seemed to coerce them both shp and @data zip values to characters

zb1@data$Total_LoanAmount_Thousands <- zb1@data$Total_LoanAmount / 1000 # converts value to thousands

zb1@data$Jobs_Retained_Per_1k_Lent <- zb1@data$Total_JobsRetained / (zb1@data$Total_LoanAmount) 


#save.image("ca_ppp_data_cleaned_merged.RData")
