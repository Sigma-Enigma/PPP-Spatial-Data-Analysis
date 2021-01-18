library(sp)
library(rgdal)
library(ggplot2)
library(RColorBrewer)
library(formattable)

library(maptools)
library(dplyr)
library(gpclib)

# library(tigris) # great library that makes DLing shapefiles and doing geo_joins easy!




##### LOAD AND AGGREGATE PPP DATA #####


#source("ppp_data_cleaned_merged.R")
load("ca_ppp_data_cleaned_merged.RData")

###### END LOAD AND AGGREGATE PPP DATA ####





##### BEGIN PLOT CONSTRUCTION #####

pal2 <- brewer.pal(8, "OrRd") # sets color palatte for legend
options(scipen = 10) # makes plotting less likely to use scientific notation (bigger range needed)
spplot2 <- spplot(zb1, "Total_LoanAmount", main = list(label = "Total Loan Amounts, in Millions", cex = 1), col.regions=pal2, at = c(0,2.5, 5,10,20,40,60,80,100), labels = list(cex = 15), par.settings = list(fontsize=list(text=100)) ) # stores plot code; ######### !!!!!make values in these ranges factors, order and then plot them!!!!! #########


# Generate and save plot in separate file (it excecutes way faster this way) ###
pdf(file = "Output Files/CA_Loan_Map_Test.pdf",   # The directory you want to save the file in
    width = 100, # The width of the plot in inches
    height = 100) # The height of the plot in inches

# Step 2: Create the plot with R code
spplot2

# Step 3: Run dev.off() to create the file!
dev.off()


# Histogram of jobs retained per $1k loaned
a <- ggplot(data = zb1@data, aes(zb1@data$Jobs_Retained_Per_1k_Lent) ) 

# full histogram
a + geom_histogram( aes( y=..count..), col = "cyan4", size = 0.75, fill = "cyan3", alpha = 0.4, breaks = seq(from=0, to=40, length.out = 41) ) + geom_density( aes(y = ..count..), col="indianred2", fill = "indianred2", size =0.5, alpha = 0.1, adjust = 1) + scale_x_continuous(breaks=seq(from=0, to=40, length.out = 21), labels = seq(from=0, to=40, length.out = 21), limits = c(0,40)) + labs(title = " Histogram: Count of Zip Regions Retaining X Jobs Per $1,000 USD Loaned", y = "Y = Count of Zip Regions", x = " X = # of Jobs Retained Per $1,000 Loaned") #note density does not need to be adjusted because full range of values is used for calculation

# truncated histogram
a + geom_histogram( aes( y=..count..), col = "cyan4", size = 0.75, fill = "cyan3", alpha = 0.4, breaks = seq(from=0, to=4, length.out = 41) ) + geom_density( aes(y = ..count..*.1), col="indianred2", fill = "indianred2", size =0.5, alpha = 0.1, adjust = .5) + scale_x_continuous(breaks=seq(from=0, to=4, length.out = 21), labels = seq(from=0, to=4, length.out = 21), limits = c(0,4)) + labs(title = " Histogram (Truncated): Count of Zip Regions Retaining X Jobs Per $1,000 USD Loaned", y = "Y = Count of Zip Regions", x = " X = # of Jobs Retained Per $1,000 Loaned")


# Histogram of total loaned 
b <- ggplot(data = zb1@data, aes(zb1@data$Total_LoanAmount) ) 

# full histogram
b + geom_histogram( aes( y=..count..), col = "cyan4", size = 0.75, fill = "cyan3", alpha = 0.4, breaks = seq(from=0, to=100, length.out = 41) ) + scale_x_continuous(breaks=seq(from=0, to=100, length.out = 21), labels = seq(from=0, to=100, length.out = 21), limits = c(0,100)) + geom_density( aes(y = ..count..*2.5), col="indianred2", fill = "indianred2", size =0.5, alpha = 0.1, adjust = 0.25) + labs(title = "Histogram: Count of Zip Regions with X Total Loaned in Millions USD", y = "Y = Count of Zip Regions", x = " X = # Total Loaned in Millions USD")

# truncated histogram
b + geom_histogram( aes( y=..count..), col = "cyan4", size = 0.75, fill = "cyan3", alpha = 0.4, breaks = seq(from=0, to=25, length.out = 41) ) + scale_x_continuous(breaks=seq(from=0, to=25, length.out = 21), labels = seq(from=0, to=25, length.out = 21), limits = c(0,25)) + geom_density( aes(y = ..count..*.65), col="indianred2", fill = "indianred2", size =0.5, alpha = 0.1, adjust = 0.125) + labs(title = "Histogram (Truncated): Count of Zip Regions with X Total Loaned in Millions USD", y = "Y = Count of Zip Regions", x = " X = # Total Loaned in Millions USD")




# add column of jobs retained per $1k loaned (but as factors)
zb1@data <- zb1@data %>% mutate(category=cut(Jobs_Retained_Per_1k_Lent, breaks=c(-.001, 0.001, 0.4, 0.8, 1.2, 1.6, 2, 4, 8, 40), labels=c("=0", "0-0.4", "0.4-0.8", "0.8-1.2", "1.2-1.6", "1.6-2", "2-4", "4-8", "40+") ) )
zb1@data$category <- as.numeric(zb1@data$category)
zb1$category <- as.numeric(zb1$category)

# jobs retained to $1k spent ratio static plot (using factors)
pal3 <- c("#bdbdbd", brewer.pal(7, "OrRd")) # sets color palatte for legend
spplot3 <- spplot(zb1, "category", main = list(label = "Jobs Retained Per $1,000 Lent", cex = 1), col.regions=pal3, labels = list(cex = 15), par.settings = list(fontsize=list(text=100)), at = c(1,2,3,4,5,6,7,8,9), names.attr = as.character(c("=0", "0-0.4", "0.4-0.8", "0.8-1.2", "1.2-1.6", "1.6-2", "2-4", "4-8", "40+")) ) # set labels after # also check out "formula" parameter

# Generate and save plot in separate file (it excecutes way faster this way) ###
pdf(file = "Output Files/CA_Loan_Map_Job_Per_1k_Lent.pdf",   # The directory you want to save the file in
    width = 100, # The width of the plot in inches
    height = 100) # The height of the plot in inches

# Step 2: Create the plot with R code
spplot3

# Step 3: Run dev.off() to create the file!
dev.off()


# jobs retained to $1k spent ratio static plot (using numeric values)
pal4 <- c("#bdbdbd", brewer.pal(9, "OrRd")) # sets color palatte for legend
#breakpoints <- as.numeric(quantile(zb1@data$Jobs_Retained_Per_1k_Lent, probs = seq(from=0, to=1, length.out = 11)))
spplot4 <- spplot(zb1, "Jobs_Retained_Per_1k_Lent", main = list(label = "Jobs Retained Per $1,000 Lent", cex = 1), col.regions=pal4, at = c(-.001, 0.001, 0.4, 0.8, 1.2, 1.6, 2, 4, 8, 40), labels = list(cex = 15), par.settings = list(fontsize=list(text=100))) # stores plot code
# get legend to show character values at set ranges between, perhaps transform to factors?? Yes do this
# set factor breaks from c(0, 0.000001, 0.4, 0.8, 1.2, 1.6, 2, 4, 8, 40) c(0, >0, >0.4, >0.8, >1.2, >1.6, >2, >4, >8, <40)
# is this plot off??

# Generate and save plot in separate file (it excecutes way faster this way) ###
pdf(file = "Output Files/CA_Loan_Map_Job_Per_1k_Lent_OLD.pdf",   # The directory you want to save the file in
    width = 100, # The width of the plot in inches
    height = 100) # The height of the plot in inches

# Step 2: Create the plot with R code
spplot4

# Step 3: Run dev.off() to create the file!
dev.off()


# NOTES: To fix legend cut points review crime mapping final plots code

#### END PLOT CONSTRUCTION ####


options(scipen = 0) # sets scientific switch setting to default value


