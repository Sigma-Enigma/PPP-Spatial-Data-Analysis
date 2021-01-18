# PPP Tabular Data Analysis


load("ca_ppp_data_cleaned_merged.RData")


# Tabular analysis
# Loans, Loan Amount, Jobs Saved, Jobs Saved/Loan Amount ratio by industry
# Jobs Saved/Loan Amount ratio by business type


f <-  levels(dat1$BusinessType) %>%
  fct_collapse( "Other" = c( "Cooperative", "Employee Stock Ownership Plan(ESOP)", "Joint Venture", "Rollover as Business Start-Ups (ROB", "Tenant in Common", "Trust", "Professional Association") ) %>%
  fct_collapse( "Non-Profit Organization" = c("Non-Profit Organization", "Non-Profit Childcare Center") ) %>%
  fct_collapse( "Limited Liability Company" = "Limited  Liability Company(LLC)")
# proportion of each business type

levels(dat1$BusinessType) <- f


# Lender Table
zip.aggregate.2 <- dat1 %>%
  group_by(Lender) %>%
  summarize(Total_LoanAmount = sum(as.numeric(LoanAmount), na.rm = TRUE),
            Count_Loans = n(),
            Total_JobsRetained = sum(as.numeric(JobsRetained), na.rm = TRUE)
  )

zip.aggregate.2$Total_LoanAmount_In_Millions <- round( (zip.aggregate.2$Total_LoanAmount / 1000000), 0)
zip.aggregate.2$Jobs_Retained_Per_1k_Lent <- round( (zip.aggregate.2$Total_JobsRetained / zip.aggregate.2$Total_LoanAmount)*10000 , 2)

names(zip.aggregate.2) <- c("Lender Name", "Total Amount Loaned", "Number of Loans", "Jobs Retained", "Total Amount Loaned (Millions)", "Jobs Retained Per $1,000 Loaned")

View(zip.aggregate.2[order(zip.aggregate.2$`Total Amount Loaned`, decreasing = TRUE)[1:50],] )

# Business Type Table
zip.aggregate.3 <- dat1 %>%
  group_by(BusinessType) %>%
  summarize(Total_LoanAmount = sum(as.numeric(LoanAmount), na.rm = TRUE),
            Count_Loans = n(), 
            Total_JobsRetained = sum(as.numeric(JobsRetained), na.rm = TRUE)
  )

zip.aggregate.3$Total_LoanAmount_In_Millions <- round( (zip.aggregate.3$Total_LoanAmount / 1000000), 0)
zip.aggregate.3$Jobs_Retained_Per_1k_Lent <- round( (zip.aggregate.3$Total_JobsRetained / zip.aggregate.3$Total_LoanAmount)*10000 , 2)

names(zip.aggregate.3) <- c("Business Type", "Total Amount Loaned", "Number of Loans", "Jobs Retained", "Total Amount Loaned (Millions)", "Jobs Retained Per $1,000 Loaned")

View(zip.aggregate.3)

# NAICS Table here


##### START PRETTY TABLES ######

# Pretty Lender name table
LenderTable <- zip.aggregate.2[, c("Lender Name", "Total Amount Loaned (Millions)", "Jobs Retained", "Jobs Retained Per $1,000 Loaned", "Number of Loans")]

LenderTable <- LenderTable[order(LenderTable$`Total Amount Loaned (Millions)` , decreasing = TRUE)[1:25],]

formattable(LenderTable, list(
  area(col = `Total Amount Loaned (Millions)`) ~ normalize_bar("pink", 0.2),
  area(col = `Jobs Retained`) ~ normalize_bar("lightblue", 0.2),
  area(col = `Jobs Retained Per $1,000 Loaned`) ~ normalize_bar("lightgreen", 0.2),
  area(col = `Number of Loans`) ~ normalize_bar("orange", 0.2)
))


# Pretty Business type table
BusinessTypeTable <- zip.aggregate.3[, c("Business Type", "Total Amount Loaned (Millions)", "Jobs Retained", "Jobs Retained Per $1,000 Loaned", "Number of Loans")]

BusinessTypeTable <- BusinessTypeTable[order(BusinessTypeTable$`Total Amount Loaned (Millions)` , decreasing = TRUE),]
View(BusinessTypeTable)

formattable(BusinessTypeTable, list(
  area(col = `Total Amount Loaned (Millions)`) ~ normalize_bar("pink", 0.2),
  area(col = `Jobs Retained`) ~ normalize_bar("lightblue", 0.2),
  area(col = `Jobs Retained Per $1,000 Loaned`) ~ normalize_bar("lightgreen", 0.2),
  area(col = `Number of Loans`) ~ normalize_bar("orange", 0.2)
))

# NAICS Code (Industry) type table

###### END PRETTY TABLES ######



# NAICS code (industry ) group them and label them
# make dropdowns for more granular NAICS code levels
# sliding day scale to see how data changed by day (or by week)
# see if you can show that some areas were greatly serviced earlier on as opposed to other areas (how to visualize this)


# Count of each group that were approved for loans by first 7 days, then by first 4 weeks

# do separate analysis on the top 50 common lenders
# they amount of loans they did
# the proportion of total loans
# the aggregate sum of loans


# things to investigate
# By Zip-Region
# Top 5 jobs saved/ Loan Amount
# Bottom 5 Jobs Saved / Loan Amount

# Integrate other spatial data?

# Total count of loans under $1000
# Total count of loans under $100


# interesting articles
#https://www.marketwatch.com/story/over-500000-businesses-got-ppp-loans-but-are-listed-as-retaining-zero-jobs-treasury-department-data-show-2020-07-08#:~:text=A%20MarketWatch%20analysis%20of%20the,%245%20million%20and%20%2410%20million.

# https://www.statmethods.net/RiA/lattice.pdf

# https://www.magesblog.com/post/2012-12-04-changing-colours-and-legends-in-lattice/

# https://blog.rstudio.com/2020/04/08/great-looking-tables-gt-0-2/

# formattable tutorial: https://renkun-ken.github.io/formattable/