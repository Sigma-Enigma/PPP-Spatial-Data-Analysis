require(rgdal)
require(dplyr)
require(RColorBrewer)
require(shiny)
require(shinydashboard)
require(leaflet)
require(DT)
# require(tigris) # BONUS: great library that makes DLing shapefiles and doing geo_joins easy!


########## DATA PREP BEGIN ###########

# ppp = Paycheck Protection Program, naics = North American Industry Classification System

# Data information: https://www.sba.gov/sites/default/files/2020-07/PPP%20Loan%20Data%20-%20Key%20Aspects-508.pdf
# Tabular data source: https://home.treasury.gov/policy-issues/cares-act/assistance-for-small-businesses/sba-paycheck-protection-program-loan-level-data
# go to Box website to DL individual states for loan amounts under 150k, or loans over 150k for all states

# loading and cleaning data test
# pppDataCalifornia <- read.csv2(file = file.choose(), header = TRUE, sep = "," )
pppDataCalifornia <- read.csv2(file = "/Users/ryanarellano/Downloads/All Data by State/California/PPP Data up to 150k - CA.csv", header = TRUE, sep = "," )

# changing blank businessType label to unanswered (perhaps use NA?)
levels(pppDataCalifornia$BusinessType)[1] <- "Unanswered"

# fixing column data formats
pppDataCalifornia$DateApproved <- as.Date(pppDataCalifornia$DateApproved, format = "%m/%d/%Y")
pppDataCalifornia$LoanAmount <- as.numeric(as.character(pppDataCalifornia$LoanAmount))
pppDataCalifornia$JobsRetained <- as.numeric(as.character(pppDataCalifornia$JobsRetained))
pppDataCalifornia$IndustryNumber <- substr(as.character(pppDataCalifornia$NAICSCode), start = 1, stop = 2)


# cleaning bad zips not in california 
pppDataCalifornia <- subset(pppDataCalifornia, subset = (Zip > 89119 & Zip < 96214) ) # removes bad zip codes not in cali
# changing zip data format to match with shapefile zip format later
pppDataCalifornia$Zip <- as.factor(pppDataCalifornia$Zip)

# download 2-6 digit 2017 NAICS Code File 
#https://www.census.gov/eos/www/naics/downloadables/downloadables.html
naicsCodeData <- read.csv2( file = "/Users/ryanarellano/Downloads/2-6 digit_2017_Codes.csv", header = TRUE, sep = ",")
naicsCodeDataClean <- naicsCodeData[which(nchar(as.character(naicsCodeData$X2017.NAICS.US...Code)) == 2),]
naicsCodeDataClean <- naicsCodeDataClean[,2:3]
names(naicsCodeDataClean) <- c("IndustryNumber", "IndustryName")
naicsCodeDataClean$IndustryNumber <- as.character(naicsCodeDataClean$IndustryNumber)
naicsCodeDataClean$IndustryName <- as.character(naicsCodeDataClean$IndustryName)


# Add missing NAICS industry rows
naicsCodeDataClean <- naicsCodeDataClean %>% add_row( IndustryNumber = "99", IndustryName = "Unclassified", .after = 17) 
naicsCodeDataClean <- naicsCodeDataClean %>% add_row( IndustryNumber = "49", IndustryName = "Transportation & Warehousing", .after = 5)
naicsCodeDataClean <- naicsCodeDataClean %>% add_row( IndustryNumber = "48", IndustryName = "Transportation & Warehousing", .after = 5)
naicsCodeDataClean <- naicsCodeDataClean %>% add_row( IndustryNumber = "45", IndustryName = "Retail Trade", .after = 5)
naicsCodeDataClean <- naicsCodeDataClean %>% add_row( IndustryNumber = "44", IndustryName = "Retail Trade", .after = 5)
naicsCodeDataClean <- naicsCodeDataClean %>% add_row( IndustryNumber = "33", IndustryName = "Manufacturing", .after = 4)
naicsCodeDataClean <- naicsCodeDataClean %>% add_row( IndustryNumber = "32", IndustryName = "Manufacturing", .after = 4)
naicsCodeDataClean <- naicsCodeDataClean %>% add_row( IndustryNumber = "31", IndustryName = "Manufacturing", .after = 4)
naicsCodeDataClean <- naicsCodeDataClean %>% add_row( IndustryNumber = NA, IndustryName = "Data Not Available", .after = 25)


mergedPppData <- left_join(x = pppDataCalifornia, y = naicsCodeDataClean, by = "IndustryNumber")



# aggregates data by zip code regions using dplyr grouped by Zip and DateApproved
zipAndDateAggregatedData <- mergedPppData %>%
  group_by(Zip, DateApproved) %>% # use ", DateApproved" after Zip to include another grouping factor and lower compute time
  summarize(Total_Amount_Loaned = sum(as.numeric(LoanAmount), na.rm = TRUE),
            Total_Loans_Approved = n(), 
            Total_Jobs_Retained = sum(as.numeric(JobsRetained), na.rm = TRUE)
            # proportion of each business type
            # more layers here
  )


# Shapefile documentation: https://www.census.gov/programs-surveys/geography/technical-documentation/complete-technical-documentation/tiger-geo-line.html
# Shapefile data location: https://www.census.gov/cgi-bin/geo/shapefiles/index.php  # SELECT DOWNLOAD ZIP CODE REGION

# load shapefiles; TIP: use tigris to load a different shapefile!
# zipBoundaryShapefile <- readOGR( dsn = file.choose(), layer = "tl_2019_us_zcta510", verbose = TRUE)
zipBoundaryShapefile <- readOGR( dsn = "/Users/ryanarellano/Downloads/tl_2019_us_zcta510", layer = "tl_2019_us_zcta510", verbose = TRUE)

# grab unique california zip codes
caZipList <- unique(pppDataCalifornia$Zip) # gets vector of unique zip codes from PPP data

# subsets CA zipcode shapefiles to remove shapefile components not in caZipList vector
finalZipBoundaryShapefile <- subset(zipBoundaryShapefile, (zipBoundaryShapefile$ZCTA5CE10) %in% caZipList ) # same as states variable #needed to do an inner join
# cache this for webserver

# subset zip and date grouped tabular zip data to figure out which in zipAndDateAggregatedData are NOT in finalZipBoundaryShapefile
zipAndDateAggregatedData <- subset(zipAndDateAggregatedData, zipAndDateAggregatedData$Zip %in% finalZipBoundaryShapefile$ZCTA5CE10 )
# cache this for webserver

# subset zip grouped to figure out which in pppDataCalifornia are NOT in finalZipBoundaryShapefile
pppDataCalifornia <- subset(pppDataCalifornia, pppDataCalifornia$Zip %in% finalZipBoundaryShapefile$ZCTA5CE10 )
# cache this for webserver

finalCaZipList <- unique(pppDataCalifornia$Zip)

# Zip code census data
# https://github.com/Ro-Data/Ro-Census-Summaries-By-Zipcode 
californiaDemographicData <- read.delim2( file = "/Users/ryanarellano/Downloads/census_demo.txt", header = TRUE, sep = "\t")
californiaDemographicData <- subset(californiaDemographicData, ZCTA5 %in% finalCaZipList)
californiaDemographicData <- californiaDemographicData[,1:2]

californiaEconomicData <- read.delim2( file = "/Users/ryanarellano/Downloads/census_econ.txt", header = TRUE, sep = "\t")
californiaEconomicData <- subset(californiaEconomicData, ZCTA5 %in% finalCaZipList)
californiaEconomicData <- californiaEconomicData[,1:3]

# Zip code organizational data
# https://www.census.gov/data/datasets/2018/econ/cbp/2018-cbp.html

californiaOrganizationData <- read.delim2( file = "/Users/ryanarellano/Downloads/zbp18totals.txt", header = TRUE, sep = ",")
californiaOrganizationData <- subset(californiaOrganizationData, zip %in% finalCaZipList)
# note missing about 31 zip code regions

# aggregates data by zip code regions using dplyr grouped by Zip only this time (useful for keeping legend min and max values constant when changing time windows!!!)
zipAggregatedData <- pppDataCalifornia %>%
  group_by(Zip) %>%
  summarize(Total_Amount_Loaned = sum(as.numeric(LoanAmount), na.rm = TRUE),
            Total_Loans_Approved = n(), 
            Total_Jobs_Retained = sum(as.numeric(JobsRetained), na.rm = TRUE)
            # proportion of each business type
            # more layers here
  )
# cache this for webserver


# NOTE: make domain a variable for each layer indicating the maxed tabulated value for the entire dataset (all times) OR that changes color scale based on size of time frame

######### DATA PREP END ########



######### DASHBOARD WIDGET BEGIN ############

# conside including a submit button to run changes in inputs (or longer delay in waiting for input changes to stop)

# define UI for dashboard
ui <- dashboardPage(
  skin = "red",
  dashboardHeader( title = "PPP Loan Dashboard"),
  dashboardSidebar(
    dateRangeInput(
      "DateApproved", 
      label = "Date Range",
      start = min(pppDataCalifornia$DateApproved),
      end = max(pppDataCalifornia$DateApproved),
      min = min(pppDataCalifornia$DateApproved),
      max = max(pppDataCalifornia$DateApproved),
      format = "yyyy-mm-dd",
      separator = " to "
    ),
    selectInput(inputId = "selectDataLayer", label = "Select Data Layer", choices = names(zipAndDateAggregatedData[3:length(zipAndDateAggregatedData)]) ),
    actionButton("actionButton", "Submit"),
    p("Click the Submit button to display the data for the features selected in the side panel.", "Note: Widget currently not optimized and can take 15-30 seconds to load changes.")
    
  ),
  dashboardBody(
    fluidRow( box( width = 12, leafletOutput( outputId = "leafletMap", height = "750px" ) ) ), #adjust height when publishing
    fluidRow( box( width = 12, dataTableOutput( outputId = "summaryTable" ) ) )
  )
  
)


# Define server input/output logic to pass map data and build map widget
server <- function(input, output){
  dataInput <- eventReactive(input$actionButton, {
    # use dplyr to aggregate data and create tables to pass to polygons 
    zipAndDateAggregatedData %>%
      filter( DateApproved >= input$DateApproved[1] ) %>% # lower slider bound
      filter( DateApproved <= input$DateApproved[2] ) %>% # upper slider bound
      group_by( Zip ) %>% # create data table given slider inputs
      summarize(Total_Amount_Loaned = sum(as.numeric(Total_Amount_Loaned), na.rm = TRUE),
                Total_Loans_Approved = n(), 
                Total_Jobs_Retained = sum(as.numeric(Total_Jobs_Retained), na.rm = TRUE)
                # add proportion of each business type
                # add proportion of each industry type NAICS
                # add CONTROL VARIABLES!!!
      )
    
  })
  
  # re-order data for label values so they line up nicely with zipboundary polygons table order
  dataInputOrdered <- eventReactive(input$actionButton, { 
    dataInput()[order(match(dataInput()$Zip, finalZipBoundaryShapefile$ZCTA5CE10)), ] 
  })
  
  
  # make eventReactive value = data layer variable name
  inputDataLayerName <- eventReactive(input$actionButton, {
    input$selectDataLayer[1]
  })
  
  # make eventReactive value = values of above data layer
  inputDataLayerValues <- eventReactive(input$actionButton, {
    pull( dataInputOrdered(), inputDataLayerName() )
  })
  
  colorPalette <- eventReactive(input$actionButton, { colorBin("RdYlBu", reverse = TRUE, domain = c(min( inputDataLayerValues() ), max( inputDataLayerValues() ) ), na.color = "#808080") 
    
  })
  
  # set label variables to pass data 
  # to do: include if statement to bold the text of the displayed data layer
  labels <- eventReactive(input$actionButton, {
    paste("<p>", "Zip Code Region: ", dataInputOrdered()$Zip, "<p>",
          "<p>", "Total Amount Loaned: ", format(dataInputOrdered()$Total_Amount_Loaned, nsmall = 0, big.mark = ","), "<p>",
          "<p>", "Total Loans Approved: ", format(dataInputOrdered()$Total_Loans_Approved, nsmall = 0, big.mark = ","), "<p>",
          "<p>", "Total Jobs Retained: ", format(dataInputOrdered()$Total_Jobs_Retained, nsmall = 0, big.mark = ","), "<p>",
          sep = "")
    
  })
  
  # build and render leaflet map
  output$leafletMap <- renderLeaflet( 
    leaflet(finalZipBoundaryShapefile) %>% 
      setView(lng = -118.01, lat = 34.00, zoom = 9) %>% # sets initial map starting view
      addProviderTiles(providers$CartoDB.Positron) %>%  # adding background base map
      addPolygons( weight = 1, # adding shapefiles
                   smoothFactor = 0.5,
                   color = "white",
                   fillOpacity = 0.25,
                   fillColor = colorPalette()( inputDataLayerValues() ), # note: double parenthesis because its a eventReactive function!
                   highlightOptions = highlightOptions(
                     weight = 5,
                     color = "#ffffff",
                     dashArray = NULL,
                     fillOpacity = 0.5,
                     bringToFront = TRUE
                   ),
                   label = lapply(labels(), HTML)) %>%
      addLegend( pal = colorPalette(), # will need to be adjusted later with dropdown layers
                 values = ~inputDataLayerValues() , # dynamically changes with layers, (should it keep constant with date changes?)
                 opacity = 0.25,
                 position = "topright",
                 title = paste( inputDataLayerName()  ) ) # dynamically changes this value when switching layers
    
  ) 
  
  # build and render data table
  output$summaryTable <- renderDataTable( format.data.frame(dataInput(), big.mark = ",") )
  
}

# execute shinyApp 
shinyApp(ui = ui, server = server)

######## DASHBOARD WIDGET END ##########

# dynamically adjust legend color scale (and polygonFill) when changing date ranges
# figure out how to use leafletProxy to speed up execution
# add additional data layers





# Extra stuff

# other data: 
# https://www.sba.gov/about-sba/sba-performance/open-government/digital-sba/open-data/open-data-sources
# https://www.sba.gov/article/2020/jul/13/sba-treasury-announce-release-paycheck-protection-program-loan-data


# NOTE: Consider reproduing this widget but instead at state level, rather than zip level (note will need to merge all state tables), this would render much more quickly and might be more interesting to a general audience.
