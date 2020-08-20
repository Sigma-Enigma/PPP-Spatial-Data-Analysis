require(rgdal)
require(dplyr)
require(RColorBrewer)
require(tigris) # great library that makes DLing shapefiles and doing geo_joins easy!
require(shiny)
require(shinydashboard)
require(leaflet)
require(DT)


# NOTE: Perhaps redo this widget but instead grouped at the state level, rather than zip level (note will need to merge all state tables), this would render much more quickly and might be more interesting to a general audience.


########## DATA PREP BEGIN ###########

# loading and cleaning data test
dat1 <- read.csv2(file = "/Users/ryanarellano/Downloads/All Data by State/California/PPP Data up to 150k - CA.csv", header = TRUE, sep = "," )

levels(dat1$BusinessType)[1] <- "Unanswered"

dat1$DateApproved <- as.Date(dat1$DateApproved, format = "%m/%d/%Y")
dat1$LoanAmount <- as.numeric(dat1$LoanAmount)
dat1$JobsRetained <- as.numeric(dat1$JobsRetained)

#cleaning bad zips when you aggregate data, consider just including them and cutting this cleaning technique (this assumes they were mislabled and missing from the proper state files. If they are not then they may be double counted, check )

dat1 <- subset(dat1, subset = (Zip > 89119 & Zip < 96214) ) # removes bad zip codes out of california
dat1$Zip <- as.factor(dat1$Zip)

########
# need to aggregate data by zip code regions using dplyr
zip.aggregate.dat.1 <- dat1 %>%
  group_by(Zip, DateApproved) %>% # use ", DateApproved" after Zip to include another grouping factor and lower compute time
  summarize(Total_LoanAmount = sum(as.numeric(LoanAmount)),
            Count_Loans = n(), 
            Total_JobsRetained = sum(as.numeric(JobsRetained)),
            # proportion of each business type
  )
########


# shapefiles
zipbounds1 <- readOGR( dsn = "/Users/ryanarellano/Downloads/tl_2019_us_zcta510", layer = "tl_2019_us_zcta510", verbose = TRUE)

# grab unique california zip codes
ca.zl <- unique(dat1$Zip) # gets list of unique zip codes from PPP data

# subsets CA zipcode shapefiles
zb1 <- subset(zipbounds1, (zipbounds1$ZCTA5CE10) %in% ca.zl ) # same as states variable
# cache this

# re-order data,  reason for polygon display bug??????
zip.aggregate.dat.1 <- zip.aggregate.dat.1[order(match(zip.aggregate.dat.1$Zip, zb1$ZCTA5CE10)), ]
# cache this

zip.aggregate.dat.2 <- dat1 %>%
  group_by(Zip) %>% # use ", DateApproved" after Zip to include another grouping factor and lower compute time
  summarize(Total_LoanAmount = sum(as.numeric(LoanAmount)),
            Count_Loans = n(), 
            Total_JobsRetained = sum(as.numeric(JobsRetained)),
            # proportion of each business type
  )
# cache this

# INCLUDE BUTTONS TO SHIFT BETWEEN LAYERS, ALSO REMEMBER TO INCLUDE DIFFERENT COLOR PALATTES FOR EACH LAYER
# This will need to be converted into a function later
pal <- colorBin("RdYlBu", domain = c(min(zip.aggregate.dat.2$Total_LoanAmount), max(zip.aggregate.dat.2$Total_LoanAmount) ), na.color = "#808080") # make domain a variable for each layer indicating the maxed tabulated value for the entire dataset (all times) OR that changes color scale based on size of time frame

######### DATA PREP END ########



######### DASHBOARD WIDGET BEGIN ############

ui <- dashboardPage(
  skin = "red",
  dashboardHeader( title = "PPP Loan Dashboard"),
  dashboardSidebar(
    sliderInput( "DateApproved", label = "Date Range", # figure out what format this slider Input is outputting data (input$DateApproved)
      min = min(dat1$DateApproved),
      max = max(dat1$DateApproved),
      value = c(min(dat1$DateApproved), max(dat1$DateApproved)), # Also consider making the imput data a dynamic variable!!
      sep = ",",
      step = 1, 
      timeFormat = "%F" # fixed slider date format issue
      )
    ),
  dashboardBody(
    fluidRow( box( width = 12, leafletOutput( outputId = "mymap", height = "750px" ) ) ), #adjust height when publishing
    fluidRow( box( width = 12, dataTableOutput( outputId = "summarytable" ) ) )
    )
  
  )


# Define server logic to build map widget  ## problem is below here
server <- function(input, output){
  data_input <- reactive({
    zip.aggregate.dat.1 %>%
      filter( DateApproved >= input$DateApproved[1] ) %>%
      filter( DateApproved <= input$DateApproved[2] ) %>%
      group_by( Zip ) %>%
      summarize(Total_LoanAmount = sum(as.numeric(Total_LoanAmount), na.rm = TRUE),
                Count_Loans = n(), 
                Total_JobsRetained = sum(as.numeric(Total_JobsRetained), na.rm = TRUE),
                # add proportion of each business type
                # add proportion of each industry type NAICS
      )
    
  })
  
  data_input_ordered <- reactive({ 
    data_input()[order(match(data_input()$Zip, zb1$ZCTA5CE10)), ] # 16:06  CHECK THIS FUNCTION LIKELY SOURCE OF BUG
    })
  
  labels <- reactive({
    paste("<p>", "Zip Code Region: ", data_input_ordered()$Zip, "<p>",
          "<p>", "Total Loan Amount: ", format(data_input_ordered()$Total_LoanAmount, nsmall = 0, big.mark = ","), "<p>",
          "<p>", "Total Loan Approvals: ", format(data_input_ordered()$Count_Loans, nsmall = 0, big.mark = ","), "<p>",
          "<p>", "Total Jobs Retained: ", format(data_input_ordered()$Total_JobsRetained, nsmall = 0, big.mark = ","), "<p>",
          sep = "")
    
  })

  output$mymap <- renderLeaflet( # for some reason only the first row in my LSPDF is being plotted
    leaflet(zb1) %>% # may be able to remove zb1 once specific polygons are implemented
      setView(lng = -122.42, lat = 37.77, zoom = 10) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% # color values also not working
      addPolygons( weight = 1,
                   smoothFactor = 0.5,
                   color = "white",
                   fillOpacity = 0.35,
                   fillColor = pal(data_input_ordered()$Total_LoanAmount), 
                   highlightOptions = highlightOptions(
                     weight = 5,
                     color = "#ffffff",
                     dashArray = NULL,
                     fillOpacity = 0.7,
                     bringToFront = TRUE
                     #, dashArray = NULL # problem with only first polygon plotting
                     #https://stackoverflow.com/questions/51275767/only-the-first-polygon-is-showing-up-in-leaflet-2-0-1-choropleth-plot/51275789
                     # https://github.com/rstudio/leaflet/issues/574
                   ),
                   label = lapply(labels(), HTML)) %>%
      addLegend(pal = pal, #will need to be adjusted later with dropdown layers... also not displaying...
                values = data_input_ordered()$Total_LoanAmount,
                opacity = 0.25,
                position = "topright")
    
  ) # build your leaflet inside this function and refer to the reactive data above! 19:04
  
  output$summarytable <- renderDataTable(data_input())
  
}

### ALL POLYGONS PLOTTING BUT OBJECTS ARE MIS-ORDERED!!! Likely issue with shapefile data. FIX AND THEN DONE

# need to add a layer color legend!!!

shinyApp(ui = ui, server = server)

######## DASHBOARD WIDGET END ##########

# figure out how to add leafletProxy to speed up execution



