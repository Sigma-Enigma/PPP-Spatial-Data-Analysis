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

# changing blank businessType label to unanswered (perhaps use NA?)
levels(dat1$BusinessType)[1] <- "Unanswered"

# fixing column data format
dat1$DateApproved <- as.Date(dat1$DateApproved, format = "%m/%d/%Y")
dat1$LoanAmount <- as.numeric(dat1$LoanAmount)
dat1$JobsRetained <- as.numeric(dat1$JobsRetained)



# cleaning bad zips when you aggregate data
dat1 <- subset(dat1, subset = (Zip > 89119 & Zip < 96214) ) # removes bad zip codes out of california
dat1$Zip <- as.factor(dat1$Zip)
# NOTE: consider just including them and cutting this cleaning technique (this assumes they were mislabled and missing from the proper state files. If they are not then they may be double counted, check )

########
# aggregates data by zip code regions using dplyr grouped by Zip and DateApproved
zip.aggregate.dat.1 <- dat1 %>%
  group_by(Zip, DateApproved) %>% # use ", DateApproved" after Zip to include another grouping factor and lower compute time
  summarize(Total_LoanAmount = sum(as.numeric(LoanAmount)),
            Count_Loans = n(), 
            Total_JobsRetained = sum(as.numeric(JobsRetained)),
            # proportion of each business type
            # more layers here
  )
########


# load shapefiles
zipbounds1 <- readOGR( dsn = "/Users/ryanarellano/Downloads/tl_2019_us_zcta510", layer = "tl_2019_us_zcta510", verbose = TRUE)

# grab unique california zip codes
ca.zl <- unique(dat1$Zip) # gets list of unique zip codes from PPP data

# subsets CA zipcode shapefiles to figure out which in zip.aggregate.dat.1 are NOT in zb1
zb1 <- subset(zipbounds1, (zipbounds1$ZCTA5CE10) %in% ca.zl ) # same as states variable #needed to do an inner join
# cache this

# subset zip and date grouped tabular zip data to figure out which in zip.aggregate.dat.1 are NOT in zb1
zip.aggregate.dat.1 <- subset(zip.aggregate.dat.1, zip.aggregate.dat.1$Zip %in% zb1$ZCTA5CE10 )
# cache this

# subset zip grouped to figure out which in dat1 are NOT in zb1
dat1 <- subset(dat1, dat1$Zip %in% zb1$ZCTA5CE10 )
# cache this

# aggregates data by zip code regions using dplyr grouped by Zip only this time
zip.aggregate.dat.2 <- dat1 %>%
  group_by(Zip) %>%
  summarize(Total_LoanAmount = sum(as.numeric(LoanAmount)),
            Count_Loans = n(), 
            Total_JobsRetained = sum(as.numeric(JobsRetained)),
            # proportion of each business type
            # more layers here
  )
# cache this

# NOTE: INCLUDE BUTTONS TO SHIFT BETWEEN LAYERS, ALSO REMEMBER TO INCLUDE DIFFERENT COLOR PALATTES FOR EACH LAYER
# NOTE: This will need to be converted into a function later
pal <- colorBin("RdYlBu", reverse = TRUE, domain = c(min(zip.aggregate.dat.2$Total_LoanAmount), max(zip.aggregate.dat.2$Total_LoanAmount) ), na.color = "#808080") 
# NOTE: make domain a variable for each layer indicating the maxed tabulated value for the entire dataset (all times) OR that changes color scale based on size of time frame

######### DATA PREP END ########



######### DASHBOARD WIDGET BEGIN ############

# define UI for dashboard
ui <- dashboardPage(
  skin = "red",
  dashboardHeader( title = "PPP Loan Dashboard"),
  dashboardSidebar(
    sliderInput( "DateApproved", label = "Date Range",
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


# Define server input/output logic to pass map data and build map widget
server <- function(input, output){
  data_input <- reactive({
    # use dplyr to aggregate data and create tables to pass to polygons 
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
  
  # re-order data for label values so they line up nicely with zipboundary polygons table order
  data_input_ordered <- reactive({ 
    data_input()[order(match(data_input()$Zip, zb1$ZCTA5CE10)), ] 
    })
  
  # set label variables to pass data 
  labels <- reactive({
    paste("<p>", "Zip Code Region: ", data_input_ordered()$Zip, "<p>",
          "<p>", "Total Loan Amount: ", format(data_input_ordered()$Total_LoanAmount, nsmall = 0, big.mark = ","), "<p>",
          "<p>", "Total Loan Approvals: ", format(data_input_ordered()$Count_Loans, nsmall = 0, big.mark = ","), "<p>",
          "<p>", "Total Jobs Retained: ", format(data_input_ordered()$Total_JobsRetained, nsmall = 0, big.mark = ","), "<p>",
          sep = "")
    
  })

  # build leaflet map
  output$mymap <- renderLeaflet( 
    leaflet(zb1) %>% 
      setView(lng = -122.42, lat = 37.77, zoom = 11) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons( weight = 1,
                   smoothFactor = 0.5,
                   color = "white",
                   fillOpacity = 0.25,
                   fillColor = pal(data_input_ordered()$Total_LoanAmount), 
                   highlightOptions = highlightOptions(
                     weight = 5,
                     color = "#ffffff",
                     dashArray = NULL,
                     fillOpacity = 0.5,
                     bringToFront = TRUE
                   ),
                   label = lapply(labels(), HTML)) %>%
      addLegend(pal = pal, # will need to be adjusted later with dropdown layers
                values = ~data_input_ordered()$Total_LoanAmount, # dynamically change this when changing dates too!
                opacity = 0.25,
                position = "topright",
                title = "Total Loan Amount") # dynamically change this value when adding more layers
    
  ) 
  
  # render map
  output$summarytable <- renderDataTable(data_input())
  
}

# execute shinyApp 
shinyApp(ui = ui, server = server)

######## DASHBOARD WIDGET END ##########

# figure out how to use leafletProxy to speed up execution
# add additional data layers
