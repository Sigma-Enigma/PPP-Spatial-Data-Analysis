require(rgdal)
require(dplyr)
require(RColorBrewer)
require(tigris) # great library that makes DLing shapefiles and doing geo_joins easy!
require(shiny)
require(shinydashboard)
require(leaflet)
require(DT)


# NOTE: Redo this widget but instead grouped at the state level, rather than zip level (note will need to merge all state tables)


# loading and cleaning data test
dat1 <- read.csv2(file = "/Users/ryanarellano/Downloads/All Data by State/California/PPP Data up to 150k - CA.csv", header = TRUE, sep = "," )

levels(dat1$BusinessType)[1] <- "Unanswered"

dat1$DateApproved <- as.Date(dat1$DateApproved, format = "%m/%d/%Y")
dat1$LoanAmount <- as.numeric(dat1$LoanAmount)
dat1$JobsRetained <- as.numeric(dat1$JobsRetained)

#cleaning bad zips when you aggregate data, consider just including them and cutting this cleaning technique (this assumes they were mislabled and missing from the proper state files. If they are not then they may be double counted, check )

dat1 <- subset(dat1, subset = (Zip > 89119 & Zip < 96214) ) # removes bad zip codes out of california


########
# need to aggregate data by zip code regions using dplyr
zip.aggregate.dat.1 <- dat1 %>%
  group_by(Zip) %>% # use ", DateApproved" after Zip to include another grouping factor and lower compute time
  summarize(Total_LoanAmount = sum(as.numeric(LoanAmount)),
            Count_Loans = n(), 
            Total_JobsRetained = sum(as.numeric(JobsRetained)),
            # proportion of each business type
  )
zip.aggregate.dat.1$Zip <- as.factor(zip.aggregate.dat.1$Zip)
########


# shapefiles
zipbounds1 <- readOGR( dsn = "/Users/ryanarellano/Downloads/tl_2019_us_zcta510", layer = "tl_2019_us_zcta510")

# grab unique california zip codes
ca.zl <- unique(dat1$Zip) # gets list of unique zip codes from PPP data

# subsets CA zipcode shapefiles
zb1 <- subset(zipbounds1, (zipbounds1$ZCTA5CE10) %in% ca.zl ) # same as states variable

zip.aggregate.dat.1 <- zip.aggregate.dat.1[order(match(zip.aggregate.dat.1$Zip, zb1$ZCTA5CE10)), ]

# may include bin function here to make a set range, be sure that the max is set to a reasonable value given the time interval (eg daily vs total)
pal <- colorBin("RdYlBu", domain = zip.aggregate.dat.1$Total_LoanAmount)



######### DASHBOARD WIDGET BEGIN ############
ui <- dashboardPage(
  skin = "red",
  dashboardHeader( title = "PPP Loan Dashboard"),
  dashboardSidebar(
    sliderInput( "DateApproved", label = "Date Range",
      min = min(zip.aggregate.dat.1$DateApproved),
      max = max(zip.aggregate.dat.1$DateApproved),
      value = c(min(zip.aggregate.dat.1$DateApproved), max(zip.aggregate.dat.1$DateApproved)),
      sep = "",
      step = 1
      )
    ),
  dashboardBody(
    fluidRow( box( width = 12, leafletOutput( outputId = "mymap" ) ) ),
    fluidRow( box( width = 12, dataTableOutput( outputId = "summarytable" ) ) )
    )
  
  )


# Define server logic to build map widget
server <- function(input, output){
  data_input <- reactive({
    zip.aggregate.dat.1 %>%
      filter( DateApproved >= input$DateApproved[1] ) %>%
      filter( DateApproved <= input$DateApproved[2] ) %>%
      group_by( Zip ) %>%
      summarize(Total_LoanAmount = sum(as.numeric(LoanAmount)),
                Count_Loans = n(), 
                Total_JobsRetained = sum(as.numeric(JobsRetained)),
                # proportion of each business type
      )
    zip.aggregate.dat.1$Zip <- as.factor(zip.aggregate.dat.1$Zip) # try removing this if it breaks it
  })
  
  data_input_ordered <- reactive({ 
    data_input()[order(match(data_input()$Zip, zipbounds1$ZCTA5CE10)), ] # 16:06
    })
  
  labels <- reactive({
    paste("<p>", data_input_ordered()$Zip, "<p>",
          "<p>", "Total Loan Amount: ", data_input_ordered()$Total_LoanAmount, "<p>",
          "<p>", "Loan Approval Counts: ", data_input_ordered()$Count_Loans, "<p>",
          "<p>", "Loan Approval Counts: ", data_input_ordered()$Total_JobsRetained, "<p>",
          sep = "")
    
  })

  output$mymap <- renderLeaflet(
    leaflet() %>%
      setView(lng = 119.4, lat = 36.7, ) %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolygons( data = zb1,
                   weight = 1,
                   smoothFactor = 0.5,
                   color = "white",
                   fillOpacity = 0.8,
                   fillColor = pal(data_input_ordered()$Total_LoanAmount),
                   highlightOptions = highlightOptions(
                     weight = 5,
                     color = "#666666",
                     dashArray = "",
                     fillOpacity = 0.7,
                     bringToFront = TRUE
                   ),
                   label = lapply(labels(), HTML)) %>%
      addLegend(pal = pal,
                values = data_input_ordered()$Total_LoanAmount,
                opacity = 0.7,
                position = "topright")
    
  ) # build your leaflet inside this function and refer to the reactive data above! 19:04
  
  output$summarytable <- renderDataTable(data_input())
  
}

shinyApp(ui = ui, server = server)






