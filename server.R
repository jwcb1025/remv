#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  set.seed(122)
  
  updateSelectizeInput(session, "selectZip", choices = zip_shp$zip, server = TRUE)
  updateSelectizeInput(session, "selectState", choices = zip_shp$State, server = TRUE)
  
  
  polygonClicked <- reactive({ # update the location selectInput on map clicks
    ifelse(is.null(input$rankingMap_shape_click), "92507",
           input$rankingMap_shape_click$id)
  })
  
  rowClicked <- reactive({ # update the location selectInput on map clicks
    ifelse(is.null(input$searchResults_rows_selected), 1, input$searchResults_rows_selected)
  })
  
  #clickedMSA <- reactive(input$map_marker_click)
  
  popupGauge <- reactive(
    gauge(round(zip_shp$REMV * 100, digits = 1), min = 0, max = 100, symbol = '%', label = paste("REMV"), gaugeSectors(success = c(70, 100), warning = c(30,69.9), danger = c(0, 29.9), colors = c("#CC6699")))
  )
  binpalZip <- colorBin("RdYlGn", zip_shp$REMV, 6, pretty = FALSE)
  popupContentZip <- paste(sep = " ", "<h3>", as.character(zip_shp$zip), "</h3>", 
                           "<hr>",
                           "<font size= '3'>","<strong>REMV Score: </strong>", paste(round(zip_shp$REMV * 100, digits = 0),"%",sep=""), "</font>",
                           #    renderGauge(gauge(round(msa_shp$REMV * 100, digits = 1), min = 0, max = 100, symbol = '%', label = paste("REMV"), gaugeSectors(success = c(70, 100), warning = c(30,69.9), danger = c(0, 29.9), colors = c("#CC6699")))),                           #   "<span style='border-radius:50%; border:solid black 3px;padding:6px'>Hello</span>",
                           "<hr>",
                           "<strong>Median Sale Price: </strong>", 
                           dollar(zip_shp$hvi), 
                           "<br>",
                           "<strong>Median Mortgage (30yr): </strong>", 
                           dollar(zip_shp$paymentMonthly30), 
                           "<br>",
                           "<strong>Median Rent: </strong>", 
                           dollar(zip_shp$medianRentEst))
  
  zipDataTemp <- reactive({
    subset(zip_shp, State == input$selectState)
  })
  
  output$rankingMap <- renderLeaflet({ withProgress(
    leaflet(zipDataTemp()) %>%
      addTiles() %>%
      addPolygons(color = ~binpalZip(REMV), weight = 1, smoothFactor = 0.9,
                  opacity = 1.0, fillOpacity = 0.4,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = FALSE), 
                  layerId = ~zip, 
                  popup =popupContentZip,
                  popupOptions = popupOptions(closeOnClick = TRUE)), message = "Loading Map") %>%
      setView(lat = 35.41969,
              lng = -90.5247,
              zoom = 5)
  })
  
  searchResultsFiltered <- reactive(
    zip_shp@data %>% 
      filter(zip == input$selectZip)
  )
  
  #SideBar Output
  output$sbarName <- renderText(as.character(searchResultsFiltered()$zip))
  
  output$sbarCashReturn <-  renderText(percent(searchResultsFiltered()$cashReturn))
  output$sbarCashFlow <-  renderText(dollar(searchResultsFiltered()$cashFlow))
  output$sbarSalePrice <- renderText(dollar(searchResultsFiltered()$hvi))
  output$sbarPayment<-  renderText(dollar(searchResultsFiltered()$paymentMonthly30))
  output$sbarRent <-  renderText(dollar(searchResultsFiltered()$medianRentEst))
  output$sbarIncome <-  renderText(dollar(searchResultsFiltered()$medianIncome))
  # output$sbarPop <-  renderText(round(searchResultsFiltered()$totalPopulation, digits = 0))
  ##########
  
  output$searchResults <- renderDataTable(
    datatable(zipDataTemp()@data, selection = 'single', colnames = c("Zip Code", "City", "State", "REMV", "Median Cash-on-Cash Return", "Median Cashflow", 
                                                               "Median Rent", "Median Payment", "Median Price", "Median Income",
                                                               "Total Population", "Lat", "Lon"), rownames = TRUE,
              options = list(columnDefs = list(list(visible=FALSE, targets=c(4,12, 13)))), filter = list(position = "top")) %>%
      formatCurrency(c("cashFlow", "hvi", "paymentMonthly30", "medianRentEst", "medianIncome")) %>%
      formatPercentage("cashReturn", 2) %>%
      formatRound("totalPopulation", 0))
  
  
  searchCoord <- reactive({
    zipDataTemp()@data %>%
      filter(zip == input$selectZip)
  })
  
  tableCoord <- reactive({
    zipDataTemp()@data %>%
      slice(rowClicked()) %>%
      select(Lat, Long)
  })
  
  observeEvent(input$searchResults_rows_selected, {
    leafletProxy("rankingMap", session) %>% clearMarkers() %>%
      setView(lat = tableCoord()$Lat, lng = tableCoord()$Long, zoom = 8) %>%
      addMarkers(lat = tableCoord()$Lat, lng = tableCoord()$Long)
  })
  
  observeEvent(input$selectZip, {
    leafletProxy("rankingMap", session) %>% clearMarkers() %>%
      setView(lat = searchCoord()$Lat, lng = searchCoord()$Long, zoom = 8) %>%
      addMarkers(lat = searchCoord()$Lat, lng = searchCoord()$Long)
  })
  
  meter <- reactive(gauge(round(searchCoord()$REMV * 100, digits = 1), min = 0, max = 100, symbol = '%', label = paste("REMV"), gaugeSectors(
    success = c(70, 100), warning = c(30,69.9), danger = c(0, 29.9), colors = c("#CC6699")
  )))
  
  output$meterREMV <- flexdashboard::renderGauge({
    meter()
  })  
  
  ################CHART##############
  
  
  #  observeEvent(input$gotoChart, {
  #    updateTabsetPanel(session = session, inputId = "mainTab", selected = "Chart" )
  #  })
  
  # To hold the selected map region id.
  
  #Chart
  
  
  zipPlotData <- reactive({
    zipData %>%
      filter(zip == input$selectZip) %>%
      mutate(isBuyHold = ifelse(paymentMonthly30 < medianRent75pct, pmax(maxMedianPrice5yrIntrst), 0)) %>%
      mutate(isBuyFlip = ifelse(paymentMonthly30 > medianRent75pct & paymentMonthly30 < medianAffordablePayment, pmax(maxMedianPrice5yrIntrst), 0)) %>%
      mutate(isSpeculative = ifelse(paymentMonthly30 > medianAffordablePayment, pmax(maxMedianPrice5yrIntrst), 0)) 
  })
  
  x1 <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = TRUE,
    showgrid = FALSE
  )
  
  
  output$zipPlot <- renderPlotly({withProgress(
    subplot(
      plot_ly(zipPlotData()) %>%
        add_trace(x = ~as.Date(date, "%Y-%m-%d"), y = ~isBuyHold, name = "Buy & Hold",  type = 'bar', opacity = 0.17, color = I("green")) %>%
        add_trace(x = ~as.Date(date, "%Y-%m-%d"), y = ~isBuyFlip, name = "Buy & Flip",  type = 'bar', opacity = 0.17, color =  I("yellow")) %>%
        add_trace(x = ~as.Date(date, "%Y-%m-%d"), y = ~isSpeculative, name = "Speculative",  type = 'bar', opacity = 0.25, color =  I("red")) %>%
        add_trace(x = ~as.Date(date, "%Y-%m-%d"), y = ~maxRentPurchasePrice, name = "Maximum Rental Purchase Price",  mode = 'lines',  line = list(color = 'rgb(255,140,0)', width = 2.5)) %>%
        add_trace(x = ~as.Date(date, "%Y-%m-%d"), y = ~maxMedianPrice5yrIntrst, name = "Max Median Price (5yr Interest)", mode = 'lines', line = list(color = 'rgb(0, 102, 0)', width = 2.5)) %>%
        add_trace(x = ~as.Date(date, "%Y-%m-%d"), y = ~hvi, name = "Median Sale Price", type = 'scatter', mode = 'lines',  line = list(color = 'rgb(0, 155, 0)', width = 3)) %>%
        add_trace(x = ~as.Date(date, "%Y-%m-%d"), y = ~medianRentEst, name = "Median Rent",  mode = 'lines', yaxis = "y2", line = list(color = 'rgb(255, 140, 0)', width = 3, dash = 'dot')) %>%
        add_trace(x = ~as.Date(date, "%Y-%m-%d"), y = ~paymentMonthly30, name = "Median Mortgage",  mode = 'lines', yaxis = "y2", line = list(color = 'rgb(0, 200, 0)', width = 3,  dash = 'dot')) %>%
        add_trace(x = ~as.Date(date, "%Y-%m-%d"), y = ~paymentMonthly5yrIntrst, name = "5 Yr Intrst Only Payment",  mode = 'lines', yaxis = "y2", line = list(color = 'rgb(0, 0, 100)', width = 2,  dash = 'dot')) %>%
        add_trace(x = ~as.Date(date, "%Y-%m-%d"), y = ~medianAffordablePayment, name = "Affordable Payment",  mode = 'lines',  yaxis = "y2",  line = list(color = 'rgb(255, 0, 0)', width = 3,  dash = 'dot')) %>%
        layout(
          title =  input$selectZip, 
          yaxis = list( tickfont = list(color = 'rgb(0, 155, 0)'), side = "left",title = ""),
          yaxis2 = list(overlaying = "y", side = "right", tickformat = "$", color = "blue", title = ""),
          xaxis = x1,
          legend = list(orientation = 'h'),
          margin = 400),     
      
      plot_ly(zipPlotData()) %>%
        # add_trace(y = ~RPCD,name = "Maximum Rental Purchase Price",  type = 'bar') %>%
        add_trace(x = ~as.Date(date, "%Y-%m-%d"), y = ~RPCD, name = "Median Cash Flow", type = 'bar') %>%
        layout(margin = 400, xaxis = x1), 
      nrows=2, shareX = TRUE)
    , message = "Loading Plot")
    
  })
  
  
}

