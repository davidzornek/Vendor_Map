####A. Front Matter####

####A.1 Packages####


#### Begin ShinyServer ####

shinyServer(function(input, output) {
  
###############################################################################################################
## Map
###############################################################################################################

#### Filter map data based on user input ####  
  mapdata <- reactive({
    if(is.null(input$billtype) || is.null(input$statistic)){
      return()}
    
    if(input$statistic == "Fee"){
      fee %>% 
        filter(bill.type == input$billtype)
    } else if(input$statistic == "Paid"){
      cost %>% 
        filter(bill.type == input$billtype)
    } else if(input$statistic == "Total"){
      total %>% 
        filter(bill.type == input$billtype)
    }
    
  })
  
  #Create a vector of colors and opacities used to draw state polygons
  
  colors <- reactive({
    temp <- mapdata()[c("region","color","Exposure")]
    
    #A for loop is used for simplicity. Because data will always be small, very little efficiency is lost.
    
    for(i in extra.regions){
      temp <- rbind(temp,c(i,mapdata()[which(mapdata()["region"]==strsplit(i,":")[[1]][1]),"color"]))
    }
    
    return(temp)
    
  })
  
  # Draw the given states, with or without highlighting
  
  drawStates <- function(x,stateNames, highlight = FALSE) {
    states <- map("state", stateNames, plot=FALSE, fill=TRUE)
    
    fill.colors <- left_join(as.data.frame(states$names),colors(),by=c("states$names" = "region"))
    
    #addPolygons() is a leaflet function that can work with the output of map() to draw shapes for each state 
    addPolygons(x, lat = I(states$y), lng = I(states$x),layerId = I(states$names),
                fill = TRUE, fillOpacity = fill.colors$Exposure, fillColor = fill.colors$color,
                stroke = TRUE, color = "white", opacity = 1, weight = ifelse(highlight, 4, 1))
  }
  
  
  
  # input$map_shape_mouseover gets updated a lot, even if the id doesn't change.
  # We don't want to update the polygons and stateInfo except when the id
  # changes, so use values$highlight to insulate the downstream reactives (as 
  # writing to values$highlight doesn't trigger reactivity unless the new value 
  # is different than the previous value).
  
  values <- reactiveValues(highlight = c())
  
  observe({
    values$highlight <- input$Map_shape_mouseover$id
  })
  
  # Dynamically render the mouseover info box in the upper-right
  output$stateInfo <- renderUI({
    if (is.null(values$highlight)) {
      return(tags$div("Hover over a state"))
    } else {
      # Get a properly formatted state name
      stateName <- getStateName(values$highlight)
      return(tags$div(
        tags$strong(simpleCap(stateName)),
        tags$div(paste0("Exposure: $",
                        prettyNum(
                          round(
                            filter(exposure, region == stateName, bill.type == input$billtype)[3],
                            digits = 0),
                          big.mark = ",",big.interval = 3)), br(),
                 em(paste0("(",
                        filter(exposure, region == stateName, bill.type == input$billtype)[4],
                        " of countrywide exposure)"
                        )),
                 br(), br(),
                 strong(paste0("Top 4 vendors in ",simpleCap(stateName))), br(),
                 em(paste0("(Lowest ",input$statistic," to Billed Ratio)")), br(),
                 paste0("1. ",filter(mapdata(), region == stateName)[8]), br(),
                 paste0("2. ",filter(mapdata(), region == stateName)[9]), br(),
                 paste0("3. ",filter(mapdata(), region == stateName)[10]), br(),
                 paste0("4. ",filter(mapdata(), region == stateName)[11]), br())
      ))
    }
  })
  
  #Controls highlighting and unhighlighting of states as the mouse moves
  
  lastHighlighted <- c()
  
  observe({
    
    #leafletProxy is a leaflet function that allows portions of a leaflet object to be updated via a proxy
    #without having to process the entire map again, which slows things down greatly.
    proxy <- leafletProxy("Map")
    
    if (length(lastHighlighted) > 0)
      drawStates(proxy,lastHighlighted, FALSE)
    lastHighlighted <<- values$highlight
    
    if (is.null(values$highlight))
      return()
    
    isolate({
      drawStates(proxy,values$highlight, TRUE)
    })
  })
  
  #Control response to mouse click
  clicked <- reactiveValues(state = c())
  
  observe({
    clicked$state <- input$Map_shape_click$id
  })
  
  #render the Map and draw state polygons
  output$Map <- renderLeaflet({
    
    m <- leaflet() %>%
      setView(lng = -98.85, lat = 40, zoom = 4) %>% 
      addTiles()
    
    drawStates(m,unique(mapdata()$region))
    
  })
  
  ###############################################################################################################
  ## Column Chart
  ###############################################################################################################
  
  #Create a data object to be used for the column chart, based on user inputs and clicked$state
  
  ChartData <- reactive({
    if(is.null(clicked$state))
      return(data.frame(matrix("",nrow = 3, ncol = 5)) %>% 
               setNames(append(vendors,"Ratio"))
             )
    if(input$chart1 == "Fee"){
      temp <- fee %>% 
        filter(region == getStateName(clicked$state), bill.type == "Overall") %>% 
        select(3:6) %>% 
        mutate_each(funs(round(.,4)*100))
    } else if(input$chart1 == "Paid"){
      temp <- cost %>% 
        filter(region == getStateName(clicked$state), bill.type == "Overall") %>% 
        select(3:6) %>% 
        mutate_each(funs(round(.,4)*100))
    } else if(input$chart1 == "Savings"){
      temp <- cost %>% 
        filter(region == getStateName(clicked$state), bill.type == "Overall") %>% 
        select(3:6) %>% 
        mutate_each(funs((1 - round(.,4))*100))
    }
    
    if(input$chart2 == "Dollar"){
      billed <- as.numeric(filter(exposure, region == getStateName(clicked$state), bill.type == "Overall")[1,3])
      
      temp %<>%
        mutate_each(funs(round(as.numeric(.)*billed/100,digits = 0)))
    }
      temp %<>%
        mutate(Ratio = input$chart1)
    
  })
  
  #render the column chart to compare vendors within a state
  output$chart <- renderChart2({
    
    a <- rCharts:::Highcharts$new()
    a$chart(type = "column")
    a$colors(palette)
    a$xAxis(categories = list(input$chart1))
    a$yAxis(title = list(text = ifelse(input$chart2 == "Ratio", "Ratio to Billed (%)", "Dollar Amount ($)")))
    a$data(ChartData()[1:4])
    
    a
    
  })
  
  #redner a Text string so that the chart title corresponds to the clicked state.
  output$title <- renderText({
    if(is.null(clicked$state))
      return("Click map view state details")
    
    paste0(simpleCap(getStateName(clicked$state)), " Vendor Comparison Chart")
  })
  

  
  

  


})