dashboardPage(dashboardHeader(title = "[Client]"),
              
    # Define Sidebar Elements
    dashboardSidebar(
      radioButtons("statistic", "Select Statistic to Compare:",
                   choices = c("Fee to Billed Ratio" = "Fee",
                               "Paid to Billed Ratio" = "Paid",
                               "Total Cost to Billed Ratio" = "Total"),
                   selected = "Fee"),
      selectInput("billtype", "Select Bill Type:",
                  choices = unique(total$bill.type),
                  multiple = FALSE)
    ),
    
    # Main body
    dashboardBody(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
                  # Map Ouput
                  leafletOutput("Map"),
                  
                  # Mouseover Info Box
                  absolutePanel(
                    right = 25, top = 75, width = 250, class = "floater",
                    div(style = "background-color: rgba(230,232,250,0.9); border-radius: 3px; padding: 10px;border-style: solid; border-width:2px; border-color: grey",
                        uiOutput("stateInfo"))
                  ), 
                  
                  # Legend
                  absolutePanel(
                    right = 30, top = 320, style = "", class = "floater",
                    div(style = "background-color: white; border-radius: 3px; padding: 10px;border-style: solid; border-width:2px; border-color: grey",
                      tags$table(
                      mapply(function(vendor, color) {
                        tags$tr(
                          tags$td(tags$div(
                            style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
                          )),
                          tags$td(vendor)
                        )
                      }, vendors, palette, SIMPLIFY=FALSE)
                    ),
                    "Opacity reflects exposure levels.")
                  ),

                  div(style = "text-align: center; background-color: white; border-radius: 3px; border-style: solid; border-width:2px; border-color: grey",
                      h2(textOutput("title")),
                      fluidRow(
                        column(6,
                          radioButtons("chart1", "Displayed Statistic:",
                                       choices = c("Fee" = "Fee", "Paid" = "Paid", "Savings" = "Savings"),
                                       selected = "Fee",
                                       inline = TRUE)
                        ),
                        column(6,
                          radioButtons("chart2", "Displayed Value:",
                                       choices = c("Ratio" = "Ratio", "Dollar Amount" = "Dollar"),
                                       selected = "Ratio",
                                       inline = TRUE)
                        )
                      ),
                      div(showOutput("chart", "highcharts"), br(), "Values displayed in chart are not filtered by bill type.", align = "center", style = "background-color: white"))
    )
)
      