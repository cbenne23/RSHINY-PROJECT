#setwd("C:\\Users\\ChrisB\\Documents\\PRINCIPALANALYTICSPREP\\DATA_VISUALIZATION\\MyApp")
mydata = read.csv('Housing.csv')
attach(mydata)
library(shiny)

library(plotly)
## app.R ##
library(shinydashboard)
library(rsconnect)

rsconnect::setAccountInfo(name='christophersbennett', token='590F0E20191CF6E08092F4B040ADBB61', secret='QXnwwCV+IaxpGiijyoLe4OVxdZQQJiDEXurgSymX')

# compute a correlation matrix
correlation <- round(cor(mydata), 3)
nms <- names(mydata)

body<- dashboardBody(
  # Boxes need to be put in a row (or column)
  fluidRow( 
    box(plotOutput("distPlot" ),title = "Histogram of Home Price",width = 6,height = 500, solidHeader = TRUE, status = "primary"),  
    
    box( # Application title
      title = "Controls", width = 6, height = 500, solidHeader = TRUE, status = "warning",
      
      # Sidebar with a slider input for number of bins
      
      sliderInput("bins",
                  "Number of bins:",
                  min =10 ,
                  max = 200,
                  value = 100 ),textInput("text", "Text input:")
      
      
      
      
      
    )),
  fluidRow( column(width = 6,box( title = "Correlation Heatmap",width = NULL,height =900, solidHeader = TRUE, status = "primary",
                                  
                                  plotlyOutput("heat"),
                                  plotlyOutput("scatterplot")
                                  ,
                                  verbatimTextOutput("selection")
  )),
  
  
  column(width = 6,box( title = "Plot of Price vs. Latitude vs. Bedrooms", width = NULL,height = 900, solidHeader = TRUE, status = "warning",
                        
                        radioButtons("plotType", "Plot Type:", choices = c("ggplotly", "plotly")),
                        plotlyOutput("plot"),
                        verbatimTextOutput("hover"),
                        verbatimTextOutput("click"),
                        verbatimTextOutput("brush"),
                        verbatimTextOutput("zoom")
                        
                        
                        
  ))))
ui <- dashboardPage(
  dashboardHeader(title = "Descriptive Statistics dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  )),body)

server <- function(input, output) {
  
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    price    <- mydata[, 1]
    
    bins <- seq(min(price), max(price), length.out = input$bins)
    
    # draw the histogram with the specified number of bins
    hist(price, 
         main="", 
         xlab="Price", 
         border="blue", 
         col="green", 
         xlim=c(75000,7700000), 
         las=1, 
         breaks=bins, 
         prob = TRUE)
    
    lines(density(price)) #Get a density curve to go along with your AirPassengers histogram
  })
  
  # Scatter Plot
  output$heat <- renderPlotly({
    plot_ly(x = nms, y = nms, z = correlation, 
            key = correlation, type = "heatmap", source = "heatplot") %>%
      layout(xaxis = list(title = ""), title = "",
             yaxis = list(title = ""))
  })
  
  output$selection <- renderPrint({
    s <- event_data("plotly_click")
    if (length(s) == 0) {
      "Click on a cell in the heatmap to display a scatterplot"
    } else {
      cat("You selected: \n\n")
      as.list(s)
    }
  })
  
  output$scatterplot <- renderPlotly({
    s <- event_data("plotly_click", source = "heatplot")
    if (length(s)) {
      vars <- c(s[["x"]], s[["y"]])
      d <- setNames(mydata[vars], c("x", "y"))
      yhat <- fitted(lm(y ~ x, data = d))
      plot_ly(d, x = ~x) %>%
        add_markers(y = ~y) %>%
        add_lines(y = ~yhat) %>%
        layout(xaxis = list(title = s[["x"]]), 
               yaxis = list(title = s[["y"]]), 
               showlegend = FALSE)
    } else {
      plotly_empty()
    }
  })
  # Normality Test
  output$plot <- renderPlotly({
    # use the key aesthetic/argument to help uniquely identify selected observations
    key <- row.names(mydata)
    if (identical(input$plotType, "ggplotly")) {
      p <- ggplot(mydata, aes(x = lat, y = price, colour = factor(bedrooms), key = key)) + 
        geom_point() +ggtitle("")
      ggplotly(p) %>% layout(dragmode = "select")
    } else {
      plot_ly(mydata, x = ~lat, y = ~price, key = ~key) %>%
        layout(dragmode = "select")
    }
  })
  
  output$hover <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover events appear here (unhover to clear)" else d
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click to clear)" else d
  })
  
  output$brush <- renderPrint({
    d <- event_data("plotly_selected")
    if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else d
  })
  
  output$zoom <- renderPrint({
    d <- event_data("plotly_relayout")
    if (is.null(d)) "Relayout (i.e., zoom) events appear here" else d
  })
  
  
  
}

shinyApp(ui, server)