library(shiny)
library(dplyr)
library(ggplot2)

pdat <- read.csv("../all_data.csv")
sp.nam <- unique(pdat$species)

# Define UI for app that draws a histogram ----
ui <- pageWithSidebar(
  headerPanel('Passerine biometrics'),
  sidebarPanel(
    selectInput('var', 'Species', unique(pdat$species),
                selected = unique(pdat$species)[[1]]),
    selectInput('xcol', 'X variable', names(pdat)[c(11,12,14,15)], 
                selected = names(pdat)[14]),
    selectInput('ycol', 'Y variable', names(pdat)[c(11,12,14,15)],
                selected = names(pdat)[15]),
    hr(),
    helpText("Wing, weight and tarsus measurements for passerine species collected as part of an ongoing study.")
  ),
  mainPanel(
    plotOutput('plot1'),
    textOutput('count'),
    textOutput("correlation")
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  selectedData <- reactive({
    a <- subset(pdat, pdat$species %in% input$var)
    a <- droplevels(a)
    a <- a[, c(input$xcol, input$ycol)]
    names(a) <- c("x", "y")
    a <- a[complete.cases(a), ]
  })
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    ggplot(selectedData(), aes(x=x, y=y)) + 
      stat_smooth(method=lm, fullrange=FALSE) + 
      geom_point()
  })
  output$count <- renderText({
    paste("Count (n) = ", count(selectedData()))
  })
  output$correlation <- renderText({
    paste("Correlation (R) = ", round(cor(selectedData()$x,selectedData()$y),2))
  })
}


shinyApp(ui = ui, server = server)
