library(shiny)
library(dplyr)
library(ggplot2)

pdat <- read.csv("data/all_data.csv")
pdat <- pdat[order(pdat$species),]
sp.nam <- unique(pdat$species)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  selectedData <- reactive({
    a <- subset(pdat, pdat$species %in% input$var)
    a <- droplevels(a)
    a <- a[, c(input$xcol, input$ycol)]
    names(a) <- c("x", "y")
    a <- a[complete.cases(a), ]
  })

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

