library(shiny)
library(markdown)
library(dplyr)
library(ggplot2)

pdat <- read.csv("data/all_data.csv")
pdat <- pdat[order(pdat$species),]
sp.nam <- unique(pdat$species)

pdat2 <- pdat %>%  group_by(species) %>%  filter(n() > 10) # Only include species with >10 records

# Dataframe for summary data
pdat3 <- pdat %>%  group_by(species) %>% summarize(
  Count = length(species),
  Male = length(sex[sex == "M"]),
  Female = length(sex[sex == "F"]),
  Weight_m = round(mean(weight, na.rm=TRUE),1),
  Weight_SD = round(sd(weight, na.rm=TRUE),1),
  Wing_m = round(mean(wing, na.rm=TRUE),1),
  Wing_SD = round(sd(wing, na.rm=TRUE),1),
  Max_t_m = round(mean(max_t, na.rm=TRUE),1),
  Max_t_SD = round(sd(max_t, na.rm=TRUE),1),
  Min_t_m = round(mean(min_t, na.rm=TRUE),1),
  Min_t_SD = round(sd(min_t, na.rm=TRUE),1)
)

names(pdat3)[1] <- "Species"

ui <- navbarPage("Passerine biometrics",
                 tabPanel("Plots",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Subsetted to only include species with more than 10 records."),
                              selectInput('var', 'Species', unique(pdat2$species),
                                          selected = unique(pdat2$species)[[1]]),
                              selectInput('xcol', 'X variable', names(pdat2)[c(8:11)], 
                                          selected = names(pdat2)[10]),
                              selectInput('ycol', 'Y variable', names(pdat2)[c(8:11)],
                                          selected = names(pdat2)[11]),
                              hr(),
                              helpText("Wing, weight and tarsus measurements for passerine species collected as part of an ongoing study.")
                            ),
                            mainPanel(
                              plotOutput('plot1'),
                              textOutput('count'),
                              textOutput("correlation")
                            )
                          )
                 ),
                 tabPanel("Summary data",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("show_vars", label = "Columns to show:",
                                                 choices = names(pdat3), selected = names(pdat3)),
                              hr(),
                              helpText("m = mean; SD = standard deviation.")
                            ),
                            mainPanel(
                              DT::dataTableOutput("summary")
                            )
                          )
                 ),
                 tabPanel("Raw data",
                          DT::dataTableOutput("raw")
                 )
)


server <- function(input, output, session) {
  
  selectedData <- reactive({
    a <- subset(pdat2, pdat2$species %in% input$var)
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
  
  
  output$summary <- DT::renderDataTable({
    DT::datatable(pdat3[, input$show_vars, drop = FALSE])
  })
  
  output$raw <- DT::renderDataTable({
    DT::datatable(pdat)
  })
}

shinyApp(ui, server)