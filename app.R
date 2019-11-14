library(shiny)
library(markdown)
library(dplyr)
library(lubridate)
library(ggplot2)

pdat <- read.csv("data/all_data.csv")
pdat <- pdat[order(pdat$species),]
pdat$visit_date <- as.Date(pdat$visit_date, format="%d/%m/%Y")
sp.nam <- unique(pdat$species)

pdat2 <- pdat %>%  group_by(species) %>%  filter(n() > 16) # Only include species with >16 records (power analysis - 0.95 power, 0.01 significance, r = 0.8)
pdat2$visit_date <- as.Date(pdat2$visit_date, format="%d/%m/%Y")

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
                 tabPanel("Correlation plot",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Subsetted to only include species with more than 16 records."),
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
                 tabPanel("Resampling",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Variance in tarsus measurements"),
                              selectInput('sp', 'Species', unique(pdat2$species),
                                          selected = unique(pdat2$species)[[1]]),
                              actionButton("update", "Update"),
                              sliderInput(inputId = 'max_grp',
                                          label = "Maximum group size:",
                                          min = 5, max = 250,value = 16),
                              sliderInput(inputId = 'reps',
                                          label = "Number of iterations:",
                                          min = 1, max = 100, value = 10),
                              hr(),
                              helpText("Mean & SD R^2 for a given species, based on minimum and maximum tarsus measurements. Data are randomly resampled-with-replacement, across X individuals ('group size') and Y iterations. Data are cleaned prior to processing - outliers are identified according to Tukey's 1.5*IQR threshold applied to the interaction between minimum and maximum tarsus measurements.")
                              ),
                          mainPanel(
                              plotOutput('plot2')
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
  
  selectedData2 <- reactive({
    a <- subset(pdat2, pdat2$species %in% input$sp)
    a <- droplevels(a)
    a$temp <- a$min_t*a$max_t
    })

  pTResample <- function(df, mi = 1, ma = input$max_grp){
    input$update
    outlierKD <- function(dt, var) {
      var_name <- eval(substitute(var),eval(dt))
      na1 <- sum(is.na(var_name))
      m1 <- mean(var_name, na.rm = T)
      outlier <- boxplot.stats(var_name)$out
      mo <- mean(outlier)
      var_name <- ifelse(var_name %in% outlier, NA, var_name)
      na2 <- sum(is.na(var_name))
      m2 <- mean(var_name, na.rm = T)
      dt[as.character(substitute(var))] <- invisible(var_name)
      assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
      dt <- dt[complete.cases(dt),] # data frame minus outliers
      return(invisible(dt))
    }
    b <- outlierKD(a, temp)
    b$temp <- NULL
    grpResample <- function(f, v1, v2){
      d <- data.frame(groupsize = c(mi:ma), s = NA)
      for(i in nrow(d)){
        for(i in mi:ma){
          a <- f[sample(x = 1:nrow(f), size = i, replace = T), ]
          d$s[i] <- cor(a$min_t,a$max_t)^2
        }
        return(d)
      }
    }
    r <- replicate(input$reps, grpResample(b))
    d <- data.frame(t((matrix(unlist(r), nrow=length(r), byrow=T))))
    d <- d[,seq(2,ncol(d),2)]
    w <- d %>% mutate(avg = apply(.,1,mean),
                      sd = apply(.,1,sd))
    w$groupsize <- c(mi:ma)
    w[is.na(w)] <- 0
    return(w)
  }
  
  selectedData3 <- reactive({
    tmp <- pTResample(selectedData2())
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    ggplot(selectedData(), aes(x=x, y=y)) + 
      stat_smooth(method=lm, fullrange=FALSE) + 
      geom_point()
  })
  
  
  output$plot2 <- renderPlot({
    par(mar = c(5.1, 4.1, 3, 3))
    ggplot(selectedData3(), aes(x = groupsize, y = avg)) +
      geom_point(size = 2) +
      geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd)) + 
      geom_smooth()
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
