pdat <- read.csv("data/all_data.csv")
pdat <- pdat[order(pdat$species),]
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
