
# https://ncss-tech.github.io/AQP/aqp/sketches.html


library(shiny)
library(aqp)
library(soilDB)


ui <- fluidPage(
  
  tags$head(tags$style('body {font-size: 10px;}')),
  
  # Application title
  titlePanel("Soil Profile Sketches"),
  
  
  sidebarLayout(
    sidebarPanel(width = 10,
      
      fluidRow(
        column(width = 1, HTML("width:")),
        column(width = 2,
          numericInput(inputId = "p.width", label = NULL, value = 0.33, updateOn = 'blur')
        ),
        column(width = 1, HTML("cex.name:")),
        column(width = 2,
          numericInput(inputId = "p.cex.name", label = NULL, value = 0.65, updateOn = 'blur')
        ),
        column(width = 1, HTML("max.depth:")),
        column(width = 2,
               numericInput(inputId = "p.max.depth", label = NULL, value = 155, updateOn = 'blur')
        ),
        column(width = 1, HTML("x.idx.offset:")),
        column(width = 2,
               numericInput(inputId = "p.x.idx.offset", label = NULL, value = 0, updateOn = 'blur')
        )
        
      ),
      fluidRow(
        column(width = 1, HTML("style:")),
        column(width = 2,
               selectInput(inputId = 'p.depth.axis.style', label = NULL, choices = c('traditional', 'compact', 'tape'))
        )
      )
      
      
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      width = 10
    )
  )
)


server <- function(input, output) {
  
  osd <- fetchOSD(c('pierre', 'lucy', 'zook', 'miami', 'leon', 'fresno', 'pardee'))
  
  output$distPlot <- renderPlot({
    
    par(mar = c(0, 0, 0, 2))
    
    plotSPC(
      osd, 
      width = input$p.width, 
      cex.name = input$p.cex.name,
      max.depth = input$p.max.depth,
      x.idx.offset = input$p.x.idx.offset,
      name.style = 'center-center', 
      depth.axis = list(
        style = input$p.depth.axis.style
      )
      )
    
  })
}


shinyApp(ui = ui, server = server)
