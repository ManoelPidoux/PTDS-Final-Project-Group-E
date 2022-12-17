library(shiny)
library(bslib)
library(shinydashboard)
library(shinyBS)
library(vembedr)
library(pkgrpEArmyDataConv)

shinyServer(function(input, output) {
  output$slider1Value <- renderText({
    input$slider1
  })
  output$slider2Value <- renderText({
    input$slider2
  })
  output$slider3Value <- renderText({
    input$slider3
  })
  output$slider3Value <- renderText({
    input$slider4
  })
  output$slider3Value <- renderText({
    input$slider5
  })
  output$output <- renderText({
    paste("Total points: ",as.character(Reduce('+',Convert(jumping = input$Jump, ball_toss = input$Ball, equilibrium = input$Equil, planking = input$Plank, running = input$Run ))))
  })
  output$plot <- renderPlot({
    Convert(jumping = input$Jump, ball_toss = input$Ball, equilibrium = input$Equil, planking = input$Plank, running = input$Run )
  })
  output$dynamic <- renderDataTable({
    Orientation(Convert(jumping = input$Jump, ball_toss = input$Ball, equilibrium = input$Equil, planking = input$Plank, running = input$Run ))}, options = list(pageLength = 5, lengthMenu = c(5, 10), autoWidth = FALSE,ordering = FALSE)
  )
}
)
