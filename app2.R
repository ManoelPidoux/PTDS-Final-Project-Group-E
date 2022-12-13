library(shiny)

ui <- fluidPage(
  # App title ----
  titlePanel("Performance Scoreboard for Recruitment"),



  # Slider 1
  fluidRow(
    column(4,
           img(src = "armee.png", height=70, width=400)
    ),
    column(3,
           sliderInput(inputId = "bins",
                       label = "Jumping:",
                       min = 0,
                       max = 2.85,
                       value = 30)
    ),
    column(4,
           img(src = "armee.png", height=70, width=400)
    ),
  ),
  # Slider 2
  fluidRow(
    column(4,
           img(src = "armee.png", height=70, width=400)
    ),
    column(3,
           sliderInput(inputId = "bins",
                       label = "Ball Toss:",
                       min = 0,
                       max = 8.5,
                       value = 30)
    ),
    column(4,
           img(src = "armee.png", height=70, width=400)
    ),
  ),
  # Slider 3
  fluidRow(
    column(4,
           img(src = "armee.png", height=70, width=400)
    ),
    column(3,
           sliderInput(inputId = "bins",
                       label = "Equilibrium:",
                       min = 0,
                       max = 100,
                       value = 30)
    ),
    column(4,
           img(src = "armee.png", height=70, width=400)
    ),
  ),
  # Slider 4
  fluidRow(
    column(4,
           img(src = "armee.png", height=70, width=400)
    ),
    column(3,
           sliderInput(inputId = "bins",
                       label = "Planking:",
                       min = 0,
                       max = 290,
                       value = 30)
    ),
    column(4,
           img(src = "armee.png", height=70, width=400)
    ),
  ),
  # Slider 5
  fluidRow(
    column(4,
           img(src = "armee.png", height=70, width=400)
    ),
    column(3,
           sliderInput(inputId = "bins",
                       label = "Planking:",
                       min = 0,
                       max = 290,
                       value = 30)
    ),
    column(4,
           img(src = "armee.png", height=70, width=400)
    )
  )
)


server <- function(input, output) {
  # Output the values of the sliders
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
}

shinyApp(ui, server)
