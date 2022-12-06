library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Performance Scoreboard for Recruitment"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Jumping:",
                  min = 0,
                  max = 2.85,
                  value = 30)

      ,

      sliderInput(inputId = "bins",
                  label = "Ball Toss:",
                  min = 0,
                  max = 8.5,
                  value = 30)

      ,

      sliderInput(inputId = "bins",
                  label = "Equilibrium:",
                  min = 0,
                  max = 100,
                  value = 30)

      ,

      sliderInput(inputId = "bins",
                  label = "Planking:",
                  min = 0,
                  max = 290,
                  value = 30)
      ,

      sliderInput(inputId = "bins",
                  label = "Running:",
                  min = 0,
                  max = 1182,
                  value = 30)

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      img(src = "armee.png", height=70, width=400)

    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({

    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#007bc2", border = "orange",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")

  })

}

shinyApp(ui = ui, server = server)
