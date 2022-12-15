library(shiny)
library(readxl)
library(fmsb)

Convert <- function(jumping = 0, ball_toss = 0, equilibrium = 0, planking = 0, running = 0){
  if(any(!is.numeric(jumping))){
    stop("'jumping' must be numeric")
  }
  if(any(!is.numeric(ball_toss))){
    stop("'ball_toss' must be numeric")
  }
  if(any(!is.numeric(equilibrium))){
    stop("'equilibrium' must be numeric")
  }
  if(any(!is.numeric(planking))){
    stop("'planking' must be numeric")
  }
  if(any(!is.numeric(running))){
    stop("'running' must be numeric")
  }
  if(any(jumping < 0)){
    stop("'jumping' must be positive")
  }
  if(any(ball_toss<0)){
    stop("'ball_toss' must be positive")
  }
  if(any(equilibrium<0)){
    stop("'equilibrium' must be positive")
  }
  if(any(planking<0)){
    stop("'planking' must be positive")
  }
  if(any(running<0)){
    stop("'running' must be positive")
  }
  points_army <- readxl::read_excel(here::here("Data/points_army.xlsx"))
  list_of_points <- list(jp = 0, btp = 0, ep = 0, pp = 0, rp = 0)
  if(jumping < points_army$jumping[25]){list_of_points$jp = points_army$points[26]}
  if(jumping >= points_army$jumping[1]){list_of_points$jp = points_army$points[1]}
  for(i in (nrow(points_army)-1):2){
    if((jumping < points_army$jumping[i-1])&(jumping >= points_army$jumping[i])){list_of_points$jp = points_army$points[i]}
  }
  if(ball_toss < points_army$ball_toss[25]){list_of_points$btp = points_army$points[26]}
  if(ball_toss >= points_army$ball_toss[1]){list_of_points$btp = points_army$points[1]}
  for(i in (nrow(points_army)-1):2){
    if((ball_toss < points_army$ball_toss[i-1])&(ball_toss >= points_army$ball_toss[i])){list_of_points$btp = points_army$points[i]}
  }
  if(equilibrium < points_army$equilibrium[25]){list_of_points$ep = points_army$points[26]}
  if(equilibrium >= points_army$equilibrium[1]){list_of_points$ep = points_army$points[1]}
  for(i in (nrow(points_army)-1):2){
    if((equilibrium < points_army$equilibrium[i-1])&(equilibrium >= points_army$equilibrium[i])){list_of_points$ep = points_army$points[i]}
  }
  if(planking < points_army$planking[25]){list_of_points$pp = points_army$points[26]}
  if(planking >= points_army$planking[1]){list_of_points$pp = points_army$points[1]}
  for(i in (nrow(points_army)-1):2){
    if((planking < points_army$planking[i-1])&(planking >= points_army$planking[i])){list_of_points$pp = points_army$points[i]}
  }
  if(running < points_army$running[25]){list_of_points$rp = points_army$points[26]}
  if(running >= points_army$running[1]){list_of_points$rp = points_army$points[1]}
  for(i in (nrow(points_army)-1):2){
    if((running < points_army$running[i-1])&(running >= points_army$running[i])){list_of_points$rp = points_army$points[i]}
  }
  data <- as.data.frame(list_of_points)
  colnames(data) <- c("jumping" , "ball toss" , "equilibrium" , "planking" , "running" )
  data <- rbind(rep(25,5) , rep(0,5) , data)
  fmsb::radarchart(data, axistype=1 ,
                   pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 ,
                   cglcol="grey", cglty=1, axislabcol="black", caxislabels=c(0,NA,12.5, NA,25), cglwd=0.8,
                   vlcex=0.8)
  return(invisible(list_of_points))
}

Orientation <- function(results){
  Performances <- readxl::read_excel(here::here("Data/Performance_sportive_et_nb_points.xlsx"))
  sum_of_points <- Reduce('+', results)
  list_of_posibilities <- list()
  list_of_points <- list()
  for(i in 1:nrow(Performances)){
    if(sum_of_points >= Performances[i,2]){list_of_posibilities <- append(list_of_posibilities,Performances[i,1])}
  }
  for(i in 1:nrow(Performances)){
    if(sum_of_points >= Performances[i,2]){list_of_points <- append(list_of_points,Performances[i,2])}
  }
  df <- data.frame("Possibilities"= as.character(list_of_posibilities), "Minimum_points" = as.numeric(list_of_points))
  return(invisible(df))
}



ui <- fluidPage(
  # App title ----
  titlePanel("Performance Scoreboard for Recruitment"),
  sidebarLayout(
    sidebarPanel(title = "Results",
             sliderInput(inputId = "Jump",
                         label = "Jumping Distance in meters:",
                         min = 0,
                         max = 3.00,
                         value = 1.5,step =0.01),
             sliderInput(inputId = "Ball",
                         label = "Ball Toss Distance in meters:",
                         min = 0,
                         max = 10.00,
                         value = 5.00,step =0.01),
             sliderInput(inputId = "Equil",
                         label = "Combined time in seconds for equilibrium:",
                         min = 0,
                         max = 110,
                         value = 50),
             sliderInput(inputId = "Plank",
                         label = "Time in seconds for planking:",
                         min = 0,
                         max = 300,
                         value = 160),
             sliderInput(inputId = "Run",
                         label = "Time in seconds for running:",
                         min = 0,
                         max = 1300,
                         value = 600)
    ) #close sidebar panel
    ,
    mainPanel(
      tabsetPanel(
        tabPanel(h4("Task 1 : Jumping"),
                 p("The goal is to jump as far forward as possible.
Stand behind the take-off line with your feet hip-width apart. Stand on your toes and make sure your feet don't bite into the line.
Bend your knees slightly and lean forward a little. Bring your arms back to lunge. Look straight ahead.
Jump forward as vigorously as possible with parallel feet, swinging your arms in front of you. Land with both feet on the mat. Hold the position (if possible) for a few seconds and step out in front of the mats. You have three tries; the best one counts."),
                 img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcT6ZfnvRwQtllKKhFK8LTTwmCBVAxTP4-TxQ_ruUMQvt9i6X5s7O3SSDK0pgD49ErSgJ7Q&usqp=CAU", width = "250px")),
        tabPanel(h4("Task 2 : Ball Toss"),
                 p("The goal is to throw the ball as far as possible in front of you with both hands.
                   Sit on the Swedish bench with your buttocks and back against the wall, feet hip-width apart.
                   Place a foam ball between your shoulder blades (it should not move during the throw).
                   Grab the heavy ball held by another candidate and hold it in front of your chest with both hands, elbows apart.
                   Throw the ball explosively (without pumping) by pushing on the legs so that the back stays against the wall.
                   The ball should be thrown at an angle of about 45 degrees. You have three tries; the best one counts."),
                 img(src = "https://i.ytimg.com/vi/uZfNDP0_1so/mqdefault.jpg", width = "300px")),
        tabPanel(h4("Task 3 : Equilibrium"),
                 p("The objective is to stay still in the hoop on one leg for as long as possible.
Stand in the hoop and wait for the expert's signal (ready?) to balance on your left leg (almost straight),
             place your right foot in the hollow of your left knee and cross your arms behind your back.
             The timer will be started once you are in this position (start!). After 10 seconds,
             close your eyes on the expert's command and, 10 seconds later, still
             on the expert's command, slowly tilt your head back. Keep this position as long as possible.
             The test ends after 60 seconds. Then repeat the exercise on the right leg."),
                 img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQwLAGU9SyXwkVJCUh67II_SOM-xuKpxAFdaxEbrteb8y9lpCSpJGjmChUH7PO9RABt58E&usqp=CAU", width = "250px")),
        tabPanel(h4("Task 4 : Planking"),
                 p("The goal is to hold the plank position for as long as possible while alternately lifting your legs in rhythm.
                   Lift your feet alternately 2 to 5 cm at a rate of once per second. The lower back must remain in constant contact
                   with the bar of the machine for the duration of the test, otherwise the timer will be stopped."),
                 img(src = "https://i.ytimg.com/vi/hdeM1WRmFkk/mqdefault.jpg", width = "300px")),
        tabPanel(h4("Task 5 : Running"),
                 p("The goal is to keep running as long as possible at the set pace.
Take your place on the back line that serves as the starting line. Wait for the 5th beep,
             a little higher than the previous ones, which marks the beginning of the test,
             to go back and forth between the two back lines as many times as possible. At each beep,
             you must be on the center line or behind the baseline. You must cross the baseline or touch
             it with the tip of your foot for the turn to be valid. The speed increases every 200 m (louder beep).
             You are eliminated as soon as you can no longer keep up the pace or catch up. Leave the race area immediately and
             make sure that the expert has noted your time."),
                 img(src = "https://i.ytimg.com/vi/JoF2a80NJ6Y/maxresdefault.jpg", width = "400px")),
      )#close tabset Panel
    ) #close main Panel
  ), #close sidebar layout
  fluidPage(
    fluidRow(
    column(6,h4("Strengths and weaknesses"), plotOutput("plot", width = "400px")),
    column(6,h4("Possible weapon class"),dataTableOutput("dynamic"))
    )
  )
)# close fluid page

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
  output$plot <- renderPlot({
    Convert(jumping = input$Jump, ball_toss = input$Ball, equilibrium = input$Equil, planking = input$Plank, running = input$Run )
  })
  output$dynamic <- renderDataTable({
    Orientation(Convert(jumping = input$Jump, ball_toss = input$Ball, equilibrium = input$Equil, planking = input$Plank, running = input$Run ))}, options = list(pageLength = 5)
  )
}

shinyApp(ui, server)
