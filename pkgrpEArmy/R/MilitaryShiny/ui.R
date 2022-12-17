library(shiny)
library(bslib)
library(shinydashboard)
library(shinyBS)
library(vembedr)

shinyapp <- function() {shiny::runApp(system.file("shinyapp", package = "pkgrpEArmy"))}

shinyUI(fluidPage(
  theme = bs_theme(version = 3,bootswatch = "simplex"),
  titlePanel("Performance Scoreboard for Recruitment"),
  sidebarLayout(
    sidebarPanel(title = "Results",
                 sliderInput(inputId = "Jump",
                             label = "Jumping distance in meters:",
                             min = 0,
                             max = 3.00,
                             value = 1.5,step =0.01),
                 sliderInput(inputId = "Ball",
                             label = "Ball Toss distance in meters:",
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
    )
    ,
    mainPanel(
      tabsetPanel(
        tabPanel(h4("Task 1 : Jumping",style = "font-size: 18pt"),
                 p("The goal is to jump as far forward as possible.
Stand behind the take-off line with your feet hip-width apart. Stand on your toes and make sure your feet don't bite into the line.
Bend your knees slightly and lean forward a little. Bring your arms back to lunge. Look straight ahead.
Jump forward as vigorously as possible with parallel feet, swinging your arms in front of you. Land with both feet on the mat. Hold the position (if possible) for a few seconds and step out in front of the mats. You have three tries; the best one counts.",style = "font-size: 12pt"),
                 div(style = "text-align: center",embed_url("https://www.youtube.com/watch?v=RkkfCWhtJkg"))),
        tabPanel(h4("Task 2 : Ball Toss",style = "font-size: 18pt"),
                 p("The goal is to throw the ball as far as possible in front of you with both hands.
                   Sit on the Swedish bench with your buttocks and back against the wall, feet hip-width apart.
                   Place a foam ball between your shoulder blades (it should not move during the throw).
                   Grab the heavy ball held by another candidate and hold it in front of your chest with both hands, elbows apart.
                   Throw the ball explosively (without pumping) by pushing on the legs so that the back stays against the wall.
                   The ball should be thrown at an angle of about 45 degrees. You have three tries; the best one counts.",style = "font-size: 12pt"),
                 div(style = "text-align: center",embed_url("https://www.youtube.com/watch?v=uZfNDP0_1so"))),
        tabPanel(h4("Task 3 : Equilibrium",style = "font-size: 18pt"),
                 p("The objective is to stay still in the hoop on one leg for as long as possible.
Stand in the hoop and wait for the expert's signal (ready?) to balance on your left leg (almost straight),
             place your right foot in the hollow of your left knee and cross your arms behind your back.
             The timer will be started once you are in this position (start!). After 10 seconds,
             close your eyes on the expert's command and, 10 seconds later, still
             on the expert's command, slowly tilt your head back. Keep this position as long as possible.
             The test ends after 60 seconds. Then repeat the exercise on the right leg.",style = "font-size: 12pt"),
                 div(style = "text-align: center",embed_url("https://www.youtube.com/watch?v=KEcJWseeHBs"))),
        tabPanel(h4("Task 4 : Planking",style = "font-size: 18pt"),
                 p("The goal is to hold the plank position for as long as possible while alternately lifting your legs in rhythm.
                   Lift your feet alternately 2 to 5 cm at a rate of once per second. The lower back must remain in constant contact
                   with the bar of the machine for the duration of the test, otherwise the timer will be stopped.",style = "font-size: 12pt"),
                 div(style = "text-align: center",embed_url("https://www.youtube.com/watch?v=hdeM1WRmFkk"))),
        tabPanel(h4("Task 5 : Running",style = "font-size: 18pt"),
                 p("The goal is to keep running as long as possible at the set pace.
Take your place on the back line that serves as the starting line. Wait for the 5th beep,
             a little higher than the previous ones, which marks the beginning of the test,
             to go back and forth between the two back lines as many times as possible. At each beep,
             you must be on the center line or behind the baseline. You must cross the baseline or touch
             it with the tip of your foot for the turn to be valid. The speed increases every 200 m (louder beep).
             You are eliminated as soon as you can no longer keep up the pace or catch up. Leave the race area immediately and
             make sure that the expert has noted your time.",style = "font-size: 12pt"),
                 div(style = "text-align: center",embed_url("https://www.youtube.com/watch?v=JoF2a80NJ6Y"))),
      )
    )
  ),
  fluidPage(
    sidebarLayout(
      sidebarPanel(h4("Strengths and weaknesses",style = "font-size: 18pt"),title = "Strengths and weaknesses",plotOutput("plot", width = "400px"), textOutput("output")),
      mainPanel(column(width = 6,h4("Possible weapon class",style = "font-size: 18pt"),dataTableOutput("dynamic")),
                column(width = 6,div(style = "text-align: center",img(src = "http://www.karate.ch/wp-content/uploads/armee-schweiz_Logo.jpg", width=500,style = "margin-top: 150px"))))
    )
  ))
)
