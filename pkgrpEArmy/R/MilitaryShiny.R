#' @title Shiny App
#' @author Group-E
#' @import shiny
#' @import bslib
#' @import shinydashboard
#' @import shinyBS
#' @import vembedr
#' @import pkgrpEArmyDataConv
#' @description This will recall the Shiny-App created by Group-E regarding         Performance Scoreboard for Recruitment
#' @return The shiny app of Group-E
#' @export

shinyapp <- function() {
  shiny::runApp(system.file("MilitaryShiny", package = "pkgrpEArmy"))
}


