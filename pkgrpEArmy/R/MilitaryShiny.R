#' @title Shiny App
#' @author Group-E
#' @import shiny
#' @import bslib
#' @import shinydashboard
#' @import shinyBS
#' @import vembedr
#' @description This will recall the Shiny-App created by Group-E regarding         Performance Scoreboard for Recruitment
#' @param
#' @return
#' @examples
#'
#' @export

shinyapp <- function() {
  shiny::runApp(system.file("MilitaryShiny", package = "pkgrpEArmy"))
}


