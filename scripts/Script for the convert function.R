#### Sript for spider chart
library(readxl)
library(fmsb)


points_army <- read_excel(here::here("Data/points_army.xlsx"))

#################### training to spider chart
# Create data: note in High school for Jonathan:
data <- as.data.frame(matrix( sample( 2:25 , 5 , replace=T) , ncol=5))
colnames(data) <- c("jumping" , "ball toss" , "equilibrium" , "planking" , "running" )

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(25,5) , rep(0,5) , data)

# Check your data, it has to look like this!
# head(data)

# The default radar chart
radarchart(data)

##############################


#creation of a function that return the good amount of points

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


list_of_results <- Convert(2,6,54,145,1000)



Orientation <- function(results){
  Performances <- readxl::read_excel(here::here("Data/Performance_sportive_et_nb_points.xlsx"))
  sum_of_points <- Reduce('+', results)
  list_of_posibilities <- list()
  for(i in 1:nrow(Performances)){
    if(sum_of_points >= Performances[i,2]){list_of_posibilities <- append(list_of_posibilities,Performances[i,1])}
  }
  return(invisible(as.character(list_of_posibilities)))
}

x <- Orientation(list_of_results)


