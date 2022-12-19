library(readxl)


# read in dataset
points_army <- readxl::read_xlsx("data-raw/points_army.xlsx")

# save dataset as RDA file
save(points_army, file =  "pkgrpEArmy/data/points_army.rda")


# read in dataset
Performances <- readxl::read_xlsx("data-raw/Performance_sportive_et_nb_points.xlsx")

# save dataset as RDA file
save(Performances, file = "pkgrpEArmy/data/Performance_sportive_et_nb_points.rda")


# save dataset as CSV file
write.csv(points_army, "pkgrpEArmy/data/points_army.csv")
write.csv(Performances, "pkgrpEArmy/data/Performance_sportive_et_nb_points.csv")
