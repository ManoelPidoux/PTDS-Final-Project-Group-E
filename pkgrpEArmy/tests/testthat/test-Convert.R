test_that("Convert contains a single wrong input that will generate an error", {
  expect_error( Convert(jumping = -2, ball_toss = 5, equilibrium = 30, planking = 50, running = 300)) #the numerical variable has to be positive
})

test_that("Convert contains all the correct input", {
  expect_no_warning( Convert(jumping = 1.7, ball_toss = 9, equilibrium = 50, planking = 50, running = 500)) #the numerical variable has to be positive
})

test_that("Convert present a wrong parameter and return an error", {
  expect_condition( Convert(jumping = 1.6 ,boll_toss = 5, equilibrium = 30, planking = 50, running = 300)) #the numerical variable has to be positive
})
