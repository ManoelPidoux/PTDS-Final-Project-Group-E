test_that("Condition will work properly ", {
  expect_no_error(Orientation(list(5, 10, 20, 19, 25))) #the same example reported in the function
})

test_that("Condition contains a character input and will return an error", {
  expect_error(Orientation(list("a", "b", "c"))) #"n" value has to be numerical
})

test_that("Condition function has no input so won't work", {
  expect_condition(Orientation(list())) #the list cannot be empty
})
