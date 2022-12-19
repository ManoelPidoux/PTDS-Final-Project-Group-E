test_that("Calling points_army will return the homonymus data-set", {
  expect_equal(points_army, points_army)
})

test_that("Calling points_army(9) will return a message of error", {
  expect_error(points_army(9))
})
