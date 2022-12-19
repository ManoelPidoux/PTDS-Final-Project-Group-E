test_that("Calling Performances will return the homonymus data-set", {
  expect_equal(Performances, Performances)
 })

test_that("Calling Performances('A') will return a message of error", {
  expect_error(Performances('A'))
})
