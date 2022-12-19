test_that("By simply calling military app we won't get any kind of error", {
  expect_no_error(militaryapp())
})

test_that("Military app won't work putting an argument", {
  expect_error(militaryapp(2))
})
