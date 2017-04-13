context("Testing formula interface: numeric 'x'")


test_that("using scalar `from` works", {
  expect_identical(
    recode(1:6, 1 ~ 10, 2 ~ 20),
    c(10, 20, 3, 4, 5 ,6)
  )
  expect_identical(
    recode(1:6, 1 ~ 10, 2 ~ 20, other=NA),
    c(10, 20, NA, NA, NA, NA)
  )
})


test_that("using [] interval works", {
  expect_identical(
    recode(1:6,   2 %[]% 3 ~ 23, 5 %[]% 6 ~ 56),
    c(1, 23, 23, 4, 56, 56)
  )
})







