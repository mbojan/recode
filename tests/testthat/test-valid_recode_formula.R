context("Testing valid_recode_formula")


test_that("using numeric vectors for numeric `x` is ok", {
  expect_true(
    valid_recode_formula(2:3 ~ 2, 1:5)
  )
})
