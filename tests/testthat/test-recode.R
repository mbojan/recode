context("Throwing errors on non-atomic objects")

non_atomic_objects <- list(
  list = list(1:5),
  data.frame = data.frame(x=1:5, y=5:1)
)
for (i in seq(along=non_atomic_objects)) {
  test_that(
    paste("Gives error for non atomic objects:", names(non_atomic_objects)[i]),
    {
      expect_error(recode( non_atomic_objects[[o]], matrix(1:4, 2, 2)))
    }
  )
}



context("Recoding numeric vectors")

test_that("M: recoding with numeric 2x2 rmatrix", {
  x <- rep(1:4, c(10, 5, 3, 2))
  m <- matrix(1:4, ncol=2, byrow=TRUE)
  r <- recode(x, m)
  expect_true( all( r[ x==1 ] == 2 ) )
  expect_true( all(r[ x==3 ] == 4) )
  # all the others are intact
  i <- !(x %in% m[,1])
  expect_true(all(r[i] == x[i]))
} )

test_that("M: supplying matrix with three columns gives error", {
  x <- 1:5
  rmat <- matrix(1:9, 3, 3)
  expect_error( recode(x, rmat) )
} )


test_that("DF: recoding numeric to character works", {
  x <- 1:5
  d <- data.frame(from=2:3, to=c("a", "b"), stringsAsFactors=FALSE)
  r <- recode(x, d)
  expect_type(r, "character")
  expect_equal(r, c("1", "a", "b", "4", "5") )
} )


test_that("DF: numeric to character with a factor works", {
  x <- 1:5
  d <- data.frame(from=2:3, to=c("a", "b"), stringsAsFactors=TRUE)
  r <- recode(x, d)
  expect_equal(r, c("1", "a", "b", "4", "5") )
} )




test_that("RS: with numeric vectors works", {
  expect_equal(
    recode(1:5, 1:2, 1, 3:5, 2),
    c(1,1, 2, 2, 2)
  )

  expect_equal( # 1:5 is integer!
    recode(1:5, 1, 10, 5, 50),
    c(10, 2, 3, 4, 50)
  )
} )

test_that("RS: recoding to character works",{
  expect_equal(
    recode(1:5, 1:2, "a", 3:5, "b"),
    rep(c("a", "b"), c(2, 3))
  )
} )
