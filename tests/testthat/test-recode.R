context("General")

test_that("Gives error for non atomic objects",
          {
            expect_error(recode( list(1:5), matrix(1:4, 2, 2)))
          } )




context("Recoding numeric vectors")

test_that("Recoding with numeric 2x2 rmatrix works", {
          x <- rep(1:4, c(10, 5, 3, 2))
          m <- matrix(1:4, ncol=2, byrow=TRUE)
          r <- recode(x, m)
          expect_true( all( r[ x==1 ] == 2 ) )
          expect_true( all(r[ x==3 ] == 4) )
          # all the others are intact
          i <- !(x %in% m[,1])
          expect_true(all(r[i] == x[i]))
} )

test_that("Supplying matrix with three columns gives error",
          {
            x <- 1:5
            rmat <- matrix(1:9, 3, 3)
            expect_error( recode(x, rmat) )
          } )





context("Recoding with data frames")

test_that("Recoding numeric vector to character works",
          {
            x <- 1:5
            d <- data.frame(from=2:3, to=c("a", "b"), stringsAsFactors=FALSE)
            r <- recode(x, d)
            expect_equal(r, c("1", "a", "b", "4", "5") )
          } )


test_that("Supplying a data frame with a factor works",
          {
            x <- 1:5
            d <- data.frame(from=2:3, to=c("a", "b"), stringsAsFactors=TRUE)
            r <- recode(x, d)
            expect_equal(r, c("1", "a", "b", "4", "5") )
          } )




context("Recoding with rule set")


test_that("Rule set with only numeric vectors",
          {
            expect_equal(recode(1:5, 1:2, 1, 3:5, 2),
                         c(1,1, 2, 2, 2) )
          } )

test_that("Rule set recoding to character",
          {
            expect_equal(recode(1:5, 1:2, "a", 3:5, "b"),
                         rep(c("a", "b"), c(2, 3)))
          } )
