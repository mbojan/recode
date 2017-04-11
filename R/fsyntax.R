if(FALSE) {

  f <- function(...) {
    list(...)
  }

  f(   1:5 ~ 1, .(1,2.) ~ 2)

  x <- f( . >= 4 & . <= 5 ~ 1 )
  str(x)

  x <- f( 5 < . < 6 ~ 1)


"%()%" <- function(lhs, rhs) {
  substitute(
    expression(x > lhs & x < rhs),
    list(lhs=lhs, rhs=rhs)
  )
}

"%[)%" <- function(lhs, rhs) {
  substitute(
    quote(x >= lhs & x < rhs),
    list(lhs=lhs, rhs=rhs)
  )
}

"%(]%" <- function(lhs, rhs) {
  substitute(
    quote(x > lhs & x <= rhs),
    list(lhs=lhs, rhs=rhs)
  )
}

"%[]%" <- function(lhs, rhs) {
  substitute(
    x >= lhs & x <= rhs,
    list(rhs=rhs, lhs=lhs)
  )
}

f <- function(x, fromto, ...) {
  dots <- list(...)

}


f(1:10,   2 %[]% 3 ~ 300,     8 %[]% 10 ~ 1000 )

}
