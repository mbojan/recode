#' Recoding variables (vectors)
#'
#' Transform the input vector into a new vector by replacing old values with
#' new values according to the specified rules.
#'
#' This is a generic function that dispatches methods based on the class
#' of the \code{fromto} argument, which expects the recoding rule(s).
#'
#' @param x atomic vector, variable to be recoded
#' @param fromto two-column matrix or data frame, or a list
#' @param ... Only used in the default method and expects further elements of
#' the recoding rule set
#'
#' @return Vector of the same mode as second column of \code{fromto} and length
#' as \code{x} with the values recoded.
#'
#' @seealso \code{\link{match}}
#'
#' @export
#' @example man-roxygen/recode.R


recode <- function(x, fromto, ...)
{
  stopifnot(is.atomic(x))
  UseMethod("recode", fromto)
}




#' @method recode default
#'
#' @details
#' If \code{fromto} is a vector it should have an even number of elements. It is
#' interpreted such that values \code{x = fromto[i]} is recoded into a value
#' \code{fromto[i+1]}, \code{x = fromto[i+2]} into \code{fromto[i+3]} and so on.
#'
#' @export
#' @rdname recode
recode.default <- function(x, fromto, ...)
{
  if(is.list(fromto))
  {
    rlist <- fromto
  } else
  {
    rlist <- c(list(fromto), list(...))
  }
  # Sanity checks
  stopifnot( all(sapply(rlist, is.atomic)) )
  from <- rlist[ seq(1, length(rlist), by=2) ]
  to <- rlist[ seq(2, length(rlist), by=2) ]
  stopifnot(length(from) == length(to))
  # 'from' of the same class as 'x'
  stopifnot( all(sapply(from, data.class) == data.class(x)) )
  # 'to' are of the same class
  stopifnot( length(unique(sapply(to, data.class))) == 1 )
  lens <- sapply(from, length)
  d <- data.frame( from=unlist(from),
                   to=rep(unlist(to), lens),
                   stringsAsFactors=FALSE)
  if(any(duplicated(d)))
    warning("duplicated rules")
  recode.data.frame(x=x, fromto=d[!duplicated(d) , ])
}






#' @method recode data.frame
#'
#' @details
#' If \code{fromto} is a data frame, then it is expected to have two columns.
#' The mode of the result is determined by the mode of the second column of
#' \code{fromto}. For each 'i', values of \code{x} equal to \code{fromto[i,1]}
#' are replaced with \code{fromto[i,2]}.
#'
#' @export
#' @rdname recode
recode.data.frame <- function(x, fromto, ...)
{
  stopifnot( mode(fromto[,1]) == mode(x) )
  stopifnot( ncol(fromto) == 2 )
  # convert factors to character
  isfac <- sapply(fromto, is.factor)
  if(any(isfac))
  {
    fromto[isfac] <- as.data.frame(lapply(fromto[isfac], as.character),
                                   stringsAsFactors=FALSE)
  }
  i <- match(x, fromto[,1])
  notna <- which(!is.na(i))
  i <- i[notna]
  rval <- x
  rval[notna] <- fromto[,2][i]
  rval
}





#' @method recode matrix
#'
#' @details
#' If \code{fromto} is a matrix, it is converted to a data frame and an
#' appropriate method is used.
#'
#' @export
#' @rdname recode
recode.matrix <- function(x, fromto, ...)
{
  fromto <- as.data.frame(fromto, stringsAsFactors=FALSE)
  recode.data.frame(x, fromto=fromto)
}










#' @rdname recode
#' @method recode formula
#'
#' @param other value insert for other unrecoded values
#'
#' @export
recode.formula <- function(x, fromto, ..., other=NULL) {
  if(!is.null(other)) {
    stopifnot(is.atomic(other))
    stopifnot(length(other) == 1)
  }
  rules <- c(list(fromto), list(...))
  # Check if rules are valid
  chk <- lapply(rules, valid_recode_formula, x=x)
  ok <- vapply(chk, isTRUE, logical(1))
  if(any(!ok))
    stop("recode formula errors:\n", paste( unlist(chk[!ok]), collapse="\n"))

  # Intervals
  "%[]%" <- function(lhs, rhs) {
    substitute(
      x >= lhs & x <= rhs,
      list(rhs=rhs, lhs=lhs)
    )
  }
  "%(]%" <- function(lhs, rhs) {
    substitute(
      x > lhs & x <= rhs,
      list(rhs=rhs, lhs=lhs)
    )
  }
  "%[)%" <- function(lhs, rhs) {
    substitute(
      x >= lhs & x < rhs,
      list(rhs=rhs, lhs=lhs)
    )
  }
  "%()%" <- function(lhs, rhs) {
    substitute(
      x > lhs & x < rhs,
      list(rhs=rhs, lhs=lhs)
    )
  }


  if(is.null(other)) {
    rval <- x
  } else {
    rval <- rep(other, length(x))
  }
  for(r in rules) {
    i <- switch(
      data.class(r[[2]]),
      call = eval(eval(r[[2]])),
      x %in% r[[2]]
    )
    rval[i] <- r[[3]]
  }
  rval
}








is_formula <- function(x) inherits(x, "formula")

# check if recode formula is valid
# @param cls character class of vector to be recoded
# @param f formula
# returns TRUE or vector of character error messages
valid_recode_formula <- function(f, x) {
  cls <- data.class(x)
  arg <- deparse(f)
  msg <- function(m, a=arg)
    paste0("in ", sQuote(a), ": ", m)
  rval <- NULL
  # `f` shoud be a formula
  if(!is_formula(f))
    rval <- c(rval, msg("it is not a formula"))
  lhs <- f[[2]]
  rhs <- eval(f[[3]])
  # `rhs` should be atomic of length 1
  if(!is.atomic(rhs))
    rval <- c(rval, msg("rhs is not atomic"))
  if(length(rhs) != 1)
    rval <- c(rval, msg("rhs is not of length 1"))
  # TODO `lhs` should be a call or vector
  # TODO if `lhs` is a vector it should be of the same class as `x`
  # TODO lhs can be a call only if cls="numeric"
  # Finalize
  if(is.null(rval)) {
    return(TRUE)
  } else {
    return(rval)
  }
}


if(FALSE) {
  m <- 2 %[]% 3 ~ 300
  m[[2]]
  f(1:10,   2 %[]% 3 ~ 300, 5 ~ 500, 7:8 ~ 80)
  f(letters[1:10], c("a", "b") ~ 1)
}
