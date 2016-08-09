#' Recoding variables (vectors)
#'
#' Transform the input vector into a new vector by replacing old values with
#' new values according to the specified rules.
#'
#' This is a generic function that dispatches methods based on the class
#' of the \code{fromto} argument, which contains the recoding rules.
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
  stopifnot( all(sapply(from, class) == class(x)) )
  # 'to' are of the same class
  stopifnot( length(unique(sapply(to, class))) == 1 )
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




