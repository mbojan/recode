# Input vector
x <- c(1,2,3,4,3,2,1)

# Recoding with rules as a matrix
rmat <- matrix( c(1, 10, 4, 40), 2, 2, byrow=TRUE)
recode(x, rmat)

# Recoding with a data frame
d <- data.frame(from = c(2, 3),
                to = c("two", "three") )
recode(x, d)


# Recoding with rule sets
r <- recode(x, c(1,2), "one or two",
       c(3,4), "three or four",
       5, "five")
data.frame(x, r)
