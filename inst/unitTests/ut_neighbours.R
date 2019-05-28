## -*- truncate-lines: t; -*-

test.logical_const_k <- function() {

    N <- neighbourfun(type = "logical",
                      kmin = 3,
                      kmax = 3)

    x <- logical(10)
    x[1:3] <- TRUE

    for (i in 1:100)
        checkEquals(3, sum(N(x)))
}


test.logical_switch1 <- function() {

    N <- neighbourfun(type = "logical")
    x <- logical(10)
    for (i in 1:100) {
        xn <- N(x)
        checkEquals(sum(xn != x), 1)
        x <- xn
    }
}

test.logical_switch3 <- function() {

    N <- neighbourfun(type = "logical", stepsize = 3)
    x <- logical(10)
    for (i in 1:100) {
        xn <- N(x)
        checkEquals(sum(xn != x), 3)
        x <- xn
    }
}

test.logical_switch3_n <- function() {

    N <- neighbourfun(type = "logical", stepsize = 3, n = 10)
    x <- logical(10)
    for (i in 1:100) {
        xn <- N(x)
        checkEquals(sum(xn != x), 3)
        x <- xn
    }
}
