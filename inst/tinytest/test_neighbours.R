## -*- truncate-lines: t; -*-

steps <- 10000


## ------ logical: constant k
N <- neighbourfun(type = "logical",
                  kmin = 3,
                  kmax = 3)

x <- logical(10)
x[1:3] <- TRUE

for (i in 1:steps)
    expect_equal(3, sum(N(x)))



## ------ logical: switch 1 element
N <- neighbourfun(type = "logical")
x <- logical(10)
for (i in 1:steps) {
    xn <- N(x)
    expect_equal(sum(xn != x), 1)
    x <- xn
}



## ------ logical: switch 3 elements
N <- neighbourfun(type = "logical", stepsize = 3)
x <- logical(10)
for (i in 1:steps) {
    xn <- N(x)
    expect_equal(sum(xn != x), 3)
    x <- xn
}



## ------ logical: switch 3 elements
N <- neighbourfun(type = "logical", stepsize = 3, n = 10)
x <- logical(10)
for (i in 1:steps) {
    xn <- N(x)
    expect_equal(sum(xn != x), 3)
    x <- xn
}



## ------ numeric: change 1 element
x.min <- -0.05
x.max <-  0.05
N <- neighbourfun(type = "numeric",
                  min = x.min,
                  max = x.max,
                  stepsize = 0.015)
x <- rep(1/25, 25)
for (i in 1:steps) {
    xn <- N(x)
    expect_true(all(xn <= x.max))
    expect_true(all(xn >= x.min))
    x <- xn
}



## ------ numeric: 'active' with min/max vectors
n <- 20
x.min <- rep(-0.1, n)
x.max <- seq(0.1, 0.2, length.out = n)
N <- neighbourfun(type = "numeric",
                  min = x.min,
                  max = x.max,
                  stepsize = 0.1,
                  active = 1:5)
x <- rep(1/n, n)
for (i in 1:steps) {
    xn <- N(x)
    expect_true(all(xn <= x.max + sqrt(.Machine$double.eps)))
    expect_true(all(xn + sqrt(.Machine$double.eps) >= x.min))
    expect_true(all(x[6:20] == xn[6:20]))
    x <- xn
}
