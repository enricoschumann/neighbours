neighbourfun <- function(min = 0,
                         max = 1,
                         kmin = NULL,
                         kmax = NULL,
                         stepsize,
                         sum = TRUE,
                         random = TRUE,
                         update = FALSE,
                         type = "numeric",
                         active = TRUE,
                         n = NULL,
                         A = NULL, ...) {

    wmin <- min
    wmax <- max
    budget <- sum

    .sub <- function(e, env) {
        expr <- substitute(substitute(e2, env),
                           env = list(e2 = e))
        eval(expr)
    }

    if (type == "numeric") {

        .body <- quote({
            toSell <- which(x > wmin)
            toBuy  <- which(x < wmax)
            i <- toSell[sample.int(length(toSell), size = 1L)]
            j <- toBuy[ sample.int(length(toBuy),  size = 1L)]
            stepsize <- .stepsize
            stepsize <- min(x[i] - wmin[i], wmax[j] - x[j], stepsize)
            x[i] <- x[i] - stepsize
            x[j] <- x[j] + stepsize
            x
        })



        ## [random]
        .body <- .sub(.body,
                      list(.stepsize =
                               if (random)
                                   quote(stepsize * runif(1))
                               else
                                   stepsize))



        ## [wmin/wmax]
        if (length(wmin) > 1L || length(wmax) > 1L) {

            ## wmin/wmax have length > 1
            if (length(wmin) == 1L)
                wmin <- rep(wmin, length(wmax))
            if (length(wmax) == 1L)
                wmax <- rep(wmax, length(wmin))

            if (!isTRUE(active))
                .body <- .sub(.body, list(wmin = quote(wmin[active]),
                                          wmax = quote(wmax[active])))
        } else {

            ## wmin/wmax length == 1
            .body[[7L]] <- quote(
                stepsize <- min(x[i] - wmin, wmax - x[j], stepsize))
        }



        ## [update]
        if (is.character(update) &&
            update == "Ax") {
            .body[[10L]] <- quote(
                attr(x, "Ax") <- attr(x, "Ax") +
                    A[, c(i, j)] %*% c(-stepsize, stepsize))
            .body[[11L]] <- quote(x)
        }



        ## [active]
        if (!isTRUE(active)) {
            .body <- .sub(.body, list(x = quote(x[active])))
            .body[[length(.body)]] <- quote(x)
        }





        ans <- function(x, ...){}
        body(ans) <- .body
        return(ans)
    }

##         } else if (budget && length(budget) == 2L && !random && !update) {
##             if (length(wmin) > 1L || length(wmax) > 1L) {
##                 if (length(wmin) == 1L)
##                     wmin <- rep(wmin, length(wmax))
##                 if (length(wmax) == 1L)
##                     wmax <- rep(wmax, length(wmin))
##                 function(w, ...) {
##                     i <- sample.int(length(w), size = 1L)
##                     stepsize <- sample(c(-1,1), size = 1)*stepsize
##                     stepsize <- if (stepsize < 0) {
##                                     max(wmin[i]-w[i], stepsize, budget[1]-sum(w))
##                                 } else {
##                                     min(wmax[i]-w[i], stepsize, budget[2]-sum(w))
##                                 }
##                     w[i] <- w[i] + stepsize
##                     w
##                 }
##             } else {
##                 function(w, ...) {
##                     i <- sample.int(length(w), size = 1L)
##                     stepsize <- sample(c(-1,1), size = 1)*stepsize
##                     stepsize <- if (stepsize < 0) {
##                                     max(wmin-w[i], stepsize, budget[1]-sum(w))
##                                 } else {
##                                     min(wmax-w[i], stepsize, budget[2]-sum(w))
##                                 }
##                     w[i] <- w[i] + stepsize
##                     w
##                 }
##             }

##         } else if (budget &&
##                    length(budget) == 1L &&
##                    !is.null(kmax) &&
##                    random && update) {

##             if (is.null(R))
##                 stop(sQuote("R"), " must be provided when ",
##                      sQuote("update"), " is TRUE")
##             if (length(wmin) > 1L || length(wmax) > 1L) {
##                 if (length(wmin) == 1L)
##                     wmin <- rep(wmin, length(wmax))
##                 if (length(wmax) == 1L)
##                     wmax <- rep(wmax, length(wmin))
##             }
##             function(w, ...) {
##                 tol <- 1e-12
##                 x <- w[[1]]
##                 J <- sum(x > tol)
##                 if (J == kmax)
##                     to_buy <- which(x > tol & w < wmax)
##                 else
##                     to_buy <- which(x < wmax)
##                 to_sell <- which(x > tol)
##                 i <- to_sell[sample.int(length(to_sell), size = 1L)]
##                 j <- to_buy[sample.int(length(to_buy), size = 1L)]
##                 eps <- runif(1) * stepsize
##                 eps <- min(x[i], wmax - x[j], eps)
##                 x[i] <- x[i] - eps
##                 x[j] <- x[j] + eps
##                 Rw <- x[[2]] + R[ , c(i, j)] %*% c(-eps, eps)
##                 list(w = x, Rw = Rw)
##             }

##         } else
##             stop("no matches")
## }


    if (type == "logical") {

        if (missing(stepsize))
            stepsize <- 1

        if (is.null(kmin) && is.null(kmax)) {

            ## no constraints on number of TRUE values
            if (!is.null(n)) {

                function(x, ...) {
                    i <- sample.int(n, stepsize)
                    x[i] <- !x[i]
                    x
                }

            } else {

                function(x, ...) {
                    i <- sample.int(length(x), stepsize)
                    x[i] <- !x[i]
                    x
                }

            }


        } else if (!is.null(kmin) && !is.null(kmax) && kmin == kmax) {

            ## logical with constant number of TRUE values

            function(x, ...) {
                true  <- which( x)
                false <- which(!x)
                x[true [sample.int(length( true), size = stepsize)]] <- FALSE
                x[false[sample.int(length(false), size = stepsize)]] <- TRUE
                x
            }

        }
    } else
        stop("no matches")
}

neighborfun <- neighbourfun

compare_vectors <- function(..., sep = "") {
    vecs <- list(...)
    if (length(unique(lengths(vecs))) != 1)
        stop("vectors have different lengths")
    if (mode(vecs[[1]]) == "logical") {
        do.call(
            "cat",
            c(list(as.integer(vecs[[1]]),
                   "\n",
                   as.integer(vecs[[2]]),
                   "\n",
                   ifelse(vecs[[1L]] == vecs[[2L]], " ", "^"),
                   "\n", sep = "")))
        d <- sum(vecs[[1]] != vecs[[2]])
        message("The vectors differ in  ", d, "  place",
                if (d != 1) "s", ".")
        invisible(d)
    }
}

random_vector <- function(min = 0,
                          max = 1,
                          kmin = NULL,
                          kmax = NULL,
                          sum = TRUE,
                          type = "numeric",
                          n = NULL,
                          ...) {

    if (type == "logical") {
        if (!is.null(kmin) && !is.null(kmax)) {
            stopifnot(kmin <= kmax)

            if (kmin == kmax)
                k <- kmin
            else
                k <- sample.int(seq(from = kmin, to = kmax), size = 1)

            ans <- logical(n)
            i <- sample(n, size = k)
            ans[i] <- TRUE

        }
    }
    ans
}
