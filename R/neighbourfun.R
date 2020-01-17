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
                         length = NULL,
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

        if (isTRUE(budget) || is.numeric(budget) && length(budget) == 1L) {
            .body <- quote({
                decrease <- which(x > wmin)
                increase  <- which(x < wmax)
                i <- decrease[sample.int(length(decrease), size = 1L)]
                j <- increase[sample.int(length(increase),  size = 1L)]
                stepsize <- .stepsize
                stepsize <- min(x[i] - wmin[i], wmax[j] - x[j], stepsize)
                x[i] <- x[i] - stepsize
                x[j] <- x[j] + stepsize
                x
            })
        } else if (is.numeric(budget) && length(budget) == 2L) {
            .body <- quote({
                i <- sample.int(length(x), size = 1L)
                stepsize <- sample(c(-1, 1), size = 1) * .stepsize
                stepsize <-
                    if (stepsize < 0)
                        max(wmin - x[i], stepsize, budget[1L] - sum(x))
                    else
                        min(wmax - x[i], stepsize, budget[2L] - sum(x))

                x[i] <- x[i] + stepsize
                x
            })
        } else if (isFALSE(budget)) {
            .body <- quote({
                i <- sample.int(length(x), size = 1L)
                stepsize <- sample(c(-1, 1), size = 1) * .stepsize
                stepsize <-
                    if (stepsize < 0) {
                        max(wmin - x[i], stepsize)
                    } else {
                        min(wmax - x[i], stepsize)
                    }
                x[i] <- x[i] + stepsize
                x
            })
        }


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
        } else if (!isFALSE(budget) && length(budget) == 1L) {

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





        ans <- function(x, ...) {}
        body(ans) <- .body
        return(ans)
    }


    if (type == "logical") {

        if (missing(stepsize))
            stepsize <- 1

        if (is.null(kmin) && is.null(kmax)) {

            ## no constraints on number of TRUE values
            if (!is.null(length)) {

                function(x, ...) {
                    i <- sample.int(length, stepsize)
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

        } else if (!is.null(kmin) && !is.null(kmax) && kmin < kmax) {

            function(x, ...) {
                true  <- which( x)
                false <- which(!x)
                n.true <- length(true)
                if (n.true == kmax) {
                    x[true[sample.int(length(true), size = stepsize)]] <- FALSE
                } else if (n.true > kmin) {
                    i <- sample.int(length(x), size = stepsize)
                    x[i] <- !x[i]
                } else {
                    x[false[sample.int(length(false), size = stepsize)]] <- TRUE
                }
                x
            }

        }

    } else if (type == "5/10/40") {
        wmax  <- 0.05
        wmax2 <- 0.1
        max.sumL <- 0.4
        if (is.null(kmax))
            kmax <- 33
        function(w, ...) {
            k <- sum(abs(w) > 0)
            eps <- runif(1)*0.5/100

            to_sell <- w > 0
            to_buy  <- if (k == kmax)
                           w > 0 & w < wmax2
                       else
                           w < wmax2
            to_sell <- which(to_sell)
            to_buy  <- which(to_buy)
            sumL <- sum(w[w > wmax])

            i <- to_sell[sample.int(length(to_sell), size = 1L)]
            j <- to_buy [sample.int(length(to_buy),  size = 1L)]
            eps <- if (w[j] < wmax)
                       min(eps, wmax  - w[j], w[i])
                   else if (w[j] == wmax)
                       min(eps, wmax2 - w[j], w[i], max(0, max.sumL - sumL - w[j]))
                   else
                       min(eps, wmax2 - w[j], w[i], max(0, max.sumL - sumL))
            w[i] <- w[i] - eps
            w[j] <- w[j] + eps
            w
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

random_vector <- function(length,
                          min = 0,
                          max = 1,
                          kmin = NULL,
                          kmax = NULL,
                          sum = TRUE,
                          type = "numeric",
                          n = 1,
                          ...) {
    ans <- NULL
    if (type == "logical") {
        if (missing(length))
            stop(sQuote("length"), " missing")
        if (is.null(kmin))
            kmin <- 0
        if (is.null(kmax))
            kmax <- length
        stopifnot(kmin <= kmax)
        ans <- array(logical(length*n), dim = c(length, n))
        for (j in seq_len(n)) {

            if (kmin == kmax)
                k <- kmin
            else
                k <- sample(seq(from = kmin, to = kmax), size = 1)

            ## ans <- logical(length)
            i <- sample(length, size = k)
            ans[i, j] <- TRUE
        }
        if (n == 1L)
            dim(ans) <- NULL

    } else if (type == "numeric") {
        if (missing(length))
            stop(sQuote("length"), " missing")
        stopifnot(min <= max)

        if (is.null(kmin) && is.null(kmax)) {
            ans <- runif(length*n, min=min, max=max)
            dim(ans) <- c(length, n)
        } else {
            if (is.null(kmin))
                kmin <- 0
            if (is.null(kmax))
                kmax <- length
            ans <- runif(length*n, min=min, max=max)
            dim(ans) <- c(length, n)
            for (j in seq_len(n)) {
                if (kmin == kmax)
                    k <- length - kmin
                else
                    k <- length - sample(seq(from = kmin, to = kmax), size = 1)
                i <- sample(length, size = k)
                ans[i, j] <- 0
            }
        }
    }
    ans
}
