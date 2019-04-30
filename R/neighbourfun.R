neighbourfun <- function(n = NULL,
                         wmin = 0,
                         wmax = 1,
                         kmin = NULL,
                         kmax = NULL,
                         stepsize,
                         budget = TRUE,
                         random = TRUE,
                         update = FALSE,
                         R = NULL, ...) {

    if (!is.null(n) && length(wmin) == 1L)
        wmin <- rep(wmin, n)

    if (!is.null(n) && length(wmax) == 1L)
        wmax <- rep(wmax, n)
    
    if (!is.null(kmin))
        stop("no such neighbourhood yet")
    
    if (!isTRUE(budget) && length(budget) == 2L && !random && !update) {
        ## budget is a _range_ , i.e. a numeric vector of length two

        stop("not implemented")

    } else if (isTRUE(budget) && random && update) {

        stop("not implemented")

        ## if (length(wmin) > 1L || length(wmax) > 1L) {
        ##     if (length(wmin) == 1L)
        ##         wmin <- rep(wmin, length(wmax))
        ##     if (length(wmax) == 1L)
        ##         wmax <- rep(wmax, length(wmin))
        ##     function(w, ...) {
        ##         toSell <- which(w > wmin)
        ##         toBuy  <- which(w < wmax)
        ##         i <- toSell[sample.int(length(toSell), size = 1L)]
        ##         j <- toBuy[ sample.int(length(toBuy),  size = 1L)]
        ##         stepsize <- runif(1) * stepsize
        ##         stepsize <- min(w[i] - wmin[i], wmax[j] - w[j], stepsize)
        ##         w[i] <- w[i] - stepsize
        ##         w[j] <- w[j] + stepsize
        ##         w
        ##     }
        ## } else {
        ##     function(w, ...) {
        ##         toSell <- which(w > wmin)
        ##         toBuy  <- which(w < wmax)
        ##         i <- toSell[sample.int(length(toSell), size = 1L)]
        ##         j <- toBuy[ sample.int(length(toBuy),  size = 1L)]
        ##         stepsize <- runif(1) * stepsize
        ##         stepsize <- min(w[i] - wmin, wmax - w[j], stepsize)
        ##         w[i] <- w[i] - stepsize
        ##         w[j] <- w[j] + stepsize
        ##         w
        ##     }
        ## }

    } else if (isTRUE(budget) && random && !update) {
        if (length(wmin) > 1L || length(wmax) > 1L) {
            if (length(wmin) == 1L)
                wmin <- rep(wmin, length(wmax))
            if (length(wmax) == 1L)
                wmax <- rep(wmax, length(wmin))
            function(w, ...) {
                toSell <- which(w > wmin)
                toBuy  <- which(w < wmax)
                i <- toSell[sample.int(length(toSell), size = 1L)]
                j <- toBuy[ sample.int(length(toBuy),  size = 1L)]
                stepsize <- runif(1) * stepsize
                stepsize <- min(w[i] - wmin[i], wmax[j] - w[j], stepsize)
                w[i] <- w[i] - stepsize
                w[j] <- w[j] + stepsize
                w
            }
        } else {
            function(w, ...) {
                toSell <- which(w > wmin)
                toBuy  <- which(w < wmax)
                i <- toSell[sample.int(length(toSell), size = 1L)]
                j <- toBuy[ sample.int(length(toBuy),  size = 1L)]
                stepsize <- runif(1) * stepsize
                stepsize <- min(w[i] - wmin, wmax - w[j], stepsize)
                w[i] <- w[i] - stepsize
                w[j] <- w[j] + stepsize
                w
            }
        }
    } else if (budget && length(budget) == 1L && !random && !update) {
        if (length(wmin) > 1L || length(wmax) > 1L) {
            if (length(wmin) == 1L)
                wmin <- rep(wmin, length(wmax))
            if (length(wmax) == 1L)
                wmax <- rep(wmax, length(wmin))
            function(w, ...) {
                toSell <- which(w > wmin)
                toBuy  <- which(w < wmax)
                i <- toSell[sample.int(length(toSell), size = 1L)]
                j <- toBuy[ sample.int(length(toBuy),  size = 1L)]
                stepsize <- min(w[i] - wmin[i], wmax[j] - w[j], stepsize)
                w[i] <- w[i] - stepsize
                w[j] <- w[j] + stepsize
                w
            }
        } else {
            function(w, ...) {
                toSell <- which(w > wmin)
                toBuy  <- which(w < wmax)
                i <- toSell[sample.int(length(toSell), size = 1L)]
                j <- toBuy[ sample.int(length(toBuy),  size = 1L)]
                stepsize <- min(w[i] - wmin, wmax - w[j], stepsize)
                w[i] <- w[i] - stepsize
                w[j] <- w[j] + stepsize
                w
            }
        }
    } else if (budget && length(budget) == 2L && !random && !update) {
        if (length(wmin) > 1L || length(wmax) > 1L) {
            if (length(wmin) == 1L)
                wmin <- rep(wmin, length(wmax))
            if (length(wmax) == 1L)
                wmax <- rep(wmax, length(wmin))
            function(w, ...) {
                i <- sample.int(length(w), size = 1L)
                stepsize <- sample(c(-1,1), size = 1)*stepsize
                stepsize <- if (stepsize < 0) {
                    max(wmin[i]-w[i], stepsize, budget[1]-sum(w))
                } else {
                    min(wmax[i]-w[i], stepsize, budget[2]-sum(w))
                }
                w[i] <- w[i] + stepsize
                w
            }
        } else {
            function(w, ...) {
                i <- sample.int(length(w), size = 1L)
                stepsize <- sample(c(-1,1), size = 1)*stepsize
                stepsize <- if (stepsize < 0) {
                    max(wmin-w[i], stepsize, budget[1]-sum(w))
                } else {
                    min(wmax-w[i], stepsize, budget[2]-sum(w))
                }
                w[i] <- w[i] + stepsize
                w
            }
        }
    } else if (budget &&
               length(budget) == 1L &&
               !is.null(kmax) &&
               random
               && update) {

        if (is.null(R))
            stop(sQuote("R"), " must be provided when ",
                 sQuote("update"), " is TRUE")
        if (length(wmin) > 1L || length(wmax) > 1L) {
            if (length(wmin) == 1L)
                wmin <- rep(wmin, length(wmax))
            if (length(wmax) == 1L)
                wmax <- rep(wmax, length(wmin))
        }
        function(w, ...) {
            tol <- 1e-12
            x <- w[[1]]
            J <- sum(x > tol)
            if (J == kmax) 
                to_buy <- which(x > tol & w < wmax)
            else
                to_buy <- which(x < wmax)
            to_sell <- which(x > tol)
            i <- to_sell[sample.int(length(to_sell), size = 1L)]
            j <- to_buy[sample.int(length(to_buy), size = 1L)]
            eps <- runif(1) * stepsize
            eps <- min(x[i], wmax - x[j], eps)
            x[i] <- x[i] - eps
            x[j] <- x[j] + eps
            Rw <- x[[2]] + R[ , c(i, j)] %*% c(-eps, eps)
            list(w = x, Rw = Rw)
        }
        
    } else
        stop("no matches")
}


compare_logicals <- function(x, y, ...) {
    argsL <- list(...)
    if (!("sep" %in% names(argsL))) 
        argsL$sep <- ""
    do.call("cat",
            c(list("\n", as.integer(x), "\n", as.integer(y), "\n",
                   ifelse(x == y, " ", "^"), "\n"), argsL))
    message("The vectors differ in ", sum(x != y), " place(s).")
}
