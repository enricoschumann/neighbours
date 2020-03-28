\name{random_vector}
\alias{random_vector}
\title{
  Create Random Vector
}
\description{
  Create one (or more) numeric or logical random vector.
}
\usage{
random_vector(length, min = 0, max = 1, kmin = NULL, kmax = NULL,
              sum = NULL, type = "numeric", n = 1L, ...)
}
\arguments{
  \item{length}{%
    integer
  }
  \item{min}{%
    numeric: either a single number of a vector of length \code{length}
  }
  \item{max}{%
    numeric: either a single number of a vector of length \code{length}
  }
  \item{kmin}{%
    numeric
  }
  \item{kmax}{%
    numeric
  }
  \item{sum}{%
    numeric. \bold{Not yet supported.}
  }
  \item{type}{
    a string: \code{"numeric"} or \code{"logical"}
  }
  \item{n}{
    integer: how many vectors to create
}
  \item{\dots}{
    other arguments
}
}
\details{

  The function creates random vectors, typically as initial solutions
  for optimization and search algorithms

}
\value{

  a vector (logical or numeric), possibly of with
  \code{\link{dim}} attribute (i.e. a matrix)

}
\references{

  Gilli, M. and Schumann, E. (2010) A note on 'good starting values'
  in numerical optimisation, \acronym{COMISEF} Working Paper Series No. 044.
  \url{https://papers.ssrn.com/sol3/papers.cfm?abstract_id=1620083}

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{compare_vectors}}
}
\examples{
random_vector(type = "logical", length = 5)
## [1] FALSE  TRUE  TRUE  TRUE  TRUE

random_vector(type = "logical", length = 5, n = 3, kmin = 4)
##       [,1] [,2]  [,3]
## [1,]  TRUE TRUE  TRUE
## [2,]  TRUE TRUE  TRUE
## [3,]  TRUE TRUE FALSE
## [4,] FALSE TRUE  TRUE
## [5,]  TRUE TRUE  TRUE

x <- random_vector(type = "numeric", length = 3, sum = 2, n = 4)
##           [,1]      [,2]      [,3]      [,4]
## [1,] 0.2063878 0.6055332 0.1314114 0.2272488
## [2,] 0.1741989 0.8975089 0.1630611 0.5474513
## [3,] 0.9440980 0.8658824 0.7872635 0.9836578
}
