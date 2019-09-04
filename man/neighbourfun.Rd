\name{neighbourfun}
\alias{neighbourfun}
\alias{neighborfun}
\alias{neighbours}
\alias{neighbors}
\alias{neighbours-package}
\title{
  Neighbourhood Functions
}
\description{
  Create neighbourhood functions, including constraints.
}
\usage{
neighbourfun(min = 0, max = 1, kmin = NULL, kmax = NULL,
             stepsize, sum = TRUE, random = TRUE, update = FALSE,
             type = "numeric", active = TRUE, n = NULL,
             R = NULL, ...)

neighborfun (min = 0, max = 1, kmin = NULL, kmax = NULL,
             stepsize, sum = TRUE, random = TRUE, update = FALSE,
             type = "numeric", active = TRUE, n = NULL,
             R = NULL, ...)
}
\arguments{
  \item{min}{
    a numeric vector
  }
  \item{max}{
    a numeric vector
  }
  \item{kmin}{
    a numeric vector
  }
  \item{kmax}{
    a numeric vector
  }
  \item{stepsize}{
    numeric
  }
  \item{sum}{
    numeric
  }
  \item{random}{
    logical
  }
  \item{active}{

    a vector: either the positions of elements that may
    be changed, or a logical vector. The default is a
    length-one logical vector, which means that all
    elements may be changed.

  }
  \item{update}{
    logical
  }
  \item{R}{
    a numeric matrix
  }
  \item{type}{
    string: either \code{"numeric"} or \code{"logical"}
  }
  \item{n}{
    integer: the length of a vector
  }
  \item{\dots}{
    other arguments
  }
}
\details{

  The function returns a closure.


}
\value{

  A function (closure).
}
\references{

  Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical
  Methods and Optimization in Finance}. Elsevier.

  Schumann, E. (2018) Financial Optimisation with R (\pkg{NMOF} Manual).
  \url{http://enricoschumann.net/NMOF.htm#NMOFmanual}

}
\author{
  Maintainer: Enrico Schumann <es@enricoschumann.net>
}
\seealso{
  \code{\link[NMOF]{TAopt}}
}
\examples{
## TODO
}
\keyword{ optimize }
