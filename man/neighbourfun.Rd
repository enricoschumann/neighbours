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
neighbourfun(wmin = 0, wmax = 1, kmin = NULL, kmax = NULL,
             stepsize, budget = TRUE, random = TRUE, update = FALSE, R = NULL, ...)
neighborfun (wmin = 0, wmax = 1, kmin = NULL, kmax = NULL,
             stepsize, budget = TRUE, random = TRUE, update = FALSE, R = NULL, ...)

}
\arguments{
  \item{wmin}{
    a numeric vector
  }
  \item{wmax}{
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
  \item{budget}{
    numeric
}
  \item{random}{
    logical
}
  \item{update}{
    logical
}
  \item{R}{
    a numeric matrix
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
  \url{https://www.elsevier.com/books/numerical-methods-and-optimization-in-finance/gilli/978-0-12-375662-6}

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
