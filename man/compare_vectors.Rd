\name{compare_vectors}
\alias{compare_vectors}
\title{
  Compare Vectors
}
\description{
  Compare numeric or logical vectors.
}
\usage{
compare_vectors(..., sep = "")
}
\arguments{
  \item{\dots}{
    vectors of the same length
  }
  \item{sep}{
    a string
  }
}
\details{

  The function is still exp

}
\value{

  typically a list

}
%% \references{
%% %% ~put references to the literature/web site here ~
%% }
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{neighbourfun}}
}
\examples{
x <- runif(5) > 0.5
nb <- neighbourfun(type = "logical")

compare_vectors(x, nb(x))
## 10011
## 10010
##     ^
## The vectors differ in  1  place.
}
