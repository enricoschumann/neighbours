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
              sum = TRUE, type = "numeric", n = 1L, ...)
}
\arguments{
  \item{length}{
    numeric
  }
  \item{min}{
    numeric
  }
  \item{max}{
    numeric
  }
  \item{kmin}{
    numeric
  }
  \item{kmax}{
    numeric
  }
  \item{sum}{
    numeric
  }
  \item{type}{
    a string: \code{"numeric"} or \code{"logical"}
  }
  \item{n}{
    numeric
}
  \item{\dots}{
    other arguments
}
}
\details{

  Highly experimental.

}
\value{

  a vector (logical or numeric), possibly of with
  \code{\link{dim}} attribute (i.e. a matrix)

}
%% \references{
%%   %% ~put references to the literature/web site here ~
%% }
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{compare_vectors}}
}
\examples{
1+1
}
