\name{DFmodel}
\alias{DFmodel}
\title{Macro to return the degrees of freedom associated with the model sum squares of a linear model.}
\usage{
  DFmodel(lmdl = stop("lmdl not supplied"))
}
\arguments{
  \item{lmdl}{a linear model object}
}
\value{
  The number of coefficients of the linear model (lmdl)
  minus 1
}
\description{
  Macro to return the degrees of freedom associated with
  the model sum squares of a linear model.
}
\details{
  Given a linear model object this macro returns the number
  of coefficients minus 1.
}
\author{
  Ben Veal
}

