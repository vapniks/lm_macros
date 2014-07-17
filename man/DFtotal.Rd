\name{DFtotal}
\alias{DFtotal}
\title{Macro to return the degrees of freedom associated with the total sum squares of a linear model.}
\usage{
  DFtotal(lmdl = stop("lmdl not supplied"))
}
\arguments{
  \item{lmdl}{a linear model object}
}
\value{
  The number of data used in the linear model (lmdl) minus
  1
}
\description{
  Macro to return the degrees of freedom associated with
  the total sum squares of a linear model.
}
\details{
  Given a linear model object this macro returns the number
  of data used minus 1.
}
\author{
  Ben Veal
}

