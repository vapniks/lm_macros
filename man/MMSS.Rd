\name{MMSS}
\alias{MMSS}
\title{Macro to calculate the mean model sum squares of a linear model}
\usage{
  MMSS(lmdl = stop("lmdl not supplied"))
}
\arguments{
  \item{lmdl}{a linear model object}
}
\value{
  The mean model sum squares of a linear model
}
\description{
  Macro to calculate the mean model sum squares of a linear
  model
}
\details{
  Given a linear model object this macro return the
  residual sum squares divided by it's associated degrees
  of freedom (i.e. MSS/(k-1) where n = number of data
  points used to estimate the model, and k = number of
  parameters of the model). Note this calculation is only
  correct if the model contains a constant term, otherwise
  you can use MSS(model)/k.
}
\author{
  Ben Veal
}

