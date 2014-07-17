\name{MRSS}
\alias{MRSS}
\title{Macro to calculate the mean residual sum squares of a linear model}
\usage{
  MRSS(lmdl = stop("lmdl not supplied"))
}
\arguments{
  \item{lmdl}{a linear model object}
}
\value{
  The mean residual sum squares of a linear model
}
\description{
  Macro to calculate the mean residual sum squares of a
  linear model
}
\details{
  Given a linear model object this macro return the
  residual sum squares divided by it's associated degrees
  of freedom (i.e. RSS/(n-k) where n = number of data
  points used to estimate the model, and k = number of
  parameters of the model)
}
\author{
  Ben Veal
}

