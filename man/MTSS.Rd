\name{MTSS}
\alias{MTSS}
\title{Macro to calculate the mean total sum squares of a linear model}
\usage{
  MTSS(lmdl = stop("lmdl not supplied"))
}
\arguments{
  \item{lmdl}{a linear model object}
}
\value{
  The mean total sum squares of a linear model
}
\description{
  Macro to calculate the mean total sum squares of a linear
  model
}
\details{
  Given a linear model object this macro return the total
  sum squares divided by it's associated degrees of freedom
  (i.e. TSS/(n-1) where n = number of data points used to
  estimate the model)
}
\author{
  Ben Veal
}

