.packageName <- "lmmacros"

## Some macros for finding sum squares of linear models

##' @include lm_macros.R
##' @title Macro to calculate the residual sum squares of a linear model
##' @param lmdl a linear model object
##' @return The residual sum squares of a linear model
##' @author Ben Veal
##' @export
RSS <- defmacro(lmdl,expr={sum((lmdl$residuals)^2)})

##' @title Macro to calculate the total sum squares of a linear model
##' @param lmdl a linear model object
##' @return The total sum squares of a linear model
##' @author Ben Veal
##' @export
TSS <- defmacro(lmdl,expr={sum((lmdl$model[1]-mean(lmdl$model[1]))^2)})

##' @title Macro to calculate the model sum squares of a linear model
##' @param lmdl a linear model object
##' @return The model sum squares of a linear model
##' @author Ben Veal
##' @export
MSS <- defmacro(lmdl,expr={sum((lmdl$fitted.values-mean(lmdl$fitted.values))^2)})

##' @title Macro to return the degrees of freedom associated with the residuals of a linear model.
##' @param lmdl a linear model object
##' @return The degrees of freedom of the residuals of the linear model lmdl.
##' @author Ben Veal
##' @export
DFresid <- defmacro(lmdl,expr={lmdl$df.residual})

##' @title Macro to return the degrees of freedom associated with the model sum squares of a linear model.
##' @details Given a linear model object this macro returns the number of coefficients minus 1.
##' @param lmdl a linear model object
##' @return The number of coefficients of the linear model (lmdl) minus 1
##' @author Ben Veal
##' @export
DFmodel <- defmacro(lmdl,expr={length(names(lmdl$coefficients))-1})

##' @title Macro to return the degrees of freedom associated with the total sum squares of a linear model.
##' @details Given a linear model object this macro returns the number of data used minus 1.
##' @param lmdl a linear model object
##' @return The number of data used in the linear model (lmdl) minus 1
##' @author Ben Veal
##' @export
DFtotal <- defmacro(lmdl,expr={dim(lmdl$model)[1]-1})

##' @title Macro to calculate the mean residual sum squares of a linear model
##' @details Given a linear model object this macro return the residual sum squares divided
##' by it's associated degrees of freedom (i.e. RSS/(n-k) where n = number of data points used to estimate the model,
##' and k = number of parameters of the model)
##' @param lmdl a linear model object
##' @return The mean residual sum squares of a linear model
##' @author Ben Veal
##' @export
MRSS <- defmacro(lmdl,expr={sum((lmdl$residuals)^2)/DFresid(lmdl)})

#MRSS <- defmacro(lmdl,expr={sum((lmdl$residuals)^2)/(lmdl$df.residual)})

##' @title Macro to calculate the mean total sum squares of a linear model
##' @details Given a linear model object this macro return the total sum squares divided
##' by it's associated degrees of freedom (i.e. TSS/(n-1) where n = number of data points used to estimate the model)
##' @param lmdl a linear model object
##' @return The mean total sum squares of a linear model
##' @author Ben Veal
##' @export
MTSS <- defmacro(lmdl,expr={sum((lmdl$model[1]-mean(lmdl$model[1]))^2)/DFtotal(lmdl)})

#MTSS <- defmacro(lmdl,expr={sum((lmdl$model[1]-mean(lmdl$model[1]))^2)/(dim(lmdl$model)[1]-1)})

##' @title Macro to calculate the mean model sum squares of a linear model
##' @details Given a linear model object this macro return the residual sum squares divided
##' by it's associated degrees of freedom (i.e. MSS/(k-1) where n = number of data points used to estimate the model,
##' and k = number of parameters of the model).
##' Note this calculation is only correct if the model contains a constant term, otherwise you can use MSS(model)/k.
##' @param lmdl a linear model object
##' @return The mean model sum squares of a linear model
##' @author Ben Veal
##' @export
MMSS <- defmacro(lmdl,expr={sum(((lmdl$fitted.values-mean(lmdl$fitted.values))^2)/DFmodel(lmdl))})

#MMSS <- defmacro(lmdl,expr={sum(((lmdl$fitted.values-mean(lmdl$fitted.values))^2)/(length(names(lmdl$coefficients))-1))})

