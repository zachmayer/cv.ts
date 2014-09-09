#' Mean forecast wrapper
#' @export
meanForecast <- function(x,h,...) {
  forecast:::meanf(x, h, ..., level=99)$mean
}

#' Naive forecast wrapper
#' @export
naiveForecast <- function(x,h,...) {
  forecast:::naive(x, h, ..., level=99)$mean
}

#' Seasonal naive forecast wrapper
#' @export
snaiveForecast <- function(x,h,...) {
  library(forecast)
  forecast:::snaive(x, h, ..., level=99)$mean
}

#' Random walk forecast wrapper
#' @export
rwForecast <- function(x,h,...) {
  forecast:::rwf(x, h, ..., level=99)$mean
}

#' Theta forecast wrapper
#' @export
thetaForecast <- function(x,h,...) {
  out <- forecast:::thetaf(x, h, ..., level=99)$mean
  return(out)
}

#' Linear model forecast wrapper
#' @export
lmForecast <- function(x,h,xreg=NULL,newxreg=NULL,...) {
  library('forecast')
  x <- data.frame(x)
  colnames(x) <- 'x'
  if (is.null(xreg) & is.null(newxreg)) {
    fit <- tslm(x ~ trend + season, data=x, ...)
    return(forecast(fit, h=h, level=99)$mean)
  } else if ((!is.null(xreg)) & !(is.null(newxreg))) {
    newnames <- c('x',colnames(xreg))
    x <- cbind(x,xreg)
    colnames(x) <- newnames
    fmla <- as.formula(paste("x ~ trend + season +", paste(colnames(xreg), collapse= "+")))
    fit <- tslm(fmla, data=x, ...)
    return(forecast(fit, h=h, level=99, newdata=newxreg)$mean)
  } else {
    stop('xreg and newxreg must both be NULL or both be provided')
  }
}

#' Structural time series forecast wrapper
#' @export
stsForecast <- function(x,h,...) {
  fit <- forecast:::StructTS(x, ...)
  forecast:::forecast(fit, h=h, level=99)$mean
}

#' Stl forecast wrapper
#' @export
stl.Forecast <- function(x, h, method='ets', ...) {
  forecast:::stlf(x, h=h, method, level=99, ...)$mean
}

#' Arima forecast wrapper
#' @export
arimaForecast <- function(x,h,xreg=NULL,newxreg=NULL,...) {
  fit <- forecast:::Arima(x, xreg=xreg, ...)
  forecast:::forecast(fit, h=h, level=99, xreg=newxreg)$mean
}

#' auto.arima forecast wrapper
#' @export
auto.arimaForecast <- function(x,h,xreg=NULL,newxreg=NULL,...) {
  fit <- forecast:::auto.arima(x, xreg=xreg, ...)
  forecast:::forecast(fit, h=h, level=99, xreg=newxreg)$mean
}

#' Ets forecast wrapper
#' @export
etsForecast <- function(x,h,...) {
  fit <- forecast:::ets(x, ...)
  forecast:::forecast(fit, h=h, level=99)$mean
}

#' BATS forecast wrapper
#' @export
batsForecast <- function(x,h,...) {
  fit <- forecast:::bats(x, ...)
  forecast:::forecast(fit, h=h, level=99)$mean
}

#' TBATS forecast wrapper
#' @export
tbatsForecast <- function(x,h,...) {
  fit <- forecast:::tbats(x, ...)
  forecast:::forecast(fit, h=h, level=99)$mean
}

#' NNetar forecast wrapper
#' @export
nnetarForecast <- function(x,h,...) {
  fit <- forecast:::nnetar(x, ...)
  forecast:::forecast(fit, h=h, level=99)$mean
}

#' Caret forecast wrapper
#' @export
caretForecast <- function(x, h, xreg, newxreg=NULL,...) {
  myData <- data.frame(x=as.numeric(x), xreg)
  fit <- caret:::train(x~., data=myData, ...)
  predict(fit, newdata=newxreg)
}

#' Create folds for a time series model UPDATE TO USE CARET!
#'
#' Define time-series cv indexes for caretForecast.
#'
#' @export
createTSfolds <- function(y, Min = max(2/3*length(y), 3), k = NA){
  out = plyr:::llply(Min:(length(y) - 1), seq)
  if (!is.na(k)) {out = out[seq(1, length(out), k)]}
  names(out) <- paste("Fold", gsub(" ", "0", format(seq(along = out))), sep = "")
  return(out)
}