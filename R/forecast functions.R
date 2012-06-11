
meanForecast <- cmpfun(function(x,h,...) {
  require(forecast)
  meanf(x, h, ..., level=99)$mean
})

naiveForecast <- cmpfun(function(x,h,...) {
  require(forecast)
  naive(x, h, ..., level=99)$mean
})

rwForecast <- cmpfun(function(x,h,...) {
  require(forecast)
  rwf(x, h, ..., level=99)$mean
})

thetaForecast <- cmpfun(function(x,h,...) {
  require(forecast)
  out <- thetaf(x, h, ..., level=99)$mean
  return(out)
})

lmForecast <- cmpfun(function(x,h,xreg=NULL,newxreg=NULL,...) {
  require(forecast)
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
})

stsForecast <- cmpfun(function(x,h,...) {
  require(forecast)
  fit <- StructTS(x, ...)
  forecast(fit, h=h, level=99)$mean
})

stl.Forecast <- cmpfun(function(x, h, method='ets', ...) {
  require(forecast)
  stlf(x, h=h, method, level=99, ...)$mean
})

arimaForecast <- cmpfun(function(x,h,xreg=NULL,newxreg=NULL,...) {
  fit <- Arima(x, xreg=xreg, ...)
  forecast(fit, h=h, level=99, xreg=newxreg)$mean
})

auto.arimaForecast <- cmpfun(function(x,h,xreg=NULL,newxreg=NULL,...) {
  require(forecast)
  fit <- auto.arima(x, xreg=xreg, ...)
  forecast(fit, h=h, level=99, xreg=newxreg)$mean
})

etsForecast <- cmpfun(function(x,h,...) {
  require(forecast)
  fit <- ets(x, ...)
  forecast(fit, h=h, level=99)$mean
})

caretForecast <- cmpfun(function(x, h, xreg, newxreg=NULL, train.fraction=2/3,...) {
  require(caret)
  stopifnot(h==1)
  myData <- data.frame(x=as.numeric(x), xreg)
  fit <- train(x~., data=myData, ...)
  predict(fit, newdata=newxreg)
})
