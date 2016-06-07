[![Build Status](https://travis-ci.org/zachmayer/cv.ts.png?branch=master)](https://travis-ci.org/zachmayer/cv.ts)
Time Series Cross-Valiation
===

This is a package to automate the cross-validation of time-series models, particularly those created by the forecast and caret packages.

Install as follows:
```{R}
devtools::install_github('zachmayer/cv.ts')
```

You can then cross-validate different time series models as follows:
```{R}
data("AirPassengers")
x <- AirPassengers
myControl <- tseriesControl(maxHorizon=4)
theta_model <- cv.ts(x, thetaForecast, tsControl=myControl)
arima_model <- cv.ts(x, auto.arimaForecast, tsControl=myControl)
ets_model <- cv.ts(x, etsForecast, tsControl=myControl)

theta_model$results
arima_model$results
ets_model$results
```

This package has been off my radar screen for a long time, and I need to write lots of documentation and tests.  Please add issues to the issue tracker, and I welcome any pull requests.
https://github.com/zachmayer/cv.ts/issues
