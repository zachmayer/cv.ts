# TEST SUIT 4: xreg tests

#ARIMA Model, skip 12
test_that("xreg", {
  library('fpp')
	#Create xregs
	X <- fourier(ldeaths,3)

	#Cross-validate model
	myControl <- list(	minObs=12,
						stepSize=1,
						maxHorizon=12,
						fixedWindow=FALSE,
            preProcess=FALSE,
						summaryFunc=tsSummary
					)
	result <- cv.ts(ldeaths, arimaForecast, myControl, order=c(1,0,1), method="ML", xreg=X)[['results']][,'MAE']

	#Define Answer //I'm not sure this answer is correct, but we're more worried about the
	#mechanics working out properly
	Answer <- c(207.107376954853, 215.046411882551, 209.566610852137, 218.081099835925,
	            215.841691289157, 220.513771680846, 219.072401504129, 226.063486425693,
	            226.415187927682, 225.361238437867, 227.374860412696, 238.611319458946,
	            220.75462138854)

	expect_equal(result, Answer)
}
)
