# TEST SUIT 4: xreg tests

#ARIMA Model, skip 12
test_that("xreg", {
  
	#Create xregs
	library(forecast)
	X <- fourier(ldeaths,3)

	#Cross-validate model
	myControl <- list(	minObs=12,
						stepSize=1, 
						maxHorizon=12, 
						fixedWindow=FALSE,
            preProcess=FALSE,
						summaryFunc=tsSummary
					)
	result <- cv.ts(ldeaths, arimaForecast, myControl, order=c(1,0,1), method="ML", xreg=X)

	#Define Answer //I'm not sure this answer is correct, but we're more worried about the
	#mechanics working out properly
	Answer <- c(0.304032405924376, 0.508548481954777, 0.572861954782555, 0.581390482006353, 
				0.724672766775095, 0.840523174316915, 0.522852550434799, 0.259914625856283, 
				0.426261682815546, 0.647471984984475, 0.744007722504971, 0.841916574629342, 
				0.581204533915457)
	expect_that(result[,'MAE'], equals(Answer))
}
)