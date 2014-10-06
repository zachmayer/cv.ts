
# TEST SUIT 1: Bug Checks

# REFERENCE:
# http://robjhyndman.com/researchtips/tscvexample/

#1-step linear model
test_that("1-step LM", {

	#Define Answer
  data(a10)
	Hynd <- c(0.779154492257176)

	#Cross-validate model
	myControl <- tseriesControl(
            minObs=60,
						stepSize=1,
						maxHorizon=1,
						fixedWindow=FALSE,
            preProcess=FALSE,
						summaryFunc=tsSummary
					)

	result <- cv.ts(a10, lmForecast, myControl, lambda=0)[['results']][1,'MAE']

	expect_equal(result, Hynd)
}
)
