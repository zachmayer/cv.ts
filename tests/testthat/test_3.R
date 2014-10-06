# TEST SUIT 3: PARTIAL ETS and Arima validation against Rob Hyndman's implementation

# REFERENCE:
# http://robjhyndman.com/researchtips/tscvexample/

#ARIMA Model, skip 12
test_that("CV 12, Growing-Window ARIMA", {
  library('fpp')
  #Define Answer <- Hyndman's new Arima Function
  Hynd <- c(0.530093131265591, 0.670045801021527, 0.675511405138484, 0.504526932858031,
            0.864255071077892, 0.583686693558371, 0.848232739738297, 0.914759546370232,
            0.905330622956631, 0.792776183343865, 0.578811097623065, 0.689325332068739
  )

	#Cross-validate model
	myControl <- list(	minObs=60,
						stepSize=12,
						maxHorizon=12,
						fixedWindow=FALSE,
            preProcess=FALSE,
						summaryFunc=tsSummary
					)
	result <- cv.ts(a10, arimaForecast, myControl, order=c(3,0,1),
		seasonal=list(order=c(0,1,1), period=12),
		include.drift=TRUE, lambda=0, method="ML")[['results']][1:12,'MAE']

	expect_equal(result, Hynd)
}
)

#ETS Model, skip 12
test_that("CV 12, Growing-Window ETS", {
  library('fpp')
	#Define Answer
	Hynd <- c(0.383842162984939, 0.613118892984361, 0.641529116111774, 0.65376999722495,
	          0.896112617471054, 1.06816137460941, 0.868167871199789, 0.792827956506136,
	          0.996343739226753, 0.62206028862627, 0.851072852100979, 1.02807977047699
	)

	#Cross-validate model
	myControl <- list(	minObs=60,
						stepSize=12,
						maxHorizon=12,
						fixedWindow=FALSE,
            preProcess=FALSE,
						summaryFunc=tsSummary
					)
	result <- cv.ts(a10, etsForecast, myControl, model="MMM", damped=TRUE)[['results']][1:12,'MAE']

	expect_equal(result, Hynd)
}
)
