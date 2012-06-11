
# TEST SUIT 2: COMPLETE Linear Model validation against Rob Hyndman's implementation

# REFERENCE: 
# http://robjhyndman.com/researchtips/tscvexample/


#Linear Model, growing window
test_that("Growing Window LM", {
  
	#Define Answer
	Hynd <- c(0.779154492257176, 0.787134921474268, 0.797615947088694, 0.810306403861217, 
				0.818905262353758, 0.824895018711762, 0.834311158410833, 0.83450689959012, 
				0.843424407504474, 0.85258027548547, 0.861880772094904, 0.871110202072342
				)

	#Cross-validate model
	myControl <- list(	minObs=60,
						stepSize=1, 
						maxHorizon=12, 
						fixedWindow=FALSE,
            preProcess=FALSE,
						summaryFunc=tsSummary
					)
	result <- cv.ts(a10, lmForecast, myControl, lambda=0)[['results']][1:12,'MAE']

	expect_that(result, equals(Hynd))
}
)

#Linear Model, fixed window
test_that("Fixed Window LM", {
  
	#Define Answer
	Hynd <- c(0.735515585745927, 0.739820386514753, 0.759837209561034, 0.7770661170043, 
				0.784411731394229, 0.796921460003072, 0.807834602011012, 0.82097878269219, 
				0.834198763732548, 0.857102388013412, 0.868038840319207, 0.876240902863029
				)

	#Cross-validate model
	myControl <- list(	minObs=60,
						stepSize=1, 
						maxHorizon=12, 
						fixedWindow=TRUE,
            preProcess=FALSE,
						summaryFunc=tsSummary
					)
	result <- cv.ts(a10, lmForecast, myControl, lambda=0)[['results']][1:12,'MAE']

	expect_that(result, equals(Hynd))
}
)

#Linear Model, skip 12
test_that("CV 12, Growing-Window LM", {
  
	#Define Answer
	Hynd <- c(0.58411005993366, 0.685233396741406, 0.661691601729084, 0.432632362459096, 
				0.878074652648806, 1.49251083114561, 0.970733483828223, 0.93325546507937, 
				0.891940891833123, 0.861585154330702, 0.571999050246289, 0.700686878794175
				)

	#Cross-validate model
	myControl <- list(	minObs=60,
						stepSize=12, 
						maxHorizon=12, 
						fixedWindow=FALSE,
            preProcess=FALSE,
						summaryFunc=tsSummary
					)
	result <- cv.ts(a10, lmForecast, myControl, lambda=0)[['results']][1:12,'MAE']

	expect_that(result, equals(Hynd))
}
)