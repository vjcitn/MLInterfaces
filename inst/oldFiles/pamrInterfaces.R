#####################
# PACKAGE: pamr
#####################
# 
#####################
# title: pamrB
# description: interface to pamr {pamr}
# arguments:
#	exprObj		ExpressionSet
#	trainInd	vector of indices for the columns to be 
#			included in the training set
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
# example:
# train <- c(sample(1:47, 23), sample(48:72, 12))
# pOut <- pamrB(golubMerge[100:200,], "ALL.AML", train)
####################

setGeneric("pamrB", function(exprObj, classifLab, trainInd, thresholdp=1, threshold, 
			n.threshold = 30, scale.sd = TRUE, threshold.scale, se.scale, 
			offset.percent = 50, prior, remove.zeros = TRUE, sign.contrast="both", 
			metric="euclidean"){
			standardGeneric("pamrB")
})
		
setMethod("pamrB", c("ExpressionSet", "character", "integer", "ANY", "ANY", "ANY", "ANY", "ANY", 
			"ANY", "ANY", "ANY", "ANY", "ANY", "ANY"),
			function(exprObj, classifLab, trainInd, thresholdp, threshold, n.threshold, 
			scale.sd, threshold.scale, se.scale, offset.percent, prior, remove.zeros, 
			sign.contrast="both", metric){
			
			if(missing(threshold)){ threshold <- NULL }
			if(missing(prior)){ prior <- NULL }
			if(missing(threshold.scale)){ threshold.scale <- NULL }
			if(missing(se.scale)){ se.scale <- NULL }

			cl <- pData(exprObj)[[classifLab]][trainInd]
			trainDat <- list(x=exprs(exprObj)[,trainInd], y = cl)
			testDat <- exprs(exprObj)[,-trainInd]
				
			dis <- dist(t(exprs(exprObj)[,-trainInd]), method=metric)
			
require(pamr)
			out <- pamr.train(trainDat, threshold=threshold, n.threshold=n.threshold, 
			scale.sd=scale.sd, threshold.scale=threshold.scale, se.scale=se.scale,
			offset.percent=offset.percent, prior=prior, remove.zeros=remove.zeros, 
			sign.contrast=sign.contrast)
			res <- pamr.predict(out, testDat, thresholdp)
                new("classifOutput", method="pamr",
                        predLabels=newPredClass(as.character(res)),
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
                        predScores=newProbArray(out$prob),
                        RObject=out, call=match.call(), distMat=dis)

})
