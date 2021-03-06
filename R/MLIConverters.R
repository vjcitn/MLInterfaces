standardMLIConverter = function(obj, data, trainInd) {
# use when the predict method directly returns factor-like predictions
# otherwise, need to define a special converter
   teData = data[-trainInd,]
   trData = data[trainInd,]
   tepr = predict(obj, teData)
   trpr = predict(obj, trData)
   names(tepr) = rownames(teData)
   names(trpr) = rownames(trData)
   new("classifierOutput", testPredictions=factor(tepr),
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverter.knn = function(k=1, l=0) function(obj, data, trainInd) {
   kpn = names(obj$traindat)
   teData = data[-trainInd,kpn] # key distinction -- typical predict methods allow
   trData = data[trainInd,kpn]  # variables in newdata to be superset of those in formula, not knn
   tepr = predict(obj, teData, k, l) # our special predict.knn2
   trpr = predict(obj, trData, k, l)
   names(tepr) = rownames(teData)
   names(trpr) = rownames(trData)
   new("classifierOutput",
       testPredictions=factor(tepr),
       testScores=attr(tepr, "prob"),
       trainPredictions=factor(trpr),
       trainScores=attr(trpr, "prob"),
       RObject=obj)
 }

MLIConverter.dlda = function(obj, data, trainInd) {
   kpn = names(obj$traindat)
   outcoLev = levels(obj$traincl) # fragile
   teData = data[-trainInd,kpn] # key distinction -- typical predict methods allow
   trData = data[trainInd,kpn]  # variables in newdata to be superset of those in formula, not knn
   tepr = outcoLev[predict(obj, teData)]
   trpr = outcoLev[predict(obj, trData)]
   names(tepr) = rownames(teData)
   names(trpr) = rownames(trData)
   new("classifierOutput", testPredictions=factor(tepr),
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverterPredType.class = function(obj, data, trainInd) {
   teData = data[-trainInd,]
   trData = data[trainInd,]
   tepr = predict(obj, teData, type="class")
   trpr = predict(obj, trData, type="class")
   names(tepr) = rownames(teData)
   names(trpr) = rownames(trData)
   new("classifierOutput", testPredictions=factor(tepr),
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverterListEl.class = function(obj, data, trainInd) {
   teData = data[-trainInd,]
   trData = data[trainInd,]
   tepr = predict(obj, teData)$class
   trpr = predict(obj, trData)$class
   names(tepr) = rownames(teData)
   names(trpr) = rownames(trData)
   new("classifierOutput", testPredictions=factor(tepr),
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverter.svm = function(obj, data, trainInd) { # decision.values parm needed
#    browser()
  teData = data[-trainInd,]
  trData = data[trainInd,]
  ##tepr = predict(obj, teData, decision.values=FALSE)
  ##trpr = predict(obj, trData, decision.values=FALSE)
  ## From ?svn: The center and scale values are returned and used for later predictions.
  tepr = predict(obj, teData, decision.values=TRUE, probability=TRUE)
  trpr = predict(obj, trData, decision.values=TRUE, probability=TRUE)
  ## names(tepr) = rownames(teData)
  ## names(trpr) = rownames(trData)
  new("classifierOutput",
      testPredictions =factor(tepr[seq_len(length(tepr))]),
      trainPredictions=factor(trpr[seq_len(length(trpr))]),
      testScores=attr(tepr,"probabilities"),
      trainScores=attr(trpr,"probabilities"),
      RObject=obj)
}

MLIConverter.ksvm <- function(obj, data, trainInd) {
  teData <- data[-trainInd,]
  trData <- data[trainInd,]
  requireNamespace("kernlab")
  teprob <- kernlab::predict(obj, teData, type="probabilities")
  rownames(teprob) <- rownames(teData)
  trprob <- kernlab::predict(obj, trData, type="probabilities")
  rownames(trprob) <- rownames(trData)
  tepred <- kernlab::predict(obj, teData, type="response")
  trpred <- kernlab::predict(obj, trData, type="response")
  new("classifierOutput",
      testPredictions =factor(tepred),
      trainPredictions=factor(trpred),
      testScores=teprob,
      trainScores=trprob,
      RObject=obj)
}

MLIConverter.ldaPredMeth = function(method) function(obj, data, trainInd) { # get binding for method to predict
   teData = data[-trainInd,]                                              # at call time
   trData = data[trainInd,]
   tepr = predict(obj, teData, method=method)$class
   trpr = predict(obj, trData, method=method)$class
   names(tepr) = rownames(teData)
   names(trpr) = rownames(trData)
   new("classifierOutput", testPredictions=factor(tepr),
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverter.logistic = function(threshold) function(obj, data, trainInd) {
   teData = data[-trainInd,]
   trData = data[trainInd,]
   tepr = 1*(predict(obj, teData, type="response") > threshold)
   trpr = 1*(predict(obj, trData, type="response") > threshold)
# from Jason Vertrees, to restore codes for levels of response
    dDf <- model.frame(obj, data)
    dRs <- model.response(dDf)
#    # Why not try the following?
#    # This just makes a decision -- as we do above that everything on one
#    # side of the threshold is class1 else class2.  Is this valid?
   tepr <- as.factor(levels(dRs)[as.integer(tepr)+1])
   trpr <- as.factor(levels(dRs)[as.integer(trpr)+1])
   tesco = predict(obj, teData, type="response")
   names(tepr) = rownames(teData)
   names(trpr) = rownames(trData)
   names(tesco) = rownames(teData)
   new("classifierOutput", testPredictions=factor(tepr), testScores=tesco,
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverter.rpart = function(obj, data, trainInd) { # decision.values parm needed
   teData = data[-trainInd,]
   trData = data[trainInd,]
   tepr = predict(obj, teData, type="class")
   trpr = predict(obj, trData, type="class")
   teprob = predict(obj, teData, type="prob")
   names(tepr) = rownames(teData)
   names(trpr) = rownames(trData)
   names(teprob) = rownames(teData)
   new("classifierOutput", testPredictions=factor(tepr), testScores=teprob, testOutcomes=factor(),
       trainPredictions=factor(trpr), RObject=obj)
   }


MLIConverter.nnet = function(obj, data, trainInd) {
   teData = data[-trainInd,]
   trData = data[trainInd,]
   tepr = predict(obj, teData, type="class")
   trpr = predict(obj, trData, type="class")
   names(tepr) = rownames(teData)
   names(trpr) = rownames(trData)
   new("classifierOutput",
       testPredictions=factor(tepr),
       testScores=predict(obj,teData),
       trainScores=predict(obj,trData),
       trainPredictions=factor(trpr),
       RObject=obj)
   }

MLIConverter.RAB = function(obj, data, trainInd) {
# use when the predict method directly returns factor-like predictions
# otherwise, need to define a special converter
   teData = data[-trainInd,]
   trData = data[trainInd,]
   tepr = predict(obj, newdata=teData)
   trpr = predict(obj, newdata=trData)
   names(tepr) = rownames(teData)
   names(trpr) = rownames(trData)
   new("classifierOutput", testPredictions=factor(tepr),
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverter.naiveBayes = function(obj, data, trainInd) {
   kpn = names(obj$tables)
   teData = data[-trainInd,kpn,drop=FALSE] # key distinction -- typical predict methods allow
   trData = data[trainInd,kpn,drop=FALSE]  # variables in newdata to be superset of those in formula, not allowed here
   tepr = predict(obj, teData, type="class")
   trpr = predict(obj, trData, type="class")
   tesc <- predict(obj, teData, type="raw")
   trsc <- predict(obj, trData, type="raw")
   names(tepr) = rownames(teData)
   names(trpr) = rownames(trData)
   new("classifierOutput",
       testPredictions=factor(tepr),
       trainPredictions=factor(trpr),
       trainScores=trsc,
       testScores=tesc,
       RObject=obj)
   }

MLIConverter.selftesting = function(obj, data, trainInd) {
   tepr = predict(obj, data)
   names(tepr) = rownames(data)
   new("classifierOutput", testPredictions=factor(tepr),
       trainPredictions=factor(tepr), RObject=obj)
   }

MLIConverter.slda = function(obj, data, trainInd) { # decision.values parm needed
   teData = data[-trainInd,]
   trData = data[trainInd,]
   teprFull = predict(obj, teData)
   trprFull = predict(obj, trData)
   tepr = teprFull$class
   trpr = trprFull$class
   teprob = predict(obj, teData, type="prob")
   trprob = predict(obj, trData, type="prob")
   names(tepr) = rownames(teData)
   names(trpr) = rownames(trData)
   names(teprob[[1]]) = rownames(teData)
   rownames(teprob[[2]]) = rownames(teData)
   names(teprob[[3]]) = rownames(teData)
   names(trprob[[1]]) = rownames(trData)
   rownames(trprob[[2]]) = rownames(trData)
   names(trprob[[3]]) = rownames(trData)
   new("classifierOutput", testPredictions=factor(tepr), testScores=teprob[[2]],
       trainPredictions=factor(tepr), trainScores=trprob[[2]], RObject=obj)
   }

MLIConverter.knncv = function(k=1, l=0) function(obj, data, trainInd) {
#
# it is assumed that there is no train/test distinction --
# the CV is embedded LOO, so all predictions are test predictions
#
   kpn = names(obj$traindat)  # name is somewhat misleading but OK
   trData = data[trainInd,kpn]  # must be sure trData just has vbls in fmla
   trpr = predict(obj, trData, k, l)
   names(trpr) = rownames(trData)
   new("classifierOutput", testPredictions=factor(trpr), testScores=attr(trpr, "prob"),
       trainPredictions=factor(), RObject=obj, embeddedCV=TRUE)
   }

## for unsupervised case all converters are of form
## obj [RObject], dstruct [distance structure or raw data for kmeans],
## ... for functions requiring additional data, like cutree, which takes
## k or h
#
#hclustConverter = function(obj, dstruct, ...) {
#   part = stats::cutree(obj, ...)
#   sil = silhouette(part, dstruct)
#   new("clusteringOutput", partition=part, silhouette=sil, RObject=obj)
#}
#
#kmeansConverter = function(obj, dstruct, ...) {
#   part = obj$clust
#   sil = silhouette(part, dist(dstruct))
#   new("clusteringOutput", partition=part, silhouette=sil, RObject=obj)
#}
#
#pamConverter = function(obj, dstruct, ...) {
#   part = obj$clustering
#   sil = silhouette(part, dist(dstruct))
#   new("clusteringOutput", partition=part, silhouette=sil, RObject=obj)
#}

MLIConverter.Bgbm = function(n.trees.pred=1000, thresh=.5) function(obj, data, trainInd) {
   anac = function(x) as.numeric(as.character(x))
   teData = data[-trainInd,] # key distinction -- typical predict methods allow
   trData = data[trainInd,]  # variables in newdata to be superset of those in formula, not gbm
   tepr = predict(obj, teData, n.trees=n.trees.pred) # our special predict.gbm2
   trpr = predict(obj, trData, n.trees=n.trees.pred)
   tesco = anac(tepr)
   tepr = anac(tepr) > thresh
   trpr = anac(trpr) > thresh
   names(tepr) = rownames(teData)
   names(trpr) = rownames(trData)
   new("classifierOutput", testPredictions=factor(tepr), testScores=tesco,
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverter.blackboost = function (obj, data, trainInd)
{
    teData = data[-trainInd, ]
    trData = data[trainInd, ]
    tepr = predict(obj, teData, type="class")
    trpr = predict(obj, trData, type="class")
    names(tepr) = rownames(teData)
    names(trpr) = rownames(trData)
    new("classifierOutput", testPredictions = factor(tepr), trainPredictions = factor(trpr),
        RObject = obj)
}

MLIConverter.randomForest = function (obj, data, trainInd) {
    teData = data[-trainInd, ]
    trData = data[trainInd, ]
    tepr = predict(obj, teData, type="response")
    tesco = predict(obj, teData, type="prob")
    trpr = predict(obj, trData, type="response")
    trsco = predict(obj, trData, type="prob")
    names(tepr) = rownames(teData)
    names(trpr) = rownames(trData)
    new("classifierOutput",
        testPredictions = factor(tepr),
        testScores = tesco,
        trainPredictions = factor(trpr),
        trainScores = trsco,
        RObject = obj)
}


MLIConverter.plsda <- function(obj, data, trainInd) {
  rhs <- colnames(obj$model$x)
  trData <- data[ trainInd,rhs]
  teData <- data[-trainInd,rhs]
  tePredProb <- predict(obj,teData,type="prob")
  if (inherits(tePredProb, "array") & dim(tePredProb)[3] == 1)
    tePredProb <- tePredProb[, , 1]
  tePredClass <- predict(obj,teData,type="class")
  names(tePredClass) <- rownames(teData)
  trPredProb <- predict(obj,trData,type="prob")
  if (inherits(trPredProb, "array") & dim(trPredProb)[3] == 1)
    trPredProb <- trPredProb[, , 1]
  trPredClass <- predict(obj,trData,type="class")
  names(trPredClass) <- rownames(trData)
  new("classifierOutput",
      testPredictions = tePredClass,
      testScores = tePredProb,
      trainPredictions = trPredClass,
      trainScores = trPredProb,
      RObject = obj)
}
