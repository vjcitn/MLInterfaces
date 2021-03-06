## randomForestI = makeLearnerSchema("randomForest", "randomForest",
##     standardMLIConverter)
randomForestI = makeLearnerSchema("randomForest", "randomForest",
  MLIConverter.randomForest,
  MLIPredicter.randomForest)

knnI = function(k=1, l=0) {
  makeLearnerSchema("MLInterfaces", "knn2",
                    MLIConverter.knn(k, l),
                    MLIPredicter.knn)}


knn.cvI = function(k=1, l=0) {
  makeLearnerSchema("MLInterfaces", "knn.cv2",
                    MLIConverter.knncv(k, l))}

dldaI = makeLearnerSchema("MLInterfaces", "dlda2",
    MLIConverter.dlda)

rpartI = makeLearnerSchema("rpart", "rpart",
  MLIConverter.rpart) # get posterior

ldaI = makeLearnerSchema("MASS", "lda",
  MLIConverterListEl.class)

## svmI = makeLearnerSchema("e1071", "svm",
##     MLIConverter.svm)
svmI <- makeLearnerSchema("MLInterfaces", "svm2",
                          MLIConverter.svm,
                          MLIPredicter.svm)

ldaI.predParms = function(method) { # use this one with argument picking method
  makeLearnerSchema("MASS", "lda", # for predict.lda
                    MLIConverter.ldaPredMeth(method))
}

qdaI = makeLearnerSchema("MASS", "qda",
  MLIConverterListEl.class)

glmI.logistic = function(threshold) { # could build ROC
  makeLearnerSchema("stats", "glm", 
                     MLIConverter.logistic(threshold))}

RABI = makeLearnerSchema("MLInterfaces", "rab",
  MLIConverter.RAB)

lvqI = makeLearnerSchema("MLInterfaces", "lvq",
  MLIConverter.dlda)

naiveBayesI = makeLearnerSchema("e1071", "naiveBayes",
  MLIConverter.naiveBayes,
  MLIPredicter.naiveBayes)

# to do as of 12 Sep 2007 -- inclass, inbagg [ need good cFUN examples before going there ]
# pamr, gbm, logitBoost

## ksvmI = makeLearnerSchema("kernlab", "ksvm",
##   standardMLIConverter)
ksvmI <- makeLearnerSchema("MLInterfaces", "ksvm2",
                           MLIConverter.ksvm,
                           MLIPredicter.ksvm)

adaI = makeLearnerSchema("ada", "ada",
  standardMLIConverter)

#hclustI = function(distMethod, agglomMethod) {
#    if (missing(distMethod)) stop("distMethod must be explicitly supplied")
#    if (missing(agglomMethod)) stop("agglomMethod must be explicitly supplied")
#    makeClusteringSchema( "stats", 
#        "hclust", distMethod, hclustConverter, agglomMethod) }
#
#kmeansI = function(algorithm, distMethod="identity") {
#    if (missing(algorithm)) stop("algorithm must be explicitly supplied")
#    makeClusteringSchema( "stats", 
#        "kmeans", distMethod=distMethod, algorithm=algorithm, 
#         converter=kmeansConverter) }
#
#pamI = function(distMethod) {
#    if (missing(distMethod)) stop("distMethod must be explicitly supplied")
#    makeClusteringSchema( "cluster", 
##        "pam", distMethod, pamConverter) }

#logitboostI = makeLearnerSchema("MLInterfaces", "logitboost2",
#    standardMLIConverter)

BgbmI = function(n.trees.pred=1000, thresh=.5) {
  makeLearnerSchema("MLInterfaces", "gbm2", 
                    MLIConverter.Bgbm(n.trees.pred,thresh))}

blackboostI = makeLearnerSchema("mboost", "blackboost",
  MLIConverter.blackboost)

                    
nnetI = makeLearnerSchema("nnet", "nnet",
    MLIConverter.nnet,
    MLIPredicter.nnet)

baggingI = makeLearnerSchema("ipred", "bagging",
  standardMLIConverter)

#rdacvI = makeLearnerSchema("MLInterfaces", "rdacvML",
#  standardMLIConverter)
#
#rdaI = makeLearnerSchema("MLInterfaces", "rdaML",
#  standardMLIConverter)

sldaI = makeLearnerSchema("ipred", "slda",
  MLIConverter.slda)

# to do as of 12 Sep 2007 -- inclass, inbagg [ need good cFUN examples before going there ]
# pamr, gbm, logitBoost


#hclustI = function(distMethod, agglomMethod) {
#    if (missing(distMethod)) stop("distMethod must be explicitly supplied")
#    if (missing(agglomMethod)) stop("agglomMethod must be explicitly supplied")
#    makeClusteringSchema( "stats", 
#        "hclust", distMethod, hclustConverter, agglomMethod) }
#
#kmeansI = function(algorithm, distMethod="identity") {
#    if (missing(algorithm)) stop("algorithm must be explicitly supplied")
#    makeClusteringSchema( "stats", 
#        "kmeans", distMethod=distMethod, algorithm=algorithm, 
#         converter=kmeansConverter) }
#
#pamI = function(distMethod) {
#    if (missing(distMethod)) stop("distMethod must be explicitly supplied")
#    makeClusteringSchema( "cluster", 
##        "pam", distMethod, pamConverter) }

#logitboostI = makeLearnerSchema("MLInterfaces", "logitboost2",
#    standardMLIConverter)

plsdaI <- makeLearnerSchema("MLInterfaces",
                            "plsda2",
                            MLIConverter.plsda,
                            MLIPredicter.plsda)
