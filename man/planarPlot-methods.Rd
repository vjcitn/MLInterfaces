\name{planarPlot-methods}
\docType{methods}
\alias{planarPlot}
\alias{planarPlot-methods}
\alias{planarPlot,classifierOutput,ExpressionSet,character-method}
\alias{planarPlot,classifierOutput,data.frame,character-method}
\title{ Methods for Function planarPlot in Package `MLInterfaces' }
\description{
show the classification boundaries on the plane dictated
by two genes in an ExpressionSet
}
\section{Methods}{
\describe{
\item{clo = "classifierOutput", eset = "ExpressionSet", classifLab = "character"}{ uses two genes in the ExpressionSet
to exhibit the decision boundaries in the plane}
\item{clo = "classifierOutput", eset = "data.frame", classifLab = "character"}{ uses two columns in the data.frame
to exhibit the decision boundaries in the plane}
}
}
\examples{
library(ALL)
library(hgu95av2.db)
data(ALL)
#
# restrict to BCR/ABL or NEG
#
bio <- which( ALL$mol.biol \%in\% c("BCR/ABL", "NEG"))
#
# restrict to B-cell
#
isb <- grep("^B", as.character(ALL$BT))
kp <- intersect(bio,isb)
all2 <- ALL[,kp]
#
# sample 2 genes at random
#
set.seed(1234)
ng <- nrow(exprs(all2)) # pick 5 in case any NAs come back
pick <- sample(1:ng, size=5, replace=FALSE)
gg <- all2[pick,]
sym <- unlist(mget(featureNames(gg), hgu95av2SYMBOL))
bad = which(is.na(sym))
if (length(bad)>0) {
  gg = gg[-bad,]
  sym = sym[-bad]
  }
gg = gg[1:2,]
sym = sym[1:2]
featureNames(gg) <- sym
gg$class = factor(ifelse(all2$mol.biol=="NEG", "NEG", "POS"))

cl1 <- which( gg$class == "NEG" )
cl2 <- which( gg$class != "NEG" )
#
# create balanced training sample
#
trainInds <- c( sample(cl1, size=floor(length(cl1)/2) ),
      sample(cl2, size=floor(length(cl2)/2)) )
#
# run rpart
#
tgg <- MLearn(class~., gg, rpartI, trainInds, minsplit=4 )
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
planarPlot( tgg, gg, "class" )
title("rpart")
points(exprs(gg)[1,trainInds], exprs(gg)[2,trainInds], col=ifelse(gg$class[trainInds]=="NEG", "yellow", "black"), pch=16)
#
# run nnet
#
ngg <- MLearn( class~., gg, nnetI, trainInds, size=8 )
planarPlot( ngg, gg, "class" )
points(exprs(gg)[1,trainInds], exprs(gg)[2,trainInds], col=ifelse(gg$class[trainInds]=="NEG", "yellow", "black"), pch=16)
title("nnet")
#
# run knn
#
kgg <- MLearn( class~.,  gg, knnI(k=3,l=1), trainInds)
planarPlot( kgg, gg, "class" )
points(exprs(gg)[1,trainInds], exprs(gg)[2,trainInds], col=ifelse(gg$class[trainInds]=="NEG", "yellow", "black"), pch=16)
title("3-nn")
#
# run svm
#
sgg <- MLearn( class~., gg, svmI, trainInds )
planarPlot( sgg, gg, "class" )
points(exprs(gg)[1,trainInds], exprs(gg)[2,trainInds], col=ifelse(gg$class[trainInds]=="NEG", "yellow", "black"), pch=16)
title("svm")
par(opar)
}
\keyword{methods}
