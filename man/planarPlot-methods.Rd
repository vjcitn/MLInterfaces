\name{planarPlot-methods}
\docType{methods}
\alias{planarPlot-methods}
\alias{planarPlot,classifOutput,exprSet,character-method}
\title{ Methods for Function planarPlot in Package `MLInterfaces' }
\description{
show the classification boundaries on the plane dictated
by two genes in an exprSet
}
\section{Methods}{
\describe{
\item{clo = "classifOutput", eset = "exprSet", classifLab = "character"}{ ~~describe this method here }
}}
\examples{
library(ALL)
data(ALL)
bio <- which( ALL$mol.biol \%in\% c("BCR/ABL", "NEG"))
isb <- grep("^B", as.character(ALL$BT))
kp <- intersect(bio,isb)
all2 <- ALL[,kp]
set.seed(1234)
ng <- nrow(exprs(all2))
pick <- sample(1:ng, size=2, replace=FALSE)
gg <- all2[pick,]
class <- as.character(all2$mol.biol)
gg@phenoData@pData$class <- factor(class)
cl1 <- which( class == "NEG" )
cl2 <- which( class != "NEG" )
trainInds <- c( sample(cl1, size=floor(length(cl1)/2) ),
      sample(cl2, size=floor(length(cl2)/2)) )
tgg <- rpartB( gg, "class", trainInds, minsplit=4 )
par(mfrow=c(2,2))
planarPlot( tgg, gg, "class" )
title("rpart")
ngg <- nnetB( gg, "class", trainInds, size=8 )
planarPlot( ngg, gg, "class" )
title("nnet")
kgg <- knnB( gg, "class", trainInds, k=3, l=1 )
planarPlot( kgg, gg, "class" )
title("3-nn")
sgg <- svmB( gg, "class", trainInds )
planarPlot( sgg, gg, "class" )
title("svm")
}
\keyword{methods}
\keyword{ ~~ other possible keyword(s)}