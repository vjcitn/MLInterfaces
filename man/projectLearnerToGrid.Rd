\name{projectLearnerToGrid}
\alias{projectLearnerToGrid}

\title{
create learned tesselation of feature space after PC transformation
}
\description{
create learned tesselation of feature space after PC transformation
}
\usage{
projectLearnerToGrid(formula, data, learnerSchema, 
   trainInds, ..., 
   dropIntercept = TRUE, 
   ngpts = 20, 
   predExtras = list(), 
   predWrapper = force)
}

\arguments{
  \item{formula}{
standard formula, typically of the form "y~." where y denotes
the class label variable to be predicted by all remaining features
in the input data frame
}
  \item{data}{
a data.frame instance
}
  \item{learnerSchema}{
an instance of \code{\link{learnerSchema-class}}
}
  \item{trainInds}{
integer vector of rows of \code{data} to use for training
}
  \item{\dots}{
additional parameters for use with \code{learnerSchema}
}
  \item{dropIntercept}{
logical indicating whether to include column of 1s among feature
column-vectors
}
  \item{ngpts}{
number of equispaced points along the range of each input feature
to use in forming a grid in feature space
}
  \item{predExtras}{
a list with named elements giving binding to extra parameters needed
to predict labels for the learner in use.  For example, with 
\code{ldaI}, set \code{predExtras=list(type="class")}
}
  \item{predWrapper}{
Sometimes a function call is needed to extract the predicted
labels from the RObject applied to the \code{fittedLearner}
slot of the output object; this parameter defines that call.
}
}



\value{
instance of \code{\link{projectedLearner-class}}





}
\references{
none.
}
\author{
VJ Carey <stvjc@channing.harvard.edu>
}









\examples{
library(mlbench)
# demostrate with 3 dimensional hypercube problem
kk = mlbench.hypercube()
colnames(kk$x) = c("f1", "f2", "f3")
hcu = data.frame(cl=kk$classes, kk$x)
set.seed(1234)
sam = sample(1:nrow(kk$x), size=nrow(kk$x)/2)
ldap = projectLearnerToGrid(cl~., data=hcu, ldaI, 
   sam, predWrapper=function(x)x$class)
plot(ldap)
confuMat(ldap@fittedLearner)
nnetp = projectLearnerToGrid(cl~., data=hcu, nnetI, sam, size=2,
   decay=.01, predExtras=list(type="class"))
plot(nnetp)
confuMat(nnetp@fittedLearner)
#if (requireNamespace("rgl") && interactive()) {
#    learnerIn3D(nnetp)
#    ## customising the rgl plot
#    learnerIn3D(nnetp, size = 10, alpha = 0.1)
#}
}


\keyword{ models }
