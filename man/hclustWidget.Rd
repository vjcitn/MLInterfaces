\name{hclustWidget}
\alias{hclustWidget}
\alias{mlearnWidget}

\title{
shiny-oriented GUI for cluster or classifier exploration
}
\description{
shiny-oriented GUI for cluster or classifier exploration
}
\usage{
hclustWidget(mat, featureName = "feature", title =
                 paste0("hclustWidget for ", deparse(substitute(mat))),
                 minfeats = 2, auxdf = NULL)

mlearnWidget(eset, infmla)
}

\arguments{
  \item{mat}{
matrix with feature vectors in rows
}
  \item{featureName}{
name to be used for control that asks for number of features to use
}
  \item{title}{
widget title
}
  \item{minfeats}{
lower bound on number of features to use
}  \item{auxdf}{
data.frame with number of rows equal to nrow(mat), with metadata
to be displayed in hovering tooltip
}
  \item{eset}{instance of \code{\link{ExpressionSet-class}}}
  \item{infmla}{instance of \code{formula}, with dependent variable
values obtained in \code{pData(eset)}, and independent variable
specified as "." or as properly munged elements of \code{featureNames(eset)}.}
}
\details{
Experimental tool to illustrate impacts of choice of distance,
agglomeration method, etc.
}
\value{
a shinyApp result that will display in active browser
}



\author{
VJ Carey <stvjc@channing.harvard.edu>
}
\note{
\code{mlearnWidget} will attempt to nicely produce a variable
importance plot using \code{\link{randomForestI}}.  This means
that the annotation package for probe identifiers should be loaded
or an error will be thrown.
}






\examples{
# should run with example(hclustWidget, ask=FALSE)
if (interactive()) {
 library(shiny)
 library(MASS)
 data(crabs)
 cr = data.matrix(crabs[,-c(1:3)])
 au = crabs[,1:3]
 show(hclustWidget(cr, auxdf=au))
## must use stop widget button to proceed
  library(ALL)
  library(hgu95av2.db)
  data(ALL)
  show(mlearnWidget(ALL[1:500,], mol.biol~.))
 }
}


\keyword{ models }
