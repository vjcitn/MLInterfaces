\name{plspinHcube}
\alias{plspinHcube}

\title{
shiny app for interactive 3D visualization of mlbench hypercube
}
\description{
shiny app for interactive 3D visualization of mlbench hypercube
}
\usage{
plspinHcube(insbwidth=4)
}
\arguments{
 \item{insbwidth}{numeric, sidebar width}
}

\details{

}
\value{
Runs \code{\link[shiny]{shinyApp}} on ui and server that
render gaussian data at hypercube vertices.  
}



\author{
VJ Carey <stvjc@channing.harvard.edu>
}






\seealso{
\code{\link[mlbench]{mlbench.hypercube}}
}
\examples{
if (interactive()) plspinHcube()
}


\keyword{ models }
