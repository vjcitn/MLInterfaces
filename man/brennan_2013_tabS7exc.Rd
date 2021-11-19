\name{brennan_2013_tabS7exc}
\alias{brennan_2013_tabS7exc}
\docType{data}
\title{
Clinical characterization of 158 GBM samples from https://doi.org/10.1016/j.cell.2013.09.034 supp table S7
}
\description{
The clinical characteristics table of https://doi.org/10.1016/j.cell.2013.09.034 in supplemental table
S7 was aligned with the GBM samples in curatedTCGAData (selecting GBM and version 2.0.1 with curatedTCGAData
1.17.0.
}
\usage{data("brennan_2013_tabS7exc")}
\format{
  A data frame with 158 observations on the following 16 variables.
  \describe{
    \item{\code{Case_ID}}{a character vector}
    \item{\code{Secondary_or_Recurrent}}{a character vector}
    \item{\code{Age_at_Procedure}}{a numeric vector}
    \item{\code{Gender}}{a character vector}
    \item{\code{Path_Dx}}{a character vector}
    \item{\code{MGMT_Status}}{a character vector}
    \item{\code{Methylation_Class_2012}}{a character vector}
    \item{\code{G_CIMP_methylation}}{a character vector}
    \item{\code{IDH1_status}}{a character vector}
    \item{\code{Expression_Subclass}}{a character vector}
    \item{\code{Therapy_Class}}{a character vector}
    \item{\code{Vital_Status}}{a character vector}
    \item{\code{OS_days}}{a numeric vector}
    \item{\code{Progression_Status}}{a character vector}
    \item{\code{PFS_days}}{a numeric vector}
    \item{\code{V16}}{a logical vector}
  }
}
\details{
Simple intersection on Case_ID in Supp Tab 7 with patientID in the GBM from
curatedTCGAData.
}
\source{
\url{https://doi.org/10.1016/j.cell.2013.09.034}
}
\references{
The Somatic Genomic Landscape of Glioblastoma by
Cameron W. Brennan, 
Roel G.W. Verhaak,
Aaron McKenna, and others, Cell Oct 10 2013.
}
\examples{
data(brennan_2013_tabS7exc)
head(brennan_2013_tabS7exc)
}
\keyword{datasets}
