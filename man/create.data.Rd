% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_data.R
\name{create.data}
\alias{create.data}
\title{Creates functional data with six channels.}
\usage{
create.data(sigma = 1, omega = 0.5)
}
\arguments{
\item{sigma}{the standard deviation.}

\item{omega}{the CS correlation between channels}
}
\value{
a list.
}
\description{
Creates functional data with six channels.
}
\examples{
cd <- create.data()
}
