% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WOEProfet_new.R
\name{WOEProfet_new}
\alias{WOEProfet_new}
\title{WOE Transformation}
\usage{
WOEProfet_new(data, id, target, varcol, IVfilter)
}
\arguments{
\item{data}{Dataframe of binned variables.}

\item{id}{ID variable.}

\item{target}{A binary target variable.}

\item{varcol}{Vector of variables to have WOE transformation.}

\item{IVfilter}{Threshold of variables' Information Value}
}
\value{
A list of three items.
}
\description{
Function that calculates the WOE for each bin and the information value for each variable.
}
\examples{
#skip for now
}
