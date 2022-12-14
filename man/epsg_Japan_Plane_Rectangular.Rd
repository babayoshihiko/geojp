% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{epsg_Japan_Plane_Rectangular}
\alias{epsg_Japan_Plane_Rectangular}
\title{EPSG code of Japan Plane Rectangular}
\usage{
epsg_Japan_Plane_Rectangular(code_pref, code_muni = NULL, crs_type = "JGD2011")
}
\arguments{
\item{code_pref}{The 2-digit code of a prefecture.}

\item{code_muni}{The 3-digit code of a municipality (city, town, or village).}

\item{crs_type}{Defaults to "JGD2000." Other options are "JGD2011" or "Census." "Census" is almost identical to "JGD2000," but slightly different in Hokkaido, Nagasaki and Kagoshima.}
}
\value{
an EPSG code
}
\description{
Function to get the correct EPSG code from prefecture/municipality
}
