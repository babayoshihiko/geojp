% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/landnuminfo_landuse.R
\name{read_landnuminfo_forestarea}
\alias{read_landnuminfo_forestarea}
\title{Download spatial data of Forest Area of Japan}
\usage{
read_landnuminfo_forestarea(
  code_pref,
  code_muni = NULL,
  year = 2006,
  data_dir = NULL
)
}
\arguments{
\item{code_pref}{The 2-digit code of prefecture.}

\item{code_muni}{Ignored.}

\item{year}{Year of the data. Defaults to 2006.}

\item{data_dir}{The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.}
}
\value{
An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
}
\description{
Function to download spatial data of Forest Area. The returned value is an sf object with extra
attr "col" and "palette". The "col" is the factored column that indicate the land use classes
and "palette" provides the colour palette based on Japan Industrial Standard.
}
