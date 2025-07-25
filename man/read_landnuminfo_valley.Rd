% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/landnuminfo_water.R
\name{read_landnuminfo_valley}
\alias{read_landnuminfo_valley}
\title{Download spatial data of Vally of Japan by mesh}
\usage{
read_landnuminfo_valley(code_pref, code_muni, year = 2009, data_dir = NULL)
}
\arguments{
\item{code_pref}{The 2-digit code of prefecture.}

\item{code_muni}{Optional. The 3-digit code of municipality.}

\item{year}{Year of the data. Defaults to 2009.}

\item{data_dir}{The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.}
}
\value{
An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
}
\description{
Function to download spatial data of Valley of Japan. The returned value is an sf object.
The function reads Valley Third Level Mesh data. See https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-L03-a.html.
}
