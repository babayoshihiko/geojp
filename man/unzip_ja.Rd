% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{unzip_ja}
\alias{unzip_ja}
\title{Unzip a zip file of files with non-ASCII filenames}
\usage{
unzip_ja(zipfile, files = NULL, exdir)
}
\arguments{
\item{zipfile}{Path to the zip file to uncompress.}

\item{files}{Not used. Just for consistency with utils::zip()}

\item{exdir}{Directory to uncompress the archive to. If it does not exist, it will be created.}
}
\description{
Unzip a zip file of files with non-ASCII filenames
}
