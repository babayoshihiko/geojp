# The Land Numerical Information has facilities data as:
# national and governmental governmental institutions
# municipal townhalls
# police stations and so on

#' Download spatial data of Welfare Facilities of Japan
#'
#' @description
#' Function to download spatial data of Welfare Facilities of Japan. The returned value is an sf object.
#'
#' The data is available for years 2021, 2015 and 2011. Somehow, the data for Osaka year 2011 is not available.
#'
#' Please note that the welfare facility data is NOT accurate.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2021.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_welfare <- function(code_pref, code_muni = NULL, year = 2021, data_dir = NULL){
  year4digit = check_year(year)

  if (code_pref == 27 & year4digit == 2015) stop(paste("Welfare data not available for Osaka Year 2015."))

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("P14", code_pref, code_muni, year4digit, data_dir)

  attr(sfLNI, "mapname") = "\u798f\u7949\u65bd\u8a2d"

  return(sfLNI)
}

#' Download spatial data of Hospitals of Japan
#'
#' @description
#' Function to download spatial data of Hospitals of Japan. The returned value is an sf object.
#'
#' The data is available for years 2020, 2014 and 2010.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Ignored.
#' @param year Year of the data. Defaults to 2021.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_hospital <- function(code_pref, code_muni = NULL, year = 2020, data_dir = NULL){
  year4digit = check_year(year)

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("P04", code_pref, NULL, year4digit, data_dir)

  if (!is.null(sfLNI)) {
    attr(sfLNI, "mapname") = "\u533b\u7642\u6a5f\u95a2"
    return(sfLNI)
  }
}

#' Download spatial data of Schools of Japan
#'
#' @description
#' Function to download spatial data of Schools of Japan. The returned value is an sf object.
#'
#' The data is available for years 2021 and 2013.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2021.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_school <- function(code_pref, code_muni = NULL, year = 2021, data_dir = NULL){
  year4digit = check_year(year)

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("P29", code_pref, code_muni, year4digit, data_dir)

  if (!is.null(sfLNI)) {
    attr(sfLNI, "mapname") = "\u5b66\u6821"
    return(sfLNI)
  }
}
