#' Download Underpopulated Areas of Japan
#'
#' @description
#' Function to download spatial data of Underpopulated Areas of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2017.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_underpopulated <- function(code_pref, code_muni = NULL, year = 2017, data_dir = NULL){
  year4digit <- check_year(year)

  if (!(code_pref %in% c(1:13,15:47))) stop(paste("The data is not available for prefecture", code_pref))
  if (code_pref == 27 & year4digit < 2016) {
      stop(paste("The data is not available for prefecture", code_pref, "for year", year4digit))
  }
  if (code_pref == 47 & year4digit <= 1975){
    stop(paste("The data is not available for prefecture", code_pref, "for year", year4digit))
  }

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A17", code_pref, code_muni, year4digit, data_dir)

  if (!is.null(sfLNI)) {
    attr(sfLNI, "mapname") <- "\u904e\u758e\u5730\u57df"
    return(sfLNI)
  }
}

#' Download Peninsulas Development Implementation Areas of Japan
#'
#' @description
#' Function to download spatial data of Peninsulas Development Implementation Areas of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2016.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_peninsulas_development_implementation <- function(code_pref, code_muni = NULL, year = 2016, data_dir = NULL){
  year4digit <- check_year(year)

  if (!(code_pref %in% c(1,2,5,12,16,17,22,24,26,29,30,32,34,35,38,39,41:46))) stop(paste("The data is not available for prefecture", code_pref))
  if (code_pref == 5 & year4digit == 1986) stop(paste("The data is not available for prefecture", code_pref, "for year 1986"))
  if (code_pref == 39 & year4digit == 1986) stop(paste("The data is not available for prefecture", code_pref, "for year 1986"))
  if (code_pref == 41 & year4digit == 1986) stop(paste("The data is not available for prefecture", code_pref, "for year 1986"))

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A18", code_pref, code_muni, year4digit, data_dir)

  if (!is.null(sfLNI)) {
    attr(sfLNI, "mapname") <- "\u534a\u5cf6\u632f\u8208\u5bfe\u7b56\u5b9f\u65bd\u5730\u57df"
    return(sfLNI)
  }
}

#' Download Remote Island Development Implementation Areas of Japan
#'
#' @description
#' Function to download spatial data of Remote Island Development Implementation Areas of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2017.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_remote_island_development_implementation <- function(code_pref, code_muni = NULL, year = 2017, data_dir = NULL){
  year4digit <- check_year(year)

  if (!(code_pref %in% c(1,4,6,13,15,17,22:24,28,30,32:46))) stop(paste("The data is not available for prefecture", code_pref))
  if (code_pref == 22 & year4digit < 1965) stop(paste("The data is not available for prefecture", code_pref, "for year", year4digit))
  if (code_pref == 23 & year4digit < 1960) stop(paste("The data is not available for prefecture", code_pref, "for year", year4digit))
  if (code_pref == 30 & year4digit > 2000) stop(paste("The data is not available for prefecture", code_pref, "for year", year4digit))
  if (code_pref == 33 & year4digit < 1960) stop(paste("The data is not available for prefecture", code_pref, "for year", year4digit))
  if (code_pref == 34 & year4digit < 1960) stop(paste("The data is not available for prefecture", code_pref, "for year", year4digit))
  if (code_pref == 36 & year4digit < 1960) stop(paste("The data is not available for prefecture", code_pref, "for year", year4digit))
  if (code_pref == 37 & year4digit < 1960) stop(paste("The data is not available for prefecture", code_pref, "for year", year4digit))
  if (code_pref == 44 & year4digit < 1960) stop(paste("The data is not available for prefecture", code_pref, "for year", year4digit))
  if (code_pref == 45 & year4digit < 1960) stop(paste("The data is not available for prefecture", code_pref, "for year", year4digit))

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A19", code_pref, code_muni, year4digit, data_dir)

  if (!is.null(sfLNI)) {
    attr(sfLNI, "mapname") <- "\u96e2\u5cf6\u632f\u8208\u5bfe\u7b56\u5b9f\u65bd\u5730\u57df"
    return(sfLNI)
  }
}

#' Download Heavy Snowfall of Japan
#'
#' @description
#' Function to download spatial data of Heavy Snowfall Areas of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2016.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_heavy_snowfall <- function(code_pref, code_muni = NULL, year = 2016, data_dir = NULL){
  year4digit <- check_year(year)

  if (!(code_pref %in% c(1:7,9:10,15:22,25,26,28,31:34))) stop(paste("The data is not available for prefecture", code_pref))
  if (code_pref == 22 & year4digit < 1960) stop(paste("The data is not available for prefecture", code_pref, "for year", year4digit))

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A22", code_pref, code_muni, year4digit, data_dir)

  if (!is.null(sfLNI)) {
    attr(sfLNI, "mapname") <- "\u8c6a\u96ea\u5730\u5e2f"
    return(sfLNI)
  }
}

#' Download Particular Soil Zones of Japan
#'
#' @description
#' Function to download spatial data of Particular Soil Zones of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2016.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_particular_soil <- function(code_pref, code_muni = NULL, year = 2016, data_dir = NULL){
  year4digit <- check_year(year)

  if (!(code_pref %in% c(22,28,31:35,38:40,43:46))) stop(paste("The data is not available for prefecture", code_pref))

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A23", code_pref, code_muni, year4digit, data_dir)

  if (!is.null(sfLNI)) {
    attr(sfLNI, "mapname") <- "\u7279\u6b8a\u571f\u58cc\u5730\u5e2f"
    return(sfLNI)
  }
}

#' Download Development Mountain Villages of Japan
#'
#' @description
#' Function to download spatial data of Development Mountain Villages of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2016.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_development_mountain_village <- function(code_pref, code_muni = NULL, year = 2016, data_dir = NULL){
  year4digit <- check_year(year)

  if (!(code_pref %in% c(1:26,28:41,43:46))) stop(paste("The data is not available for prefecture", code_pref))
  if (code_pref == 13 & year4digit < 1970) stop(paste("The data is not available for prefecture", code_pref, "for year", year4digit))
  if (year == 1966) {
    if (!(code_pref %in% c(1:11,15:26,28:36,38:40,43:45))) stop(paste("The data is not available for prefecture", code_pref, "for year 1966"))
  }

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A24", code_pref, code_muni, year4digit, data_dir)

  if (!is.null(sfLNI)) {
    attr(sfLNI, "mapname") <- "\u632f\u8208\u5c71\u6751"
    return(sfLNI)
  }
}

#' Download Designated Rural Areas of Japan
#'
#' @description
#' Function to download spatial data of Designated Rural Areas of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2016.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_designated_rural <- function(code_pref, code_muni = NULL, year = 2016, data_dir = NULL){
  year4digit <- check_year(year)

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A25", code_pref, code_muni, year4digit, data_dir)

  if (!is.null(sfLNI)) {
    attr(sfLNI, "mapname") <- "\u7279\u5b9a\u8fb2\u5c71\u6751\u5730\u57df"
    return(sfLNI)
  }
}


