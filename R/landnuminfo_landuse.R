#' Download spatial data of Forest Area of Japan
#'
#' @description
#' Function to download spatial data of Forest Area. The returned value is an sf object with extra
#' attr "col" and "palette". The "col" is the factored column that indicate the land use classes
#' and "palette" provides the colour palette based on Japan Industrial Standard.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Ignored.
#' @param year Year of the data. Defaults to 2006.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_forestarea <- function(code_pref, code_muni = NULL, year = 2006, data_dir = NULL){
  year4digit <- check_year(year)

  if (year4digit == 2015) warning("The zip files for year 2015 are very large. Download may fail.")
  if (year4digit == 2011 & code_pref == 1) stop("The year 2011 for Hokkaido is not supported.")

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A13", code_pref, code_muni, year4digit, data_dir)

  if (!is.null(sfLNI)){
    attr(sfLNI, "mapname") = "\u68ee\u6797\u5730\u57df"
  }

  return(sfLNI)
}

#' Download spatial data of Forest Area (State Owned) of Japan
#'
#' @description
#' Function to download spatial data of Forest Area (State Owned). The returned value is an sf object with extra
#' attr "col" and "palette". The "col" is the factored column that indicate the land use classes
#' and "palette" provides the colour palette based on Japan Industrial Standard.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Ignored.
#' @param year Year of the data. Defaults to 2011.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_forestarea_stateowned <- function(code_pref, code_muni = NULL, year = 2011, data_dir = NULL){
  year4digit <- check_year(year)

  if (year4digit == 2015) warning("The zip files for year 2015 are very large. Download may fail.")
  if (year4digit == 2011 & code_pref == 1) stop("The year 2011 for Hokkaido is not supported.")

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A13-08", code_pref, code_muni, year4digit, data_dir)

  if (!is.null(sfLNI)){
    attr(sfLNI, "mapname") = "\u56fd\u6709\u6797"
  }

  return(sfLNI)
}

#' Download spatial data of Forest Area (Private Owned) of Japan
#'
#' @description
#' Function to download spatial data of Forest Area (Private Owned). The returned value is an sf object with extra
#' attr "col" and "palette". The "col" is the factored column that indicate the land use classes
#' and "palette" provides the colour palette based on Japan Industrial Standard.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Ignored.
#' @param year Year of the data. Defaults to 2011.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_forestarea_privateowned <- function(code_pref, code_muni = NULL, year = 2011, data_dir = NULL){
  year4digit <- check_year(year)

  if (year4digit == 2015) warning("The zip files for year 2015 are very large. Download may fail.")
  if (year4digit == 2011 & code_pref == 1) stop("The year 2011 for Hokkaido is not supported.")

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A13-09", code_pref, code_muni, year4digit, data_dir)

  if (!is.null(sfLNI)){
    attr(sfLNI, "mapname") = "\u5730\u57df\u68ee\u6797\u8a08\u753b\u5bfe\u8c61\u6c11\u6709\u6797"
  }

  return(sfLNI)
}

#' Download spatial data of Forest Area (Reserve) of Japan
#'
#' @description
#' Function to download spatial data of Forest Area (Reserve). The returned value is an sf object with extra
#' attr "col" and "palette". The "col" is the factored column that indicate the land use classes
#' and "palette" provides the colour palette based on Japan Industrial Standard.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Ignored.
#' @param year Year of the data. Defaults to 2011.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_forestarea_reserve <- function(code_pref, code_muni = NULL, year = 2011, data_dir = NULL){
  year4digit <- check_year(year)

  if (year4digit == 2015) warning("The zip files for year 2015 are very large. Download may fail.")
  if (year4digit == 2011 & code_pref == 1) stop("The year 2011 for Hokkaido is not supported.")

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A13-10", code_pref, code_muni, year4digit, data_dir)

  if (!is.null(sfLNI)){
    attr(sfLNI, "mapname") = "\u4fdd\u5b89\u6797"
  }

  return(sfLNI)
}

#' Download spatial data of Land Use of Japan
#'
#' @description
#' Function to download spatial data of Land Use. The returned value is an sf object with extra
#' attr "col" and "palette". The "col" is the factored column that indicate the land use classes
#' and "palette" provides the colour palette based on Japan Industrial Standard.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni The 3-digit code of municipality (city, town, or village).
#' @param year Year of the data. Defaults to 2019.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_landuse <- function(code_pref, code_muni = NULL, year = 2019, data_dir = NULL){
  year4digit <- check_year(year)

  sfLNI <- NULL

  if (year4digit == 2019 & is.null(code_muni) & code_pref == 13) {
    # The year 2019 is very unique. Tokyo has A29-19_code_pref000.shp.
    code_muni = 0
  }

  sfLNI <- read_landnuminfo_by_csv("A29", code_pref, code_muni, year4digit, data_dir)

  if (!is.null(sfLNI)){
    attr(sfLNI, "mapname") = "\u7528\u9014\u5730\u57df"
  }

  return(sfLNI)
}

#' Download spatial data of Location Normalization of Japan
#'
#' @description
#' Function to download spatial data of Location Normalization. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni The 3-digit code of municipality (city, town, or village).
#' @param year Year of the data. Defaults to 2020.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_locnorm <- function(code_pref, code_muni, year = 2020, data_dir = NULL){
  year4digit <- check_year(year)

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A50", code_pref, code_muni, year4digit, data_dir, encoding = "UTF-8")

  if (!is.null(sfLNI)){
    attr(sfLNI, "mapname") = "\u7acb\u5730\u9069\u6b63\u5316\u8a08\u753b\u533a\u57df"
    return(sfLNI)
  }
}

#' Download spatial data of Urbanized Areas of Japan
#'
#' @description
#' Function to download spatial data of Urbanized Areas of Japan. The returned value is an sf object. The license for year 2018 is open; or non-commercial for 2011 and 2006.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality. If specified, subtract the data by the column A48_003.
#' @param year Year of the data. The available years are 2018, 2011 and 2006. Defaults to 2018.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" for tmap.
#'
#' @export
read_landnuminfo_urbanarea <- function(code_pref,
                                       code_muni,
                                       year = 2018,
                                       data_dir = NULL){
  year4digit = check_year(year)
  if (year4digit == 2011 & code_pref == 1) stop("The year 2011 for Hokkaido is not supported.")
  if (year4digit == 2018 && code_pref == 23) {
    year4digit = 2011
    warning("The data is not available for year 2018 for Aichi prefcture. Uses year 2011 instead.")
  }

  if (year4digit == 2011) {
    sfLNI <- read_landnuminfo_urbanarea_2011(code_pref, data_dir = data_dir)
  } else {
    sfLNI <- read_landnuminfo_by_csv("A09", code_pref, code_muni, year4digit, data_dir = data_dir)
  }

  sfLNI$layer = NA
  sfLNI$layer = factor(sfLNI$layer, levels = c("\u5e02\u8857\u5316\u533a\u57df",
                                               "\u5e02\u8857\u5316\u8abf\u6574\u533a\u57df",
                                               "\u305d\u306e\u4ed6\u7528\u9014\u5730\u57df",
                                               "\u7528\u9014\u672a\u8a2d\u5b9a",
                                               "\u90fd\u5e02\u5730\u57df"))
  # 2011
  if (year4digit == 2011) {
    sf::st_crs(sfLNI) <- "EPSG:4612"
    sfLNI[sfLNI$layer_no == 1, "layer"] <- "\u90fd\u5e02\u5730\u57df"
    sfLNI[sfLNI$layer_no == 2, "layer"] <- "\u5e02\u8857\u5316\u533a\u57df"
    sfLNI[sfLNI$layer_no == 3, "layer"] <- "\u5e02\u8857\u5316\u8abf\u6574\u533a\u57df"
    sfLNI[sfLNI$layer_no == 4, "layer"] <- "\u305d\u306e\u4ed6\u7528\u9014\u5730\u57df"
  }

  return(sfLNI)
}

#' @importFrom utils download.file
#' @importFrom sf read_sf
#' @importFrom sf st_collection_extract
#' @importFrom sf st_make_valid
read_landnuminfo_urbanarea_2011 <- function(code_pref, data_dir = NULL){
  strTempDir = check_data_dir(data_dir)
  year4digit = 2011

  if (mode(code_pref) == "numeric"){
    if (code_pref < 0 || code_pref > 47) {
      stop("Invalid argument: code_pref must be between 1 and 47.")
    } else if (code_pref < 10) {
      code_pref = paste("0", as.character(code_pref), sep = "")
    } else {
      code_pref = as.character(code_pref)
    }
  }
  if (nchar(code_pref) != 2) stop(paste("Invalid argument: code_pref:", code_pref))

  strLNIZip = file.path(strTempDir,
                        paste("A09-11_", code_pref, "_GML.zip", sep = ""))
  strLNIUrl = paste("https://nlftp.mlit.go.jp/ksj/gml/data/A09/A09-11/A09-11_", code_pref, "_GML.zip", sep = "")

  if (!file.exists(strLNIZip)) {
    utils::download.file(strLNIUrl, strLNIZip, mode="wb")
    message(paste("Downloaded the file and saved in", strLNIUrl))
  }
  unzip_ja(strLNIZip, exdir = strTempDir)

  # There should be four SHP files as:
  # "a001" code_pref "002012030" layer_no ".shp"
  strLNI1 = file.path(strTempDir,
                      paste("A09-11_", code_pref, "_GML", sep = ""),
                      paste("a001", code_pref, "0020120301.shp", sep = ""))
  strLNI2 = file.path(strTempDir,
                      paste("A09-11_", code_pref, "_GML", sep = ""),
                      paste("a001", code_pref, "0020120302.shp", sep = ""))
  strLNI3 = file.path(strTempDir,
                      paste("A09-11_", code_pref, "_GML", sep = ""),
                      paste("a001", code_pref, "0020120303.shp", sep = ""))
  strLNI4 = file.path(strTempDir,
                      paste("A09-11_", code_pref, "_GML", sep = ""),
                      paste("a001", code_pref, "0020120304.shp", sep = ""))

  sfLNI1 = sf::read_sf(strLNI1, options = "ENCODING=CP932", stringsAsFactors=FALSE)
  sfLNI2 = sf::read_sf(strLNI2, options = "ENCODING=CP932", stringsAsFactors=FALSE)
  sfLNI3 = sf::read_sf(strLNI3, options = "ENCODING=CP932", stringsAsFactors=FALSE)
  sfLNI4 = sf::read_sf(strLNI4, options = "ENCODING=CP932", stringsAsFactors=FALSE)

  sfLNI = rbind(sfLNI1, sfLNI2, sfLNI3, sfLNI4)
  sfLNI = sf::st_collection_extract(sfLNI, type = "POLYGON")
  sfLNI = sf::st_make_valid(sfLNI)
  attr(sfLNI, "year") = year4digit

  return(sfLNI)
}
