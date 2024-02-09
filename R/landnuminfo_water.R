#' Download spatial data of Coastal Lines of Japan
#'
#' @description
#' Function to download spatial data of Coastal Lines of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2006.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_coast <- function(code_pref, code_muni = NULL, year = 2006, data_dir = NULL){

  if (code_pref %in% c(9:11, 19:21, 25, 29)) stop(paste("The prefecture", code_pref, "has no coast!"))

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("C23", code_pref, code_muni, year, data_dir)

  attr(sfLNI, "mapname") <- "\u6d77\u5cb8\u7dda"

  return(sfLNI)
}

#' Download spatial data of Coastal Facilities (point) of Japan
#'
#' @description
#' Function to download spatial data of Coastal Facilities (point) of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2012.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_coastalfacility_point <- function(code_pref, code_muni = NULL, year = 2012, data_dir = NULL){

  if (code_pref %in% c(9:11, 19:21, 25, 29)) stop(paste("The prefecture", code_pref, "has no coast!"))

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("P23a", code_pref, code_muni, year, data_dir)

  attr(sfLNI, "mapname") <- "\u6d77\u5cb8\u4fdd\u5168\u65bd\u8a2d"

  return(sfLNI)
}

#' Download spatial data of Coastal Facilities (linestring) of Japan
#'
#' @description
#' Function to download spatial data of Coastal Facilities (linestring) of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2012.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_coastalfacility_line <- function(code_pref, code_muni = NULL, year = 2012, data_dir = NULL){

  if (code_pref %in% c(9:11, 19:21, 25, 29)) stop(paste("The prefecture", code_pref, "has no coast!"))

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("P23b", code_pref, code_muni, year, data_dir)

  attr(sfLNI, "mapname") <- "\u6d77\u5cb8\u4fdd\u5168\u65bd\u8a2d"

  return(sfLNI)
}

#' Download spatial data of Lakes of Japan
#'
#' @description
#' Function to download spatial data of Lakes of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2005.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_lake <- function(code_pref = NULL, code_muni = NULL, year = 2005, data_dir = NULL){

  # Dummy pref_code
  if (is.null(code_pref)) code_pref <- 13

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("W09", code_pref, code_muni, year, data_dir)

  attr(sfLNI, "mapname") <- "\u6e56\u6cbc"

  return(sfLNI)
}

#' Download spatial data of Vally of Japan by mesh
#'
#' @description
#' Function to download spatial data of Valley of Japan. The returned value is an sf object.
#' The function reads Valley Third Level Mesh data. See https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-L03-a.html.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2009.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @importFrom sf st_drop_geometry
#' @export
read_landnuminfo_valley <- function(code_pref, code_muni, year = 2009, data_dir = NULL){
  year4digit <- check_year(year)

  # Get the mesh code
  lstMesh1Codes <- get_mesh1_by_muni(code_pref, code_muni)

  sfLNI <- NULL
  if (length(lstMesh1Codes) >= 1) {
    for (code_mesh1 in lstMesh1Codes) {
      if (is.null(sfLNI)) {
        sfLNI <- read_landnuminfo_mesh_by_csv("W07", code_mesh1, year4digit, data_dir)
      } else {
        sfLNI2 <- NULL
        sfLNI2 <- read_landnuminfo_mesh_by_csv("W07", code_mesh1, year4digit, data_dir)
        if (!is.null(sfLNI2)) sfLNI <- rbind(sfLNI, sfLNI2)
      }
    }
    attr(sfLNI, "mapname") <- "\u6d41\u57df\u30e1\u30c3\u30b7\u30e5"
  } else {
    stop(paste("No city found for pref:", code_pref, ", city:", code_muni))
  }

  #sfLNI <- add_attr(sfLNI, "W07", year4digit)
  return(sfLNI)
}

#' Download spatial data of Dams of Japan
#'
#' @description
#' Function to download spatial data of Dams of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2014.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_dam <- function(code_pref = NULL, code_muni = NULL, year = 2014, data_dir = NULL){

  # Dummy pref_code
  if (is.null(code_pref)) code_pref <- 13

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("W01", code_pref, code_muni, year, data_dir)

  attr(sfLNI, "mapname") <- "\u30c0\u30e0"

  return(sfLNI)
}

#' Download spatial data of rivers of Japan
#'
#' @description
#' Function to download spatial data of rivers of Japan. The returned value is an sf object.
#'
#' Please note that the river data has a lot of invalid geometries.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Ignored. Defaults based on pref_code.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_river <- function(code_pref, code_muni = NULL, year = NULL, data_dir = NULL){
  # Ignore year argument
  num_code_pref = as.integer(code_pref)
  if (num_code_pref == 1
      || num_code_pref == 25
      || num_code_pref == 26
      || num_code_pref == 27
      || num_code_pref == 28
      || num_code_pref == 29
      || num_code_pref == 30) {
    year4digit <- 2009
  } else if (num_code_pref == 8
             || num_code_pref == 9
             || num_code_pref == 10
             || num_code_pref == 11
             || num_code_pref == 12
             || num_code_pref == 13
             || num_code_pref == 14
             || num_code_pref == 19
             || num_code_pref == 20
             || num_code_pref == 21
             || num_code_pref == 22
             || num_code_pref == 23
             || num_code_pref == 24
             || num_code_pref == 31
             || num_code_pref == 32
             || num_code_pref == 33
             || num_code_pref == 34
             || num_code_pref == 35
  ) {
    year4digit <- 2008
  } else if (num_code_pref == 2
             || num_code_pref == 3
             || num_code_pref == 4
             || num_code_pref == 5
             || num_code_pref == 6
             || num_code_pref == 7
             || num_code_pref == 15
             || num_code_pref == 16
             || num_code_pref == 17
             || num_code_pref == 18
             || num_code_pref == 40
             || num_code_pref == 41
             || num_code_pref == 42
             || num_code_pref == 43
             || num_code_pref == 44
             || num_code_pref == 45
             || num_code_pref == 46
             || num_code_pref == 47
             || num_code_pref == 48) {
    year4digit <- 2007
  } else if (num_code_pref == 36
             || num_code_pref == 37
             || num_code_pref == 38
             || num_code_pref == 39) {
    year4digit <- 2006
  } else {
    stop("Invalid pref_code.")
  }

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("W05", code_pref, NULL, year4digit, data_dir)

  if (!is.null(sfLNI)) {
    attr(sfLNI, "mapname") = "\u6cb3\u5ddd"
  }

  return(sfLNI)
}
