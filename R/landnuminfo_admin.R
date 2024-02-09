#' Download spatial data of Administrative Boundary of Japan
#'
#' @description
#' Function to download spatial data of Administrative Boundary of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2023.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_admin <- function(code_pref, code_muni = NULL, year = 2023, data_dir = NULL){

  # Administrative Boundaries data
  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("N03", code_pref, code_muni, year, data_dir)

  attr(sfLNI, "mapname") <- "\u884c\u653f\u533a\u57df"

  return(sfLNI)
}

#' Download spatial data of Junior High School Districts of Japan
#'
#' @description
#' Function to download spatial data of Junior High School Districts of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2021.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_jhighschool_district <- function(code_pref, code_muni = NULL, year = 2021, data_dir = NULL){

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A32", code_pref, code_muni, year, data_dir)

  attr(sfLNI, "mapname") <- "\u4e2d\u5b66\u6821\u533a"

  return(sfLNI)
}

#' Download spatial data of Elementary School Districts of Japan
#'
#' @description
#' Function to download spatial data of Elementary School Districts of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2021.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_elementaryschool_district <- function(code_pref, code_muni = NULL, year = 2021, data_dir = NULL){

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A27", code_pref, code_muni, year, data_dir)

  attr(sfLNI, "mapname") <- "\u5c0f\u5b66\u6821\u533a"

  return(sfLNI)
}

#' Download spatial data of First Medical Areas of Japan
#'
#' @description
#' Function to download spatial data of First Medical Areas of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Ignored.
#' @param year Year of the data. Defaults to 2020.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_first_medical_area <- function(code_pref, code_muni = NULL, year = 2020, data_dir = NULL){

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A38-1", code_pref, NULL, year, data_dir)

  attr(sfLNI, "mapname") <- "\u5c0f\u5b66\u6821\u533a"

  return(sfLNI)
}

#' Download spatial data of Second Medical Areas of Japan
#'
#' @description
#' Function to download spatial data of Second Medical Areas of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Ignored.
#' @param year Year of the data. Defaults to 2020.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_second_medical_area <- function(code_pref, code_muni = NULL, year = 2020, data_dir = NULL){

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A38-2", code_pref, NULL, year, data_dir)

  attr(sfLNI, "mapname") <- "\u5c0f\u5b66\u6821\u533a"

  return(sfLNI)
}

#' Download spatial data of Third Medical Areas of Japan
#'
#' @description
#' Function to download spatial data of Third Medical Areas of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Ignored.
#' @param year Year of the data. Defaults to 2020.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_third_medical_area <- function(code_pref, code_muni = NULL, year = 2020, data_dir = NULL){

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A38-3", code_pref, NULL, year, data_dir)

  attr(sfLNI, "mapname") <- "\u5c0f\u5b66\u6821\u533a"

  return(sfLNI)
}

#' Download spatial data of Landscape Plan Areas of Japan
#'
#' @description
#' Function to download spatial data of Landscape Plan Areas of Japan. The returned value is an sf object.
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
read_landnuminfo_landscapeplan_area <- function(code_pref, code_muni = NULL, year = 2014, data_dir = NULL){

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A35a", code_pref, code_muni, year, data_dir)

  attr(sfLNI, "mapname") <- "\u666f\u89b3\u8a08\u753b\u533a\u57df"

  return(sfLNI)
}

#' Download spatial data of Landscape Emphasis Areas of Japan
#'
#' @description
#' Function to download spatial data of Landscape Emphasis Areas of Japan. The returned value is an sf object.
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
read_landnuminfo_landscapeemphasis_area <- function(code_pref, code_muni = NULL, year = 2014, data_dir = NULL){

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A35c", code_pref, code_muni, year, data_dir)

  attr(sfLNI, "mapname") <- "\u666f\u89b3\u91cd\u70b9\u5730\u533a"

  return(sfLNI)
}

#' Download spatial data of Landscape Areas of Japan
#'
#' @description
#' Function to download spatial data of Landscape Areas of Japan. The returned value is an sf object.
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
read_landnuminfo_landscape_area <- function(code_pref, code_muni = NULL, year = 2014, data_dir = NULL){

  if (!(code_pref %in% c(1, 3, 4, 13, 14, 21, 22, 26, 28, 30, 32, 33, 34, 44, 46, 47))) stop(paste("The data is not available for prefecture", code_pref))

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A35d", code_pref, code_muni, year, data_dir)

  attr(sfLNI, "mapname") <- "\u666f\u89b3\u5730\u533a\u30fb\u6e96\u666f\u89b3\u5730\u533a"

  return(sfLNI)
}

#' Download spatial data of Landscape Area Zones of Japan
#'
#' @description
#' Function to download spatial data of Landscape Area Zones of Japan. The returned value is an sf object.
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
read_landnuminfo_landscape_zone <- function(code_pref, code_muni = NULL, year = 2014, data_dir = NULL){

  if (!(code_pref %in% c(1, 3, 4, 13, 14, 21, 22, 26, 28, 30, 32, 33, 34, 44, 46, 47))) stop(paste("The data is not available for prefecture", code_pref))

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A35f", code_pref, code_muni, year, data_dir)

  attr(sfLNI, "mapname") <- "\u666f\u89b3\u5730\u533a\u30fb\u6e96\u666f\u89b3\u5730\u533a"

  return(sfLNI)
}

#' Download spatial data of Landscape Buildings of Japan
#'
#' @description
#' Function to download spatial data of Landscape Buildings of Japan. The returned value is an sf object.
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
read_landnuminfo_landscape_building <- function(code_pref, code_muni = NULL, year = 2014, data_dir = NULL){

  if (!(code_pref %in% c(1, 2, 3, 6, 7, 8, 10, 11, 12, 13, 14, 17, 20, 21:25, 27, 28, 30, 33, 35, 39:43, 45, 46))) stop(paste("The data is not available for prefecture", code_pref))

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A35g", code_pref, code_muni, year, data_dir)

  attr(sfLNI, "mapname") <- "\u666f\u89b3\u91cd\u8981\u5efa\u9020\u7269"

  return(sfLNI)
}

#' Download spatial data of Landscape Trees of Japan
#'
#' @description
#' Function to download spatial data of Landscape Trees of Japan. The returned value is an sf object.
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
read_landnuminfo_landscape_tree <- function(code_pref, code_muni = NULL, year = 2014, data_dir = NULL){

  if (!(code_pref %in% c(6, 8, 11, 12, 13, 14, 17, 20, 21, 22, 24, 25, 26, 29, 32, 39, 43, 45, 46))) stop(paste("The data is not available for prefecture", code_pref))

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A35h", code_pref, code_muni, year, data_dir)

  attr(sfLNI, "mapname") <- "\u666f\u89b3\u91cd\u8981\u6a39\u6728"

  return(sfLNI)
}

#' Download spatial data of Preservation Areas of Historic Landscape of Japan
#'
#' @description
#' Function to download spatial data of Preservation Areas of Historic Landscape of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2018.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_preservationarea_historiclandscape <- function(code_pref = NULL, code_muni = NULL, year = 2018, data_dir = NULL){

  # Dummy pref_code
  if (is.null(code_pref)) code_pref <- 13

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A42", code_pref, NULL, year, data_dir)

  attr(sfLNI, "mapname") <- "\u6b74\u53f2\u7684\u98a8\u571f\u4fdd\u5b58\u533a\u57df"

  return(sfLNI)
}

#' Download spatial data of Special Preservation Areas of Historic Landscape of Japan
#'
#' @description
#' Function to download spatial data of Special Preservation Areas of Historic Landscape of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2018.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_specialpreservationarea_historiclandscape <- function(code_pref = NULL, code_muni = NULL, year = 2018, data_dir = NULL){

  # Dummy pref_code
  if (is.null(code_pref)) code_pref <- 13

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A42s", code_pref, NULL, year, data_dir)

  attr(sfLNI, "mapname") <- "\u6b74\u53f2\u7684\u98a8\u571f\u7279\u5225\u4fdd\u5b58\u5730\u533a"

  return(sfLNI)
}

#' Download spatial data of Preservation District of Histric Buildings of Japan
#'
#' @description
#' Function to download spatial data of Preservation District of Histric Buildings of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2018.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_preservationdistrict_historicbuildings <- function(code_pref = NULL, code_muni = NULL, year = 2018, data_dir = NULL){

  # Dummy pref_code
  if (is.null(code_pref)) code_pref <- 13

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A43", code_pref, code_muni, year, data_dir)

  attr(sfLNI, "mapname") <- ""

  return(sfLNI)
}

#' Download spatial data of Priority Areas of Plan for the Maintenance and Improvement of Historical Scenic Beauty of Japan
#'
#' @description
#' Function to download spatial data of Priority Areas of Plan for the Maintenance and Improvement of Historical Scenic Beauty of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2018.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_historicscenicbeauty <- function(code_pref = NULL, code_muni = NULL, year = 2018, data_dir = NULL){

  # Dummy pref_code
  if (is.null(code_pref)) code_pref <- 13

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A44", code_pref, code_muni, year, data_dir)

  attr(sfLNI, "mapname") <- "\u6b74\u53f2\u7684\u98a8\u81f4\u7dad\u6301\u5411\u4e0a\u8a08\u753b\u306e\u91cd\u70b9\u5730\u533a"

  return(sfLNI)
}
