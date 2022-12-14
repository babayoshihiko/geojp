#
# Municipality Code Excel File:
# https://nlftp.mlit.go.jp/ksj/gml/codelist/AdminiBoundary_CD.xlsx
#

#' Check year format
#'
#' @description
#' Function to check and validate year
#'
#' @param x year in Gregorian or Japanese calendar (eg "R2" and "\u5e73\u6210\u5143\u5e74\u5ea6")
#'
#' @return year in Gregorian calendar
#'
#' @export
check_year <- function(x) {
  intYear = 0
  if (mode(x) == "numeric") {
    intYear = x
  } else if (mode(x) == "character") {
    x = sub("\u5e74", "", x)
    x = sub("\u5ea6", "", x)
    x = sub("\u5143", "1", x)
    x = sub("\u4ee4\u548c", "R", x)
    x = sub("\u5e73\u6210", "H", x)
    x = sub("\u662d\u548c", "S", x)
    x = sub("\u5927\u6b63", "T", x)
    if (x == "R4") intYear = 2022
    if (x == "R3") intYear = 2021
    if (x == "R2") intYear = 2020
    if (x == "R1" || x == "H31") intYear = 2019
    if (x == "H30") intYear = 2018
    if (x == "H29") intYear = 2017
    if (x == "H28") intYear = 2016
    if (x == "H27") intYear = 2015
    if (x == "H26") intYear = 2014
    if (x == "H25") intYear = 2013
    if (x == "H24") intYear = 2012
    if (x == "H23") intYear = 2011
    if (x == "H22") intYear = 2010
    if (x == "H21") intYear = 2009
    if (x == "H20") intYear = 2008
    if (x == "H19") intYear = 2007
    if (x == "H18") intYear = 2006
    if (x == "H17") intYear = 2005
    if (x == "H16") intYear = 2004
    if (x == "H15") intYear = 2003
    if (x == "H14") intYear = 2002
    if (x == "H13") intYear = 2001
    if (x == "H12") intYear = 2000
    if (x == "H11") intYear = 1999
    if (x == "H10") intYear = 1998
    if (x == "H09" || x == "H9") intYear = 1997
    if (x == "H08" || x == "H8") intYear = 1996
    if (x == "H07" || x == "H7") intYear = 1995
    if (x == "H06" || x == "H6") intYear = 1994
    if (x == "H05" || x == "H5") intYear = 1993
    if (x == "H04" || x == "H4") intYear = 1992
    if (x == "H03" || x == "H3") intYear = 1991
    if (x == "H02" || x == "H2") intYear = 1990
    if (x == "H01" || x == "H1" || x == "S64") intYear = 1989
    if (x == "S63") intYear = 1988
    if (x == "S62") intYear = 1987
    if (x == "S6`") intYear = 1986
    if (x == "S60") intYear = 1985
    if (x == "S59") intYear = 1984
    if (x == "S58") intYear = 1983
    if (x == "S55") intYear = 1980
    if (x == "S50") intYear = 1975
    if (x == "S45") intYear = 1970
    if (x == "S40") intYear = 1965
    if (x == "S35") intYear = 1960
    if (x == "S30") intYear = 1955
    if (x == "S25") intYear = 1950
    if (x == "T9" || x == "T09") intYear = 1920
  }
  if (intYear < 1983 || 2022 < intYear) {
    stop("Invalid year.")
  }

  return (intYear)
}

#' Find the target geojson file
#'
#' @description
#' Function to find the target geojson file in data_dir.
#'
#' @param maptype landnuminfo maptype (eg "A01" and "P01").
#' @param code_pref The 2-digit code of a prefecture.
#' @param code_muni The 3-digit code of a municipality (city, town, or village).
#' @param year Year of the data. Defaults to 2012.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#' @param maptypeextra File name suffix.
#'
#' @return the target filepath
#'
#' @export
find_geojson_file <- function(maptype, code_pref, code_muni, year, data_dir, maptypeextra = ""){
  strTypicalFolderName = paste(maptype, "-", year, "_", code_pref, sep = "")
  strLNIFile = file.path(data_dir,
    paste(strTypicalFolderName, "_GML", sep = ""),
    paste(code_pref, "_GeoJSON", sep = ""),
    paste(strTypicalFolderName, code_muni, maptypeextra, ".geojson", sep = ""))
  if (!file.exists(strLNIFile)) {
    strLNIFile = file.path(data_dir,
      paste(strTypicalFolderName, sep = ""),
      paste(code_pref, "_GeoJSON", sep = ""),
      paste(strTypicalFolderName, code_muni, maptypeextra, ".geojson", sep = ""))
  }
  if (!file.exists(strLNIFile)) {
    strLNIFile = file.path(data_dir,
      paste(strTypicalFolderName, "_GML", sep = ""),
      paste(strTypicalFolderName, code_muni, maptypeextra, ".geojson", sep = ""))
  }
  if (!file.exists(strLNIFile)) {
    strLNIFile = file.path(data_dir,
      paste(strTypicalFolderName, maptypeextra, ".geojson", sep = ""))
  }
  if (!file.exists(strLNIFile)) {
    strLNIFile = Sys.glob(file.path(data_dir,
      paste("*", strTypicalFolderName, code_muni, maptypeextra, "*.geojson", sep = "")))
  }
  if (length(strLNIFile) != 1) {
    strLNIFile = Sys.glob(file.path(data_dir,
      paste(strTypicalFolderName, "*", sep = ""),
      "*",
      paste("*", strTypicalFolderName, code_muni, maptypeextra, "*.geojson", sep = "")))
  }
  if (length(strLNIFile) != 1) {
    strLNIFile = Sys.glob(file.path(data_dir,
      paste(strTypicalFolderName, "*", sep = ""),
      "*", "*",
      paste("*", strTypicalFolderName, code_muni, maptypeextra, "*.geojson", sep = "")))
  }
  if (length(strLNIFile) != 1) {
    strLNIFile = file.path(data_dir,
      paste("*", strTypicalFolderName, code_muni, maptypeextra, "*.geojson", sep = ""))
  }
  if (length(strLNIFile) != 1) stop(paste("Cannot find geosjon file in", data_dir))
  message(paste("Found a geojson file:", strLNIFile))

  return(strLNIFile)
}

#' Find the target shp file
#'
#' @description
#' Function to find the target shp file in data_dir.
#'
#' @param maptype landnuminfo maptype (eg "A01" and "P01").
#' @param code_pref The 2-digit code of a prefecture.
#' @param code_muni The 3-digit code of a municipality (city, town, or village).
#' @param year Year of the data. Defaults to 2012.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#' @param maptypeextra File name suffix.
#'
#' @return the target filepath
find_shp_file <- function(maptype, code_pref, code_muni, year, data_dir, maptypeextra = ""){
  strTypicalFolderName = paste(maptype, "-", year, "_", code_pref, sep = "")
  strLNIFile = file.path(data_dir,
    paste(strTypicalFolderName, code_muni, maptypeextra, ".shp", sep = ""))
  if (!file.exists(strLNIFile)) {
    strLNIFile = file.path(data_dir,
      paste(strTypicalFolderName, maptypeextra, ".shp", sep = ""))
  }
  if (!file.exists(strLNIFile)) {
    strLNIFile = file.path(data_dir,
      paste(strTypicalFolderName, "_GML", sep = ""),
      paste(strTypicalFolderName, maptypeextra, code_muni, ".shp", sep = ""))
  }
  if (!file.exists(strLNIFile)) {
    strLNIFile = file.path(data_dir,
      paste(strTypicalFolderName, sep = ""),
      paste(strTypicalFolderName, code_muni, maptypeextra, ".shp", sep = ""))
  }
  if (!file.exists(strLNIFile)) {
    strLNIFile = Sys.glob(file.path(data_dir,
      paste("*", strTypicalFolderName, code_muni, maptypeextra, "*.shp", sep = "")))
  }
  if (length(strLNIFile) != 1) {
    strLNIFile = Sys.glob(file.path(data_dir,
      paste(strTypicalFolderName, "*", sep = ""),
      "*",
      paste("*", strTypicalFolderName, code_muni, maptypeextra, "*.shp", sep = "")))
  }
  if (length(strLNIFile) != 1) {
    strLNIFile = Sys.glob(file.path(data_dir,
      paste(strTypicalFolderName, "*", sep = ""),
      "*", "*",
      paste("*", strTypicalFolderName, code_muni, maptypeextra, "*.shp", sep = "")))
  }
  if (length(strLNIFile) != 1) {
    strLNIFile = Sys.glob(file.path(data_dir,
      "*",
      paste("*", strTypicalFolderName, code_muni, maptypeextra, "*.shp", sep = "")))
  }
  if (length(strLNIFile) != 1) {
    strLNIFile = Sys.glob(file.path(data_dir,
      paste(strTypicalFolderName, "*", sep = ""),
      "*", "*",
      paste(strTypicalFolderName, maptypeextra, "*.shp", sep = "")))
  }
  if (length(strLNIFile) != 1) {
    strLNIFile = Sys.glob(file.path(data_dir,
      "*",
      paste(strTypicalFolderName, maptypeextra, "*.shp", sep = "")))
  }
  if (length(strLNIFile) != 1) stop(paste("Cannot find shp file in", data_dir))
  message(paste("Found a shp file:", strLNIFile))

  return(strLNIFile)
}

#' EPSG code of Japan Plane Rectangular
#'
#' @description
#' Function to get the correct EPSG code from prefecture/municipality
#'
#' @param code_pref The 2-digit code of a prefecture.
#' @param code_muni The 3-digit code of a municipality (city, town, or village).
#' @param crs_type Defaults to "JGD2000." Other options are "JGD2011" or "Census." "Census" is almost identical to "JGD2000," but slightly different in Hokkaido, Nagasaki and Kagoshima.
#'
#' @return an EPSG code
#'
#' @export
epsg_Japan_Plane_Rectangular <- function(code_pref, code_muni = NULL, crs_type = "JGD2011") {

  epsg = 0
  crs_type = toupper(crs_type)
  if (crs_type != "CENSUS" & crs_type != "JGD2000" & crs_type != "JGD2011" & crs_type != "TOKYO") stop("Error: Invalid crs_type.")

  if (!is.numeric(code_pref)) stop("Can accept only one integer between 1 and 47.")
  if(length(code_pref) != 1) stop("Cannot accept list/vector.")
  if (code_pref == 1){
    # Hokkaido Island
    if (crs_type != "CENSUS" & is.null(code_muni)) stop("Error: Hokkaido requires code_muni.")

    if (crs_type == "CENSUS") {
      epsg = 2454
    } else if (code_muni == 202 || code_muni == 202 || code_muni == 233 || code_muni == 236) {
      # Otaru, Hakodate, Date, Hokuto
      epsg = 2453
    } else if (390	<= code_muni && code_muni <= 409) {
      # Shiribeshi region
      epsg = 2453
    } else if (code_muni == 571 || code_muni == 575 || code_muni == 584) {
      # Toyoura, Sobetsu and Toyako
      epsg = 2453
    } else if (330	<= code_muni && code_muni <= 346) {
      # Oshima region
      epsg = 2453
    } else if (360	<= code_muni && code_muni <= 370) {
      # Hiyama region
      epsg = 2453
    } else if (code_muni == 208 || code_muni == 207 || code_muni == 206 || code_muni == 211 || code_muni == 223) {
      # Kitami, Obihiro, Kushiro, Abashiri and Numero
      epsg = 2455
    } else if (code_muni == 543 || code_muni == 544 || code_muni == 545 || code_muni == 546 || code_muni == 547 || code_muni == 549 || code_muni == 550 || code_muni == 552 || code_muni == 564) {
      # Mihoro, Tsubetsu, Shari, Kiyosato, Koshimizu, Kunneppu, Oketo, Saroma, Ozora. Tokachi region.
      epsg = 2455
    } else if (660	<= code_muni) {
      # Kushiro region and Nemuro region
      epsg = 2455
    } else {
      epsg = 2454
    }
  }
  if (2 <= code_pref & code_pref <= 6) epsg = 2452
  if (7 <= code_pref & code_pref <= 14) epsg = 2451
  if (code_pref == 15 || code_pref == 19 || code_pref == 20 || code_pref == 22) epsg = 2450
  if (code_pref == 16 || code_pref == 17 || code_pref == 21 || code_pref == 23) epsg = 2449
  if (code_pref == 18 || code_pref == 24 || code_pref == 25 || code_pref == 26 || code_pref == 27 || code_pref == 29 || code_pref == 30) epsg = 2448
  if (code_pref == 28 || code_pref == 31 || code_pref == 33) epsg = 2447
  if (36 <= code_pref & code_pref <= 39) epsg = 2446
  if (code_pref == 32 || code_pref == 34 || code_pref == 35) epsg = 2445
  if (code_pref == 40 || code_pref == 41 || code_pref == 43 || code_pref == 44 || code_pref == 45) epsg = 2444
  if (code_pref == 42) epsg = 2443
  if (code_pref == 46) epsg = 2444 # Requires fix
  if (code_pref == 47) epsg = 2457

  if(epsg == 0) stop("Invalid code_pref or code_muni.")
  if (crs_type == "JGD2011") epsg = epsg + 4226
  if (crs_type == "TOKYO") epsg = epsg + 27718
  return(epsg)
}














