#
# Municipality Code Excel File:
# https://nlftp.mlit.go.jp/ksj/gml/codelist/AdminiBoundary_CD.xlsx
#

#' Check year format
#'
#' @description
#' Function to check and validate year
#'
#' @param x year in Gregorian or Japanese calendar (eg "R2" and "平成元年度")
#'
#' @return year in Gregorian calendar
#'
#' @export
check_year <- function(x) {
  intYear = 0
  if (mode(x) == "numeric") {
    intYear = x
  } else if (mode(x) == "character") {
    x = sub("年", "", x)
    x = sub("度", "", x)
    x = sub("元", "1", x)
    x = sub("令和", "R", x)
    x = sub("平成", "H", x)
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
    }
  if (intYear < 1995 || 2022 < intYear) {
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
#'
#'
#' @return the target filepath
#'
#' @export
find_geojson_file <- function(maptype, code_pref, code_muni, year, data_dir){
  strLNIFile = file.path(data_dir,
                         paste(maptype, "-", year, "_", code_pref, "_GML", sep = ""),
                         paste(code_pref, "_GeoJSON", sep = ""),
                         paste(maptype, "-", year, "_", code_pref, code_muni, ".geojson", sep = ""))
  if (!file.exists(strLNIFile)) {
    strLNIFile = file.path(data_dir,
                           paste(maptype, "-", year, "_", code_pref, sep = ""),
                           paste(code_pref, "_GeoJSON", sep = ""),
                           paste(maptype, "-", year, "_", code_pref, code_muni, ".geojson", sep = ""))
  }
  if (!file.exists(strLNIFile)) {
    strLNIFile = file.path(data_dir,
                           paste(maptype, "-", year, "_", code_pref, "_GML", sep = ""),
                           paste(maptype, "-", year, "_", code_pref, code_muni, ".geojson", sep = ""))
  }
  if (!file.exists(strLNIFile)) {
    strLNIFile = file.path(data_dir,
                           paste(maptype, "-", year, "_", code_pref, ".geojson", sep = ""))
  }
  if (!file.exists(strLNIFile)) {
    strLNIFile = Sys.glob(file.path(data_dir,
                                    paste(maptype, "-", year, "_", code_pref, "*", sep = ""),
                                    "*",
                                    paste(maptype, "-", year, "_", code_pref, code_muni, ".geojson", sep = "")))
  }
  if (length(strLNIFile) != 1) {
    strLNIFile = Sys.glob(file.path(data_dir,
                                    paste(maptype, "-", year, "_", code_pref, "*", sep = ""),
                                    "*", "*",
                                    paste(maptype, "-", year, "_", code_pref, code_muni, ".geojson", sep = "")))
  }
  if (length(strLNIFile) != 1) {
    strLNIFile = file.path(data_dir,
                           paste(maptype, "-", year, "_", code_pref, code_muni, ".geojson", sep = ""))
  }
  if (length(strLNIFile) != 1) stop(paste("Cannot find geosjon file in", data_dir))
  print(paste("Found a geojson file:", strLNIFile))

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
#'
#'
#' @return the target filepath
find_shp_file <- function(maptype, code_pref, code_muni, year, data_dir){
  strLNIFile = file.path(data_dir,
                         paste(maptype, "-", year, "_", code_pref, code_muni, ".shp", sep = ""))
  if (!file.exists(strLNIFile)) {
    strLNIFile = file.path(data_dir,
                           paste(maptype, "-", year, "_", code_pref, ".shp", sep = ""))
  }
  if (!file.exists(strLNIFile)) {
    strLNIFile = file.path(data_dir,
                           paste(maptype, "-", year, "_", code_pref, "_GML", sep = ""),
                           paste(maptype, "-", year, "_", code_pref, code_muni, ".shp", sep = ""))
  }
  if (!file.exists(strLNIFile)) {
    strLNIFile = file.path(data_dir,
                           paste(maptype, "-", year, "_", code_pref, sep = ""),
                           paste(maptype, "-", year, "_", code_pref, code_muni, ".shp", sep = ""))
  }
  if (!file.exists(strLNIFile)) {
    strLNIFile = Sys.glob(file.path(data_dir,
                                    paste(maptype, "-", year, "_", code_pref, "*", sep = ""),
                                    "*",
                                    paste(maptype, "-", year, "_", code_pref, code_muni, ".shp", sep = "")))
  }
  if (length(strLNIFile) != 1) {
    strLNIFile = Sys.glob(file.path(data_dir,
                                    paste(maptype, "-", year, "_", code_pref, "*", sep = ""),
                                    "*", "*",
                                    paste(maptype, "-", year, "_", code_pref, code_muni, ".shp", sep = "")))
  }
  if (length(strLNIFile) != 1) {
    strLNIFile = Sys.glob(file.path(data_dir,
                                    "*",
                                    paste(maptype, "-", year, "_", code_pref, code_muni, ".shp", sep = "")))
  }
  if (length(strLNIFile) != 1) {
    strLNIFile = Sys.glob(file.path(data_dir,
                                    paste(maptype, "-", year, "_", code_pref, "*", sep = ""),
                                    "*", "*",
                                    paste(maptype, "-", year, "_", code_pref, ".shp", sep = "")))
  }
  if (length(strLNIFile) != 1) {
    strLNIFile = Sys.glob(file.path(data_dir,
                                    "*",
                                    paste(maptype, "-", year, "_", code_pref, ".shp", sep = "")))
  }
  if (length(strLNIFile) != 1) stop(paste("Cannot find shp file in", data_dir))
  print(paste("Found a shp file:", strLNIFile))

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
epsg_Japan_Plane_Rectangular <- function(code_pref, code_muni = NULL, crs_type = "JGD2000") {

  epsg = 0
  crs_type = toupper(crs_type)
  if (crs_type != "CENSUS" & crs_type != "JGD2000" & crs_type != "JGD2011" & crs_type != "TOKYO") stop("Error: Invalid crs_type.")

  if (!is.numeric(code_pref)) stop("Can accept only one integer between 1 and 47.")
  if(length(code_pref) != 1) stop("Cannot accept list/vector.")
  if (code_pref == 1){
    #
    if (crs_type != "CENSUS" & is.null(code_muni)) stop("Error: Hokkaido requires code_muni.")

    if (crs_type == "CENSUS") {
      epsg = 2454
    } else if (code_muni == 202 || code_muni == 202 || code_muni == 233 || code_muni == 236) {
      # 小樽市　函館市　伊達市　北斗市
      epsg = 2453
    } else if (390	<= code_muni && code_muni <= 409) {
      # 北海道後志総合振興局の所管区域　
      epsg = 2453
    } else if (code_muni == 571 || code_muni == 575 || code_muni == 584) {
      # 北海道胆振総合振興局の所管区域のうち豊浦町、壮瞥町及び洞爺湖町
      epsg = 2453
    } else if (330	<= code_muni && code_muni <= 346) {
      # 北海道渡島総合振興局の所管区域
      epsg = 2453
    } else if (360	<= code_muni && code_muni <= 370) {
      # 北海道檜山振興局の所管区域
      epsg = 2453
    } else if (code_muni == 208 || code_muni == 207 || code_muni == 206 || code_muni == 211 || code_muni == 223) {
      # 北見市　帯広市　釧路市　網走市　根室市
      epsg = 2455
    } else if (code_muni == 543 || code_muni == 544 || code_muni == 545 || code_muni == 546 || code_muni == 547 || code_muni == 549 || code_muni == 550 || code_muni == 552 || code_muni == 564) {
      # 北海道オホーツク総合振興局の所管区域のうち美幌町、津別町、斜里町、清里町、小清水町、訓子府町、置戸町、佐呂間町及び大空町　北海道十勝総合振興局の所管区域
      epsg = 2455
    } else if (660	<= code_muni) {
      # 北海道釧路総合振興局の所管区域　北海道根室振興局の所管区域
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














