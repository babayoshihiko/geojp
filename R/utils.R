#
# Municipality Code Excel File:
# https://nlftp.mlit.go.jp/ksj/gml/codelist/AdminiBoundary_CD.xlsx
#


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

  return(strLNIFile)
}

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
  if (length(strLNIFile) != 1) stop(paste("Cannot find shp file in", data_dir))

  return(strLNIFile)
}
