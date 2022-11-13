#' Download spatial data of National Lang Numeric Information of Japan
#'
#' @description
#' Low level function to download spatial data of National Lang Numeric Information
#' (or kokudo suuchi joho in Japanese) of Japan.
#'
#' @param maptype The map type code (e.g. "A01").
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni The 3-digit code of municipality (city, town, or village).
#' @param year Year of the data. Defaults to 2020.
#' @param filetype either "geojson" or "shp".
#' @param geometry "POINT", "LINESTRING", or "POLYGON"
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#' @param maptypeextra File name suffix.
#'
#' @return An `"sf" "data.frame"` object
#'
#' @export
read_landnuminfo <- function(maptype, code_pref, code_muni = NULL, year = 2020,
                             filetype = "geojson",
                             geometry = NULL,
                             data_dir = NULL,
                             maptypeextra = ""){
  strTempDir = tempdir()
  year4digit = year
  if (!is.null(data_dir)) {
    if (dir.exists(data_dir)) {
      strTempDir = data_dir
    }
  }

  if (mode(code_pref) == "numeric"){
    if (code_pref < 0 || code_pref > 47) {
      stop("Invalid argument: code_pref must be between 1 and 47.")
    } else if (code_pref < 10) {
      code_pref = paste("0", as.character(code_pref), sep = "")
    } else {
      code_pref = as.character(code_pref)
    }
  }
  if (mode(code_muni) == "numeric"){
    if (code_muni < 100 || code_muni > 700) {
      stop("Invalid argument: code_muni must be between 100 and 700.")
    } else {
      code_muni = as.character(code_muni)
    }
  }
  if (is.null(code_muni)) code_muni = ""
  if (mode(year) == "numeric"){
    if (year > 2022 || year < 1983) {
      stop("Invalid argument: year must between 2022 and 1983")
    }
    if (year > 2009) {
      year = as.character(as.integer(year) - 2000)
    } else if (year > 1999) {
      year = paste("0", as.character(as.integer(year) - 2000), sep = "")
    } else {
      # year 19xx, for L01 (official land price) and N05 (administrative boundary)
      year = as.character(as.integer(year) - 1900)
    }
  }
  if (nchar(code_pref) != 2) stop(paste("Invalid argument: code_pref:", code_pref))
  if (nchar(year) != 2) stop(paste("Invalid argument: year:", year))

  strLNIZip = file.path(strTempDir,
                        paste(maptype, "-", year, "_", code_pref, "_GML.zip", sep = ""))
  strLNIUrl = paste("https://nlftp.mlit.go.jp/ksj/gml/data/", maptype, "/", maptype, "-", year, "/", maptype, "-", year, "_", code_pref, "_GML.zip", sep = "")

  if (!file.exists(strLNIZip)) {
    utils::download.file(strLNIUrl, strLNIZip, mode="wb")
    print(paste("Downloaded the file and saved in", strLNIUrl))
  }

  if (filetype == "geojson") {
    GEOJSONFILE = paste(maptype,"-",year,"_",code_pref,code_muni,maptypeextra,".geojson",sep="")
    utils::unzip(strLNIZip, files = GEOJSONFILE, exdir = strTempDir)
    if (code_muni != "") {
      GEOJSONFILE = paste(maptype,"-",year,"_",code_pref,maptypeextra,".geojson",sep="")
      utils::unzip(strLNIZip, files = GEOJSONFILE, exdir = strTempDir)
    }
    GEOJSONFILE = file.path(paste(maptype,"-",year,"_",code_pref,"_GML",sep=""),
                                  GEOJSONFILE)
    utils::unzip(strLNIZip, files = GEOJSONFILE, exdir = strTempDir)
    strLNIFile = find_geojson_file(maptype, code_pref, code_muni, year, strTempDir, maptypeextra)
    if (!file.exists(strLNIFile)){
      utils::unzip(strLNIZip, exdir = strTempDir)
      strLNIFile = find_geojson_file(maptype, code_pref, code_muni, year, strTempDir, maptypeextra)
    }
    sfLNI = sf::read_sf(strLNIFile)
  } else if (filetype == "shp") {
    # Avoid the unzip error due to Japanese filenames
    # This error happens with maptype = "A31", code_pref = 4, code_muni = 215
    SHPFiles = c( paste(maptype,"-",year,"_",code_pref,maptypeextra,".shp",sep=""),
                  paste(maptype,"-",year,"_",code_pref,maptypeextra,".shx",sep=""),
                  paste(maptype,"-",year,"_",code_pref,maptypeextra,".dbf",sep=""),
                  paste(maptype,"-",year,"_",code_pref,maptypeextra,".prj",sep=""))
    utils::unzip(strLNIZip, files = SHPFiles, exdir = strTempDir)
    strLNIFile = file.path(strTempDir,
                           paste(maptype, "-", year,"_", code_pref,maptypeextra,".shp",sep=""))
    if (!file.exists(strLNIFile)){
      utils::unzip(strLNIZip, exdir = strTempDir)
      strLNIFile = find_shp_file(maptype, code_pref, code_muni, year, strTempDir, maptypeextra)
    }
    sfLNI = sf::read_sf(strLNIFile, options = "ENCODING=CP932", stringsAsFactors=FALSE)
    # Older data may not have *.prj. Set CRS manually.
    if (is.na(sf::st_crs(sfLNI))) {
      if (year >= 2014) {
        sf::st_crs(sfLNI) = 6668
      } else {
        sf::st_crs(sfLNI) = 4612
      }
    }
  } else {
    stop(paste("Unknown filetype:", filetype))
  }


  if (!is.null(geometry)) {
    if (geometry == "POINT") sfLNI = sf::st_collection_extract(sfLNI, type = "POINT")
    if (geometry == "LINESTRING") sfLNI = sf::st_collection_extract(sfLNI, type = "LINESTRING")
    if (geometry == "POLYGON") sfLNI = sf::st_collection_extract(sfLNI, type = "POLYGON")
  }
  sfLNI = sf::st_make_valid(sfLNI)
  attr(sfLNI, "year") = year4digit

  return(sfLNI)
}


#' Download spatial data of Land Use of Japan
#'
#' @description
#' Function to download spatial data of  land uses. The returned value is an sf object with extra
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
read_landnuminfo_landuse <- function(code_pref, code_muni, year = 2019, data_dir = NULL){
  year = check_year(year)
  if (year != 2019 & year != 2011) stop(paste("The data is not available for year", year))

  if (year == 2019) {
    sfLNI = read_landnuminfo("A29", code_pref, code_muni, year, filetype = "geojson", geometry = "POLYGON", data_dir = data_dir)
  } else if (year == 2011) {
    sfLNI = read_landnuminfo("A29", code_pref, NULL, year, filetype = "shp", geometry = "POLYGON", data_dir = data_dir)
  }

  if(exists("sfLNI")) {
    sfLNI$A29_004 <- factor(sfLNI$A29_004, levels=c(1,2,3,4,5,6,7,8,9,10,11,12,21,99))
    sfLNI$A29_005 <- factor(sfLNI$A29_005, levels=c("\u7b2c\u4e00\u7a2e\u4f4e\u5c64\u4f4f\u5c45\u5c02\u7528\u5730\u57df",
                                                    "\u7b2c\u4e8c\u7a2e\u4f4e\u5c64\u4f4f\u5c45\u5c02\u7528\u5730\u57df",
                                                    "\u7b2c\u4e00\u7a2e\u4e2d\u9ad8\u5c64\u4f4f\u5c45\u5c02\u7528\u5730\u57df",
                                                    "\u7b2c\u4e8c\u7a2e\u4e2d\u9ad8\u5c64\u4f4f\u5c45\u5c02\u7528\u5730\u57df",
                                                    "\u7b2c\u4e00\u7a2e\u4f4f\u5c45\u5730\u57df",
                                                    "\u7b2c\u4e8c\u7a2e\u4f4f\u5c45\u5730\u57df",
                                                    "\u6e96\u4f4f\u5c45\u5730\u57df",
                                                    "\u8fd1\u96a3\u5546\u696d\u5730\u57df",
                                                    "\u5546\u696d\u5730\u57df",
                                                    "\u6e96\u5de5\u696d\u5730\u57df",
                                                    "\u5de5\u696d\u5730\u57df",
                                                    "\u5de5\u696d\u5c02\u7528\u5730\u57df",
                                                    "\u7530\u5712\u4f4f\u5c45\u5730\u57df",
                                                    "\u4e0d\u660e"))

    attr(sfLNI, "mapname") = "\u7528\u9014\u5730\u57df"
    attr(sfLNI, "sourceName") = "\u300c\u56fd\u571f\u6570\u5024\u60c5\u5831\uff08\u7528\u9014\u5730\u57df\u30c7\u30fc\u30bf\uff09\u300d\uff08\u56fd\u571f\u4ea4\u901a\u7701\uff09"
    attr(sfLNI, "sourceURL") = "https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A29-v2_1.html"
    attr(sfLNI, "col") = "A29_005"
    attr(sfLNI, "palette") = c("#00BEA9","#6AD5BD","#A8D666","#FAE294","#F4E268","#F8D79C","#FFB580","#F3A5B9","#EE534F","#B3A8CB","#8AD0E4","#2CB3DE","#F4B187","#FFFFFF")
  } else {
    sfLNI = NULL
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
  year = check_year(year)
  if (year != 2020) stop(paste("The data is not available for year", year))

  sfLNI = read_landnuminfo("A50", code_pref, code_muni, year, filetype = "geojson", geometry = "POLYGON", data_dir = data_dir)
  sfLNI$A50_006_label <- factor(sfLNI$A50_006, levels=c("1","2","3"), labels=c("\u7acb\u5730\u9069\u6b63\u5316\u8a08\u753b\u533a\u57df",
                                                                               "\u5c45\u4f4f\u8a98\u5c0e\u533a\u57df",
                                                                               "\u90fd\u5e02\u6a5f\u80fd\u8a98\u5c0e\u533a\u57df"))

  attr(sfLNI, "mapname") = "\u7acb\u5730\u9069\u6b63\u5316\u8a08\u753b\u533a\u57df"
  attr(sfLNI, "sourceName") = "\u300c\u56fd\u571f\u6570\u5024\u60c5\u5831\uff08\u7acb\u5730\u9069\u6b63\u5316\u8a08\u753b\u533a\u57df\u30c7\u30fc\u30bf\uff09\u300d\uff08\u56fd\u571f\u4ea4\u901a\u7701\uff09"
  attr(sfLNI, "sourceURL") = "https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A50-v1_0.html"
  attr(sfLNI, "col") = "A50_006_label"
  attr(sfLNI, "palette") = c("#E2FFE3","#99CDFD","#F87E88")
  return(sfLNI)
}

#' Download spatial data of Flood Inundation Risk of Japan
#'
#' @description
#' Function to download spatial data of Flood Inundation Risk of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni The 3-digit code of municipality (city, town, or village).
#' @param year Year of the data. Defaults to 2012.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_flood <- function(code_pref, year = 2012, data_dir = NULL){
  year = check_year(year)
  if (year != 2012) stop(paste("The data is not available for year", year))

  sfLNI = read_landnuminfo("A31", code_pref, code_muni = NULL, year, filetype = "shp", geometry = "POLYGON", data_dir = data_dir)
  if (min(sfLNI$A31_001) >= 20) {
    sfLNI$A31_001_label <- factor(sfLNI$A31_001, levels=c(21,22,23,24,25,26,27), labels=c("0～0.5ｍ未満","0.5～1.0ｍ未満","1.0～2.0ｍ未満","2.0～3.0ｍ未満","3.0～4.0ｍ未満","4.0～5.0ｍ未満","5.0ｍ以上"))
    attr(sfLNI, "palette") = c("#EFF3FF","#C6DBEF","#9ECAE1","#6BAED6","#4292C6", "#2171B5","#084594") # RColorBrewer::brewer.pal(5, "Blues")
  } else if (max(sfLNI$A31_001) >= 15) {
    sfLNI$A31_001_label <- factor(sfLNI$A31_001, levels=c(11,12,13,14,15), labels=c("0～0.5ｍ未満","0.5～1.0ｍ未満","1.0～2.0ｍ未満","2.0～5.0ｍ未満","5.0ｍ以上"))
    attr(sfLNI, "palette") = c("#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C") # RColorBrewer::brewer.pal(5, "Blues")
  } else {
    sfLNI$A31_001_label = 0
    sfLNI[sfLNI$A31_001 == 21, "A31_001_label"] = 11
    sfLNI[sfLNI$A31_001 == 22, "A31_001_label"] = 12
    sfLNI[sfLNI$A31_001 == 23, "A31_001_label"] = 13
    sfLNI[sfLNI$A31_001 == 24, "A31_001_label"] = 14
    sfLNI[sfLNI$A31_001 == 25, "A31_001_label"] = 14
    sfLNI[sfLNI$A31_001 == 26, "A31_001_label"] = 14
    sfLNI[sfLNI$A31_001 == 27, "A31_001_label"] = 15
    sfLNI$A31_001_label <- factor(sfLNI$A31_001_label, levels=c(11,12,13,14,15),
                                  labels=c("0\uff5e0.5\uff4d\u672a\u6e80","Show in New Window
0\uff5e0.5\uff4d\u672a\u6e80","1.0\uff5e2.0\uff4d\u672a\u6e80","2.0\uff5e5.0\uff4d\u672a\u6e80","5.0\uff4d\u4ee5\u4e0a"))
    attr(sfLNI, "palette") = c("#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C") # RColorBrewer::brewer.pal(5, "Blues")
  }


  attr(sfLNI, "mapname") = "\u6d2a\u6c34\u6d78\u6c34\u60f3\u5b9a\u533a\u57df"
  attr(sfLNI, "col") = "A31_001_label"
  attr(sfLNI, "sourceName") = "\u300c\u56fd\u571f\u6570\u5024\u60c5\u5831\uff08\u6d2a\u6c34\u6d78\u6c34\u60f3\u5b9a\u533a\u57df\u30c7\u30fc\u30bf\uff09\u300d\uff08\u56fd\u571f\u4ea4\u901a\u7701\uff09"
  attr(sfLNI, "sourceURL") = "https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A31.html"

  return(sfLNI)
}

#' Download spatial data of Welfare Facilities of Japan
#'
#' @description
#' Function to download spatial data of Welfare Facilities of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni The 3-digit code of municipality (city, town, or village).
#' @param year Year of the data. Defaults to 2012.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_welfare <- function(code_pref, code_muni = NULL, year = 2021, data_dir = NULL){
  year = check_year(year)
  if (year != 2021 & year != 2015 & year != 2011) stop(paste("The data is not available for year", year))

  sfLNI = read_landnuminfo("P14", code_pref, code_muni, year, filetype = "geojson", geometry = "POINT", data_dir = data_dir)
  sfLNI$P14_005_label <- factor(sfLNI$P14_005, levels=c("01","02","03","04","05","06","99"),
                          labels=c("\u4fdd\u8b77\u65bd\u8a2d",
                                   "\u8001\u4eba\u798f\u7949\u65bd\u8a2d",
                                   "\u969c\u5bb3\u8005\u652f\u63f4\u65bd\u8a2d\u7b49",
                                   "\u8eab\u4f53\u969c\u5bb3\u8005\u793e\u4f1a\u53c2\u52a0\u652f\u63f4\u65bd\u8a2d",
                                   "\u5150\u7ae5\u798f\u7949\u65bd\u8a2d\u7b49",
                                   "\u6bcd\u5b50\u30fb\u7236\u5b50\u798f\u7949\u65bd\u8a2d",
                                   "\u305d\u306e\u4ed6\u306e\u793e\u4f1a\u798f\u7949\u65bd\u8a2d\u7b49"))

  attr(sfLNI, "mapname") = "\u798f\u7949\u65bd\u8a2d"
  attr(sfLNI, "sourceName") = "\u300c\u56fd\u571f\u6570\u5024\u60c5\u5831\uff08\u798f\u7949\u65bd\u8a2d\u30c7\u30fc\u30bf\uff09\u300d\uff08\u56fd\u571f\u4ea4\u901a\u7701\uff09"
  attr(sfLNI, "sourceURL") = "https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-P14-v2_1.html"
  attr(sfLNI, "col") = "P14_005_label"
  attr(sfLNI, "palette") = c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D") # RColorBrewer::brewer.pal(7, "Dark2")
  return(sfLNI)
}

#' Download spatial data of Hazard Areas of Japan
#'
#' @description
#' Function to download spatial data of Hazard Areas of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality. If specified, subtract the data by the column A48_003.
#' @param year Year of the data. Defaults to 2012.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_hazard <- function(code_pref, code_muni = NULL, year = 2021, data_dir = NULL){
  year = check_year(year)
  if (year != 2021 & year != 2020) stop(paste("The data is not available for year", year))

  # Hazard Area data is given by prefecture
  sfLNI = read_landnuminfo("A48", code_pref, NULL, year, filetype = "geojson", geometry = "POLYGON", data_dir = data_dir)

  sfLNI$A48_007_label <- factor(sfLNI$A48_007, levels=c(1,2,3,4,5,6,7),
                          labels=c("\u6c34\u5bb3\uff08\u6cb3\u5ddd\uff09",
                                   "\u6c34\u5bb3\uff08\u6d77\uff09",
                                   "\u6c34\u5bb3\uff08\u6cb3\u5ddd\u30fb\u6d77\uff09",
                                   "\u6025\u50be\u659c\u5730\u5d29\u58ca\u7b49",
                                   "\u5730\u3059\u3079\u308a\u7b49",
                                   "\u706b\u5c71\u88ab\u5bb3",
                                   "\u305d\u306e\u4ed6"))

  attr(sfLNI, "mapname") = "\u707d\u5bb3\u5371\u967a\u533a\u57df"
  attr(sfLNI, "sourceName") = "\u300c\u56fd\u571f\u6570\u5024\u60c5\u5831\uff08\u707d\u5bb3\u5371\u967a\u533a\u57df\u30c7\u30fc\u30bf\uff09\u300d\uff08\u56fd\u571f\u4ea4\u901a\u7701\uff09"
  if (year == 2020) {
    attr(sfLNI, "sourceURL") = "https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A48-v1_1.html"
  } else {
    attr(sfLNI, "sourceURL") = "https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A48-v1_2.html"
  }
  attr(sfLNI, "col") = "A48_007_label"
  attr(sfLNI, "palette") = c("#16A085","#D1F2EB","#1F618D","#229954","#BA4A00","#E74C3C","#808B96")

  return(sfLNI)
}

#' Download spatial data of rivers of Japan
#'
#' @description
#' Function to download spatial data of rivers of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality. If specified, subtract the data by the column A48_003.
#' @param year Year of the data. Defaults to 2007.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
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
    year = 2009
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
             || num_code_pref == 40
             || num_code_pref == 41
             || num_code_pref == 42
             || num_code_pref == 43
             || num_code_pref == 44
             || num_code_pref == 45
             || num_code_pref == 46
             || num_code_pref == 47
             || num_code_pref == 48
  ) {
    year = 2008
  } else if (num_code_pref == 2
             || num_code_pref == 3
             || num_code_pref == 4
             || num_code_pref == 5
             || num_code_pref == 6
             || num_code_pref == 7
             || num_code_pref == 15
             || num_code_pref == 16
             || num_code_pref == 17
             || num_code_pref == 18) {
    year = 2007
  } else if (num_code_pref == 36
             || num_code_pref == 37
             || num_code_pref == 38
             || num_code_pref == 39) {
    year = 2006
  } else {
    print("Invalid pref_code.")
  }

  year = check_year(year)
  if (year < 2006 || year > 2009) stop(paste("The data is not available for year", year))

  # River data file has a suffix "-g_Stream"
  sfLNI = read_landnuminfo("W05", code_pref, NULL, year, filetype = "shp", geometry = "LINESTRING",
                           data_dir = data_dir,
                           maptypeextra = "-g_Stream")

  attr(sfLNI, "mapname") = "\u6cb3\u5ddd"
  attr(sfLNI, "sourceName") = "\u300c\u56fd\u571f\u6570\u5024\u60c5\u5831\uff08\u6cb3\u5ddd\u30c7\u30fc\u30bf\uff09\u300d\uff08\u56fd\u571f\u4ea4\u901a\u7701\uff09"
  attr(sfLNI, "sourceURL") = "https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-W05.html"
  attr(sfLNI, "col") = "W05_002"
  #attr(sfLNI, "palette") = c("#16A085","#D1F2EB","#1F618D","#229954","#BA4A00","#E74C3C","#808B96")

  return(sfLNI)
}

#' Download spatial data of Administrative Boundary of Japan
#'
#' @description
#' Function to download spatial data of Administrative Boundary of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality. If specified, subtract the data by the column A48_003.
#' @param year Year of the data. Defaults to 2012.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_admin <- function(code_pref, code_muni = NULL, year = 2022, data_dir = NULL){
  year = check_year(year)
  if (year > 2022 || year < 1940) stop(paste("The data is not available for year", year))

  # Administrative Boundaries data
  sfLNI = read_landnuminfo("N05", code_pref, NULL, year, filetype = "geojson", geometry = "POLYGON", data_dir = data_dir)

  attr(sfLNI, "mapname") = "\u884c\u653f\u533a\u57df"
  attr(sfLNI, "sourceName") = "\u300c\u56fd\u571f\u6570\u5024\u60c5\u5831\uff08\u884c\u653f\u533a\u57df\u30c7\u30fc\u30bf\uff09\u300d\uff08\u56fd\u571f\u4ea4\u901a\u7701\uff09"
  if (year == 2022) {
    attr(sfLNI, "sourceURL") = "https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03-v3_1.html"
  } else if (year == 2021){
    attr(sfLNI, "sourceURL") = "https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03-v3_0.html"
  }
  attr(sfLNI, "col") = ""
  attr(sfLNI, "palette") = ""

  return(sfLNI)
}

#' Download spatial data of Official Land Price of Japan
#'
#' @description
#' Function to download spatial data of Official Land Price of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality. If specified, subtract the data by the column A48_003.
#' @param year Year of the data. Defaults to 2012.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" for tmap.
#'
#' @export
read_landnuminfo_officiallandprice <- function(code_pref, code_muni = NULL, year = 2021, data_dir = NULL){
  year = check_year(year)
  if (year < 1983 || year > 2022) stop(paste("The data is not available for year", year))
  if (year < 2018) {
    filetype = "shp"
  } else {
    filetype = "geojson"
  }

  # Hazard Area data is given by prefecture
  sfLNI = read_landnuminfo("L01", code_pref, NULL, year, filetype = filetype, geometry = "POINT", data_dir = data_dir)

  # if code_muni is specified, subtract the data
  if(!is.null(code_muni)) {
    if (mode(code_pref) == "numeric"){
      if (code_pref < 0 || code_pref > 47) {
        stop("Invalid argument: code_pref must be between 1 and 47.")
      } else if (code_pref < 10) {
        code_pref = paste("0", as.character(code_pref), sep = "")
      } else {
        code_pref = as.character(code_pref)
      }
    }
    if (mode(code_muni) == "numeric"){
      if (code_muni < 100 || code_muni > 700) {
        stop("Invalid argument: code_muni must be between 100 and 700.")
      } else {
        code_muni = as.character(code_muni)
      }
    }
    sfLNI2 = subset(sfLNI, L01_021 == paste(code_pref, code_muni, sep=""))
    if (nrow(sfLNI2) > 0) sfLNI = sfLNI2
  }

  sfLNI$L01_006 = as.integer(sfLNI$L01_006)
  attr(sfLNI, "mapname") = "\u5730\u4fa1\u516c\u793a"
  attr(sfLNI, "sourceName") = "\u300c\u56fd\u571f\u6570\u5024\u60c5\u5831\uff08\u5730\u4fa1\u516c\u793a\u30c7\u30fc\u30bf\uff09\u300d\uff08\u56fd\u571f\u4ea4\u901a\u7701\uff09"
  attr(sfLNI, "sourceURL") = "https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-L01-v3_1.html"
  attr(sfLNI, "col") = "L01_006"
  # attr(sfLNI, "palette") = c("#16A085","#D1F2EB","#1F618D","#229954","#BA4A00","#E74C3C","#808B96")

  return(sfLNI)
}

#' Print the list of available Land Numerical Information data.
#'
#' @description
#' Print the list of available Land Numerical Information data.
#'
#' @export
list_landnuminfo <- function(){
  dfTestedMap <- utils::read.table(text = "MapCode,Year,FileType,MapUnit,MuniColumn,Geometry,Desc,URL
A29,2019,geojson,muni,A29_003,POLYGON,用途地域,https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A29-v2_1.html
A29,2011,shp,muni,A29_003,POLYGON,用途地域,https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A29-v2_1.html
A50,2020,geojson,muni,A50_004,POLYGON,立地適正化計画区域,https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A50-v1_0.html
A31,2012,shp,pref,,POLYGON,洪水浸水想定区域,https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A31.html
P14,2021,geojson,pref,P14_003,POINT,福祉施設,https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-P14-v2_1.html
A48,2021,geojson,pref,A48_003,POLYGON,災害危険区域,https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A48-v1_2.html
A48,2020,geojson,pref,A48_003,POLYGON,災害危険区域,https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A48-v1_1.html",
                                   header = TRUE, sep=",", colClasses = "character")
  print(dfTestedMap)
}
