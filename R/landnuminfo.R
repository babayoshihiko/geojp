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
#' @importFrom utils download.file
#' @importFrom sf read_sf
#' @importFrom sf st_crs
#' @importFrom lwgeom lwgeom_make_valid
#' @export
read_landnuminfo <- function(maptype, code_pref, code_muni = NULL, year = 2020,
                             filetype = "geojson",
                             geometry = NULL,
                             data_dir = NULL,
                             maptypeextra = ""){
  # Checks the arguments
  year4digit <- check_year(year)
  strTempDir <- check_data_dir(data_dir)
  code_pref <- check_code_pref_as_char(code_pref)
  code_muni <- check_code_muni_as_char(code_muni, code_pref)

  if (mode(year) == "numeric"){
    year = year_2digit(year)
  }
  if (nchar(code_pref) != 2) stop(paste("Invalid argument: code_pref:", code_pref))
  if (nchar(year) != 2) stop(paste("Invalid argument: year:", year))

  strLNIZip = file.path(strTempDir,
                        paste(maptype, "-", year, "_", code_pref, "_GML.zip", sep = ""))
  strLNIUrl = paste("https://nlftp.mlit.go.jp/ksj/gml/data/", maptype, "/", maptype, "-", year, "/", maptype, "-", year, "_", code_pref, "_GML.zip", sep = "")

  if (maptype == "N03" ) {
    strLNIZip = file.path(strTempDir, paste("N03-", year4digit, "0101_", code_pref, "_GML.zip", sep = ""))
    strLNIUrl = paste("https://nlftp.mlit.go.jp/ksj/gml/data/N03/N03-", year4digit, "/N03-", year4digit, "0101_", code_pref, "_GML.zip", sep = "")
  }

  if (!file.exists(strLNIZip)) {
    utils::download.file(strLNIUrl, strLNIZip, mode="wb")
    message(paste("Downloaded the file and saved in", strLNIUrl))
  }
  unzip_ja(strLNIZip, exdir = strTempDir)

  if (filetype == "geojson") {
    GEOJSONFILE = paste(maptype,"-",year,"_",code_pref,code_muni,maptypeextra,".geojson",sep="")

    if (code_muni != "") {
      GEOJSONFILE = paste(maptype,"-",year,"_",code_pref,maptypeextra,".geojson",sep="")
    }
    GEOJSONFILE = file.path(paste(maptype,"-",year,"_",code_pref,"_GML",sep=""),
                                  GEOJSONFILE)
    strLNIFile = find_geojson_file(maptype, code_pref, code_muni, year, strTempDir, maptypeextra)
    if (!file.exists(strLNIFile)){
      strLNIFile = find_geojson_file(maptype, code_pref, code_muni, year, strTempDir, maptypeextra)
    }
    sfLNI = sf::read_sf(strLNIFile)
  } else if (filetype == "shp") {
    strLNIFile = file.path(strTempDir,
                           paste(maptype, "-", year,"_", code_pref,maptypeextra,".shp",sep=""))
    if (!file.exists(strLNIFile)){
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
  sfLNI$geometry <- lwgeom::lwgeom_make_valid(sfLNI$geometry)
  attr(sfLNI, "year") = year4digit

  return(sfLNI)
}

read_landnuminfo_by_csv <- function(maptype, code_pref, code_muni = NULL,
                                    year,
                                    data_dir = NULL,
                                    multifiles = "error",
                                    encoding = "CP932"){
  # Checks the arguments
  year4digit <- check_year(year)
  strTempDir <- check_data_dir(data_dir)
  code_pref <- check_code_pref_as_char(code_pref)
  code_muni <- check_code_muni_as_char(code_pref, code_muni)

  if (nchar(code_pref) != 2) stop(paste("Invalid argument: code_pref:", code_pref))
  if (code_pref == "47" && year4digit < 1973) stop("No data available for Okinaya before year 1973.")

  # Read the MapType definition
  dfTemp <- get_definition(maptype)
  dfTemp <- dfTemp[dfTemp$year == year4digit,]

  # Set the files
  strLNIUrl <- gsub("code_pref",code_pref,dfTemp[1,"url"])
  strLNIZip <- file.path(strTempDir,gsub("code_pref",code_pref,dfTemp[1,"zip"]))
  strLNIFile1 <- file.path(strTempDir,gsub("code_muni",code_muni,gsub("code_pref",code_pref,dfTemp[1,"shp"])))
  strLNIFile2 <- file.path(strTempDir,gsub("code_muni",code_muni,gsub("code_pref",code_pref,dfTemp[1,"altdir"])),gsub("code_muni",code_muni,gsub("code_pref",code_pref,dfTemp[1,"shp"])))
  strLNIFile3 <- file.path(strTempDir,paste(gsub("code_muni",code_muni,gsub("code_pref",code_pref,dfTemp[1,"altdir"])),"\\\\",gsub("code_muni",code_muni,gsub("code_pref",code_pref,dfTemp[1,"shp"])),sep=""))

  sfLNI <- get_sfLNI(maptype, strLNIFile1, strLNIFile2, strLNIFile3, strLNIUrl, strLNIZip, year4digit, strTempDir, multifiles, encoding)
}

#' @importFrom utils download.file
#' @importFrom sf read_sf
get_sfLNI <- function(maptype, strLNIFile1, strLNIFile2, strLNIFile3, strLNIUrl, strLNIZip, year4digit,
                      strTempDir,
                      multifiles,
                      encoding = "CP932"){
  # Read the MapType definition
  dfTemp <- get_definition(maptype)
  dfTemp <- dfTemp[dfTemp$year == year4digit,]
  if (nrow(dfTemp) != 1) stop(paste("The target year", year, "not found in", paste("data/", maptype, ".csv", sep = "")))

  strLNIFile <- ""
  strOptions <- paste("ENCODING=", encoding, sep="")

  # Checks if the shp file exists
  strLNIFile <- get_sfLNI_file(strLNIFile1, strLNIFile2, strLNIFile3, strTempDir, multifiles)

  # If the file does not exist, download the zip file and uzip
  if (length(strLNIFile) == 1){
    if (!file.exists(strLNIFile)){
      if (!file.exists(strLNIZip)) {
        utils::download.file(strLNIUrl, strLNIZip, mode="wb")
        message(paste("Downloaded the file and saved in", strTempDir))
      }
      unzip_ja(strLNIZip, exdir = strTempDir)
      # Checks if the shp file exists
      strLNIFile <- get_sfLNI_file(strLNIFile1, strLNIFile2, strLNIFile3, strTempDir, multifiles)
    }
  }

  if (length(strLNIFile) == 1){
    if (strLNIFile == ""){
      stop(paste("Cannot find the file:", strLNIFile1))
    } else {
      sfLNI <- sf::read_sf(strLNIFile, options = strOptions, stringsAsFactors=FALSE)
    }
  } else {
    sfLNI <- NULL
    for (strLNIFile_single in strLNIFile){
      if (is.null(sfLNI)) {
        sfLNI <- sf::read_sf(strLNIFile_single, options = strOptions, stringsAsFactors=FALSE)
      } else {
        sfLNI <- rbind(sfLNI, sf::read_sf(strLNIFile_single, options = strOptions, stringsAsFactors=FALSE))
      }
    }

  }

  if (!is.null("sfLNI")) {
    # Older data may not have *.prj. Set CRS manually.
    if (is.na(sf::st_crs(sfLNI))) {
      # The new CRSs reflect  crustal movements by the Great East Japan Earthquake in 2011.
      # The new CRSs (EPSG codes) were defined by EPSG v 8.4 (2014).
      if (year >= 2014) {
        warning("CRS is not set. Defaults to 6668 (geojp::get_sfLNI).")
        sf::st_crs(sfLNI) <- 6668
      } else {
        sf::st_crs(sfLNI) <- 4612
        warning("CRS is not set. Defaults to 4612 (geojp::get_sfLNI).")
      }
    }
    attr(sfLNI, "sourceName") <- "\u300c\u56fd\u571f\u6570\u5024\u60c5\u5831\uff08\u884c\u653f\u533a\u57df\u30c7\u30fc\u30bf\uff09\u300d\uff08\u56fd\u571f\u4ea4\u901a\u7701\uff09" # MLIT
    attr(sfLNI, "sourceURL") <- as.character(dfTemp[1,"source"])
    attr(sfLNI, "col") = as.character(dfTemp[1,"col"])
    attr(sfLNI, "year") <- year4digit

    if (!is.na(dfTemp[1,"levels"])){
      temp_levels <- unlist(strsplit(as.character(dfTemp[1,"levels"]), " "))
      temp_labels <- ""
      if ("labels" %in% colnames(dfTemp)){
        if (!is.na(dfTemp[1,"labels"])) {
          temp_labels <- unlist(strsplit(as.character(dfTemp[1,"labels"]), " "))
        }
      }
      if (!is.na(dfTemp[1,"palette"])) {
        temp_palette <- unlist(strsplit(as.character(dfTemp[1,"palette"]), " "))
      } else {
        temp_palette <- ""
      }
      if (length(temp_levels) > 0){
        if (as.character(dfTemp[1,"col"]) %in% colnames(sfLNI)){
          if (length(temp_levels) == length(temp_labels)){
            sfLNI[,as.character(dfTemp[1,"col"])] <- factor(unlist(sf::st_drop_geometry(sfLNI)[,as.character(dfTemp[1,"col"])]), levels = temp_levels, labels = temp_labels)
          } else {
            sfLNI[,as.character(dfTemp[1,"col"])] <- factor(unlist(sf::st_drop_geometry(sfLNI)[,as.character(dfTemp[1,"col"])]), levels = temp_levels)
          }
        }
      }
      if (length(temp_levels) == length(temp_palette)){
        attr(sfLNI, "palette") <- temp_palette
      }
    }

    return(sfLNI)
  }
}

get_sfLNI_file <- function(strLNIFile1, strLNIFile2, strLNIFile3, strTempDir, multifiles){
  # Some file (e.g. A29-19_05) is nested in another folder as "A29-19_05_GML/A29-19_05/*.shp"
  # This is a dirty workaround.
  strLNIFile4 <- gsub(strTempDir, paste(strTempDir,"*",sep="/"), strLNIFile1)
  strLNIFile5 <- gsub(strTempDir, paste(strTempDir,"*",sep="/"), strLNIFile2)
  strLNIFile6 <- gsub(strTempDir, paste(strTempDir,"*",sep="/"), strLNIFile3)
  strLNIFile7 <- gsub(strTempDir, paste(strTempDir,"*","*",sep="/"), strLNIFile1)
  strLNIFile8 <- gsub(strTempDir, paste(strTempDir,"*","*",sep="/"), strLNIFile2)
  strLNIFile9 <- gsub(strTempDir, paste(strTempDir,"*","*",sep="/"), strLNIFile3)

  strLNIFile <- ""
  if (length(Sys.glob(strLNIFile1)) == 1){
    strLNIFile <- Sys.glob(strLNIFile1)
  } else if (length(Sys.glob(strLNIFile2)) == 1){
    strLNIFile <- Sys.glob(strLNIFile2)
  } else if (length(Sys.glob(strLNIFile3)) == 1){
    strLNIFile <- Sys.glob(strLNIFile3)
  } else if (length(Sys.glob(strLNIFile4)) == 1){
    strLNIFile <- Sys.glob(strLNIFile4)
  } else if (length(Sys.glob(strLNIFile5)) == 1){
    strLNIFile <- Sys.glob(strLNIFile5)
  } else if (length(Sys.glob(strLNIFile6)) == 1){
    strLNIFile <- Sys.glob(strLNIFile6)
  } else if (length(Sys.glob(strLNIFile7)) == 1){
    strLNIFile <- Sys.glob(strLNIFile7)
  } else if (length(Sys.glob(strLNIFile8)) == 1){
    strLNIFile <- Sys.glob(strLNIFile8)
  } else if (length(Sys.glob(strLNIFile9)) == 1){
    strLNIFile <- Sys.glob(strLNIFile9)
  } else {
    if (multifiles == "error"){
      if (length(Sys.glob(strLNIFile1)) > 1){
        stop(paste("Found more than 1 files:", strLNIFile1))
      } else if (length(Sys.glob(strLNIFile2)) > 1){
        stop(paste("Found more than 1 files:", strLNIFile2))
      } else if (length(Sys.glob(strLNIFile3)) > 1){
        stop(paste("Found more than 1 files:", strLNIFile3))
      } else if (length(Sys.glob(strLNIFile4)) > 1){
        stop(paste("Found more than 1 files:", strLNIFile4))
      } else if (length(Sys.glob(strLNIFile5)) > 1){
        stop(paste("Found more than 1 files:", strLNIFile5))
      } else if (length(Sys.glob(strLNIFile6)) > 1){
        stop(paste("Found more than 1 files:", strLNIFile6))
      } else if (length(Sys.glob(strLNIFile7)) > 1){
        stop(paste("Found more than 1 files:", strLNIFile7))
      } else if (length(Sys.glob(strLNIFile8)) > 1){
        stop(paste("Found more than 1 files:", strLNIFile8))
      } else if (length(Sys.glob(strLNIFile9)) > 1){
        stop(paste("Found more than 1 files:", strLNIFile9))
      }
    } else if (multifiles == "first"){
      if (length(Sys.glob(strLNIFile1)) > 1){
        strLNIFile <- Sys.glob(strLNIFile1)[1]
      } else if (length(Sys.glob(strLNIFile2)) > 1){
        strLNIFile <- Sys.glob(strLNIFile2)[1]
      } else if (length(Sys.glob(strLNIFile3)) > 1){
        strLNIFile <- Sys.glob(strLNIFile3)[1]
      } else if (length(Sys.glob(strLNIFile4)) > 1){
        strLNIFile <- Sys.glob(strLNIFile4)[1]
      } else if (length(Sys.glob(strLNIFile5)) > 1){
        strLNIFile <- Sys.glob(strLNIFile5)[1]
      } else if (length(Sys.glob(strLNIFile6)) > 1){
        strLNIFile <- Sys.glob(strLNIFile6)[1]
      } else if (length(Sys.glob(strLNIFile7)) > 1){
        strLNIFile <- Sys.glob(strLNIFile7)[1]
      } else if (length(Sys.glob(strLNIFile8)) > 1){
        strLNIFile <- Sys.glob(strLNIFile8)[1]
      } else if (length(Sys.glob(strLNIFile9)) > 1){
        strLNIFile <- Sys.glob(strLNIFile9)[1]
      }
    } else if (multifiles == "multiple"){
      if (length(Sys.glob(strLNIFile1)) > 1){
        strLNIFile <- Sys.glob(strLNIFile1)
      } else if (length(Sys.glob(strLNIFile2)) > 1){
        strLNIFile <- Sys.glob(strLNIFile2)
      } else if (length(Sys.glob(strLNIFile3)) > 1){
        strLNIFile <- Sys.glob(strLNIFile3)
      } else if (length(Sys.glob(strLNIFile4)) > 1){
        strLNIFile <- Sys.glob(strLNIFile4)
      } else if (length(Sys.glob(strLNIFile5)) > 1){
        strLNIFile <- Sys.glob(strLNIFile5)
      } else if (length(Sys.glob(strLNIFile6)) > 1){
        strLNIFile <- Sys.glob(strLNIFile6)
      } else if (length(Sys.glob(strLNIFile7)) > 1){
        strLNIFile <- Sys.glob(strLNIFile7)
      } else if (length(Sys.glob(strLNIFile8)) > 1){
        strLNIFile <- Sys.glob(strLNIFile8)
      } else if (length(Sys.glob(strLNIFile9)) > 1){
        strLNIFile <- Sys.glob(strLNIFile9)
      }
    }

  }
  return (strLNIFile)
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
  year4digit <- check_year(year)

  sfLNI <- NULL

  if (year4digit == 2019) {
    sfLNI <- read_landnuminfo_by_csv("A29", code_pref, code_muni, year4digit, data_dir)
  } else {
    sfLNI <- read_landnuminfo_by_csv("A29", code_pref, code_muni, year4digit, data_dir)

    if (!is.null(sfLNI)) {
      if (!is.null(code_muni)){
        lstCodeMuni <- get_wards(code_pref, code_muni, year4digit)
        if (length(lstCodeMuni) > 0) {
          sfLNI2 <- NULL
          for (code_muni_single in lstCodeMuni){
            strNameMuni <- get_muni_name(code_pref, code_muni_single)
            if (is.null(sfLNI2)) {
              sfLNI2 <- subset(sfLNI, A29_001 == paste(check_code_pref_as_char(code_pref),check_code_muni_as_char(code_pref,code_muni),sep=""))
            } else {
              sfLNI2 <- rbind(sfLNI2, subset(sfLNI, A29_001 == paste(check_code_pref_as_char(code_pref),check_code_muni_as_char(code_pref,code_muni),sep="")))
            }
          }
        }
        if (!is.null(sfLNI2)) sfLNI <- sfLNI2
      }
    }
  }

  if (!is.null(sfLNI)){
    attr(sfLNI, "mapname") = "\u7528\u9014\u5730\u57df"
    return(sfLNI)
  }
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

#' Download spatial data of Flood Inundation Risk of Japan
#'
#' @description
#' Function to download spatial data of Flood Inundation Risk of Japan. The returned value is an sf object.
#'
#' Note that the data of year 2012 is available for all prefectures, but not of other years.
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
read_landnuminfo_flood <- function(code_pref, code_muni = NULL, year = 2012, data_dir = NULL){
  year4digit <- check_year(year)

  if (year4digit == 2012){
    sfLNI <- NULL
    sfLNI <- read_landnuminfo_by_csv("A31", code_pref, NULL, year4digit, data_dir)

    if (min(sfLNI$A31_001) >= 20) {
      sfLNI$A31_Label <- factor(sfLNI$A31_001, levels=c(21,22,23,24,25,26,27),
                                    labels=c("0\uff5e0.5\uff4d\u672a\u6e80",
                                             "0.5\uff5e1.0\uff4d\u672a\u6e80",
                                             "1.0\uff5e2.0\uff4d\u672a\u6e80",
                                             "2.0\uff5e3.0\uff4d\u672a\u6e80",
                                             "3.0\uff5e4.0\uff4d\u672a\u6e80",
                                             "4.0\uff5e5.0\uff4d\u672a\u6e80",
                                             "5.0\uff4d\u4ee5\u4e0a"))
      # https://www.mlit.go.jp/river/shishin_guideline/pdf/manual_kouzuishinsui_1710.pdf
      attr(sfLNI, "palette") = c("#f7f5a9",
                                 "#f8e1a6",
                                 "#ffd8c0",
                                 "#ffd8c0",
                                 "#ffb7b7",
                                 "#ffb7b7",
                                 "#dc7adc")
    } else if (max(sfLNI$A31_001) >= 15) {
      sfLNI$A31_Label <- factor(sfLNI$A31_001, levels=c(11,12,13,14,15),
                                    labels=c("0\uff5e0.5\uff4d\u672a\u6e80",
                                             "0.5\uff5e1.0\uff4d\u672a\u6e80",
                                             "1.0\uff5e2.0\uff4d\u672a\u6e80",
                                             "2.0\uff5e5.0\uff4d\u672a\u6e80",
                                             "5.0\uff4d\u4ee5\u4e0a"))
      # https://www.mlit.go.jp/river/shishin_guideline/pdf/manual_kouzuishinsui_1710.pdf
      attr(sfLNI, "palette") = c("#f7f5a9",
                                 "#f8e1a6",
                                 "#ffd8c0",
                                 "#ffb7b7",
                                 "#dc7adc")
    } else {
      sfLNI$A31_001_label = 0
      sfLNI[sfLNI$A31_001 == 21, "A31_Label"] = 11
      sfLNI[sfLNI$A31_001 == 22, "A31_Label"] = 12
      sfLNI[sfLNI$A31_001 == 23, "A31_Label"] = 13
      sfLNI[sfLNI$A31_001 == 24, "A31_Label"] = 14
      sfLNI[sfLNI$A31_001 == 25, "A31_Label"] = 14
      sfLNI[sfLNI$A31_001 == 26, "A31_Label"] = 14
      sfLNI[sfLNI$A31_001 == 27, "A31_Label"] = 15
      sfLNI$A31_001_label <- factor(sfLNI$A31_Label, levels=c(11,12,13,14,15),
                                    labels=c("0\uff5e0.5\uff4d\u672a\u6e80",
                                             "0.5\uff5e1.0\uff4d\u672a\u6e80",
                                             "1.0\uff5e2.0\uff4d\u672a\u6e80",
                                             "2.0\uff5e5.0\uff4d\u672a\u6e80",
                                             "5.0\uff4d\u4ee5\u4e0a"))
      # https://www.mlit.go.jp/river/shishin_guideline/pdf/manual_kouzuishinsui_1710.pdf
      attr(sfLNI, "palette") = c("#f7f5a9",
                                 "#f8e1a6",
                                 "#ffd8c0",
                                 "#ffb7b7",
                                 "#dc7adc")
    }
  } else {
    code_region <- get_region(code_pref)
    sfLNI <- NULL
    sfLNI1 <- read_landnuminfo_by_csv("A31", code_region, NULL, year4digit, data_dir, "multiple")
    sfLNI2 <- read_landnuminfo_by_csv("A31", code_pref, NULL, year4digit, data_dir, "multiple")
    if (!is.null(sfLNI1) & !is.null(sfLNI1)) {
      sfLNI <- rbind(sfLNI1, sfLNI2)
    } else if (is.null(sfLNI1) & !is.null(sfLNI1)) {
      sfLNI <- sfLNI2
    } else if (is.null(sfLNI1) & !is.null(sfLNI1)) {
      sfLNI <- sfLNI1
    }
  }

  if (!is.null(sfLNI)) {
    attr(sfLNI, "mapname") = "\u6d2a\u6c34\u6d78\u6c34\u60f3\u5b9a\u533a\u57df"
  }

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
#' @param year Year of the data. Defaults based on pref_code.
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
    year = 2007
  } else if (num_code_pref == 36
             || num_code_pref == 37
             || num_code_pref == 38
             || num_code_pref == 39) {
    year = 2006
  } else {
    stop("Invalid pref_code.")
  }

  year4digit <- check_year(year)

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("W05", code_pref, NULL, year4digit, data_dir)

  if (!is.null(sfLNI)) {
    attr(sfLNI, "mapname") = "\u6cb3\u5ddd"
    attr(sfLNI, "palette") = ""
  }

  return(sfLNI)
}

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
  sfLNI = read_landnuminfo_by_csv("N03", code_pref, , year, data_dir)

  attr(sfLNI, "mapname") = "\u884c\u653f\u533a\u57df"
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
#' @param year Year of the data. Defaults to 2021.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" for tmap.
#'
#' @export
read_landnuminfo_officiallandprice <- function(code_pref, code_muni = NULL, year = 2021, data_dir = NULL){
  year4digit <- check_year(year)

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("L01", code_pref, NULL, year4digit, data_dir)

  if (!is.null(sfLNI)) {
    if (!is.null(code_muni)){
      lstCodeMuni <- get_wards(code_pref, code_muni, year4digit)
      if (length(lstCodeMuni) > 0) {
        sfLNI2 <- NULL
        for (code_muni_single in lstCodeMuni){
          strNameMuni <- get_muni_name(code_pref, code_muni_single)
          if (is.null(sfLNI2)) {
            if (year4digit == 2022) {
              sfLNI2 <- subset(sfLNI, L01_022 == paste(check_code_pref_as_char(code_pref),check_code_muni_as_char(code_pref,code_muni),sep=""))
            } else if (year4digit %in% c(2021, 2020, 2019, 2018)) {
              sfLNI2 <- subset(sfLNI, L01_021 == paste(check_code_pref_as_char(code_pref),check_code_muni_as_char(code_pref,code_muni),sep=""))
            } else {
              sfLNI2 <- subset(sfLNI, L01_017 == paste(check_code_pref_as_char(code_pref),check_code_muni_as_char(code_pref,code_muni),sep=""))
            }
          } else {
            if (year4digit == 2022) {
              sfLNI2 <- rbind(sfLNI2, subset(sfLNI, L01_020 == paste(check_code_pref_as_char(code_pref),check_code_muni_as_char(code_pref,code_muni),sep="")))
            } else if (year4digit %in% c(2021, 2020, 2019, 2018)) {
              sfLNI2 <- rbind(sfLNI2, subset(sfLNI, L01_021 == paste(check_code_pref_as_char(code_pref),check_code_muni_as_char(code_pref,code_muni),sep="")))
            } else {
              sfLNI2 <- rbind(sfLNI2, subset(sfLNI, L01_017 == paste(check_code_pref_as_char(code_pref),check_code_muni_as_char(code_pref,code_muni),sep="")))
            }
          }
        }
      }
      if (!is.null(sfLNI2)) sfLNI <- sfLNI2
    }

    if (!is.null(sfLNI)) {
      attr(sfLNI, "mapname") = "\u5730\u4fa1\u516c\u793a"
      attr(sfLNI, "palette") = ""
    }

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
#' @param year Year of the data. Defaults to 2018.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" for tmap.
#'
#' @export
read_landnuminfo_urbanarea <- function(code_pref, code_muni, year = 2018, data_dir = NULL){
  year = check_year(year)
  if (year != 2011 && year != 2018) stop(paste("The data is not available for year", year))
  if (year == 2018 && code_pref == 23) {
    year = 2011
    warning("The data is not available for year 2018 for Aichi prefcture. Uses year 2011 instead.")
  }

  if (year == 2006) {
    sfLNI = read_landnuminfo("A09", code_pref, code_muni, year, filetype = "shp", geometry = "POLYGON", data_dir = data_dir)
  } else if (year == 2011) {
    sfLNI = read_landnuminfo_urbanarea_2011(code_pref, data_dir = data_dir)
  } else {
    sfLNI = read_landnuminfo("A09", code_pref, code_muni, year, filetype = "geojson", geometry = "POLYGON", data_dir = data_dir)
  }

  sfLNI$layer = NA
  sfLNI$layer = factor(sfLNI$layer, levels = c("\u5e02\u8857\u5316\u533a\u57df",
                                               "\u5e02\u8857\u5316\u8abf\u6574\u533a\u57df",
                                               "\u305d\u306e\u4ed6\u7528\u9014\u5730\u57df",
                                               "\u7528\u9014\u672a\u8a2d\u5b9a",
                                               "\u90fd\u5e02\u5730\u57df"))
  # 2011
  if (year == 2011 || year == 2006) {
    st_crs(sfLNI) <- "EPSG:4612"
    sfLNI[sfLNI$layer_no == 1, "layer"] <- "\u90fd\u5e02\u5730\u57df"
    sfLNI[sfLNI$layer_no == 2, "layer"] <- "\u5e02\u8857\u5316\u533a\u57df"
    sfLNI[sfLNI$layer_no == 3, "layer"] <- "\u5e02\u8857\u5316\u8abf\u6574\u533a\u57df"
    sfLNI[sfLNI$layer_no == 4, "layer"] <- "\u305d\u306e\u4ed6\u7528\u9014\u5730\u57df"
  } else {
    # 2018
    st_crs(sfLNI) <- "EPSG:6668"
    sfLNI[sfLNI$layer_no == 1, "layer"] <- "\u5e02\u8857\u5316\u533a\u57df"
    sfLNI[sfLNI$layer_no == 2, "layer"] <- "\u5e02\u8857\u5316\u8abf\u6574\u533a\u57df"
    sfLNI[sfLNI$layer_no == 3, "layer"] <- "\u305d\u306e\u4ed6\u7528\u9014\u5730\u57df"
    sfLNI[sfLNI$layer_no == 4, "layer"] <- "\u7528\u9014\u672a\u8a2d\u5b9a"
  }

  sfLNI$layer_no = factor(sfLNI$layer_no,
    levels = c(1,2,3,4))
  attr(sfLNI, "mapname") = "\u90fd\u5e02\u5730\u57df\u30c7\u30fc\u30bf"
  attr(sfLNI, "sourceName") = "\u300c\u56fd\u571f\u6570\u5024\u60c5\u5831\uff08\u5730\u4fa1\u516c\u793a\u30c7\u30fc\u30bf\uff09\u300d\uff08\u56fd\u571f\u4ea4\u901a\u7701\uff09"
  attr(sfLNI, "sourceURL") = "https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A09.html"
  attr(sfLNI, "col") = "layer"
  # attr(sfLNI, "palette") = c("#16A085","#D1F2EB","#1F618D","#229954","#BA4A00","#E74C3C","#808B96")

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

#' Download spatial data of Prefecture-surveyed Land Prices of Japan
#'
#' @description
#' Function to download spatial data of Prefecture-surveyed Land Prices of Japan. The returned value is an sf object.
#'
#' The organisation defines this data as "PrefectureLandPriceResearch," but we call "Prefecture-surveyed Land Priced" instead.
#'
#' Please note that some columns of real number (L02_033, L02_034) are coerced to integer for several years (e.g. 2018).
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2023.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_preflandprice <- function(code_pref, code_muni = NULL, year = 2022, data_dir = NULL){
  year4digit <- check_year(year)

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("L02", code_pref, NULL, year4digit, data_dir)

  if (!is.null(sfLNI)) {
    if (!is.null(code_muni)){
      lstCodeMuni <- get_wards(code_pref, code_muni, year4digit)
      if (length(lstCodeMuni) > 0) {
        sfLNI2 <- NULL
        for (code_muni_single in lstCodeMuni){
          strNameMuni <- get_muni_name(code_pref, code_muni_single)
          if (is.null(sfLNI2)) {
            if (year4digit == 2022 || year4digit == 2021) {
              sfLNI2 <- subset(sfLNI, L02_020 == paste(check_code_pref_as_char(code_pref),check_code_muni_as_char(code_pref,code_muni),sep=""))
            } else if (year4digit %in% c(2020, 2018, 2017)) {
              sfLNI2 <- subset(sfLNI, L02_021 == paste(check_code_pref_as_char(code_pref),check_code_muni_as_char(code_pref,code_muni),sep=""))
            } else {
              sfLNI2 <- subset(sfLNI, L02_017 == paste(check_code_pref_as_char(code_pref),check_code_muni_as_char(code_pref,code_muni),sep=""))
            }
          } else {
            if (year4digit == 2022 || year4digit == 2021) {
              sfLNI2 <- rbind(sfLNI2, subset(sfLNI, L02_020 == paste(check_code_pref_as_char(code_pref),check_code_muni_as_char(code_pref,code_muni),sep="")))
            } else if (year4digit %in% c(2020, 2018, 2017)) {
              sfLNI2 <- rbind(sfLNI2, subset(sfLNI, L02_021 == paste(check_code_pref_as_char(code_pref),check_code_muni_as_char(code_pref,code_muni),sep="")))
            } else {
              sfLNI2 <- rbind(sfLNI2, subset(sfLNI, L02_017 == paste(check_code_pref_as_char(code_pref),check_code_muni_as_char(code_pref,code_muni),sep="")))
            }
          }
        }
      }
      if (!is.null(sfLNI2)) sfLNI <- sfLNI2
    }

    if (!is.null(sfLNI)) {
      attr(sfLNI, "mapname") = "\u90fd\u9053\u5e9c\u770c\u5730\u4fa1\u8abf\u67fb"
      attr(sfLNI, "palette") = ""
    }

    return(sfLNI)
  }
}

#' message the list of available Land Numerical Information data.
#'
#' @description
#' message the list of available Land Numerical Information data.
#'
#' @export
list_landnuminfo <- function(){
  dfTestedMap <- get_definition("landnuminfo")
  message(dfTestedMap)
}
