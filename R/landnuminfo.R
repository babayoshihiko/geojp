#' Download spatial data of National Lang Numeric Information of Japan
#'
#' @description
#' Low level function to download spatial data of National Lang Numeric Information
#' (or kokudo suuchi joho in Japanese) of Japan.
#'
#' @param maptype The map type code (e.g. "A01").
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality (city, town, or village).
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
  code_muni <- check_code_muni_as_char(code_pref, code_muni)

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
  if (!is.null(code_muni)){
    code_muni <- check_code_muni_as_char(code_pref, code_muni)
  } else {
    code_muni <- ""
  }

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

  # Subset the data by municipality
  if (!is.null("sfLNI") & code_muni != "" & !is.na(dfTemp[1,"muni_column"])) {
    # If the municipality is Ordnance Disignated, then
    # several municipality codes
    lstCodeMuni <- get_wards(code_pref, code_muni, year4digit)
    if (length(lstCodeMuni) > 0) {
      sfLNI2 <- NULL
      for (code_muni_single in lstCodeMuni){
        if (is.null(sfLNI2)) {
          sfLNI2 <-
            sfLNI[sfLNI[[as.character(dfTemp[1,"muni_column"])]] ==
                    check_code_muni_as_char(code_pref,
                                            code_muni_single,
                                            return=dfTemp[1,"muni_type"]),]
        } else {
          sfLNI2 <- rbind(sfLNI2,
            sfLNI[sfLNI[[as.character(dfTemp[1,"muni_column"])]] ==
                    check_code_muni_as_char(code_pref,
                                            code_muni_single,
                                            return=dfTemp[1,"muni_type"]),]
          )
        }
      }
      if (!is.null(sfLNI2)) {
        sfLNI <- sfLNI2
      } else {
        warning("Not subsetted. The given code_pref and code_muni matched none (geojp::read_landnuminfo_by_csv).")
      }
    }
  }

  return(sfLNI)
}

#' @importFrom utils download.file
#' @importFrom sf read_sf
get_sfLNI <- function(maptype, strLNIFile1, strLNIFile2, strLNIFile3, strLNIUrl, strLNIZip, year4digit,
                      strTempDir,
                      multifiles = "error",
                      encoding = "CP932"){
  # Read the MapType definition
  dfTemp <- get_definition(maptype)
  dfTemp <- dfTemp[dfTemp$year == year4digit,]
  if (nrow(dfTemp) != 1) stop(paste("The target year", year4digit, "not found in", paste("data-raw/", maptype, ".csv", sep = "")))

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
      message(paste("Unzipped the file in", strTempDir))
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
      if (year4digit >= 2014) {
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

#' Download spatial data of Official Land Price of Japan
#'
#' @description
#' Function to download spatial data of Official Land Price of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality. If specified, subtract the data by the column A48_003.
#' @param year Year of the data. Defaults to 2025.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" for tmap.
#'
#' @export
read_landnuminfo_officiallandprice <- function(code_pref, code_muni = NULL, year = 2025, data_dir = NULL){
  year4digit <- check_year(year)

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("L01", code_pref, code_muni, year4digit, data_dir)

  if (!is.null(sfLNI)) {
    attr(sfLNI, "mapname") = "\u5730\u4fa1\u516c\u793a"
  }

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
  sfLNI <- read_landnuminfo_by_csv("L02", code_pref, code_muni, year4digit, data_dir)

  if (!is.null(sfLNI)) {
    attr(sfLNI, "mapname") = "\u90fd\u9053\u5e9c\u770c\u5730\u4fa1\u8abf\u67fb"
  }

  return(sfLNI)
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
