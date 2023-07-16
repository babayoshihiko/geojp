read_landnuminfo_mesh_by_csv <- function(maptype, code_mesh, year, data_dir = NULL){
  year4digit = check_year(year)

  strTempDir = tempdir()
  if (!is.null(data_dir)) {
    if (dir.exists(data_dir)) {
      strTempDir = data_dir
    }
  }

  # Checks the arguments
  if (mode(code_mesh) == "numeric"){
    if (code_mesh < 3035 || code_mesh > 6848) {
      stop(paste("Invalid argument: code_mesh must be between 3035 and 6848. Your mesh code was", code_mesh))
    } else {
      code_mesh = as.character(code_mesh)
    }
  }
  if (nchar(code_mesh) != 4) stop(paste("Invalid argument: code_mesh:", code_mesh))

  df <- read.csv(file.path("data",paste(maptype, ".csv", sep = "")))
  df <- df[df$year == year4digit,]
  if (nrow(df) != 1) stop(paste("The target year", year, "not found in", paste("data/", maptype, ".csv", sep = "")))

  strLNIUrl = gsub("code_mesh",code_mesh,df$url)
  strLNIZip = file.path(strTempDir,gsub("code_mesh",code_mesh,df$zip))
  strLNIFile = ""
  strLNIFile1 = file.path(strTempDir,gsub("code_mesh",code_mesh,df$shp))
  strLNIFile2 = file.path(strTempDir,gsub("code_mesh",code_mesh,df$altdir),gsub("code_mesh",code_mesh,df$shp))
  strLNIFile3 = file.path(strTempDir,paste(gsub("code_mesh",code_mesh,df$altdir),"\\",gsub("code_mesh",code_mesh,df$shp),sep=""))

  # Checks if the shp file exists
  if (length(Sys.glob(strLNIFile1)) == 1){
    strLNIFile = Sys.glob(strLNIFile1)
  } else if (length(Sys.glob(strLNIFile2)) == 1){
    strLNIFile = Sys.glob(strLNIFile2)
  } else if (length(Sys.glob(strLNIFile3)) == 1){
    strLNIFile = Sys.glob(strLNIFile3)
  }

  # If the file does not exist, download the zip file and uzip
  if (!file.exists(strLNIFile)){
    if (!file.exists(strLNIZip)) {
      utils::download.file(strLNIUrl, strLNIZip, mode="wb")
      message(paste("Downloaded the file and saved in", strLNIUrl))
    }
    unzip_ja(strLNIZip, exdir = strTempDir)
  }

  # Checks if the shp file exists
  if (length(Sys.glob(strLNIFile1)) == 1){
    strLNIFile = Sys.glob(strLNIFile1)
  } else if (length(Sys.glob(strLNIFile2)) == 1){
    strLNIFile = Sys.glob(strLNIFile2)
  } else if (length(Sys.glob(strLNIFile3)) == 1){
    strLNIFile = Sys.glob(strLNIFile3)
  }
  if (!file.exists(strLNIFile)){
    stop(paste("Cannot find the file:", strLNIFile))
  }

  sfLNI = sf::read_sf(strLNIFile, options = "ENCODING=CP932", stringsAsFactors=FALSE)

  if (exists("sfLNI")) {
    # Older data may not have *.prj. Set CRS manually.
    if (is.na(sf::st_crs(sfLNI))) {
      sf::st_crs(sfLNI) = 4612
    }
    attr(sfLNI, "sourceURL") = df$source
    attr(sfLNI, "year") = year4digit
    return(sfLNI)
  }
}

get_mesh3_by_muni <- function(code_pref, code_muni) {
  df <- read.csv(file.path("data","muni_mesh3.csv"))
  df <- df[df$code_pref == code_pref & df$code_muni == code_muni,]

  return(df$code_mesh3)
}

#' Download spatial data of Mesh 3 of Japan
#'
#' @description
#' Function to download spatial data of Mesh 3 of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2016. The years 2016,2014,2009,2006 are clean. Some mesh codes not available for other years.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_mesh3 <- function(code_pref, code_muni, year = 2016, data_dir = NULL){
  year4digit = check_year(year)

  lstMesh3Codes = get_mesh3_by_muni(code_pref, code_muni)
  if (length(lstMesh3Codes) > 1) {
    i = 1
    for (code_mesh3 in lstMesh3Codes) {
      if (i == 1) {
        sfLNI = read_landnuminfo_mesh_by_csv("L03", code_mesh3, year4digit, data_dir)
        i = i + 1
      } else {
        sfLNI = rbind(sfLNI, read_landnuminfo_mesh_by_csv("L03", code_mesh3, year4digit, data_dir))
      }
    }
  }

  if (exists("sfLNI")) {
    #sfLNI$Max_Column <- factor(levels = c("L03a_002","L03a_003","L03a_004","L03a_005","L03a_006","L03a_007","L03a_008","L03a_009","L03a_010","L03a_011","L03a_012","L03a_013","L03a_014","L03a_015","L03a_016"))
    sfLNI$Max_Column <- apply(sfLNI, 1, get_max_column)
    attr(sfLNI, "mapname") = "\u571f\u5730\u5229\u75283\u6b21\u30e1\u30c3\u30b7\u30e5"
    attr(sfLNI, "sourceName") = "\u300c\u56fd\u571f\u6570\u5024\u60c5\u5831\uff08\u7acb\u5730\u9069\u6b63\u5316\u8a08\u753b\u533a\u57df\u30c7\u30fc\u30bf\uff09\u300d\uff08\u56fd\u571f\u4ea4\u901a\u7701\uff09"
    attr(sfLNI, "col") = "Max_Column"
    # https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-L03-b_r.html
    # May not be compatible for older years
    attr(sfLNI, "palette") = c("#FFFF00","#FFCC99","#00AA00","#FF9900","#FF0000","#8C8C8C","#B4B4B4","#C8460F","#0000FF","#FFFF99","#00CCFF","#00FF00","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF")

    return(sfLNI)
  } else if (year4digit == 2021) {
    warning("The mesh may not be available for the year 2021.")
  }
}

get_max_column <- function(row) {
  row <- sapply(row, is.numeric)
  max_column <- names(row)[which.max(row)]
  return(max_column)
}


