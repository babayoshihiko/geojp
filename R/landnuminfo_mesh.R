read_landnuminfo_mesh_by_csv <- function(maptype, code_mesh, year, data_dir = NULL){
  year4digit = check_year(year)

  strTempDir = tempdir()
  if (!is.null(data_dir)) {
    if (dir.exists(data_dir)) {
      strTempDir = data_dir
    }
  }

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
  strLNIFile2 = file.path(strTempDir, gsub("code_mesh",code_mesh,df$altdir),gsub("code_mesh",code_mesh,df$shp))
  strLNIFile3 = file.path(strTempDir, paste(gsub("code_mesh",code_mesh,df$altdir),"\\",gsub("code_mesh",code_mesh,df$shp),sep=""))
  if (!file.exists(strLNIZip)) {
    utils::download.file(strLNIUrl, strLNIZip, mode="wb")
    message(paste("Downloaded the file and saved in", strLNIUrl))
  }
  unzip_ja(strLNIZip, exdir = strTempDir)
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
  # Older data may not have *.prj. Set CRS manually.
  if (is.na(sf::st_crs(sfLNI))) {
    sf::st_crs(sfLNI) = 4612
  }
  attr(sfLNI, "sourceURL") = df$source
  attr(sfLNI, "year") = year4digit

  return(sfLNI)
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
#' @param year Year of the data. Defaults to 2016.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_mesh3 <- function(code_pref, code_muni, year = 2016, data_dir = NULL){
  year = check_year(year)

  lstMesh3Codes = get_mesh3_by_muni(code_pref, code_muni)
  if (length(lstMesh3Codes) > 1) {
    i = 1
    for (code_mesh3 in lstMesh3Codes) {
      if (i == 1) {
        sfLNI = read_landnuminfo_mesh_by_csv("L03", code_mesh3, year, data_dir)
        i = i + 1
      } else {
        sfLNI = rbind(sfLNI, read_landnuminfo_mesh_by_csv("L03", code_mesh3, year, data_dir))
      }
    }
  }

  attr(sfLNI, "mapname") = ""
  attr(sfLNI, "sourceName") = "\u300c\u56fd\u571f\u6570\u5024\u60c5\u5831\uff08\u884c\u653f\u533a\u57df\u30c7\u30fc\u30bf\uff09\u300d\uff08\u56fd\u571f\u4ea4\u901a\u7701\uff09"
  attr(sfLNI, "col") = ""
  attr(sfLNI, "palette") = ""

  return(sfLNI)
}
