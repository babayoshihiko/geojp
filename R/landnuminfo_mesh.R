read_landnuminfo_mesh_by_csv <- function(maptype, code_mesh, year, data_dir = NULL, epsg = NULL){
  year4digit <- check_year(year)
  strTempDir <- check_data_dir(data_dir)

  # Checks the arguments
  if (mode(code_mesh) == "numeric"){
    if (code_mesh < 3035 || code_mesh > 6848) {
      stop(paste("Invalid argument: code_mesh must be between 3035 and 6848. Your mesh code was", code_mesh))
    } else {
      code_mesh <- as.character(code_mesh)
    }
  }
  if (nchar(code_mesh) != 4) stop(paste("Invalid argument: code_mesh:", code_mesh))

  # Read the MapType definition
  dfTemp <- get_definition(maptype)
  dfTemp <- dfTemp[dfTemp$year == year4digit,]

  # Set the files
  strLNIUrl <- gsub("code_mesh",code_mesh,dfTemp[1,"url"])
  strLNIZip <- file.path(strTempDir,gsub("code_mesh",code_mesh,dfTemp[1,"zip"]))
  strLNIFile1 <- file.path(strTempDir,gsub("code_mesh",code_mesh,dfTemp[1,"shp"]))
  strLNIFile2 <- file.path(strTempDir,gsub("code_mesh",code_mesh,dfTemp[1,"altdir"]),gsub("code_mesh",code_mesh,dfTemp[1,"shp"]))
  strLNIFile3 <- file.path(strTempDir,paste(gsub("code_mesh",code_mesh,dfTemp[1,"altdir"]),"\\\\",gsub("code_mesh",code_mesh,dfTemp[1,"shp"]),sep=""))

  sfLNI <- get_sfLNI(maptype, strLNIFile1, strLNIFile2, strLNIFile3, strLNIUrl, strLNIZip, year4digit, strTempDir)

  if (!is.null(sfLNI)) {
    attr(sfLNI, "year") <- year4digit
    attr(sfLNI, "col") <- as.character(dfTemp[1,"col"])
    attr(sfLNI, "sourceName") <- "\u300c\u56fd\u571f\u6570\u5024\u60c5\u5831\uff08\u884c\u653f\u533a\u57df\u30c7\u30fc\u30bf\uff09\u300d\uff08\u56fd\u571f\u4ea4\u901a\u7701\uff09" # MLIT
    attr(sfLNI, "sourceURL") <- dfTemp[1,"source"]
  }

  return(sfLNI)
}

get_mesh1_by_muni <- function(code_pref, code_muni) {

  dfTemp <- get_definition("muni_mesh1")
  dfTemp <- dfTemp[dfTemp$code_pref == code_pref & dfTemp$code_muni == code_muni,]
  mesh1 <- unique(dfTemp$code_mesh1)
  mesh1 <- mesh1[!is.na(mesh1)]
  return(mesh1)
}

#' Download spatial data of Mesh 3 of Japan
#'
#' @description
#' Function to download spatial data of Mesh 3 of Japan. The returned value is an sf object.
#' The function reads Land Use Third Level Mesh data. See https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-L03-a.html.
#'
#' The available years are: 2021, 2016, 2014, 2009, 2006, 1997, 1991, 1987, 1976.
#'
#' If the file of a mesh is not available at the site, then the mesh is simply ignored.
#'
#' The default year is 2016 because 2021 lacks many areas. For year 2016, mesh_code 6740 (part of Rebun Town, pref_code 1, muni_code 517) is missing.
#'
#' The year 1997 lacks meshes like 6748, 6747, 6740, 6646, 6645, 6541, 6546, 6339, 6243, 5038 which include part of several cities and towns.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2016. The years 2016,2014,2009,2006 are clean. Some mesh codes not available for other years.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @importFrom sf st_drop_geometry
#' @export
read_landnuminfo_mesh3 <- function(code_pref, code_muni, year = 2016, data_dir = NULL){
  year4digit <- check_year(year)

  # Get the mesh code
  lstMesh1Codes <- get_mesh1_by_muni(code_pref, code_muni)

  sfLNI <- NULL
  if (length(lstMesh1Codes) >= 1) {
    for (code_mesh1 in lstMesh1Codes) {
      if (is.null(sfLNI)) {
        tryCatch(sfLNI <- read_landnuminfo_mesh_by_csv("L03", code_mesh1, year4digit, data_dir),
          error = function(cnd){ sfLNI <- NULL }
        )
      } else {
        sfLNI2 <- NULL
        tryCatch(sfLNI2 <- read_landnuminfo_mesh_by_csv("L03", code_mesh1, year4digit, data_dir),
          error = function(cnd){ sfLNI2 <- NULL }
        )
        if (!is.null(sfLNI2)) sfLNI <- rbind(sfLNI, sfLNI2)
      }
    }
  } else {
    stop(paste("No city found for pref:", code_pref, ", city:", code_muni))
  }

  if (!is.null(sfLNI)) {
    # Read the MapType definition
    dfTemp <- get_definition("L03")
    dfTemp <- dfTemp[dfTemp$year == year4digit,]

    sfTemp <- sfLNI[,-1]
    sfLNI[,as.character(dfTemp[1,"col"])] <- colnames(sfTemp)[apply(sfTemp,1,which.max)]
    attr(sfLNI, "mapname") <- "\u571f\u5730\u5229\u75283\u6b21\u30e1\u30c3\u30b7\u30e5"

    # https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-L03-b_r.html
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
        if (length(temp_levels) == length(temp_labels)){
          sfLNI[,as.character(dfTemp[1,"col"])] <- factor(unlist(sf::st_drop_geometry(sfLNI)[,as.character(dfTemp[1,"col"])]), levels = temp_levels, labels = temp_labels)
        } else {
          sfLNI[,as.character(dfTemp[1,"col"])] <- factor(unlist(sf::st_drop_geometry(sfLNI)[,as.character(dfTemp[1,"col"])]), levels = temp_levels)
        }
      }
      if (length(temp_levels) == length(temp_palette)){
        attr(sfLNI, "palette") <- temp_palette
      }
    }
    return(sfLNI)
  } else if (year4digit == 2021) {
    warning("The mesh may not be available for the year 2021.")
  }
}

#' Download spatial data of Mesh Subdivision of Japan
#'
#' @description
#' Function to download spatial data of Mesh Subdivision of Japan. The returned value is an sf object.
#' The function reads Land Use Subdivision Level Mesh data. See https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-L03-a.html.
#'
#' Beware! The object size will be large! More than 620,000 rows! Use read_landnuminfo_mesh3() instead.
#'
#' The available years are: 2021, 2016, 2014, 2009, 2006, 1997, 1991, 1987, 1976.
#'
#' If the file of a mesh is not available at the site, then the mesh is simply ignored.
#'
#' The default year is 2006 because the year covers isolated islands.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2006. The years 2016,2014,2009,2006 are clean. Some mesh codes not available for other years.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_meshsub <- function(code_pref, code_muni, year = 2006, data_dir = NULL){
  year4digit <- check_year(year)

  lstMesh1Codes <- get_mesh1_by_muni(code_pref, code_muni)

  sfLNI <- NULL
  if (length(lstMesh1Codes) >= 1) {
    for (code_mesh1 in lstMesh1Codes) {
      if (is.null(sfLNI)) {
        tryCatch(sfLNI <- read_landnuminfo_mesh_by_csv("L03-b", code_mesh1, year4digit, data_dir),
          error = function(cnd){ sfLNI <- NULL }
        )
      } else {
        sfLNI2 <- NULL
        tryCatch(sfLNI2 <- read_landnuminfo_mesh_by_csv("L03-b", code_mesh1, year4digit, data_dir),
          error = function(cnd){ sfLNI2 <- NULL }
        )
        if (!is.null(sfLNI2)) sfLNI <- rbind(sfLNI, sfLNI2)
      }
    }
  } else {
    stop(paste("No city found for pref:", code_pref, ", city:", code_muni))
  }

  if (!is.null(sfLNI)) {
    attr(sfLNI, "mapname") = "\u571f\u5730\u5229\u75283\u6b21\u30e1\u30c3\u30b7\u30e5"

    dfTemp <- get_definition("L03-b")
    dfTemp <- dfTemp[dfTemp$year == year4digit,]
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
        if (length(temp_levels) == length(temp_labels)){
          sfLNI[,as.character(dfTemp[1,"col"])] <- factor(unlist(sf::st_drop_geometry(sfLNI)[,as.character(dfTemp[1,"col"])]), levels = temp_levels, labels = temp_labels)
        } else {
          sfLNI[,as.character(dfTemp[1,"col"])] <- factor(unlist(sf::st_drop_geometry(sfLNI)[,as.character(dfTemp[1,"col"])]), levels = temp_levels)
        }
      }
      if (length(temp_levels) == length(temp_palette)){
        attr(sfLNI, "palette") <- temp_palette
      }
    }

    return(sfLNI)
  }
}

