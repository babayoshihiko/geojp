#' Download spatial data of National Lang Numeric Information of Japan
#'
#' @description
#' Low level function to download spatial data of National Lang Numeric Information
#' (or kokudo suuchi joho in Japanese) of Japan.
#'
#' @param maptype The map type code (e.g. "A01").
#' @param code_pref The 2-digit code of a prefecture.
#' @param code_muni The 3-digit code of a municipality (city, town, or village).
#' @param year Year of the data. Defaults to 2020.
#' @param filetype either "geojson" or "shp".
#' @param geometry "POINT", "LINESTRING", or "POLYGON"
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object
#'
#' @export
read_landnuminfo <- function(maptype, code_pref, code_muni = NULL, year = 2020, filetype = "geojson", geometry = NULL, data_dir = NULL){
  strTempDir = tempdir()
  #if (!is.null(data_dir)) {
  #  if (dir.exists(data_dir)) {
  #    strTempDir = data_dir
  #  }
  #}

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
    if (year > 2021 || year < 2000) {
      stop("Invalid argument: year must between 2021 and 2000.")
    }
    if (year < 2010) {
      year = paste("0", as.character(year - 2000), sep = "")
    } else {
      year = as.character(year - 2000)
    }
  }
  if (nchar(code_pref) != 2) stop(paste("Invalid argument: code_pref:", code_pref))
  if (nchar(year) != 2) stop(paste("Invalid argument: year:", year))

  strLNIZip = file.path(strTempDir,
                        paste(maptype, "-", year, "_", code_pref, "_GML.zip", sep = ""))
  strLNIUrl = paste("https://nlftp.mlit.go.jp/ksj/gml/data/", maptype, "/", maptype, "-", year, "/", maptype, "-", year, "_", code_pref, "_GML.zip", sep = "")

  if (!file.exists(strLNIZip)) utils::download.file(strLNIUrl, strLNIZip, mode="wb")


  if (filetype == "geojson") {
    utils::unzip(strLNIZip, exdir = strTempDir)
    strLNIFile = find_geojson_file(maptype, code_pref, code_muni, year, strTempDir)
    sfLNI = sf::read_sf(strLNIFile)
  } else if (filetype == "shp") {
    # Avoid the unzip error due to Japanese filenames
    # This error happens with maptype = "A31", code_pref = 4, code_muni = 215
    SHPFiles = c( paste(maptype,"-",year,"_",code_pref,".shp",sep=""),
                  paste(maptype,"-",year,"_",code_pref,".shx",sep=""),
                  paste(maptype,"-",year,"_",code_pref,".dbf",sep=""),
                  paste(maptype,"-",year,"_",code_pref,".prj",sep=""))
    utils::unzip(strLNIZip, files = SHPFiles, exdir = strTempDir)
    strLNIFile = file.path(strTempDir,
                           paste(maptype, "-", year,"_", code_pref,".shp",sep=""))
    if (!file.exists(strLNIFile)){
      utils::unzip(strLNIZip, exdir = strTempDir)
      strLNIFile = find_shp_file(maptype, code_pref, code_muni, year, strTempDir)
    }

    sfLNI = sf::read_sf(strLNIFile, options = "ENCODING=CP932", stringsAsFactors=FALSE)
    # Older data may not have *.prj. Set CRS manually.
    if (is.na(sf::st_crs(sfLNI))) {
      if (year >= 2014) {
        sf::st_crs(sfLNI) = 6668
      } else {
        sf::st_crs(sfLNI) = 4612
      }
    } else {
      sfLNI = sf::st_transform(sfLNI, "EPSG:6668")
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

  return(sfLNI)
}


#' Download spatial data of Land Use of Japan
#'
#' @description
#' Function to download spatial data of  land uses. The returned value is an sf object with extra
#' attr "col" and "palette". The "col" is the factored column that indicate the land use classes
#' and "palette" provides the colour palette based on Japan Industrial Standard.
#'
#' @param code_pref The 2-digit code of a prefecture.
#' @param code_muni The 3-digit code of a municipality (city, town, or village).
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

  if (year == 2019) sf = read_landnuminfo("A29", code_pref, code_muni, year, filetype = "geojson", geometry = "POLYGON", data_dir = data_dir)
  if (year == 2011) {
    sf = read_landnuminfo("A29", code_pref, code_muni, year, filetype = "shp", geometry = "POLYGON", data_dir = data_dir)
    sf = sf::st_set_crs(sf, 4612)
  }
  sf$A29_004 <- factor(sf$A29_004, levels=c(1,2,3,4,5,6,7,8,9,10,11,12,21,99))
  sf$A29_005 <- factor(sf$A29_005, levels=c("第一種低層住居専用地域","第二種低層住居専用地域","第一種中高層住居専用地域","第二種中高層住居専用地域","第一種住居地域","第二種住居地域","準住居地域","近隣商業地域","商業地域","準工業地域","工業地域","工業専用地域","田園住居地域","不明"))

  attr(sf, "mapname") = "用途地域"
  attr(sf, "sourceName") = "「国土数値情報（用途地域データ）」（国土交通省）"
  attr(sf, "sourceURL") = "https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A29-v2_1.html"
  attr(sf, "col") = "A29_005"
  attr(sf, "palette") = c("#00BEA9","#6AD5BD","#A8D666","#FAE294","#F4E268","#F8D79C","#FFB580","#F3A5B9","#EE534F","#B3A8CB","#8AD0E4","#2CB3DE","#F4B187","#FFFFFF")
  return(sf)
}

#' Download spatial data of Location Normalization of Japan
#'
#' @description
#' Function to download spatial data of Location Normalization. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of a prefecture.
#' @param code_muni The 3-digit code of a municipality (city, town, or village).
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

  sf = read_landnuminfo("A50", code_pref, code_muni, year, filetype = "geojson", geometry = "POLYGON", data_dir = data_dir)
  sf$A50_006 <- factor(sf$A50_006, levels=c("1","2","3"), labels=c("立地適正化計画区域","居住誘導区域","都市機能誘導区域"))

  attr(sf, "mapname") = "立地適正化計画区域"
  attr(sf, "sourceName") = "「国土数値情報（立地適正化計画区域データ）」（国土交通省）"
  attr(sf, "sourceURL") = "https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A50-v1_0.html"
  attr(sf, "col") = "A50_006"
  attr(sf, "palette") = c("#E2FFE3","#99CDFD","#F87E88")
  return(sf)
}

#' Download spatial data of Flood Inundation Risk of Japan
#'
#' @description
#' Function to download spatial data of Flood Inundation Risk of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of a prefecture.
#' @param code_muni The 3-digit code of a municipality (city, town, or village).
#' @param year Year of the data. Defaults to 2012.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_flood <- function(code_pref, code_muni, year = 2012, data_dir = NULL){
  year = check_year(year)
  if (year != 2012) stop(paste("The data is not available for year", year))

  sf = read_landnuminfo("A31", code_pref, code_muni, year, filetype = "shp", geometry = "POLYGON", data_dir = data_dir)
  sf$A31_001 <- factor(sf$A31_001, levels=c(11,12,13,14,15), labels=c("0～0.5ｍ未満","0.5～1.0ｍ未満","1.0～2.0ｍ未満","2.0～5.0ｍ未満","5.0ｍ以上"))

  attr(sf, "mapname") = "洪水浸水想定区域"
  attr(sf, "col") = "A31_001"
  attr(sf, "palette") = c("#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C") # RColorBrewer::brewer.pal(5, "Blues")
  return(sf)
}

#' Download spatial data of Welfare Facilities of Japan
#'
#' @description
#' Function to download spatial data of Welfare Facilities of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of a prefecture.
#' @param code_muni The 3-digit code of a municipality (city, town, or village).
#' @param year Year of the data. Defaults to 2012.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_welfare <- function(code_pref, code_muni, year = 2021, data_dir = NULL){
  year = check_year(year)
  if (year != 2021 & year != 2015 & year != 2011) stop(paste("The data is not available for year", year))

  sf = read_landnuminfo("P14", code_pref, code_muni, year, filetype = "geojson", geometry = "POINT", data_dir = data_dir)
  sf$P14_005 <- factor(sf$P14_005, levels=c("01","02","03","04","05","06","99"),
                       labels=c("保護施設","老人福祉施設","障害者支援施設等","身体障害者社会参加支援施設","児童福祉施設等","母子・父子福祉施設","その他の社会福祉施設等"))

  attr(sf, "mapname") = "洪水浸水想定区域"
  attr(sf, "sourceName") = "「国土数値情報（洪水浸水想定区域データ）」（国土交通省）"
  attr(sf, "sourceURL") = "https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A31.html"
  attr(sf, "col") = "P14_005"
  attr(sf, "palette") = c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D") # RColorBrewer::brewer.pal(7, "Dark2")
  return(sf)
}

list_landnuminfo <- function(){
  dfTestedMap <- utils::read.table(text = "MapCode,Year,FileType,MapUnit,MuniColumn,Geometry,Desc,URL
A29,2019,geojson,muni,A29_003,POLYGON,用途地域,https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A29-v2_1.html
A29,2011,shp,muni,A29_003,POLYGON,用途地域,https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A29-v2_1.html
A50,2020,geojson,muni,A50_004,POLYGON,立地適正化計画区域,https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A50-v1_0.html
A31,2012,shp,pref,,POLYGON,洪水浸水想定区域,https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A31.html
P14,2021,geojson,pref,P14_003,POINT,福祉施設,https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-P14-v2_1.html",
  header = TRUE, sep=",", colClasses = "character")
  print(dfTestedMap)
}
