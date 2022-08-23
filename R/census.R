#' Download spatial data of census tracts of the Japanese Population Census
#'
#' @description
#' Download spatial data of census tracts of the Japanese Population Census
#'
#' @param code_pref The 2-digit code of a prefecture.
#' @param code_muni The 3-digit code of a municipality (city, town, or village).
#' @param year Year of the data. Defaults to 2020.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object
#'
#' @export
read_census_tract <- function(code_pref, code_muni, year = 2020, data_dir = NULL){
  strTempDir = tempdir()
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
  if (mode(year) == "numeric"){
    if (year != 2020 && year != 2015 && year != 2010 && year != 2005 && year != 2000) {
      stop("Invalid argument: year must be one of 2020, 2015, 2010, 2005 or 2000.")
      year = as.character(year)
    }
  }
  if (nchar(code_pref) != 2) stop(paste("Invalid argument: code_pref:", code_pref))
  if (nchar(code_muni) != 3) stop(paste("Invalid argument: code_muni:", code_muni))
  if (nchar(year) != 4) stop(paste("Invalid argument: yera:", year))

  strCensusUrl = paste("https://www.e-stat.go.jp/gis/statmap-search/data?dlserveyId=A00200521",
                       year,
                       "&code=",
                       code_pref, code_muni,
                       "&coordSys=2&format=shape&downloadType=5&datum=2000",
                       sep = "")
  strCensusZip = file.path(strTempDir,
                           paste("A00200521", year, "DDSWC", code_pref, code_muni, ".zip", sep = ""))
  strCensusFile = file.path(strTempDir,
                            paste("r2ka", code_pref, code_muni, ".shp", sep = ""))

  if (!file.exists(strCensusZip)) download.file(strCensusUrl, strCensusZip, mode="wb")
  if (!file.exists(strCensusFile)) unzip(strCensusZip, exdir = strTempDir)
  sfCensus = sf::read_sf(strCensusFile,
                      options = "ENCODING=CP932",
                      stringsAsFactors=FALSE)
  sfCensus = sfCensus[,c(1,10,11,25,26)]   # columns: KEY_CODE, AREA, PERIMETER, JINKO, SETAI
  sfCensus = sf::st_transform(sfCensus, "EPSG:6668")
  sfCensus = sf::st_make_valid(sfCensus)

  return(sfCensus)
}

#' Download spatial data of census tracts of the Japanese Population Census
#'
#' @description
#' Download spatial data of census tracts of the Japanese Population Census
#' of an Ordnance-Disignated City of all wards (ku) combined.
#'
#' @param city_name an Ordnance-Disignated City in Japanese.
#' @param year Year of the data. Defaults to 2020.
#'
#' @return An `"sf" "data.frame"` object
#'
#' @export
read_census_odcity <- function(city_name, year = 2020){

  if (is.list(city_name)) stop("Invalid argument: city_name. Please give one city name only.")
  if (!is.vector(city_name)) stop("Invalid argument: city_name. Please give one city name only.")
  if (length(city_name) > 1) stop("Invalid argument: city_name. Please give one city name only.")
  city_name = sub("市", "", city_name)

  if (city_name == "札幌") {
    sfCity = read_census_tract("01", "101")
    sfCity = rbind(sfCity, read_census_tract("01", "102"))
    sfCity = rbind(sfCity, read_census_tract("01", "103"))
    sfCity = rbind(sfCity, read_census_tract("01", "104"))
    sfCity = rbind(sfCity, read_census_tract("01", "105"))
    sfCity = rbind(sfCity, read_census_tract("01", "106"))
    sfCity = rbind(sfCity, read_census_tract("01", "107"))
    sfCity = rbind(sfCity, read_census_tract("01", "108"))
    sfCity = rbind(sfCity, read_census_tract("01", "109"))
    sfCity = rbind(sfCity, read_census_tract("01", "110"))
  } else if (city_name == "仙台") {
    sfCity = read_census_tract("04", "101")
    sfCity = rbind(sfCity, read_census_tract("04", "102"))
    sfCity = rbind(sfCity, read_census_tract("04", "103"))
    sfCity = rbind(sfCity, read_census_tract("04", "104"))
    sfCity = rbind(sfCity, read_census_tract("04", "105"))
  } else if (city_name == "さいたま" || city_name == "埼玉") {
    sfCity = read_census_tract("11", "101")
    sfCity = rbind(sfCity, read_census_tract("11", "102"))
    sfCity = rbind(sfCity, read_census_tract("11", "103"))
    sfCity = rbind(sfCity, read_census_tract("11", "104"))
    sfCity = rbind(sfCity, read_census_tract("11", "105"))
    sfCity = rbind(sfCity, read_census_tract("11", "106"))
    sfCity = rbind(sfCity, read_census_tract("11", "107"))
    sfCity = rbind(sfCity, read_census_tract("11", "108"))
    sfCity = rbind(sfCity, read_census_tract("11", "109"))
    sfCity = rbind(sfCity, read_census_tract("11", "110"))
  } else if (city_name == "千葉") {
    sfCity = read_census_tract("11", "101")
    sfCity = rbind(sfCity, read_census_tract("11", "102"))
    sfCity = rbind(sfCity, read_census_tract("11", "103"))
    sfCity = rbind(sfCity, read_census_tract("11", "104"))
    sfCity = rbind(sfCity, read_census_tract("11", "105"))
    sfCity = rbind(sfCity, read_census_tract("11", "106"))
    sfCity = rbind(sfCity, read_census_tract("11", "107"))
    sfCity = rbind(sfCity, read_census_tract("11", "108"))
    sfCity = rbind(sfCity, read_census_tract("11", "109"))
    sfCity = rbind(sfCity, read_census_tract("11", "110"))
  } else if (city_name == "東京" || city_name == "東京都" || city_name == "２３区") {
    sfCity = read_census_tract("13", "101")
    sfCity = rbind(sfCity, read_census_tract("13", "102"))
    sfCity = rbind(sfCity, read_census_tract("13", "103"))
    sfCity = rbind(sfCity, read_census_tract("13", "104"))
    sfCity = rbind(sfCity, read_census_tract("13", "105"))
    sfCity = rbind(sfCity, read_census_tract("13", "106"))
    sfCity = rbind(sfCity, read_census_tract("13", "107"))
    sfCity = rbind(sfCity, read_census_tract("13", "108"))
    sfCity = rbind(sfCity, read_census_tract("13", "109"))
    sfCity = rbind(sfCity, read_census_tract("13", "110"))
    sfCity = rbind(sfCity, read_census_tract("13", "111"))
    sfCity = rbind(sfCity, read_census_tract("13", "112"))
    sfCity = rbind(sfCity, read_census_tract("13", "113"))
    sfCity = rbind(sfCity, read_census_tract("13", "114"))
    sfCity = rbind(sfCity, read_census_tract("13", "115"))
    sfCity = rbind(sfCity, read_census_tract("13", "116"))
    sfCity = rbind(sfCity, read_census_tract("13", "117"))
    sfCity = rbind(sfCity, read_census_tract("13", "118"))
    sfCity = rbind(sfCity, read_census_tract("13", "119"))
    sfCity = rbind(sfCity, read_census_tract("13", "120"))
    sfCity = rbind(sfCity, read_census_tract("13", "121"))
    sfCity = rbind(sfCity, read_census_tract("13", "122"))
    sfCity = rbind(sfCity, read_census_tract("13", "123"))
  } else if (city_name == "横浜") {
    sfCity = read_census_tract("14", "101")
    sfCity = rbind(sfCity, read_census_tract("14", "102"))
    sfCity = rbind(sfCity, read_census_tract("14", "103"))
    sfCity = rbind(sfCity, read_census_tract("14", "104"))
    sfCity = rbind(sfCity, read_census_tract("14", "105"))
    sfCity = rbind(sfCity, read_census_tract("14", "106"))
    sfCity = rbind(sfCity, read_census_tract("14", "107"))
    sfCity = rbind(sfCity, read_census_tract("14", "108"))
    sfCity = rbind(sfCity, read_census_tract("14", "109"))
    sfCity = rbind(sfCity, read_census_tract("14", "110"))
    sfCity = rbind(sfCity, read_census_tract("14", "111"))
    sfCity = rbind(sfCity, read_census_tract("14", "112"))
    sfCity = rbind(sfCity, read_census_tract("14", "113"))
    sfCity = rbind(sfCity, read_census_tract("14", "114"))
    sfCity = rbind(sfCity, read_census_tract("14", "115"))
    sfCity = rbind(sfCity, read_census_tract("14", "116"))
    sfCity = rbind(sfCity, read_census_tract("14", "117"))
    sfCity = rbind(sfCity, read_census_tract("14", "118"))
  } else if (city_name == "川崎") {
    sfCity = read_census_tract("14", "131")
    sfCity = rbind(sfCity, read_census_tract("14", "132"))
    sfCity = rbind(sfCity, read_census_tract("14", "133"))
    sfCity = rbind(sfCity, read_census_tract("14", "134"))
    sfCity = rbind(sfCity, read_census_tract("14", "135"))
    sfCity = rbind(sfCity, read_census_tract("14", "136"))
    sfCity = rbind(sfCity, read_census_tract("14", "137"))
  } else if (city_name == "相模原") {
    sfCity = read_census_tract("14", "151")
    sfCity = rbind(sfCity, read_census_tract("14", "152"))
    sfCity = rbind(sfCity, read_census_tract("14", "153"))
  } else if (city_name == "新潟") {
    sfCity = read_census_tract("15", "101")
    sfCity = rbind(sfCity, read_census_tract("15", "102"))
    sfCity = rbind(sfCity, read_census_tract("15", "103"))
    sfCity = rbind(sfCity, read_census_tract("15", "104"))
    sfCity = rbind(sfCity, read_census_tract("15", "105"))
    sfCity = rbind(sfCity, read_census_tract("15", "106"))
    sfCity = rbind(sfCity, read_census_tract("15", "107"))
    sfCity = rbind(sfCity, read_census_tract("15", "108"))
  } else if (city_name == "静岡") {
    sfCity = read_census_tract("22", "101")
    sfCity = rbind(sfCity, read_census_tract("22", "102"))
    sfCity = rbind(sfCity, read_census_tract("22", "103"))
  } else if (city_name == "浜松") {
    sfCity = read_census_tract("22", "131")
    sfCity = rbind(sfCity, read_census_tract("22", "132"))
    sfCity = rbind(sfCity, read_census_tract("22", "133"))
    sfCity = rbind(sfCity, read_census_tract("22", "134"))
    sfCity = rbind(sfCity, read_census_tract("22", "135"))
    sfCity = rbind(sfCity, read_census_tract("22", "136"))
    sfCity = rbind(sfCity, read_census_tract("22", "137"))
  } else if (city_name == "名古屋") {
    sfCity = read_census_tract("23", "101")
    sfCity = rbind(sfCity, read_census_tract("23", "102"))
    sfCity = rbind(sfCity, read_census_tract("23", "103"))
    sfCity = rbind(sfCity, read_census_tract("23", "104"))
    sfCity = rbind(sfCity, read_census_tract("23", "105"))
    sfCity = rbind(sfCity, read_census_tract("23", "106"))
    sfCity = rbind(sfCity, read_census_tract("23", "107"))
    sfCity = rbind(sfCity, read_census_tract("23", "108"))
    sfCity = rbind(sfCity, read_census_tract("23", "109"))
    sfCity = rbind(sfCity, read_census_tract("23", "110"))
    sfCity = rbind(sfCity, read_census_tract("23", "111"))
    sfCity = rbind(sfCity, read_census_tract("23", "112"))
    sfCity = rbind(sfCity, read_census_tract("23", "113"))
    sfCity = rbind(sfCity, read_census_tract("23", "114"))
    sfCity = rbind(sfCity, read_census_tract("23", "115"))
    sfCity = rbind(sfCity, read_census_tract("23", "116"))
  } else if (city_name == "京都") {
    sfCity = read_census_tract("26", "101")
    sfCity = rbind(sfCity, read_census_tract("26", "102"))
    sfCity = rbind(sfCity, read_census_tract("26", "103"))
    sfCity = rbind(sfCity, read_census_tract("26", "104"))
    sfCity = rbind(sfCity, read_census_tract("26", "105"))
    sfCity = rbind(sfCity, read_census_tract("26", "106"))
    sfCity = rbind(sfCity, read_census_tract("26", "107"))
    sfCity = rbind(sfCity, read_census_tract("26", "108"))
    sfCity = rbind(sfCity, read_census_tract("26", "109"))
    sfCity = rbind(sfCity, read_census_tract("26", "110"))
    sfCity = rbind(sfCity, read_census_tract("26", "111"))
  } else if (city_name == "大阪") {
    sfCity = read_census_tract("27", "102")
    sfCity = rbind(sfCity, read_census_tract("27", "103"))
    sfCity = rbind(sfCity, read_census_tract("27", "104"))
    sfCity = rbind(sfCity, read_census_tract("27", "105"))
    sfCity = rbind(sfCity, read_census_tract("27", "106"))
    sfCity = rbind(sfCity, read_census_tract("27", "107"))
    sfCity = rbind(sfCity, read_census_tract("27", "108"))
    sfCity = rbind(sfCity, read_census_tract("27", "109"))
    sfCity = rbind(sfCity, read_census_tract("27", "110"))
    sfCity = rbind(sfCity, read_census_tract("27", "111"))
    sfCity = rbind(sfCity, read_census_tract("27", "112"))
    sfCity = rbind(sfCity, read_census_tract("27", "113"))
    sfCity = rbind(sfCity, read_census_tract("27", "114"))
    sfCity = rbind(sfCity, read_census_tract("27", "115"))
    sfCity = rbind(sfCity, read_census_tract("27", "116"))
    sfCity = rbind(sfCity, read_census_tract("27", "117"))
    sfCity = rbind(sfCity, read_census_tract("27", "118"))
    sfCity = rbind(sfCity, read_census_tract("27", "119"))
    sfCity = rbind(sfCity, read_census_tract("27", "120"))
    sfCity = rbind(sfCity, read_census_tract("27", "121"))
    sfCity = rbind(sfCity, read_census_tract("27", "122"))
    sfCity = rbind(sfCity, read_census_tract("27", "123"))
    sfCity = rbind(sfCity, read_census_tract("27", "124"))
    sfCity = rbind(sfCity, read_census_tract("27", "125"))
    sfCity = rbind(sfCity, read_census_tract("27", "126"))
    sfCity = rbind(sfCity, read_census_tract("27", "127"))
    sfCity = rbind(sfCity, read_census_tract("27", "128"))
  } else if (city_name == "堺") {
    sfCity = read_census_tract("27", "141")
    sfCity = rbind(sfCity, read_census_tract("27", "142"))
    sfCity = rbind(sfCity, read_census_tract("27", "143"))
    sfCity = rbind(sfCity, read_census_tract("27", "144"))
    sfCity = rbind(sfCity, read_census_tract("27", "145"))
    sfCity = rbind(sfCity, read_census_tract("27", "146"))
    sfCity = rbind(sfCity, read_census_tract("27", "147"))
  } else if (city_name == "神戸") {
    sfCity = read_census_tract("28", "101")
    sfCity = rbind(sfCity, read_census_tract("28", "102"))
    sfCity = rbind(sfCity, read_census_tract("28", "105"))
    sfCity = rbind(sfCity, read_census_tract("28", "106"))
    sfCity = rbind(sfCity, read_census_tract("28", "107"))
    sfCity = rbind(sfCity, read_census_tract("28", "108"))
    sfCity = rbind(sfCity, read_census_tract("28", "109"))
    sfCity = rbind(sfCity, read_census_tract("28", "110"))
    sfCity = rbind(sfCity, read_census_tract("28", "111"))
  } else if (city_name == "岡山") {
    sfCity = read_census_tract("33", "101")
    sfCity = rbind(sfCity, read_census_tract("33", "102"))
    sfCity = rbind(sfCity, read_census_tract("33", "103"))
    sfCity = rbind(sfCity, read_census_tract("33", "104"))
  } else if (city_name == "広島") {
    sfCity = read_census_tract("34", "101")
    sfCity = rbind(sfCity, read_census_tract("34", "102"))
    sfCity = rbind(sfCity, read_census_tract("34", "103"))
    sfCity = rbind(sfCity, read_census_tract("34", "104"))
    sfCity = rbind(sfCity, read_census_tract("34", "105"))
    sfCity = rbind(sfCity, read_census_tract("34", "106"))
    sfCity = rbind(sfCity, read_census_tract("34", "107"))
    sfCity = rbind(sfCity, read_census_tract("34", "108"))
  } else if (city_name == "福岡") {
    sfCity = read_census_tract("40", "101")
    sfCity = rbind(sfCity, read_census_tract("40", "102"))
    sfCity = rbind(sfCity, read_census_tract("40", "103"))
    sfCity = rbind(sfCity, read_census_tract("40", "104"))
    sfCity = rbind(sfCity, read_census_tract("40", "105"))
    sfCity = rbind(sfCity, read_census_tract("40", "106"))
    sfCity = rbind(sfCity, read_census_tract("40", "107"))
    sfCity = rbind(sfCity, read_census_tract("40", "108"))
    sfCity = rbind(sfCity, read_census_tract("40", "109"))
  } else if (city_name == "北九州") {
    sfCity = read_census_tract("40", "131")
    sfCity = rbind(sfCity, read_census_tract("40", "132"))
    sfCity = rbind(sfCity, read_census_tract("40", "133"))
    sfCity = rbind(sfCity, read_census_tract("40", "134"))
    sfCity = rbind(sfCity, read_census_tract("40", "135"))
    sfCity = rbind(sfCity, read_census_tract("40", "136"))
    sfCity = rbind(sfCity, read_census_tract("40", "137"))
  } else if (city_name == "熊本") {
    sfCity = read_census_tract("43", "101")
    sfCity = rbind(sfCity, read_census_tract("43", "102"))
    sfCity = rbind(sfCity, read_census_tract("43", "103"))
    sfCity = rbind(sfCity, read_census_tract("43", "104"))
    sfCity = rbind(sfCity, read_census_tract("43", "105"))
  } else {
    stop("Invalid city_name: No simple feature generated.")
  }
  return (sfCity)
}
