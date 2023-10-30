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
#' @importFrom utils download.file
#' @importFrom sf read_sf
#' @importFrom sf st_make_valid
#' @export
read_census_tract <- function(code_pref, code_muni, year = 2020, data_dir = NULL){
  strTempDir <- check_data_dir(data_dir)

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
    }
  }
  year = as.character(year)
  if (nchar(code_pref) != 2) stop(paste("Invalid argument: code_pref:", code_pref))
  if (nchar(code_muni) != 3) stop(paste("Invalid argument: code_muni:", code_muni))
  if (year == "2020") {
    year_suffix = "r2ka"
  } else if (year == "2015") {
    year_suffix = "h27ka"
  } else if (year == "2010") {
    year_suffix = "h22ka"
  } else if (year == "2005") {
    year_suffix = "h17ka"
  } else if (year == "2000") {
    year_suffix = "h12ka"
  } else {
    stop(paste("Invalid argument: year:", year, ". year must be one of 2020, 2015, 2010, 2005 or 2000."))
  }


  strCensusUrl = paste("https://www.e-stat.go.jp/gis/statmap-search/data?dlserveyId=A00200521",
                       year,
                       "&code=",
                       code_pref, code_muni,
                       "&coordSys=1&format=shape&downloadType=5&datum=2000",
                       sep = "")
  strCensusZip = file.path(strTempDir,
                           paste("A00200521", year, "DDSWC", code_pref, code_muni, ".zip", sep = ""))
  strCensusFile = file.path(strTempDir,
                            paste(year_suffix, code_pref, code_muni, ".shp", sep = ""))

  if (!file.exists(strCensusZip)) utils::download.file(strCensusUrl, strCensusZip, mode="wb")
  if (!file.exists(strCensusFile)) utils::unzip(strCensusZip, exdir = strTempDir)
  sfCensus = sf::read_sf(strCensusFile,
                      options = "ENCODING=CP932",
                      stringsAsFactors=FALSE)
  sfCensus = sfCensus[,c("KEY_CODE","AREA","PERIMETER","JINKO","SETAI")]
  # sfCensus = sf::st_transform(sfCensus, "EPSG:6668")
  sfCensus = sf::st_make_valid(sfCensus)

  attr(sfCensus, "year") = year
  attr(sfCensus, "mapname") = "\u56fd\u52e2\u8abf\u67fb"
  attr(sfCensus, "sourceName") = "\u300c\u653f\u5e9c\u7d71\u8a08\u306e\u7dcf\u5408\u7a93\u53e3(e-Stat)\u300d"
  attr(sfCensus, "sourceURL") = "https://www.e-stat.go.jp/gis/"

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
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object
#'
#' @export
read_census_odcity <- function(city_name, year = 2020, data_dir = NULL){

  if (is.list(city_name)) stop("Invalid argument: city_name. Please give one city name only.")
  if (!is.vector(city_name)) stop("Invalid argument: city_name. Please give one city name only.")
  if (length(city_name) > 1) stop("Invalid argument: city_name. Please give one city name only.")
  city_name = sub("\u5e02", "", city_name)  # Remove Japanese term "SHI"
  city_name_upper =toupper(city_name)

  if (city_name == "\u672d\u5e4c" || city_name_upper == "SAPPORO") {
    sfCity = read_census_tract("01", "101", year, data_dir)
    sfCity = rbind(sfCity, read_census_tract("01", "102", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("01", "103", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("01", "104", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("01", "105", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("01", "106", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("01", "107", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("01", "108", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("01", "109", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("01", "110", year, data_dir))
  } else if (city_name == "\u4ed9\u53f0" || city_name_upper == "SENDAI") {
    sfCity = read_census_tract("04", "101", year, data_dir)
    sfCity = rbind(sfCity, read_census_tract("04", "102", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("04", "103", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("04", "104", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("04", "105", year, data_dir))
  } else if (city_name == "\u3055\u3044\u305f\u307e" || city_name == "\u57fc\u7389" || city_name_upper == "SAITAMA") {
    if (year >= 2005){
    sfCity = read_census_tract("11", "101", year, data_dir)
    sfCity = rbind(sfCity, read_census_tract("11", "102", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("11", "103", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("11", "104", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("11", "105", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("11", "106", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("11", "107", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("11", "108", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("11", "109", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("11", "110", year, data_dir))
    } else {
      sfCity = read_census_tract("11", "204", year, data_dir)
      sfCity = rbind(sfCity, read_census_tract("11", "205", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("11", "220", year, data_dir))
    }
  } else if (city_name == "\u5343\u8449" || city_name_upper == "CHIBA"  || city_name_upper == "TIBA") {
    sfCity = read_census_tract("11", "101", year, data_dir)
    sfCity = rbind(sfCity, read_census_tract("11", "102", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("11", "103", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("11", "104", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("11", "105", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("11", "106", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("11", "107", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("11", "108", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("11", "109", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("11", "110", year, data_dir))
  } else if (city_name == "\u6771\u4eac" || city_name == "\u6771\u4eac\u90fd" || city_name == "\uff12\uff13\u533a" || city_name_upper == "TOKYO") {
    sfCity = read_census_tract("13", "101", year, data_dir)
    sfCity = rbind(sfCity, read_census_tract("13", "102", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "103", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "104", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "105", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "106", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "107", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "108", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "109", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "110", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "111", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "112", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "113", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "114", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "115", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "116", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "117", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "118", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "119", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "120", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "121", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "122", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("13", "123", year, data_dir))
  } else if (city_name == "\u6a2a\u6d5c" || city_name == "\u6a2a\u6ff1" || city_name_upper == "YOKOHAMA") {
    sfCity = read_census_tract("14", "101", year, data_dir)
    sfCity = rbind(sfCity, read_census_tract("14", "102", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "103", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "104", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "105", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "106", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "107", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "108", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "109", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "110", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "111", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "112", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "113", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "114", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "115", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "116", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "117", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "118", year, data_dir))
  } else if (city_name == "\u5ddd\u5d0e" || city_name_upper == "KAWASAKI") {
    sfCity = read_census_tract("14", "131", year, data_dir)
    sfCity = rbind(sfCity, read_census_tract("14", "132", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "133", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "134", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "135", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "136", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("14", "137", year, data_dir))
  } else if (city_name == "\u76f8\u6a21\u539f" || city_name_upper == "SAGAMIHARA") {
    if (year >= 2010){
      sfCity = read_census_tract("14", "151", year, data_dir)
      sfCity = rbind(sfCity, read_census_tract("14", "152", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("14", "153", year, data_dir))
    } else {
      sfCity = read_census_tract("14", "209", year, data_dir)
    }
  } else if (city_name == "\u65b0\u6f5f" || city_name_upper == "NIIGATA") {
    if (year >= 2007){
      sfCity = read_census_tract("15", "101", year, data_dir)
      sfCity = rbind(sfCity, read_census_tract("15", "102", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("15", "103", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("15", "104", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("15", "105", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("15", "106", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("15", "107", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("15", "108", year, data_dir))
    } else {
      sfCity = read_census_tract("15", "201", year, data_dir)
    }
  } else if (city_name == "\u9759\u5ca1" || city_name_upper == "SHIZUOKA" || city_name_upper == "SIZUOKA") {
    if (year >= 2007){
      sfCity = read_census_tract("22", "101", year, data_dir)
      sfCity = rbind(sfCity, read_census_tract("22", "102", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("22", "103", year, data_dir))
    } else {
      sfCity = read_census_tract("22", "201", year, data_dir)
    }
  } else if (city_name == "\u6d5c\u677e" || city_name_upper == "HAMAMATSU" || city_name_upper == "HAMAMATU") {
    if (year >= 2007){
      sfCity = read_census_tract("22", "131", year, data_dir)
      sfCity = rbind(sfCity, read_census_tract("22", "132", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("22", "133", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("22", "134", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("22", "135", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("22", "136", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("22", "137", year, data_dir))
    } else {
      sfCity = read_census_tract("22", "202", year, data_dir)
    }
  } else if (city_name == "\u540d\u53e4\u5c4b" || city_name_upper == "NAGOYA") {
    sfCity = read_census_tract("23", "101", year, data_dir)
    sfCity = rbind(sfCity, read_census_tract("23", "102", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("23", "103", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("23", "104", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("23", "105", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("23", "106", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("23", "107", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("23", "108", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("23", "109", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("23", "110", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("23", "111", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("23", "112", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("23", "113", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("23", "114", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("23", "115", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("23", "116", year, data_dir))
  } else if (city_name == "\u4eac\u90fd" || city_name_upper == "KYOTO") {
    sfCity = read_census_tract("26", "101", year, data_dir)
    sfCity = rbind(sfCity, read_census_tract("26", "102", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("26", "103", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("26", "104", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("26", "105", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("26", "106", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("26", "107", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("26", "108", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("26", "109", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("26", "110", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("26", "111", year, data_dir))
  } else if (city_name == "\u5927\u962a" || city_name_upper == "OSAKA") {
    sfCity = read_census_tract("27", "102", year, data_dir)
    sfCity = rbind(sfCity, read_census_tract("27", "103", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "104", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "105", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "106", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "107", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "108", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "109", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "110", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "111", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "112", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "113", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "114", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "115", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "116", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "117", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "118", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "119", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "120", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "121", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "122", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "123", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "124", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "125", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "126", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "127", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("27", "128", year, data_dir))
  } else if (city_name == "\u583a" || city_name_upper == "SAKAI") {
    if (year >= 2006){
      sfCity = read_census_tract("27", "141", year, data_dir)
      sfCity = rbind(sfCity, read_census_tract("27", "142", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("27", "143", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("27", "144", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("27", "145", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("27", "146", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("27", "147", year, data_dir))
    } else {
      sfCity = read_census_tract("27", "201", year, data_dir)
    }
  } else if (city_name == "\u795e\u6238" || city_name_upper == "KOBE") {
    sfCity = read_census_tract("28", "101", year, data_dir)
    sfCity = rbind(sfCity, read_census_tract("28", "102", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("28", "105", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("28", "106", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("28", "107", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("28", "108", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("28", "109", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("28", "110", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("28", "111", year, data_dir))
  } else if (city_name == "\u5ca1\u5c71" || city_name_upper == "OKAYAMA") {
    if (year >= 2010){
      sfCity = read_census_tract("33", "101", year, data_dir)
      sfCity = rbind(sfCity, read_census_tract("33", "102", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("33", "103", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("33", "104", year, data_dir))
    } else {
      sfCity = read_census_tract("33", "201", year, data_dir)
    }
  } else if (city_name == "\u5e83\u5cf6" || city_name_upper == "HIROSHIMA" || city_name_upper == "HIROSIMA") {
    sfCity = read_census_tract("34", "101", year, data_dir)
    sfCity = rbind(sfCity, read_census_tract("34", "102", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("34", "103", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("34", "104", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("34", "105", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("34", "106", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("34", "107", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("34", "108", year, data_dir))
  } else if (city_name == "\u798f\u5ca1" || city_name_upper == "FUKUOKA") {
    sfCity = read_census_tract("40", "101", year, data_dir)
    sfCity = rbind(sfCity, read_census_tract("40", "102", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("40", "103", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("40", "104", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("40", "105", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("40", "106", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("40", "107", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("40", "108", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("40", "109", year, data_dir))
  } else if (city_name == "\u5317\u4e5d\u5dde" || city_name_upper == "KITAKYUSHU" || city_name_upper == "KITAKYUSYU") {
    sfCity = read_census_tract("40", "131", year, data_dir)
    sfCity = rbind(sfCity, read_census_tract("40", "132", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("40", "133", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("40", "134", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("40", "135", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("40", "136", year, data_dir))
    sfCity = rbind(sfCity, read_census_tract("40", "137", year, data_dir))
  } else if (city_name == "\u718a\u672c" || city_name_upper == "KUMAMOTO") {
    if (year >= 2012){
      sfCity = read_census_tract("43", "101", year, data_dir)
      sfCity = rbind(sfCity, read_census_tract("43", "102", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("43", "103", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("43", "104", year, data_dir))
      sfCity = rbind(sfCity, read_census_tract("43", "105", year, data_dir))
    } else{
      sfCity = read_census_tract("43", "201", year, data_dir)
    }
  } else {
    stop("Invalid city_name: No simple feature generated.")
  }
  return (sfCity)
}
