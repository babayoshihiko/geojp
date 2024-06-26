# The Land Numerical Information has facilities data as:
# national and governmental governmental institutions
# municipal townhalls
# police stations and so on

#' Download spatial data of Long-Term Care Insurance services
#'
#' @description
#' Function to download spatial data of Long-Term Care Insurance services,
#' supplied by Ministry of Health, Labour, and Welfare of Japan. The returned
#' value is an sf object.
#'
#' The available years are: 2023, 2022, 2021 and 2020 (month = 12 only for 2020).
#' The default year is 2023.
#'
#' @param code_type LTCI service code, e.g. 110 = Home-Visit Care.  See attribute sourceURL for details.
#' @param name_type LTCI service name in Japanese.
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data.
#' @param month Month of the data. Either 6 or 12.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#' @param exclude_outside_Japan Exclude the data if they are outside the boundary.
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @importFrom utils download.file
#' @importFrom sf read_sf
#' @importFrom sf st_crs
#' @importFrom sf st_drop_geometry
#' @export
read_mhlw_ltci <- function(code_type = NULL, name_type = NULL,
                           code_pref = NULL, code_muni = NULL,
                           year = 2023, month = 6,
                           data_dir = NULL,
                           exclude_outside_Japan = FALSE){
  if (is.null(code_type) & is.null(code_type)) stop("Please give code_type or name_type")
  year4digit <- check_year(year)
  strTempDir <- check_data_dir(data_dir)

  # Read the MapType definition
  dfTemp <- get_definition("mhlw_ltci")

  dfTemp <- dfTemp[dfTemp$year == year4digit & dfTemp$month == month,]
  if (!is.null(code_type)){
    dfTemp <- dfTemp[dfTemp$code_type == code_type,]
  } else {
    dfTemp <- dfTemp[dfTemp$name_type == name_type,]
  }
  if (nrow(dfTemp) >= 1) {
    strUrl <- as.character(dfTemp[1,"url"]) # Get URL from the first row
    strZip <- basename(strUrl)
    strCSV <- as.character(dfTemp[1,"csv"])
  } else {
    stop("ERROR: Cannot find the definition (read_mhlw_ltci).")
  }

  # Checks if the CSV file exists. If not, download and unzip the file.
  if (!file.exists(file.path(strTempDir, strCSV))){
    if (!file.exists(file.path(strTempDir, strZip))){
      utils::download.file(strUrl, file.path(strTempDir, strZip), mode="wb")
    }
    unzip_ja(file.path(strTempDir, strZip), exdir = strTempDir)
  }

  sfMHLW <- sf::read_sf(file.path(strTempDir, strCSV), options=c("X_POSSIBLE_NAMES=\u7d4c\u5ea6","Y_POSSIBLE_NAMES=\u7def\u5ea6"))

  sf::st_crs(sfMHLW) <- 6668

  # Subset by pref and muni
  if (!is.null(code_pref)) {
    code_pref <- check_code_pref_as_char(code_pref)
    if (!is.null(code_muni)) {
      temp_rows <- sf::st_drop_geometry(sfMHLW[,1])
      sfMHLW <- sfMHLW[temp_rows == check_code_muni_as_char(code_pref, code_muni, return = "code_dantai"),]
    } else {
      temp_rows <- sf::st_drop_geometry(sfMHLW[,3])
      sfMHLW <- sfMHLW[temp_rows == get_pref_name(code_pref),]
    }
  }

  if (exclude_outside_Japan) {
    sfMHLW <- exclude_outside_Japan(sfMHLW)
  }

  # Set attributes
  attr(sfMHLW, "mapname") = as.character(dfTemp[1,"name_type"])
  attr(sfMHLW, "source") = "\u539a\u751f\u52b4\u50cd\u7701"
  attr(sfMHLW, "sourceURL") = "https://www.mhlw.go.jp/stf/kaigo-kouhyou_opendata.html"

  return(sfMHLW)
}
