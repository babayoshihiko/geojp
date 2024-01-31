#' Download spatial data of Hazard Areas of Japan
#'
#' @description
#' Function to download spatial data of Hazard Areas of Japan. The returned value is an sf object.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2021.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_hazard <- function(code_pref, code_muni = NULL, year = 2021, data_dir = NULL){
  year4digit <- check_year(year)

  if (code_pref == 6 & year4digit == 2021) stop("The zip file does not contain any .shp/.geosjon file for Yamagata 2021.")
  if (code_pref == 9 & year4digit == 2020) stop("No data available for Tochigi 2020.")
  if (code_pref == 11) stop("No data available for Saitama.")
  if (code_pref == 18 & year4digit == 2020) stop("No data available for Fukui 2020.")
  if (code_pref == 19) stop("No data available for Yamanashi.")
  if (code_pref == 24) stop("No data available for Mie.")
  if (code_pref == 29) stop("No data available for Nara.")
  if (code_pref == 33) stop("No data available for Okayama.")
  if (code_pref == 35) stop("No data available for Yamaguchi.")
  if (code_pref == 37) stop("No data available for Kagawa.")
  if (code_pref == 39) stop("No data available for Kochi.")
  if (code_pref == 40) stop("No data available for Fukuoka.")
  if (code_pref == 44) stop("No data available for Oita.")

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("A48", code_pref, NULL, year4digit, data_dir)

  if (!is.null(sfLNI)) {
    if (!is.null(code_muni)){
      lstCodeMuni <- get_wards(code_pref, code_muni, year4digit)
      if (length(lstCodeMuni) > 0) {
        sfLNI2 <- NULL
        for (code_muni_single in lstCodeMuni){
          strNameMuni <- get_muni_name(code_pref, code_muni_single)
          if (is.null(sfLNI2)) {
            #sfLNI2 <- subset(sfLNI, A48_003 == paste(check_code_pref_as_char(code_pref),check_code_muni_as_char(code_pref,code_muni),sep=""))
            sfLNI2 <- sfLNI[sfLNI$A48_003 == check_code_muni_as_char(code_pref,code_muni,return="code_pref_muni"),]
          } else {
            #sfLNI2 <- rbind(sfLNI2, subset(sfLNI, A48_003 == paste(check_code_pref_as_char(code_pref),check_code_muni_as_char(code_pref,code_muni),sep="")))
            sfLNI2 <- rbind(sfLNI2, sfLNI[sfLNI$A48_003 == check_code_muni_as_char(code_pref,code_muni,return="code_pref_muni"),])
          }
        }
      }
      if (!is.null(sfLNI2)) sfLNI <- sfLNI2
    }
  }

  if (!is.null(sfLNI)) {
    attr(sfLNI, "mapname") = "\u707d\u5bb3\u5371\u967a\u533a\u57df"
    return(sfLNI)
  }
}
