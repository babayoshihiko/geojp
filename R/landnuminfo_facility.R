# The Land Numerical Information has facilities data as:
# national and governmental governmental institutions
# municipal townhalls
# police stations and so on

#' Download spatial data of Welfare Facilities of Japan
#'
#' @description
#' Function to download spatial data of Welfare Facilities of Japan. The returned value is an sf object.
#'
#' The data is avaiable for years 2021, 2015 and 2011. Somehow, the data for Osaka year 2011 is not available.
#'
#' Please note that the welfare facility data is NOT accurate.
#'
#' @param code_pref The 2-digit code of prefecture.
#' @param code_muni Optional. The 3-digit code of municipality.
#' @param year Year of the data. Defaults to 2021.
#' @param data_dir The directory to store downloaded zip and extracted files. If not specified, the data will be stored in a temp directory and will be deleted after you quit the session.
#'
#' @return An `"sf" "data.frame"` object with extra attr "col" and "palette" for tmap.
#'
#' @export
read_landnuminfo_welfare <- function(code_pref, code_muni = NULL, year = 2021, data_dir = NULL){
  year4digit = check_year(year)

  if (code_pref == 27 & year4digit == 2015) stop(paste("Welfare data not available for Osaka Year 2015."))

  sfLNI <- NULL
  sfLNI <- read_landnuminfo_by_csv("P14", code_pref, NULL, year4digit, data_dir)

  if (!is.null(code_muni)){
    if (year4digit >= 2015){
      lstCodeMuni <- get_wards(code_pref, code_muni, year4digit)
      if (length(lstCodeMuni) > 0) {
        sfLNI2 <- NULL
        for (code_muni_single in lstCodeMuni){
          strNameMuni <- get_muni_name(code_pref, code_muni_single)
          if (is.null(sfLNI2)) {
            sfLNI2 <- subset(sfLNI, P14_002 == strNameMuni)
          } else {
            sfLNI2 <- rbind(sfLNI2, subset(sfLNI, P14_002 == strNameMuni))
          }
        }
      }
      if (!is.null(sfLNI2)) sfLNI <- sfLNI2
    } else {
      strNameMuni = get_muni_name(code_pref, code_muni)
      sfLNI <- subset(sfLNI, P14_002 == strNameMuni)
    }
  }


  if (!is.null(sfLNI)) {
    if (year >= 2021) {
      sfLNI$P14_label <- factor(sfLNI$P14_005, levels=c("01","02","03","04","05","06","99"),
                                  labels=c("\u4fdd\u8b77\u65bd\u8a2d",
                                           "\u8001\u4eba\u798f\u7949\u65bd\u8a2d",
                                           "\u969c\u5bb3\u8005\u652f\u63f4\u65bd\u8a2d\u7b49",
                                           "\u8eab\u4f53\u969c\u5bb3\u8005\u793e\u4f1a\u53c2\u52a0\u652f\u63f4\u65bd\u8a2d",
                                           "\u5150\u7ae5\u798f\u7949\u65bd\u8a2d\u7b49",
                                           "\u6bcd\u5b50\u30fb\u7236\u5b50\u798f\u7949\u65bd\u8a2d",
                                           "\u305d\u306e\u4ed6\u306e\u793e\u4f1a\u798f\u7949\u65bd\u8a2d\u7b49"))
      attr(sfLNI, "palette") = c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D") # RColorBrewer::brewer.pal(7, "Dark2")
    } else{
      sfLNI$P14_label <- factor(sfLNI$P14_005, levels=c("16011","19001","19002","19003","19004","19007","19008","19009","19010","19012","19013","19014"),
                                labels=c("\u5e7c\u7a1a\u5712",
                                         "\u8001\u4eba\u798f\u7949\u65bd\u8a2d",
                                         "\u8001\u4eba\u61a9\u306e\u5bb6",
                                         "\u8001\u4eba\u4f11\u990a\u30db\u30fc\u30e0",
                                         "\u6709\u6599\u8001\u4eba\u30db\u30fc\u30e0",
                                         "\u8eab\u4f53\u969c\u5bb3\u8005\u66f4\u751f\u63f4\u8b77\u65bd\u8a2d",
                                         "\u5150\u7ae5\u798f\u7949\u65bd\u8a2d",
                                         "\u77e5\u7684\u969c\u5bb3\u8005\u63f4\u8b77\u65bd\u8a2d",
                                         "\u7cbe\u795e\u969c\u5bb3\u8005\u793e\u4f1a\u5fa9\u5e30\u65bd\u8a2d",
                                         "\u305d\u306e\u4ed6\u306e\u793e\u4f1a\u798f\u7949\u65bd\u8a2d",
                                         "\u4fdd\u80b2\u6240",
                                         "\u3078\u304d\u5730\u4fdd\u80b2\u6240"))
      attr(sfLNI, "palette") = c("#1B9E77","#D95F02","#D95F02","#D95F02","#D95F02","#E7298A","#66A61E","#7570B3","#7570B3","#A6761D","#1B9E77","#1B9E77")
    }
    attr(sfLNI, "mapname") = "\u798f\u7949\u65bd\u8a2d"
    attr(sfLNI, "col") = "P14_label"
    return(sfLNI)
  }
}
