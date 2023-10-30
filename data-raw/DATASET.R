## code to prepare `DATASET` dataset goes here
library(readr)


df_landnuminfo <- readr::read_csv("data-raw/A29.csv",
                                  col_types = cols(year = col_integer(),
                                                   url = col_character(),
                                                   zip = col_character(),
                                                   shp = col_character(),
                                                   geojson = col_character(),
                                                   altdir = col_character(),
                                                   source = col_character(),
                                                   col = col_character(),
                                                   levels = col_character(),
                                                   labels = col_character(),
                                                   palette = col_character()))
df_landnuminfo$maptype <- "A29"

for (maptype in c("A31", "A48", "A50",
                  "L01", "L02", "L03", "L03-b",
                  "N03",
                  "P04", "P14", "P29",
                  "W05")) {
  dfTemp <- readr::read_csv(paste("data-raw/", maptype, ".csv", sep=""),
                        col_types = cols(year = col_integer(),
                                         url = col_character(),
                                         zip = col_character(),
                                         shp = col_character(),
                                         geojson = col_character(),
                                         altdir = col_character(),
                                         source = col_character(),
                                         col = col_character(),
                                         levels = col_character(),
                                         labels = col_character(),
                                         palette = col_character()))

  print(maptype)
  dfTemp$maptype <- maptype
  df_landnuminfo <- rbind(df_landnuminfo, dfTemp)
}


df_mhlw_ltci <- readr::read_csv("data-raw/mhlw_ltci.csv",
                                col_types = cols(year = col_integer(),
                                                 month = col_integer(),
                                                 code_type = col_integer(),
                                                 name_type = col_character(),
                                                 url = col_character(),
                                                 csv = col_character()))
df_code_pref_muni <- readr::read_csv("data-raw/code_pref_muni.csv",
                                col_types = cols(code_dantai = col_integer(),
                                                  code_region = col_integer(),
                                                 code_pref = col_integer(),
                                                  code_muni = col_integer(),
                                                 name_pref = col_character(),
                                                  name_muni = col_character(),
                                                 name_muni_kana = col_character()))
df_muni_mesh1 <- readr::read_csv("data-raw/muni_mesh1.csv",
                                 col_types = cols(code_pref = col_integer(),
                                                  code_muni = col_integer(),
                                                  name_muni = col_character(),
                                                  code_mesh3 = col_integer()))
df_muni_mesh3 <- readr::read_csv("data-raw/muni_mesh3.csv",
                                 col_types = cols(code_pref = col_integer(),
                                                  code_muni = col_integer(),
                                                  name_muni = col_character(),
                                                  code_mesh1 = col_integer(),
                                                  code_mesh3 = col_integer(),
                                                  code_mesh2 = col_integer()))

usethis::use_data(df_landnuminfo,
                  df_mhlw_ltci,
                  df_code_pref_muni,
                  df_muni_mesh1,
                  df_muni_mesh3,
                  internal = TRUE,
                  overwrite = TRUE)
