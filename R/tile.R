#' Provides URL of tile servers for base map
#'
#' @description
#' Provides URL of tile servers for base map. "standard" and "light"
#' services have zoom levels 2 to 18.
#' For details, check:
#' https://maps.gsi.go.jp/development/ichiran.html
#'
#' @param map_type c("standard", "pale", "English", "photo")
#'
#' @return A list of characters containing server and reference information
#'
#' @export
get_tile_server <- function(map_type = "standard") {
  tile_server = "https://cyberjapandata.gsi.go.jp/xyz/std/{z}/{x}/{y}.png"
  tile_ref = "\u5730\u7406\u9662\u30bf\u30a4\u30eb"  # Chiriin Tile
  tile_legend = "https://maps.gsi.go.jp/development/ichiran.html#std"
  map_type = tolower(map_type)
  map_type = substr(map_type, 1, 2)
  if (map_type == "pa" | map_type == "li" | map_type == "\u6de1\u8272") {
    tile_server = "https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png"
    tile_legend = "https://maps.gsi.go.jp/development/ichiran.html#pale"
  } else if (map_type == "bl" | map_type == "\u767d\u5730") {
    tile_server = "https://cyberjapandata.gsi.go.jp/xyz/blank/{z}/{x}/{y}.png"
    tile_legend = "https://maps.gsi.go.jp/development/ichiran.html#blank"
  } else if (map_type == "ph" | map_type == "se" | map_type == "\u5199\u771f") {
    tile_server = "https://cyberjapandata.gsi.go.jp/xyz/seamlessphoto/{z}/{x}/{y}.png"
    tile_legend = "https://maps.gsi.go.jp/development/ichiran.html#seamlessphoto"
  } else if (map_type == "en") {
    tile_server = "https://cyberjapandata.gsi.go.jp/xyz/english/{z}/{x}/{y}.png"
    tile_legend = "https://maps.gsi.go.jp/development/ichiran.html#english"
  } else if (map_type == "lc" | map_type == "\u6570\u5024") {
    tile_server = "https://cyberjapandata.gsi.go.jp/xyz/lcm25k_2012/{z}/{x}/{y}.png"
    tile_legend = "https://maps.gsi.go.jp/development/ichiran.html#english"
  } else if (map_type == "\u571f\u5730") {
    tile_server = "https://cyberjapandata.gsi.go.jp/xyz/lcm25k/{z}/{x}/{y}.png"
  } else if (map_type == "cc" | map_type == "\u6cbf\u5cb8") {
    tile_server = "https://cyberjapandata.gsi.go.jp/xyz/ccm1/{z}/{x}/{y}.png"
  } else if (map_type == "vb" | map_type == "\u706b\u5c71") {
    tile_server = "https://cyberjapandata.gsi.go.jp/xyz/vbmd_colorrel/{z}/{x}/{y}.png"
  } else if (map_type == "la" | map_type == "\u6e56\u6cbc") {
    tile_server = "https://cyberjapandata.gsi.go.jp/xyz/lakedata/{z}/{x}/{y}.png"
    tile_legend = "https://cyberjapandata.gsi.go.jp/legend/lakedata.pdf"
  }

    return(list(server = tile_server, reference = tile_ref))
}
