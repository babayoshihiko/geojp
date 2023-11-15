#' Prefecture/Municipality/Mesh Codes at Mesh Level 1
#'
#' @format ## `df_muni_mesh1`
#' A data frame with 2,922 rows and 4 columns:
#' \describe{
#'   \item{code_pref}{Prefecture Code}
#'   \item{code_muni}{Municipality Code}
#'   \item{code_mesh1}{Mesh Code (Level 1)}
#' }
#' @source <https://www.stat.go.jp/data/mesh/m_itiran.html>
"df_muni_mesh1"

#' Prefecture/Municipality/Mesh Codes at Mesh Level 3
#'
#' @format ## `df_muni_mesh3`
#' A data frame with 2,922 rows and 4 columns:
#' \describe{
#'   \item{code_pref}{Prefecture Code}
#'   \item{code_muni}{Municipality Code}
#'   \item{name_muni}{Municipality Name}
#'   \item{code_mesh1}{Mesh Code (Level 1)}
#'   \item{code_mesh2}{Mesh Code (Level 2)}
#'   \item{code_mesh3}{Mesh Code (Level 3)}
#' }
#' @source <https://www.stat.go.jp/data/mesh/m_itiran.html>
"df_muni_mesh3"

#' Prefecture/Municipality Codes and Names
#'
#' @format ## `df_code_pref_muni`
#' A data frame with 1,965 rows and 7 columns:
#' \describe{
#'   \item{code_dantai}{Dantai Code (5 digits)}
#'   \item{code_region}{Region Code}
#'   \item{code_pref}{Prefecture Code}
#'   \item{code_muni}{Municipality Code}
#'   \item{name_pref}{Prefecture Name}
#'   \item{name_muni}{Municipality Name}
#'   \item{name_muni}{Municipality Name in Hiragana}
#' }
#' @source <https://www.soumu.go.jp/denshijiti/code.html>
"df_code_pref_muni"
