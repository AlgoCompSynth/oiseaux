#' @title Download Usable Files
#' @name download_usable_files
#' @description Downloads files from xeno-canto with usable licenses, i.e.,
#'    not restriced by non-commercial or no-derivative clauses
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom stringr str_match
#' @importFrom warbleR query_xc
#' @export download_usable_files
#' @param species_english_name string: the English name of the species.
#' @param destination_path string: where to store the downloaded files. It
#'     will be created if it doesn't already exist.
#' @return a data frame listing the downloaded files
#' @examples
#' \dontrun{
#'   owl_metadata <- oiseaux::download_usable_files(
#'     "Great Horned Owl", "~/owls"
#'   )
#'   loon_metadata <- oiseaux::download_usable_files(
#'     "Common Loon", "~/loons"
#'   )
#' }
#' @details
#' `download_usable_files` uses the `warbleR` package to query the
#' [xeno-canto](https://xeno-canto.org/) collection of bird sounds from
#' around the world. Xeno-canto licenes each file with one of the
#' [Creative Commons](https://creativecommons.org/share-your-work/) licenses.
#'
#' Since we want to analyze and resynthesize the sounds, and will be using
#' them in compositions that may be recorded and published, we can't use
#' files that have Creative Commons licenses with the "ND" (no derivative
#' works) or "NC" (non-commercial) attribute. That leaves us with "CC0"
#' (public domain), "CC-BY" (attribution), or "CC-BY-SA" (attribution and
#' share-alike).
#'
#' It turns out that few of the xeno-canto files are usable by this
#' definition. For example, out of 424 recordings of the great horned owl,
#' only seven are usable. And most of the usable files are "CC-BY-SA", so
#' we use that license for output files distributed with `oiseaux`.

download_usable_files <- function(species_english_name, destination_path) {

  # get all the files for the species
  all_files <- warbleR::query_xc(qword = species_english_name)

  # filter in the ones we can use
  usable_files <- all_files %>%
    dplyr::filter(
      is.na(stringr::str_match(License, "-nc")) &
        is.na(stringr::str_match(License, "/-nd"))
    )

  # download
  dir.create(destination_path, recursive = TRUE)
  warbleR::query_xc(X = usable_files, path = destination_path)

  return(usable_files)
}

utils::globalVariables(
  "License"
)
