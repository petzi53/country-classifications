##########################################################
# Project: Some utility scripts for my Quarto books
# Author: Peter Baumgartner
# Edit date: May 19, 2024
# CONTENT:
## - pb_create_folder: create folder at path
## - pb_save_data_file: save data file
## - pb_class_scheme: list groups with its elements
##########################################################



library(glossary)

glossary::glossary_path("../glossary-pb/glossary.yml")




################################################################
# pb_create_folder:
# Purpose:
# check if folder already exists at parameter "path"
# if not, then create folder
# Author: Peter Baumgartner
# path = character string:
#                example: "/Users/xxyyzz/Documents/my-data/"
################################################################
pb_create_folder <- function(path){

  if (!base::file.exists(path))
  {base::dir.create(path)}
}


################################################################
# pb_save_data_file: Save data file for the specified chapter
# Purpose:
# If folder not exists, create it and save object as .rds file
# Author: Peter Baumgartner
# chapter_folder = character: folder inside "data" folder
#                  example "chap05"
# object = data to save
# file_name = character: example "xy_object.rds"
# I have used the function in my notes on "Statistics with R"
# # See: https://bookdown.org/pbaumgartner/swr-harris/
################################################################

pb_save_data_file <- function(chapter_folder, object, file_name){
  data_folder <- base::paste0(here::here(), "/data/")
  if (!base::file.exists(data_folder))
  {base::dir.create(data_folder)}

  chap_folder <-
    base::paste0(
      here::here(),
      paste0("/data/", chapter_folder, "/")
    )
  if (!base::file.exists(chap_folder))
  {base::dir.create(chap_folder)}

  base::saveRDS(object = object,
                file = paste0(chap_folder, "/", file_name))
}

################################################################
# pb_class_scheme: Display elements categorized into a group
# Purpose:
# Inspect number of unique group names and their elements
# Author: Peter Baumgartner
# df = dataframe to inspect as a tibble
# sel1 = name of the 1st selected column (elements) as a string
# sel2 = name of the 2nd selected column (group names) as a string
# returns a datatable sorted by 2nd column (groups)
################################################################

pb_class_scheme <- function(df, sel1, sel2) {
  df |>
    dplyr::select(!!sel1, !!sel2) |>
    dplyr::nest_by(!!sel2) |>
    dplyr::mutate(data = as.vector(data)) |>
    dplyr::mutate(data = stringr::str_c(data, collapse = "; ")) |>
    dplyr::mutate(data = paste(data, ";")) |>
    dplyr::mutate(N = lengths(gregexpr(";", data))) |>
    dplyr::rename(Country = data) |>
    dplyr::arrange(!!sel2) |>
    DT::datatable(class = 'cell-border compact stripe',
                  options = list(
                    pageLength = 25,
                    lengthMenu = c(5, 10, 15, 20, 25, 50)
                  )
    )
}

## END

