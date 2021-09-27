## bad.names helpers

tidyverse_names <- function(x){
  stringr::str_detect(x, "(?<=^)\\.\\.\\.\\d+(?=$)")
}

base_names <- function(x){
  stringr::str_detect(x, "(?<=^)X\\.?\\d*(?=$)")
}



