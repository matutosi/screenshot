## code to prepare `clipboard_sample` dataset goes here

path <- "data-raw/clipboard_sample.txt"
clipboard_sample <- 
  readLines(path) |>
    as.hexmode() |>
    as.raw()
usethis::use_data(clipboard_sample, overwrite = TRUE)
