# see [yihui](https://github.com/rstudio/pagedown/issues/68#issuecomment-462032816)

require("pagedown")

file_dir <- "d://github/course-econometrics/03-slide-class/"
file_list <- list.files(file_dir)
id_target <- which(stringr::str_detect(file_list, "^[[\\d]]{2}.*-slide\\.html$"))
files_target <- sort((file_list)[id_target])
files_path <- paste0(file_dir, files_target)

# chrome app path
path_browser <- "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"

# input and output argument
path_input <- paste0("file:///", files_path) 
out_dir <- "d://github/course-econometrics/public/slide-pdf/2021-9/"
path_out <- paste0(out_dir, stringr::str_replace(files_target,"html", "pdf"))


for (i in 1:length(files_target)) {
  pagedown::chrome_print(input = path_input[i],output = path_out[i] ,format = "pdf",
                         browser = path_browser,timeout = 120)
  Sys.sleep(0.5)
  print(glue::glue("convert {i} / {length(files_target)} file: {files_target[i]} successed!"))
}

