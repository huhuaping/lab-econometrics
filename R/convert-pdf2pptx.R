
# see github [repo](https://github.com/jirilukavsky/pdf2pptx)

#devtools::install_github("jirilukavsky/pdf2pptx")


require(pdf2pptx)

file_dir <- "d://github/course-econometrics/public/slide-pdf/2021-9/"
file_list <- list.files(file_dir)
id_target <- which(stringr::str_detect(file_list, "^[[\\d]]{2}.*-slide\\.pdf$"))
files_target <- sort((file_list)[id_target])
files_path <- paste0(file_dir, files_target)

path_out_43 <- paste0(file_dir, stringr::str_replace(files_target,"\\.pdf", "\\-43\\.pptx"))
path_out_169 <- paste0(file_dir, stringr::str_replace(files_target,"\\.pdf", "\\-169\\.pptx"))

# tem dir
dir_tem <- "d://github/course-econometrics/public/slide-pdf/tem/"

# loop to convert
# this will take long time, maybe 2~5 minutes per 100 slides.
ratio <- 169
for (i in 1:length(files_target)) {
  if (ratio ==169) {
    pdf2pptx::pdf2pptx(pdf_filename =files_path[i],
                       pptx_filename = path_out_169[i],
                       ratio = ratio,
                       path = dir_tem)
  } else {
    pdf2pptx::pdf2pptx(pdf_filename =files_path[i],
                       pptx_filename = path_out_43[i],
                       ratio = ratio,
                       path = dir_tem)
  }
  Sys.sleep(0.5)
  print(glue::glue("convert {i} / {length(files_target)} file on ratio of {ratio}: {files_target[i]} successed!"))
}
