# background info

#' Title
#'
#' @param file character. file path of the xlsx
#' @param id_year character. id year of students
#' @param n_add integer. how many rows for additional students
#'
#' @return
#' @export
#'
#' @examples
#' 
#' files_students <- "data/students-list-2021.xlsx"
#' id_year <- 2019
#' n_add <- 5
#' df_students <- get_students(files_students, id_year, 5)
#' 
get_students <- function(file, id_year, n_add){
  names_chn <- c("学号", "姓名", "分组", "班级")
  names_eng <- c("id", "name", "group", "class")
  
  out <- openxlsx::read.xlsx(file) %>%
    rename_all(., ~all_of(names_eng)) %>%
    tibble::add_row(id = glue("{id_year}00000{1:n_add}"),
                    name = glue("跟班0{1:n_add}"),
                    class=rep("农管1903",n_add)) %>%
    mutate(class_f = factor(str_extract(class, "(\\d{1})$"))) %>%
    arrange(class_f, id) %>%
    add_column(
      dataset = paste0(
        "dataset", 
        str_pad(1:nrow(.),width = 3,
                side = "l",pad = "0")),
      .before = "id") 
  return(out)
}




