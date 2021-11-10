
#' Demo file names for students exercise When submit the task
#' 
#' @param num
#' @param id_demo 
#' @param name_demo 
#' @param ext 
#'
#' @return character
#' @export name_file
#'
#' @examples
#' 
name_file <- function(num = 0, id_demo ="2019000001", 
                      name_demo = "张三", ext = "docx"){
  name_file <- paste0(
    "lab",
    stringr::str_pad(num,2,'left','0'
    ),
    "_",name_demo,
    "_",id_demo,
    ".", ext,
    collapse = ""
  )
  return(name_file)
}