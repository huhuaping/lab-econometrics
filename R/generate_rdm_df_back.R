
#' Helper function to add random value for a list
#'
#' @param x 
#' @param mu 
#' @param sgm 
#'
#' @return
#' @export
#'
#' @examples
#' 
add_rdm_single <- function(x,
                         mu = 0,sgm=1){
  y <- x +rnorm(length(x), mu, sgm)
  return(y)
}


#' Helper function to add random value across a dataframe
#'
#' @param dt 
#' @param cols 
#'
#' @return
#' @export
#'
#' @examples
add_rdm_across <- function(dt,cols){
  out <-  dt %>%
    mutate(
      across(.cols =  all_of(cols),
             .fns = add_rdm_single ))
  return(out)
}


#' Map random data frame to an exist data frame
#'
#' @param df_info data.frame. data.frame which 
#'   including students basic information.
#' @param df_basic data.frame. data.frame which 
#'   including basic data set.
#' @param cols character vector. which columns will 
#'   be add random values
#'
#' @return data.frame. nesting one.
#' @export gen_rdm_df
#'
#' @examples
#' file_path <- "data/exercise-Lab01-Table-CPI.xlsx"
#' 
#' df_cpi <- openxlsx::read.xlsx(here(file_path)) %>%
#'   add_column(index = 1:nrow(.)) %>%
#'   select(index, everything(.))
#'   
#' cols_rdm <- names(df_cpi)[c(-1,-2)]
#' set.seed(20211108)
#' 
#' df_handout<- gen_rdm_df(
#'   df_info = df_students,
#'   df_basic = df_cpi,
#'   cols = cols_rdm)
#' 
gen_rdm_df <- function(df_info, df_basic, cols){
  # make sure the 'obs' column exist
  stopifnot("obs" %in% names(df_basic))
  out <- df_info %>%
    mutate(data = map(
      .x = id,
      .f = function(..){df_basic})
    ) %>%
    mutate(data = map(
      .x = data,
      .f = add_rdm_across, 
      # argument pass to `add_rdm_across`
      cols = cols
    ))
}

