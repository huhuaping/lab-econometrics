#### prepare data for students ####

# load pkgs
require(openxlsx)
require(magrittr)
require(tidyverse)
require(purrr)
require(glue)
require(here)
require(zip)

# ====get students information====
source(here("R/get_students.R"), encoding = "UTF-8")

id_year <- 2021
teach_year <- 2023
lab_num <- "02"
lab_topic <- "consumption"
n_add <- 5

files_students <- paste0("data/students-list-",
                         teach_year,
                         ".xlsx")

df_students <- get_students(
  file = here(files_students), 
  id_year = id_year, 
  n_add = 5)
id_list <- df_students$id

#==== Mento-Carlo DGP====

# read the basic data
df_basic <- openxlsx::read.xlsx("data/lab2-mento-carlo.xlsx")

# helper function
gen_dt <- function(df = df_basic, seeds){
  set.seed(seeds)
  b2 <- 0.6
  b1 <- 17
  x <- df$income
  u <- rnorm(n = nrow(df))
  y <- b1 + b2*x +u
  
  out <-  df %>% mutate(spend = y) %>%
    select(obs, income, spend)
  
  return(out)
}

cols_vars <- c("income", "spend")

# give the head for output
vars_sel <- c("dataset", "class", "id", "name", 
              "obs", sort(cols_vars))
vars_chn <-c("数据集", "班级", "学号", "姓名", 
             "obs", sort(cols_vars))

# get all exercise data table
df_exercise <- df_students %>%
  mutate(data = map(.x = id, ~gen_dt(seeds = .x))) %>%
  unnest(data) %>%
  mutate(spend = round(spend, 4)) %>% # control digits
  select(all_of(vars_sel))


# ==== generate subset data ====
## create directory
path_dir <- here(glue("lab{lab_num}-{lab_topic}/dataset/lab{lab_num}-dataset-{teach_year}/"))
dir.create(path_dir, recursive = T)

file_path <- as.list(paste0(
  glue("{path_dir}/lab{lab_num}-dataset-"), 
  id_list,
  ".xlsx"))

i <- 1
for (i in 1:length(id_list)){
  out <- df_exercise %>%
    filter(id == id_list[i]) 
  
  openxlsx::write.xlsx(out, here::here(file_path[i]))
  cat("finished", i, sep ="\n")
}

#==== zip data set ====
## zip all files
ziped_file <- glue("lab{lab_num}-dataset-{teach_year}.zip")
zip::zip(zipfile = ziped_file,
         files = path_dir,
         root = path_dir,
         mode = "cherry-pick")

## remove all `.xlsx` files
sapply(file_path, file.remove)
