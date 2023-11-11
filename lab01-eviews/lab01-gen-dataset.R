
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
n_add <- 5

files_students <- paste0("data/students-list-",
                         teach_year,
                         ".xlsx")

df_students <- get_students(
  file = here(files_students), 
  id_year = id_year, 
  n_add = 5)

# ==== read basic exercise data set ====
file_path <- here("data/exercise-Lab01-Table-CPI.xlsx")
df_cpi <- openxlsx::read.xlsx(file_path) %>%
  # make sure 
  add_column(obs = 1:nrow(.)) %>%
  select(obs, everything(.))
n <- nrow(df_cpi)
id_list <- df_students$id


#==== create random column data ====
source(here("R/generate_rdm_df.R"), encoding = "UTF-8")

cols_rdm <- names(df_cpi)[c(-1,-2)]
#set.seed(20211108) # for year 2021 fall
set.seed(20231108) # for year 2023 fall


# call external function
df_handout <- gen_rdm_df(
  df_info = df_students,
  df_basic = df_cpi,
  cols = cols_rdm
) 

# give the head for output
vars_sel <- c("dataset", "class", "id", "name", 
              "obs","year", sort(cols_rdm))
vars_chn <-c("数据集", "班级", "学号", "姓名", 
             "obs","year", sort(cols_rdm))
# get all exercise data table

df_exercise <- df_handout %>%
  unnest(data)%>%
  mutate(
    across(
      .cols = all_of(cols_rdm),
      \(x) round(x, digits = 2)
    )
  ) %>%
  select(all_of(vars_sel)) #%>%
# rename_all(., ~all_of(vars_chn))

# ==== generate subset data ====
## create directory
path_dir <- here(glue("lab01-eviews/dataset/lab01-dataset-{teach_year}/"))
dir.create(path_dir, recursive = T)

file_path <- as.list(paste0(
  glue("{path_dir}/lab01-dataset-"), 
  id_list,
  ".xlsx"))

i <- 1
for (i in 1:length(id_list)){
  out <- df_exercise %>%
    filter(id == id_list[i]) %>%
    select(obs:usa)
    
  openxlsx::write.xlsx(out, here::here(file_path[i]))
  cat("finished", i, sep ="\n")
}

#==== zip data set ====
## zip all files
ziped_file <- glue("lab01-dataset-{teach_year}.zip")
zip::zip(zipfile = ziped_file,
         files = path_dir,
         root = path_dir,
         mode = "cherry-pick")

## remove all `.xlsx` files
sapply(file_path, file.remove)

