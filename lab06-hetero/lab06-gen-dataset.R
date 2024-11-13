#### prepare data for students ####

# load pkgs
require(openxlsx)
require(magrittr)
require(tidyverse)
require(purrr)
require(glue)
require(here)
require(zip)
require(janitor)

# ====get students information====
source(here("R/get_students.R"), encoding = "UTF-8")

id_year <- 2022
teach_year <- 2024
lab_num <- "06"
lab_topic <- "hetero"
n_add <- 5

files_students <- paste0("data/students-list-",
                         teach_year,
                         ".xlsx")

df_students <- get_students(
  file = here(files_students), 
  id_year = id_year, 
  n_add = 5)
id_list <- df_students$id

#==== read basic exercise data set====

file_path <- "data/Lab6-salary-ceo.xlsx"
df_base <- readxl::read_excel(here(file_path)) %>%
  mutate(obs = 1:nrow(.)) %>%
  select(obs, everything(.))
n <- nrow(df_base)


#==== create random column data ====
source(here("R/generate_rdm_df.R"), encoding = "UTF-8")

cols_rdm <- c("Y")
#set.seed(20211108) # for year 2021 fall
set.seed(20231115) # for year 2023 fall


# call external function
df_handout <- gen_rdm_df(
  df_info = df_students,
  df_basic = df_base,
  cols = cols_rdm
) 

# give the head for output
vars_sel <- c("dataset", "class", "id", "name", 
              "obs", sort(cols_rdm), 
              paste0("X", 2:6))
# get all exercise data table

df_exercise <- df_handout %>%
  unnest(data)%>%
  mutate(
    across(
      .cols = all_of(cols_rdm),
      \(x) round(x, digits = 4)
    )
  ) %>%
  select(all_of(vars_sel)) #%>%
# rename_all(., ~all_of(vars_chn))

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

