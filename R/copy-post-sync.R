# post render copy _site/dir to `_site_sync/` 

dir_from <- here::here("_site")
dir_to   <- here::here("_site_sync")

library(fs)
fs::dir_copy(
  path = dir_from,
  new_path = dir_to, overwrite = TRUE)
