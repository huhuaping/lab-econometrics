# Generate lab information with parameters

start <- as.Date(params$hw_start)
start_line <- glue::glue("{start}（{wday(start,week_start = 1,label = T)}）24:00:00")
end <- as.Date(params$hw_end)
end_line <- glue::glue("{end}（{wday(end,week_start = 1,label = T)}）24:00:00")
lab.num <- params$hand_out$lab_num

lab_topic <- paste0(
  "lab",
  str_pad(params$hand_out$lab_num,
          2,'left','0'),
  '-',params$topic)

# class and id
if (params$class == "major"){
  courseID <- params$course_id$major
} else {
  courseID <- params$course_id$academic
}