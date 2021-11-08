function(input, ...) {
  #purrr::walk(
  #  .x = list(
  #    hand_out = list(
  #      lab_num = 2,
  #      name_chn = "一元线性回归OLS方法分析过程",
  #      name_eng = "hhp-handout")),
    ~ rmarkdown::render(
      input = input,
      output_file = glue::glue("{here::here('public')}/exercise-lab-{stringr::str_pad(params$lab_num,2,'l','0')}-{.x$name_eng}.docx")#,
      #output_options = list(theme = "journal"),
  #    params = list(hand_out = {.x}#,
                    #hw_start= "2021-11-03"
                    #hw_end = "2021-11-23"
                    )
    #)
  #)
}