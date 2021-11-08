
library("tidyverse")
library("openxlsx")
library("knitr")
library("rmarkdown")
library("here")
library("glue")
library("xaringan")
library("xaringanExtra")
library("xaringanBuilder")
library("kableExtra")
library("ggplot2")
library("latex2exp")

# webshot with DT
library("DT")
library("webshot")
if(is.null(webshot:::find_phantom())){webshot::install_phantomjs()}


library("car")
library("pls")

# time series
library("lmtest")
library("foreign")

# dummy model
library("fastDummies")
library( "psych")

# endogenous x

library('forcats')
library("survival")

# text data set
library("wooldridge")
library("AER")