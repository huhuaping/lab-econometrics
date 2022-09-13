require(bookdown)
library("tidyverse")
library("openxlsx")
library("knitr")
library("rmarkdown")
library("here")
library("glue")
library("xaringan")
library("xaringanExtra")
#library("xaringanBuilder")
library("kableExtra")
library("ggplot2")
library("latex2exp")

# webshot with DT
library("DT")
library("webshot")
if(is.null(webshot:::find_phantom())){webshot::install_phantomjs()}

# github repo
#renv::install("jirilukavsky/pdf2pptx")
require(pdf2pptx)
#renv::install("KWB-R/kwb.utils")
#renv::install("huhuaping/xmerit")
require(xmerit)
#renv::install("jhelvy/xaringanBuilder")
#require(xaringanBuilder)

# econometrics test
library(lmtest)
library(sandwich)
library(systemfit)
library(gmm)

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

# public
require(bibtex)