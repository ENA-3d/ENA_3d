library(plotly)
library(readxl)
library('rENA')
library(data.table)

# you may need to set the path of your working directory to this folder in order to load the data.xlsx
# setwd("/Users/mac/Desktop")

# load data
enaData <- read_excel(toString('tests/testthat/testing_data.xlsx'))

# selecting units for ENA
units = c('condition','NO')

# selecting conversation for ENA
conversation = c("condition",'NO')

# selecting code columns 
# codes = c('PC','NM','NA','NE')
codes = c(
  'P01',	'P02',	'P03',	'P04',	'P05',	'P06',
  'A01',	'A02',	'A03',	'A04',	'A05',	'L01',	'L02',
  'L03',	'L04',	'S01',	'S02')

# construct ena object with T-test and Wilcox
ena_obj = ena(
  data=as.data.frame(enaData),
  units = units,
  conversation = conversation,
  codes = codes,
  window.size.back = 4,
  runTest=TRUE,
  print.plots = FALSE,
  groupVar = "condition",
  groups = c(1, 3),
  plot=FALSE
)
save(ena_obj,file='testing_data.Rdata')