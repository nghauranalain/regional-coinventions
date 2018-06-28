#### working directory
getwd()
rm(list = ls())

#### load some packages
library(openxlsx)
library(tidyverse)

#### load data
# list of establishments in our sample
ech <- read.csv("./data/ech_final.csv", header = TRUE, colClasses = "character",
                stringsAsFactors = FALSE)
ech$treatment <- c(rep(1, 118), rep(0, 932)) # the first 118 etabs are treated 
# and the remaining 932 are not
# establishments data (region and sector)
etabs <- read.xlsx("./data/etabs.xlsx", 1, colNames = TRUE)
glimpse(etabs)
etabs <- select(etabs, siret, code_naf, region = region_final)
## patent data for the first and second periods (p0 & p1)
net.p0 <- read.xlsx("./data/network_p0.xlsx", 1, colNames = TRUE)
net.p1 <- read.xlsx("./data/network_p1.xlsx", 1, colNames = TRUE)
glimpse(list(net.p0, net.p1))

#### definition of the "number of intra-regional co-inventions" for each
#### establishments in each during p0 and p1
table(unique(net.p0$siret) %in% etabs$siret)
table(unique(net.p1$siret) %in% etabs$siret)

## analysis for p0





## analysis for p1