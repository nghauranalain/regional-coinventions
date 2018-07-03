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
net.p0 <- distinct(net.p0)
net.p1 <- read.xlsx("./data/network_p1.xlsx", 1, colNames = TRUE)
net.p1 <- distinct(net.p1)
glimpse(list(net.p0, net.p1))

#### definition of the "number of intra-regional co-inventions" for each
#### establishments in each during p0 and p1
table(unique(net.p0$siret) %in% etabs$siret)
table(unique(net.p1$siret) %in% etabs$siret)

## analysis for p0

# example: FR2968364; which(net.p0$pubnum == "FR2968364") = [1] 1568 10574 13380
# add region to net.p0
net.p0 <- left_join(net.p0, select(etabs, siret, region), by = "siret")
net.p0$siren <- substr(net.p0$siret, 1, 9)

# (test)
# 07150239700079
# FR2963026
# FR2963027
# FR2963624
# FR2959235
str(net.p0)
nrow(filter(net.p0, pubnum == "FR2968364" &
               siren != substr("41481521700073", 1, 9) &
               region == "ILE-DE-FRANCE"))
nrow(filter(net.p0, pubnum == "FR2959235" &
                    siren != substr("07150239700079", 1, 9) &
                    region != "PICARDIE"))
nrow(filter(net.p0, pubnum == pubnum[20214] &
                    siren != substr(siret[20214], 1, 9) &
                    region == region[20214]))


net.p0$nb_partners_reg <- NA # new variable

for(i in 1:nrow(net.p0)){
        # looping over rows
        net.p0$nb_partners_reg[i] <- nrow(filter(net.p0, pubnum == pubnum[i] &
                                                         siren != substr(siret[i], 1, 9) &
                                                         region == region[i]))
}
        

net.p0.final <- net.p0 %>%
        mutate(reg_coinv = ifelse(nb_partners_reg > 0, 1, 0)) %>%
        group_by(siret) %>%
        mutate(nb_coinv_reg = sum(reg_coinv)) %>%
        select(siret, nb_coinv_reg) %>%
        distinct()

table(net.p0.final$nb_coinv_reg > 0) # 332 establishments involved in 
# intra regional co-invention (i.e projets with establishement(s) from their own
# region)

# export
#write.csv(net.p0.final,
 #         "T:/These_GATE/Traitement II/Final/data_CASD/outcomes/nb_coinv_reg_p0.csv",
  #        row.names = FALSE)


## analysis for p1
# add region to net.p1
net.p1 <- left_join(net.p1, select(etabs, siret, region), by = "siret")
net.p1$siren <- substr(net.p1$siret, 1, 9)

# new variable
net.p1$nb_partners_reg <- NA 

for(i in 1:nrow(net.p1)){
        # looping over rows
        net.p1$nb_partners_reg[i] <- nrow(filter(net.p1, pubnum == pubnum[i] &
                                                         siren != substr(siret[i], 1, 9) &
                                                         region == region[i]))
}


net.p1.final <- net.p1 %>%
        mutate(reg_coinv = ifelse(nb_partners_reg > 0, 1, 0)) %>%
        group_by(siret) %>%
        mutate(nb_coinv_reg = sum(reg_coinv)) %>%
        select(siret, nb_coinv_reg) %>%
        distinct()

table(net.p1.final$nb_coinv_reg > 0) # 356 establishments involved in 
# intra regional co-invention (i.e projets with establishement(s) from their own
# region)

# export
#write.csv(net.p1.final,
 #         "T:/These_GATE/Traitement II/Final/data_CASD/outcomes/nb_coinv_reg_p1.csv",
  #        row.names = FALSE)


