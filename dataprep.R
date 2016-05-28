library(tidyr)
library(scales)
library(magrittr)
library(plyr)
library(dplyr)

# Change the list of vendors and color palette for each update.
# The number of colors must equal the number of vendors.
# Order determines how colors are mapped to vendors.
# They should match the colors and vendors used in the server and ui scripts.

palette <- c("#8e44ad", "#c0392b", "#27ae60", "#2980b9")
vendors <- c("Massive Dynamic", "Lex Corp","The Dryad Institute","Wolfram & Hart")

#### Import Data.
# Verify that csv files are formatted the same as in prior VCORE analyses and that file paths are correct.
# Add additional lines and modify names as necessary, depending on TPAs included in the analysis.

map.regions <- readRDS("data/map.regions.rds")

Massive_Dynamic_fee <- readRDS("data/Massive_Dynamic_fee.rds")
Massive_Dynamic_paid <- readRDS("data/Massive_Dynamic_costs.rds")

Wolfram_Hart_fee <- readRDS("data/Wolfram_Hart_fee.rds")
Wolfram_Hart_paid <- readRDS("data/Wolfram_Hart_costs.rds")

Lex_Corp_fee <- readRDS("data/Lex_Corp_fee.rds")
Lex_Corp_paid <- readRDS("data/Lex_Corp_costs.rds")

Dyad_fee <- readRDS("data/Dyad_fee.rds")
Dyad_paid <- readRDS("data/Dyad_costs.rds")

exposure <- readRDS("data/exposure.rds")

#####################################################################################################
#### Helper functions
#####################################################################################################

#Calculates an overall ratio as the exposure-weighted average of ratios for each bill type.
#The argument x is a raw data object imported above.
Overall.Ratio <- function(x){
  sapply(1:nrow(exposure),function(i){
    weighted.mean(x[i,2:6],exposure[i,2:6], na.rm = TRUE)
  })
}

#Performs preliminary data transformations on vendor data.
#The argument x is a raw data object imported above.
#The argument y is a character string represented a vendor included in the analysis.
prep <- function(x,y){
  x %<>%
    mutate(Overall = Overall.Ratio(x)) %>% 
    gather_("bill.type",y,c(names(x)[2:6],"Overall"))
}

#### Prep Client Exposure for joining with vendor data

# Exposure levels are represented as a ratio to the max exposure across all states.
# Exposure is represented in this way because exposure is visaulized in the app as an opacity.
# This ensures that exposure level is always in (0,1), with the highest-exposure state being visualized at 100% opacity.
exposure.long <- exposure %>%
  mutate(Marketing = round(Marketing/max(Marketing), digits = 2),
         Manufacturing = round(Manufacturing/max(Manufacturing), digits = 2),
         Payroll = round(Payroll/max(Payroll), digits = 2),
         Overhead = round(Overhead/max(Overhead), digits = 2),
         Other = round(Other/max(Other), digits = 2),
         Overall = round(Overall/max(Overall), digits = 2)) %>% 
  gather(bill.type,Exposure,2:7)

#### Call prep() function for each vendor and type of cashflow.
Massive_Dynamic_fee <- prep(Massive_Dynamic_fee,"Massive Dynamic")
Massive_Dynamic_paid <- prep(Massive_Dynamic_paid,"Massive Dynamic")
Lex_Corp_fee <- prep(Lex_Corp_fee,"Lex Corp")
Lex_Corp_paid <- prep(Lex_Corp_paid,"Lex Corp")
Dyad_fee <- prep(Dyad_fee,"The Dryad Institute")
Dyad_paid <- prep(Dyad_paid,"The Dryad Institute")
Wolfram_Hart_fee <- prep(Wolfram_Hart_fee,"Wolfram & Hart")
Wolfram_Hart_paid <- prep(Wolfram_Hart_paid,"Wolfram & Hart")

# Combines all vendors into a single data object for each ratio
fee.ratio <- cbind(Massive_Dynamic_fee, Lex_Corp_fee[3], Dyad_fee[3], Wolfram_Hart_fee[3])
paid.ratio <- cbind(Massive_Dynamic_paid, Lex_Corp_paid[3], Dyad_paid[3], Wolfram_Hart_paid[3])


#### Fee ratios
# Step 1: For each state and bill type, rank vendors based on fee ratio, where lowest is best and highest is worst.
# Step 2: Assign the color corresponding to the best vendor.
# Step 3: Join on the client's exposure level for each state and bill type.

fee.ratio %<>% 
  left_join(map.regions) %>% 
  mutate(best = sapply(1:nrow(fee.ratio), FUN = function(i){
    names(fee.ratio)[which(rank(fee.ratio[i,3:6])==1)+2]
  }),
  rank.2 = sapply(1:nrow(fee.ratio), FUN = function(i){
    names(fee.ratio)[which(rank(fee.ratio[i,3:6])==2)+2]
  }),
  rank.3 = sapply(1:nrow(fee.ratio), FUN = function(i){
    names(fee.ratio)[which(rank(fee.ratio[i,3:6])==3)+2]
  }),
  rank.4 = sapply(1:nrow(fee.ratio), FUN = function(i){
    names(fee.ratio)[which(rank(fee.ratio[i,3:6])==4)+2]
  }),
  color = mapvalues(best, vendors, palette)
  ) %>% 
  left_join(exposure.long, by = c("State" = "State", "bill.type" = "bill.type"))

#### Paid ratios
# Step 1: For each state and bill type, rank TPAs based on paid ratio, where lowest is best and highest is worst.
# Step 2: Assign the color corresponding to the best TPA.
# Step 3: Join on the client's exposure level for each state and bill type.

paid.ratio %<>% 
  left_join(map.regions) %>% 
  mutate(best = sapply(1:nrow(paid.ratio), FUN = function(i){
    names(paid.ratio)[which(rank(paid.ratio[i,3:6])==1)+2]
  }),
  rank.2 = sapply(1:nrow(paid.ratio), FUN = function(i){
    names(paid.ratio)[which(rank(paid.ratio[i,3:6])==2)+2]
  }),
  rank.3 = sapply(1:nrow(paid.ratio), FUN = function(i){
    names(paid.ratio)[which(rank(paid.ratio[i,3:6])==3)+2]
  }),
  rank.4 = sapply(1:nrow(paid.ratio), FUN = function(i){
    names(paid.ratio)[which(rank(paid.ratio[i,3:6])==4)+2]
  }),
  color = mapvalues(best, vendors, palette)
  ) %>% 
  left_join(exposure.long, by = c("State" = "State", "bill.type" = "bill.type"))

#### Total ratios
# Step 1: Create a total ratio, which is simply the sum of fee ratio and paid ratio.
# Step 2: For each state and bill type, rank TPAs based on paid ratio, where lowest is best and highest is worst.
# Step 3: Assign the color corresponding to the best TPA.
# Step 4: Join on the client's exposure level for each state and bill type.

total.ratio <- paid.ratio[c(1,2,7)]

total.ratio %<>% 
  mutate(Massive_Dynamic = fee.ratio[,3] + paid.ratio[,3],
         Lex_Corp = fee.ratio[,4] + paid.ratio[,4],
         Dyad = fee.ratio[,5] + paid.ratio[,5],
         Wolfram_Hart = fee.ratio[,6] + paid.ratio[,6]) %>% 
  select(State,bill.type,Massive_Dynamic,Lex_Corp,Dyad,Wolfram_Hart,region)

names(total.ratio)[3:6] <- vendors

total.ratio %<>% 
  mutate(best = sapply(1:nrow(total.ratio), FUN = function(i){
    names(total.ratio)[which(rank(total.ratio[i,3:6])==1)+2]
  }),
  rank.2 = sapply(1:nrow(total.ratio), FUN = function(i){
    names(total.ratio)[which(rank(total.ratio[i,3:6])==2)+2]
  }),
  rank.3 = sapply(1:nrow(total.ratio), FUN = function(i){
    names(total.ratio)[which(rank(total.ratio[i,3:6])==3)+2]
  }),
  rank.4 = sapply(1:nrow(total.ratio), FUN = function(i){
    names(total.ratio)[which(rank(total.ratio[i,3:6])==4)+2]
  }),
  color = mapvalues(best, vendors, palette)
  ) %>% 
  left_join(exposure.long, by = c("State" = "State", "bill.type" = "bill.type"))

#### Prep Exposure Data
# This is different from the exposure levels joined onto the ratio data above.
# The above exposures were prepped for the purpose of visualizing exposure as an opacity.
# Here, exposure is prepped for displaying the actual exposure in the mouseover info box.

exposure.data <- {
  temp <- exposure %>%
    left_join(map.regions) %>%
    gather(bill.type,Exposure,2:7) %>% 
    select(region,bill.type,Exposure)
  
  temp2 <- temp %>% 
    group_by(bill.type) %>% 
    summarise(Exposure.Total = sum(Exposure))
  
  left_join(temp,temp2) %>% 
    mutate(Exposure.Percent = percent(round(Exposure/Exposure.Total,digits = 3))) %>% 
    select(-Exposure.Total)
}


#### Saves out everything as and RDS file for use in the app.
saveRDS(fee.ratio, file = "data/fee.rds")
saveRDS(paid.ratio, file = "data/paid.rds")
saveRDS(total.ratio, file = "data/total.rds")
saveRDS(exposure.data, file = "data/exposuredata.rds")

