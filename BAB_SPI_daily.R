
#######################################################################################################################
#######################################################################################################################
##################################  Investment Strategies and Asset Management  #######################################
##################################           Paper Replication                  #######################################
##################################         Betting against Beta                 #######################################
##################################        by Frazzini & Pedersen                #######################################
#######################################################################################################################
############################################  #heftige CEOs  ##########################################################
#######################################################################################################################
#######################################################################################################################
### Table of Content                                                             ######################################
#######################################################################################################################
### 1. Prepare Dataset                                                           ######################################
###   1.1. Import Data                                                           ######################################
###     1.1.1. SPI constituents for each month between 1990-01-01 and 2020-09-01 ######################################
###     1.1.2. RI data for each constituent between 1990-01-01 and 2020-09-01    ######################################
###     1.1.3. MV data for each constituent between 1990-01-01 and 2020-09-01    ######################################
###     1.1.4. SP500 Index between 1990-01-01 and 2020-09-01                     ######################################
###     1.1.5. RF between 1990-01-01 and 2020-09-01                              ######################################
###   1.2. Arrange Datasets for Analysis                                         ######################################
###     1.2.1. Get names and codes                                               ######################################
###     1.2.2. Arrange RI dataset                                                ######################################
###     1.2.3. Arrange MV dataset                                                ######################################
###     1.2.4. Arrange Constituents dataset                                      ######################################
###     1.2.5. Arrange SP500 Index dataset                                       ######################################
###     1.2.6. Arrange RF dataset                                                ######################################
### 2. Calculations                                                              ######################################
###   2.1. Calculate BAB Factor                                                  ######################################
###     2.1.1. While-loop 1: Betas and PF weights                                ######################################
###     2.1.2. While-loop 2: BAB Factor                                          ######################################
###     2.1.3. While-loop 3: Dates of Performance Measurement                    ######################################
###   2.2. Calculate Decile Portfolio Returns                                    ######################################
###   2.3. Replicate Table 3                                                     ######################################
###     2.3.1. Get Portfolio Returns                                             ######################################
###     2.3.2. Calculate mean (excess returns) and t-stats                       ######################################
###     2.3.3. Ex-ante Betas                                                     ######################################
###     2.3.4. Calculate vola and sd                                             ######################################
###     2.3.5. Calculate alphas                                                  ######################################
###        2.3.5.1. CAPM alpha                                                   ######################################
###     2.3.6. Output Table (Table 3)                                            ######################################
### 3. Graphical Analysis                                                        ######################################
###   3.1. BAB Factor (Return and Desity)                                        ######################################
###   3.2. Comparing Portfolio Performance                                       ######################################
###   3.3. TED Spread                                                            ######################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################


# Define Working Directory
setwd("C:/Users/Administrator/Documents/GitHub/Master_Finance/Betting against Beta")
getwd()


#######################################################################################################################
##### 1. Prepare Dataset ----------------------------------------------------------------------------------------------
#######################################################################################################################


#### 1.1 Import Data --------------------------------------------------------------------------------------------------

library(readxl)
library(tidyverse)

### 1.1.1 SPI constituents for each month between 1990-01-01 and 2020-09-01
#constituents_import   <- read_xlsx("data/SPI_RI_data.xlsx", sheet = "Constituents")
#constituents_import_2 <- constituents_import[-1,-1]

#constituents_vec <- as.vector(as.matrix(constituents_import_2))
#constituents     <- unique(constituents_vec) #Unique constituents vector
#constituents_tbl <- as.tibble(constituents)  #Unique constituents tibble



### 1.1.2 RI data for each constituent between 1990-01-01 and 2020-09-01
RI_import   <- read_xlsx("data/SPI_RI_data.xlsx", sheet = "TS_daily")
#save(RI_import, file="data/RI_Import.RData")
#load("data/RI_Import.RData")
RI_import_2 <- RI_import %>% dplyr::rename(Date = "Name")



### 1.1.3 MV data for each constituent between 1990-01-01 and 2020-09-01 -> not needed here!!!
#MV_import   <- read_xlsx("data/SPI_MV_data.xlsx", sheet = "TS_daily")
#save(MV_import, file="data/MV_Import.RData")
#load("data/MV_Import.RData")
#MV_import_2 <- MV_import %>% dplyr::rename(Date = "Name")



### 1.1.4. SP500 Index between 1990-01-01 and 2020-09-01
SPI_import_daily   <- read_xlsx("data/SPI.xlsx", sheet = "SPI_daily")
SPI_import_monthly <- read_xlsx("data/SPI.xlsx", sheet = "SPI_monthly")


### 1.1.5. RF between 1990-01-01 and 2020-09-01
Eidg.10_import <- read_xlsx("data/SPI.xlsx", sheet = "RF_monthly")





#### 1.2. Arrange Datasets for Analysis -------------------------------------------------------------------------------

### 1.2.1 Get names and codes
names <- colnames(RI_import_2)[-1]                                        # get stock names
codes <- as.vector(RI_import_2[1,-1]) %>% as.character()                  # get stock codes
names_code <- as.tibble(cbind(names, codes))                              # Combine names and codes into a tibble
colnames(names_code) <- c("Instrument","Code")                            # Rename column names
library(stringi)
names_code$Code <- names_code$Code %>%
  stri_replace_all_fixed("(RI)", "")                                      # Replace code structure
names_code$Instrument <- names_code$Instrument %>%
  stri_replace_all_fixed(" - TOT RETURN IND", "") 

# Identify Dead stocks
dead.stocks <- names_code %>% filter(str_detect(Instrument, "DEAD"))      # We have 71 Dead stocks
char.vec <- dead.stocks$Instrument
time_to_die <- str_sub(char.vec, -8, -1)
dead.stocks$time_to_die <- time_to_die
library(lubridate)
dead.stocks2 <- dead.stocks %>% mutate(time_to_die=as.Date(time_to_die, format= "%d/%m/%y"))



### 1.2.2 Arrange RI dataset
RI_raw <- RI_import_2[-1,]
# Generate right date format and make all prices  values 
RI_raw2 <- RI_raw %>%
  mutate(Date=as.Date(as.numeric(Date), origin = "1899-12-30")) %>%         # Generate right date format
  mutate_if(is.character, as.numeric) %>%                                   # Make all RI's numeric values
  pivot_longer(-Date, names_to = "Instrument", values_to = "RI") %>%        # Bring it into a long format
  mutate(Instrument = str_replace(Instrument, " - TOT RETURN IND", "")) %>% # Replace instrument structure
  left_join(names_code, by="Instrument")                                    # Add Instrument codes

library(tidyquant)
RI_raw3 <- RI_raw2 %>% mutate(Date_mon = as.yearmon(Date))                  # Add year-mon column
RI_raw4 <- RI_raw3[,c(1,5,2,4,3)]                                           # Arrange column order



### 1.2.3 Arrange MV dataset -> not needed here!!!
#MV_raw <- MV_import_2[-1,]
# Generate right date format and make all prices numeric values 
#MV_raw2 <- MV_raw %>%
  #mutate(Date=as.Date(as.numeric(Date), origin = "1899-12-30")) %>%       # Generate right date format
  #mutate_if(is.character, as.numeric) %>%                                 # Make all MV's numeric values
  #pivot_longer(-Date, names_to = "Instrument", values_to = "MV") %>%      # Bring it into a long format
  #mutate(Instrument = str_replace(Instrument, " - MARKET VALUE", "")) %>% # Replace instrument structure
  #left_join(names_code, by="Instrument")                                  # Add Instrument codes

#MV_raw3 <- MV_raw2 %>% mutate(Date_mon = as.yearmon(Date))                # Add year-mon column
#MV_raw4 <- MV_raw3[,c(1,5,2,4,3)]                                         # Arrange column order



### 1.2.4 Arrange Constituents dataset
#library(lubridate)
#constituents_raw <- constituents_import %>%
  #mutate(Date = as_date(Date), Date_mon = as.yearmon(Date)) %>%
  #dplyr::arrange(Date)

#constituents_raw2 <- constituents_raw %>%
  #select(Date, Date_mon) %>%
  #left_join(constituents_raw, by=c("Date", "Date_mon")) %>% mutate_if(is.numeric, as.character)
#colnames(constituents_raw2) <- c("Date", "Date_mon",1:229)

#constituents_raw3 <- constituents_raw2 %>%
  #pivot_longer(-c(Date, Date_mon), names_to = "Flag", values_to = "Code") %>%
  #na.omit() %>%
  #filter(Code != "NA")


### 1.2.5 Arrange SP500 Index dataset
SPI.raw_daily   <- SPI_import_daily %>% mutate(Date = as_date(Date)) %>% select(-SPI_P)
SPI.raw_monthly <- SPI_import_monthly %>% mutate(Date = as_date(Date)) %>% select(-SPI_P)


### 1.2.6 Arrange RF dataset
Eidg.10.raw <- Eidg.10_import %>% mutate(Date = as.Date(Date),
                                         RF10y.m=(1+RF10y/100)^(1/12)-1,
                                         RF10y.d = (1+RF10y/100)^(1/365)-1)




#### 1.3. Create Dataset for Analysis ---------------------------------------------------------------------------------

# Merge Datasets together
SPI_raw <- RI_raw4 %>%
  left_join(SPI.raw_daily, by ="Date") %>%                                 # Join Index
  left_join(Eidg.10.raw %>% select(Date, RF10y.d), by="Date") %>%          # Join RF rate
  filter(Date <= "2020-09-01")                                             # Filter dates <= "2020-09-01"

# Identify dead stocks
names.id <- dead.stocks2$Instrument
SPI_raw_dead <- SPI_raw %>% filter(Instrument %in% names.id)
SPI_raw_life <- SPI_raw %>% filter(!Instrument %in% names.id)


SPI_raw_dead_2 <- SPI_raw_dead %>%
  left_join(dead.stocks2 %>% select(-Instrument), by= "Code") %>%
  group_by(Instrument) %>%
  mutate(RI = ifelse(Date<=time_to_die, RI, NA)) %>%
  select(-time_to_die) %>%
  ungroup()


SPI_raw2 <- bind_rows(SPI_raw_life, SPI_raw_dead_2)

SPI_raw3 <- SPI_raw2 %>%
  expand(Date, Instrument) %>%
  left_join(SPI_raw2, by=c("Date", "Instrument")) %>%
  fill(RF10y.d, .direction="up")


library(roll)
SPI_data <- SPI_raw3 %>%
  group_by(Instrument) %>%
  dplyr::mutate(Return = log(RI)-lag(log(RI)),
         SPI.ret = log(SPI)-lag(log(SPI)),
         Inst.rf = Return-RF10y.d,
         Mkt.rf = SPI.ret - RF10y.d) %>%
         filter(SPI.ret != 0, Inst.rf != "NA", Return != 0)




#######################################################################################################################
##### 2. Calculations -------------------------------------------------------------------------------------------------
#######################################################################################################################

# Dataset for the first while loop (Beta calculation) 
SPI_data_w1 <- SPI_data %>% select(Date, Instrument, Mkt.rf, Inst.rf)
# RF rate for the second while loop (BAB calculation) 
RF10y <- SPI_data %>% ungroup() %>% expand(Date) %>% left_join(SPI_data %>% ungroup() %>% select(Date, RF10y.d), by="Date")


### 2.1. Calculate BAB Factor -----------------------------------------------------------------------------------------

## Do parallel computing
library(parallel)
detectCores()
library(doParallel)
registerDoParallel(cores = 4)


### 2.1.1 While-loop 1: Betas and PF weights

## Define parameters
roll  <- 60                           # Rolling Window 5 Years - > 1825 days (3*365)
rebal <- 1                            # Rebalance each 30 days (~monthly)
ult.startp <- "1990-01-01"            # First day of Portfolio Optimization
ult.endp   <- "2020-09-01"            # Last day of Portfolio Optimization
w <- 0.6                              # Weight for beta shrinkage
beta.xs <- 1                          # Assumption for cross sectional beta

startp <- as.Date(ult.startp)                        # Insample start point
endp   <- as.Date(ult.startp) %m+% months(roll) - 1  # Insample end point

## Store the results of each iteration
beta.L.list        <- vector(mode = "list", length = 350)
beta.H.list        <- vector(mode = "list", length = 350)
weights.long.list  <- vector(mode = "list", length = 350)
weights.short.list <- vector(mode = "list", length = 350)
names.long.list    <- vector(mode = "list", length = 350)
names.short.list   <- vector(mode = "list", length = 350)

names.PF1.list  <- vector(mode = "list", length = 350)
names.PF2.list  <- vector(mode = "list", length = 350)
names.PF3.list  <- vector(mode = "list", length = 350)
names.PF4.list  <- vector(mode = "list", length = 350)
names.PF5.list  <- vector(mode = "list", length = 350)
names.PF6.list  <- vector(mode = "list", length = 350)
names.PF7.list  <- vector(mode = "list", length = 350)
names.PF8.list  <- vector(mode = "list", length = 350)
names.PF9.list  <- vector(mode = "list", length = 350)
names.PF10.list <- vector(mode = "list", length = 350)

beta.ex1.list  <- vector(mode = "list", length = 350)
beta.ex2.list  <- vector(mode = "list", length = 350)
beta.ex3.list  <- vector(mode = "list", length = 350)
beta.ex4.list  <- vector(mode = "list", length = 350)
beta.ex5.list  <- vector(mode = "list", length = 350)
beta.ex6.list  <- vector(mode = "list", length = 350)
beta.ex7.list  <- vector(mode = "list", length = 350)
beta.ex8.list  <- vector(mode = "list", length = 350)
beta.ex9.list  <- vector(mode = "list", length = 350)
beta.ex10.list <- vector(mode = "list", length = 350)


i <- 1
while(endp <= as.Date(ult.endp) %m-% months(rebal)){
  cat("Calculating Betas",as.character(startp),"to",as.character(endp),"!\n")
  ## Define window
  data.is <- SPI_data_w1 %>%
    dplyr::filter(Date>=startp,Date<=endp) %>%
    mutate(corr = roll_cor(Inst.rf, Mkt.rf, width=1250, min_obs = 750),
           sd.inst  = roll_sd(Inst.rf, width = 250, min_obs = 120),
           sd.index = roll_sd(Mkt.rf, width = 250, min_obs = 120),
           Marker   = ifelse(!is.na(corr), T, F),
           beta     = corr*(sd.inst/sd.index)) %>% filter(Marker == TRUE)
  
  rows <- nrow(data.is); last.date <- data.is[rows,1] %>% .$Date
  
  data.beta <- data.is %>%
    filter(Date == last.date) %>%
    select(Instrument, beta)%>%
    mutate(beta.s = w*beta+(1-w)*beta.xs) %>%
    arrange(desc(beta.s))
  
  
  ## Calculate neccessary input parameters
  median   <- median(data.beta$beta.s)        # Median (cross section)
  z        <- rank(data.beta$beta.s)          # Ranks
  z.bar    <- mean(z)                         # Average rank
  diff     <- z-z.bar                         # difference
  abs.diff <- abs(z-z.bar)                    # Absolute difference between rank and average rank
  sum      <- sum(abs.diff)                   # Sum of absolute differences
  k        <- rep(2/sum, nrow(data.beta))     # Normalizing constant
  
  
  ## Calculate portfolio weights
  data.beta$z        <- z
  data.beta$diff     <- diff
  data.beta$abs.diff <- abs.diff
  data.beta$k        <- k
  
  
  ## Add two weight columns
  data.beta.2 <- data.beta %>%
    mutate(wH = k*max(diff,0),
           wL = k*min(diff,0),
           wL = wL*(-1),
           beta.weighted = ifelse(wH != 0, wH*beta.s, wL*beta.s)) %>%
    ungroup() %>%
    mutate(decile = ntile(beta.s, 10))
  
  
  ## Assign into long and short portfolio
  short.instruments <- data.beta.2 %>% filter(wH != 0) %>% .$Instrument
  long.instruments  <- data.beta.2 %>% filter(wL != 0) %>% .$Instrument
  
  beta.short <- data.beta.2 %>% dplyr::filter(wH != 0) %>% ungroup() %>% summarise(beta = sum(beta.weighted)) %>% .$beta
  beta.long  <- data.beta.2 %>% dplyr::filter(wL != 0) %>% ungroup() %>% summarise(beta = sum(beta.weighted)) %>% .$beta

  
  ## Calculate weight of long and short portfolios
  beta.H <- 1/beta.short
  beta.L <- 1/beta.long
  
  
  ## Store calculation results
  beta.H.list[[i]]        <- beta.H
  beta.L.list[[i]]        <- beta.L
  weights.long.list[[i]]  <- data.beta.2 %>% filter(Instrument %in% long.instruments) %>% select(Instrument, wL)
  weights.short.list[[i]] <- data.beta.2 %>% filter(Instrument %in% short.instruments) %>% select(Instrument, wH) 
  names.long.list[[i]]    <- long.instruments
  names.short.list[[i]]   <- short.instruments
  
  
  ## Sort Portfolios
  names.PF1  <- data.beta.2 %>% filter(decile == 1) %>% .$Instrument
  names.PF2  <- data.beta.2 %>% filter(decile == 2) %>% .$Instrument
  names.PF3  <- data.beta.2 %>% filter(decile == 3) %>% .$Instrument
  names.PF4  <- data.beta.2 %>% filter(decile == 4) %>% .$Instrument
  names.PF5  <- data.beta.2 %>% filter(decile == 5) %>% .$Instrument
  names.PF6  <- data.beta.2 %>% filter(decile == 6) %>% .$Instrument
  names.PF7  <- data.beta.2 %>% filter(decile == 7) %>% .$Instrument
  names.PF8  <- data.beta.2 %>% filter(decile == 8) %>% .$Instrument
  names.PF9  <- data.beta.2 %>% filter(decile == 9) %>% .$Instrument
  names.PF10 <- data.beta.2 %>% filter(decile == 10) %>% .$Instrument
  
  ## Ex-Ante Betas
  beta.ex1  <- data.beta.2 %>% filter(decile == 1) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  beta.ex2  <- data.beta.2 %>% filter(decile == 2) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  beta.ex3  <- data.beta.2 %>% filter(decile == 3) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  beta.ex4  <- data.beta.2 %>% filter(decile == 4) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  beta.ex5  <- data.beta.2 %>% filter(decile == 5) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  beta.ex6  <- data.beta.2 %>% filter(decile == 6) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  beta.ex7  <- data.beta.2 %>% filter(decile == 7) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  beta.ex8  <- data.beta.2 %>% filter(decile == 8) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  beta.ex9  <- data.beta.2 %>% filter(decile == 9) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  beta.ex10 <- data.beta.2 %>% filter(decile == 10) %>% summarise(beta.ex = mean(beta.s)) %>% .$beta.ex
  
  names.PF1.list[[i]]  <- names.PF1
  names.PF2.list[[i]]  <- names.PF2
  names.PF3.list[[i]]  <- names.PF3
  names.PF4.list[[i]]  <- names.PF4
  names.PF5.list[[i]]  <- names.PF5
  names.PF6.list[[i]]  <- names.PF6
  names.PF7.list[[i]]  <- names.PF7
  names.PF8.list[[i]]  <- names.PF8
  names.PF9.list[[i]]  <- names.PF9
  names.PF10.list[[i]] <- names.PF10
  
  beta.ex1.list[[i]]  <- beta.ex1
  beta.ex2.list[[i]]  <- beta.ex2
  beta.ex3.list[[i]]  <- beta.ex3
  beta.ex4.list[[i]]  <- beta.ex4
  beta.ex5.list[[i]]  <- beta.ex5
  beta.ex6.list[[i]]  <- beta.ex6
  beta.ex7.list[[i]]  <- beta.ex7
  beta.ex8.list[[i]]  <- beta.ex8
  beta.ex9.list[[i]]  <- beta.ex9
  beta.ex10.list[[i]] <- beta.ex10
  
  i <- i+1
  startp <- as.Date(startp) %m+% months(rebal)
  endp <- as.Date(startp) %m+% months(roll)-1
}



### 2.1.2 While-loop 2: BAB Factor

## Define parameters
rebal <- 1                             # Rebalance monthly
ult.startp <- "1995-01-01"             # First day of Portfolio Optimization
ult.endp   <- "2020-09-01"             # Last day of Portfolio Optimization

startp <- as.Date(ult.startp)
endp   <- as.Date(ult.startp) %m+% months(rebal) - 1

## Store the results of each iteration
BAB.list    <- vector(mode = "list", length = 350)


##  Data for While-loop 2
SPI_data_w2 <- SPI_data %>% select(Date, Instrument, Return) %>% complete(nesting(Instrument), Date)
RF10y.m <- Eidg.10.raw %>% filter(Date>="1995-01-31",Date<="2020-08-31") %>% select(Date, RF10y.m)

j <- 1
while(endp <= as.Date(ult.endp)){
  cat("Calculating BAB of",as.character(startp),"to",as.character(endp),as.character(j),"!\n")
  weights.long  <- weights.long.list[[j]]
  weights.short <- weights.short.list[[j]] 
  names.long    <- names.long.list[[j]]
  names.short   <- names.short.list[[j]]
  beta.L        <- beta.L.list[[j]]
  beta.H        <- beta.H.list[[j]]
  
  return.low.oos <- SPI_data_w2 %>%
    filter(Date>=startp,Date<=endp, Instrument %in% names.long) %>%
    left_join(weights.long, by="Instrument") %>%
    mutate(Return.w = Return * wL)
  
  pf.long.ret <- return.low.oos %>% summarise(ret = sum(Return.w)) %>% summarise(PF = sum(ret)) %>% .$PF
  
  
  return.high.oos <- SPI_data_w2 %>%
    filter(Date>=startp,Date<=endp, Instrument %in% names.short) %>%
    left_join(weights.short, by="Instrument") %>%
    mutate(Return.w = Return * wH)
  
  pf.short.ret <- return.high.oos %>% summarise(ret = sum(Return.w)) %>% summarise(PF = sum(ret)) %>% .$PF
  
  Date.rf <- return.high.oos %>% ungroup() %>% select(Date)
  n <- nrow(Date.rf)
  Date.rf <- Date.rf[n,] %>% .$Date
  rf <- RF10y.m %>% filter(Date>=Date.rf,Date<=endp) %>% select(RF10y.m) %>% .$RF10y.m
  rf <- rf[[1]]
  
  leverage.low  <- beta.L
  leverage.high <- beta.H
  
  BAB <- leverage.low*(pf.long.ret-rf) - leverage.high*(pf.short.ret-rf)
  BAB.list[[j]] <- BAB
  
  j <- j+1
  startp <- as.Date(startp) %m+% months(rebal)
  endp <- as.Date(startp) %m+% months(rebal) - 1
}



### 2.1.3 While-loop 3: Dates of Performance Measurement
library(lubridate)
## Define parameters
rebal <- 1                            # Rebalance monthly
ult.startp <- "1995-01-01"            # First day of Portfolio Optimization
ult.endp   <- "2020-09-01"            # Last day of Portfolio Optimization
## While-loop
startp <- as.Date(ult.startp)
endp   <- as.Date(ult.startp) %m+% months(rebal) - 1

## Store the results of each iteration
date.list    <- vector(mode = "list", length = 350)

j <- 1
while(endp <= as.Date(ult.endp)){
  date.list[[j]] <- endp
  
  startp <- as.Date(startp) %m+% months(rebal)
  endp <- as.Date(startp) %m+% months(rebal) - 1
  
  j <- j+1
}


Dates <- as.tibble(as.vector(unlist(date.list)))
colnames(Dates) <- "Date"
Dates <- Dates %>% mutate(Date = as.Date(as.numeric(Date)))

BAB <- as.vector(unlist(BAB.list))
plot(BAB, type="l")
sum(BAB)
Dates$BAB <- BAB 

Dates$BAB.ann <- Dates$BAB*12
t.test(Dates$BAB.ann)
t.test(Dates$BAB*100)




### 2.2. Calculate Portfolio Returns ----------------------------------------------------------------------------------

## Define parameters
rebal <- 1                             # Rebalance monthly
ult.startp <- "1995-01-01"             # First day of Portfolio Optimization
ult.endp   <- "2020-09-01"             # Last day of Portfolio Optimization

startp <- as.Date(ult.startp)
endp   <- as.Date(ult.startp) %m+% months(rebal) - 1

## Store the results of each iteration
PF1.list    <- vector(mode = "list", length = 350)
PF2.list    <- vector(mode = "list", length = 350)
PF3.list    <- vector(mode = "list", length = 350)
PF4.list    <- vector(mode = "list", length = 350)
PF5.list    <- vector(mode = "list", length = 350)
PF6.list    <- vector(mode = "list", length = 350)
PF7.list    <- vector(mode = "list", length = 350)
PF8.list    <- vector(mode = "list", length = 350)
PF9.list    <- vector(mode = "list", length = 350)
PF10.list   <- vector(mode = "list", length = 350)

##  Data for While-loop 4
SPI_data_w4 <- SPI_data %>% select(Date, Instrument, Inst.rf, Mkt.rf) %>% complete(nesting(Instrument), Date)

i <- 1
while(endp <= as.Date(ult.endp)){
  cat("Calculating Portfolio Returns from",as.character(startp),"to",as.character(endp),"!\n")
  PF.data <- SPI_data_w4 %>% filter(Date>=startp,Date<=endp) %>% ungroup()
  
  PF1.data <- PF.data %>% filter(Instrument %in% names.PF1.list[[i]])
  n.inst <- nrow(PF1.data %>% dplyr::count(Instrument))
  PF1.perf <- PF1.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.rf)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return

  PF2.data <- PF.data %>% filter(Instrument %in% names.PF2.list[[i]])
  n.inst <- nrow(PF2.data %>% dplyr::count(Instrument))
  PF2.perf <- PF2.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.rf)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF3.data <- PF.data %>% filter(Instrument %in% names.PF3.list[[i]])
  n.inst <- nrow(PF3.data %>% dplyr::count(Instrument))
  PF3.perf <- PF3.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.rf)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF4.data <- PF.data %>% filter(Instrument %in% names.PF4.list[[i]])
  n.inst <- nrow(PF4.data %>% dplyr::count(Instrument))
  PF4.perf <- PF4.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.rf)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF5.data <- PF.data %>% filter(Instrument %in% names.PF5.list[[i]])
  n.inst <- nrow(PF5.data %>% dplyr::count(Instrument))
  PF5.perf <- PF5.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.rf)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF6.data <- PF.data %>% filter(Instrument %in% names.PF6.list[[i]])
  n.inst <- nrow(PF6.data %>% dplyr::count(Instrument))
  PF6.perf <- PF6.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.rf)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF7.data <- PF.data %>% filter(Instrument %in% names.PF7.list[[i]])
  n.inst <- nrow(PF7.data %>% dplyr::count(Instrument))
  PF7.perf <- PF7.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.rf)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF8.data <- PF.data %>% filter(Instrument %in% names.PF8.list[[i]])
  n.inst <- nrow(PF8.data %>% dplyr::count(Instrument))
  PF8.perf <- PF8.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.rf)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF9.data <- PF.data %>% filter(Instrument %in% names.PF9.list[[i]])
  n.inst <- nrow(PF9.data %>% dplyr::count(Instrument))
  PF9.perf <- PF9.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.rf)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF10.data <- PF.data %>% filter(Instrument %in% names.PF10.list[[i]])
  n.inst <- nrow(PF10.data %>% dplyr::count(Instrument))
  PF10.perf <- PF10.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.rf)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  

  PF1.list[[i]]  <- PF1.perf
  PF2.list[[i]]  <- PF2.perf
  PF3.list[[i]]  <- PF3.perf
  PF4.list[[i]]  <- PF4.perf
  PF5.list[[i]]  <- PF5.perf
  PF6.list[[i]]  <- PF6.perf
  PF7.list[[i]]  <- PF7.perf
  PF8.list[[i]]  <- PF8.perf
  PF9.list[[i]]  <- PF9.perf
  PF10.list[[i]] <- PF10.perf
  
  i <- i+1
  startp <- as.Date(startp) %m+% months(rebal)
  endp <- as.Date(startp)  %m+% months(rebal)-1
}



### 2.3. Replicate Table 3 --------------------------------------------------------------------------------------------

## 2.3.1 Get Portfolio Returns
PF1 <- as.vector(unlist(PF1.list))
PF2 <- as.vector(unlist(PF2.list))
PF3 <- as.vector(unlist(PF3.list))
PF4 <- as.vector(unlist(PF4.list))
PF5 <- as.vector(unlist(PF5.list))
PF6 <- as.vector(unlist(PF6.list))
PF7 <- as.vector(unlist(PF7.list))
PF8 <- as.vector(unlist(PF8.list))
PF9 <- as.vector(unlist(PF9.list))
PF10<- as.vector(unlist(PF10.list))

## Portfolio Returns Dataset
Output.raw <- Dates
Output.raw$PF1 <- PF1
Output.raw$PF2 <- PF2
Output.raw$PF3 <- PF3
Output.raw$PF4 <- PF4
Output.raw$PF5 <- PF5
Output.raw$PF6 <- PF6
Output.raw$PF7 <- PF7
Output.raw$PF8 <- PF8
Output.raw$PF9 <- PF9
Output.raw$PF10 <- PF10

Output.raw <- Output.raw %>% filter(BAB != "NaN")

## 2.3.2 Calculate mean (excess returns) and t-stats
BAB.tmp <- t.test(Output.raw$BAB*100)
BAB.ret <- as.numeric(BAB.tmp$estimate)
BAB.t <- as.numeric(BAB.tmp$statistic)

PF1.tmp <- t.test(Output.raw$PF1*100)
PF1.ret <- as.numeric(PF1.tmp$estimate)
PF1.t <- as.numeric(PF1.tmp$statistic)

PF2.tmp <- t.test(Output.raw$PF2*100)
PF2.ret <- as.numeric(PF2.tmp$estimate)
PF2.t <- as.numeric(PF2.tmp$statistic)

PF3.tmp <- t.test(Output.raw$PF3*100)
PF3.ret <- as.numeric(PF3.tmp$estimate)
PF3.t <- as.numeric(PF3.tmp$statistic)

PF4.tmp <- t.test(Output.raw$PF4*100)
PF4.ret <- as.numeric(PF4.tmp$estimate)
PF4.t <- as.numeric(PF4.tmp$statistic)

PF5.tmp <- t.test(Output.raw$PF5*100)
PF5.ret <- as.numeric(PF5.tmp$estimate)
PF5.t <- as.numeric(PF5.tmp$statistic)

PF6.tmp <- t.test(Output.raw$PF6*100)
PF6.ret <- as.numeric(PF6.tmp$estimate)
PF6.t <- as.numeric(PF6.tmp$statistic)

PF7.tmp <- t.test(Output.raw$PF7*100)
PF7.ret <- as.numeric(PF7.tmp$estimate)
PF7.t <- as.numeric(PF7.tmp$statistic)

PF8.tmp <- t.test(Output.raw$PF8*100)
PF8.ret <- as.numeric(PF8.tmp$estimate)
PF8.t <- as.numeric(PF8.tmp$statistic)

PF9.tmp <- t.test(Output.raw$PF9*100)
PF9.ret <- as.numeric(PF9.tmp$estimate)
PF9.t <- as.numeric(PF9.tmp$statistic)

PF10.tmp <- t.test(Output.raw$PF10*100)
PF10.ret <- as.numeric(PF10.tmp$estimate)
PF10.t <- as.numeric(PF10.tmp$statistic)

## Mean (excess returns) and t-stats vector
exc.ret <- c(PF1.ret,PF2.ret,PF3.ret,PF4.ret,PF5.ret,PF6.ret,PF7.ret,PF8.ret,PF9.ret,PF10.ret,BAB.ret)
exc.t <- c(PF1.t,PF2.t,PF3.t,PF4.t,PF5.t,PF6.t,PF7.t,PF8.t,PF9.t,PF10.t,BAB.t)



## 2.3.3 Ex-ante Betas
beta.ex1 <- mean(as.vector(unlist(beta.ex1.list)))
beta.ex2 <- mean(as.vector(unlist(beta.ex2.list)))
beta.ex3 <- mean(as.vector(unlist(beta.ex3.list)))
beta.ex4 <- mean(as.vector(unlist(beta.ex4.list)))
beta.ex5 <- mean(as.vector(unlist(beta.ex5.list)))
beta.ex6 <- mean(as.vector(unlist(beta.ex6.list)))
beta.ex7 <- mean(as.vector(unlist(beta.ex7.list)))
beta.ex8 <- mean(as.vector(unlist(beta.ex8.list)))
beta.ex9 <- mean(as.vector(unlist(beta.ex9.list)))
beta.ex10 <- mean(as.vector(unlist(beta.ex10.list)))

## Ex-ante Beta vector
beta.ex <- c(beta.ex1,beta.ex2,beta.ex3,beta.ex4,beta.ex5,beta.ex6,beta.ex7,beta.ex8,beta.ex9,beta.ex10,0)



## 2.3.4 Calculate vola and sd
Output.raw2 <- Output.raw %>% pivot_longer(-Date, names_to = "Portfolio", values_to = "Return")

factors.SPI <- SPI.raw_monthly %>%
  mutate(Mkt = log(SPI)-lag(log(SPI))) %>%
  filter(Date>="1994-12-31",Date<="2020-08-31") %>%
  mutate(Date=Dates$Date) %>%
  left_join(Eidg.10.raw %>% select(Date, RF10y.m) %>% filter(Date>="1994-12-31",Date<="2020-08-31") %>% mutate(Date=Dates$Date), by="Date") %>%
  mutate(Mkt.rf = Mkt-RF10y.m) %>%
  select(Date, Mkt.rf)

Output.raw3 <- Output.raw2 %>%
  filter(Portfolio != "BAB.ann") %>%
  left_join(factors.SPI, by="Date") %>%
  mutate(Return = ifelse(Return == "NaN", 0, Return))

Output.raw3 %>% filter(Portfolio == "BAB")
library(tidyverse)
vola.tmp <- Output.raw3 %>% filter(Return != "NaN") %>% group_by(Portfolio) %>% dplyr::summarise(vola = sd(Return))
vola.tmp2 <- vola.tmp[2,]
vola.tmp3 <- vola.tmp[4:11,]
vola.tmp4 <- vola.tmp[3,]
vola.tmp5 <- vola.tmp[1,]
vola <- rbind(vola.tmp2,vola.tmp3,vola.tmp4,vola.tmp5)
vola$ex.ret <- exc.ret
vola.2 <- vola %>% mutate(vola.y = 100*vola*(12^0.5),
                          ex.ret.y= ex.ret*12,
                          SR = ex.ret.y/vola.y)



## 2.3.5 Calculate alphas
library(broom)
library(tibble)
library(tidyr)
library(purrr)  

# 2.3.5.1 CAPM alpha
reg.CAPM <- Output.raw3 %>% group_by(Portfolio) %>%
  nest(-Portfolio) %>%
  mutate(fit = map(data, ~lm(Return~Mkt.rf, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied)

CAPM.data.tmp <- reg.CAPM %>% filter(term =="(Intercept)") %>% select(Portfolio, estimate, statistic)
CAPM.data.tmp2 <- CAPM.data.tmp[2:11,]
CAPM.data.tmp3 <- CAPM.data.tmp[1,]
CAPM.data <- rbind(CAPM.data.tmp2,CAPM.data.tmp3)

CAPM.beta.tmp <- reg.CAPM %>% filter(term == "Mkt.rf") %>% select(Portfolio, estimate, statistic)
CAPM.beta.tmp2 <- CAPM.beta.tmp[2:11,]
CAPM.beta.tmp3 <- CAPM.beta.tmp[1,]
CAPM.beta <- rbind(CAPM.beta.tmp2,CAPM.beta.tmp3)



## 2.3.6 Output Table (Table 3)
library(xtable)
table_3 <- matrix(NA, 8, 11)
colnames(table_3) <- c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","BAB")
rownames(table_3) <- c("Excess return","t-stat","CAPM alpha", "t-stat", "Beta (ex ante)", "Beta (realized)", "Volatility", "Sharpe ratio")

table_3[1,] <- exc.ret
table_3[2,] <- exc.t
table_3[3,] <- round(CAPM.data$estimate*100,2)
table_3[4,] <- round(CAPM.data$statistic,2)
table_3[5,] <- round(beta.ex,2)
table_3[6,] <- round(CAPM.beta$estimate,3)
table_3[7,] <- round(vola.2$vola.y,2)
table_3[8,] <- round(vola.2$SR,2)

# Plot Table
table_3 %>% xtable() %>% 
  kableExtra::kable(digits=3, booktabs = TRUE,
                    caption = "SPI: returns 1995-2020") %>%
  kableExtra::kable_styling(position = "center",bootstrap_options = c("striped", "hover", "condensed"),
                            latex_options = c("striped","HOLD_position","HOLD_position"), font_size = 9,
                            full_width = TRUE)


library(rio)
export(table_3, "Outputs/Table3_SPI.xlsx")





#######################################################################################################################
##### 3. Graphical Analysis -------------------------------------------------------------------------------------------
#######################################################################################################################

### 3.1. BAB Factor -----------------------------------------------------------------------------

## BAB Return Plot
ggplot(data = Output.raw3 %>% filter(Portfolio == "BAB"), aes(x = Date, y = BAB))+
  geom_line(color = "blue", size = 0.7)+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  theme_bw()+
  geom_rect(xmin = as.numeric(ymd("1995-01-01")), 
            xmax = as.numeric(ymd("1996-12-31")), 
            ymin = -0.5, ymax = 0.5, 
            fill = 'grey', alpha = 0.01)+
  geom_rect(xmin = as.numeric(ymd("2000-03-01")), 
            xmax = as.numeric(ymd("2002-10-01")), 
            ymin = -1, ymax = 15, 
            fill = 'grey', alpha = 0.01)+
  geom_rect(xmin = as.numeric(ymd("2007-12-01")), 
            xmax = as.numeric(ymd("2009-06-01")), 
            ymin = -0.5, ymax = 0.5, 
            fill = 'grey', alpha = 0.01)+
  geom_rect(xmin = as.numeric(ymd("2015-01-01")), 
            xmax = as.numeric(ymd("2015-03-31")), 
            ymin = -0.5, ymax = 0.5, 
            fill = 'grey', alpha = 0.01)+
  geom_rect(xmin = as.numeric(ymd("2020-02-01")), 
            xmax = as.numeric(ymd("2020-09-01")), 
            ymin = -0.5, ymax = 0.5, 
            fill = 'grey', alpha = 0.01)+
  labs(title="BAB Factor", subtitle = "1995-01-01 to 2020-09-01", y="BAB Return")


## BAB Return Density Plot
ggplot(data = Output.raw3 %>% filter(Portfolio == "BAB"), aes(x = BAB))+
  geom_density(color = "blue", size = 1, fill="lightblue")+
  theme_bw()+
  labs(title="Density BAB Factor", subtitle = "1995-01-01 to 2020-09-01", x="Return", y="Density")




### 3.2. Comparing Portfolios ------------------------------------------------------------------------------------------

## Cummulative BAB and PF1 to PF10 returns
cum.BAB  <- Output.raw3 %>% filter(Portfolio == "BAB") %>% select(Date, Portfolio, Return) %>% mutate(BAB = cumprod(1 + Return))
cum.PF1  <- Output.raw3 %>% filter(Portfolio == "PF1") %>% select(Date, Portfolio, Return) %>% mutate(PF1 = cumprod(1 + Return))
cum.PF2  <- Output.raw3 %>% filter(Portfolio == "PF2") %>% select(Date, Portfolio, Return) %>% mutate(PF2 = cumprod(1 + Return))
cum.PF3  <- Output.raw3 %>% filter(Portfolio == "PF3") %>% select(Date, Portfolio, Return) %>% mutate(PF3 = cumprod(1 + Return))
cum.PF4  <- Output.raw3 %>% filter(Portfolio == "PF4") %>% select(Date, Portfolio, Return) %>% mutate(PF4 = cumprod(1 + Return))
cum.PF5  <- Output.raw3 %>% filter(Portfolio == "PF5") %>% select(Date, Portfolio, Return) %>% mutate(PF5 = cumprod(1 + Return))
cum.PF6  <- Output.raw3 %>% filter(Portfolio == "PF6") %>% select(Date, Portfolio, Return) %>% mutate(PF6 = cumprod(1 + Return))
cum.PF7  <- Output.raw3 %>% filter(Portfolio == "PF7") %>% select(Date, Portfolio, Return) %>% mutate(PF7 = cumprod(1 + Return))
cum.PF8  <- Output.raw3 %>% filter(Portfolio == "PF8") %>% select(Date, Portfolio, Return) %>% mutate(PF8 = cumprod(1 + Return))
cum.PF9  <- Output.raw3 %>% filter(Portfolio == "PF9") %>% select(Date, Portfolio, Return) %>% mutate(PF9 = cumprod(1 + Return))
cum.PF10 <- Output.raw3 %>% filter(Portfolio == "PF10") %>% select(Date, Portfolio, Return) %>% mutate(PF10 = cumprod(1 + Return))

## Cummulative Fama French returns
cum.Mkt.RF <- factors.SPI %>% mutate(Mkt.rf = cumprod(1 + Mkt.rf))

## Cummulative SPI returns
cum.SPI <- SPI.raw_monthly %>%
  mutate(SPI = log(SPI)-lag(log(SPI))) %>%
  filter(Date>="1994-12-31",Date<="2020-08-31") %>%
  mutate(Date=Dates$Date) %>%
  mutate(SPI.cum = cumprod(1 + SPI))

## Dataset for ggplot
cum.factors     <- cum.BAB
cum.factors$PF1 <- cum.PF1$PF1
cum.factors$PF2 <- cum.PF2$PF2
cum.factors$PF3 <- cum.PF3$PF3
cum.factors$PF4 <- cum.PF4$PF4
cum.factors$PF5 <- cum.PF5$PF5
cum.factors$PF6 <- cum.PF6$PF6
cum.factors$PF7 <- cum.PF7$PF7
cum.factors$PF8 <- cum.PF8$PF8
cum.factors$PF9 <- cum.PF9$PF9
cum.factors$PF10 <- cum.PF10$PF10
cum.factors$Mkt.RF <- cum.Mkt.RF$Mkt.rf
cum.factors$SPI <- cum.SPI$SPI.cum

## Plot cummulative Returns
cum.factors %>%
  gather(key,value, BAB, PF1, PF2, PF3, PF4, PF5, PF6, PF7, PF8, PF9, PF10, Mkt.RF) %>%
  ggplot(aes(x=Date, y=value, colour=key)) +
  geom_line()+
  labs(title="Cumulative Portfolio Returns", subtitle = "1995-01-01 to 2020-09-01", x="Date", y="Cumulative Return")+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  scale_y_continuous(breaks = c(-1:12), limits = c(-1,12))+
  theme_bw()


## Plot Alphas
data.fig1.tmp <- as.tibble(table_3)
data.fig1.tmp$Name <- row.names(table_3)

data.fig1 <- data.fig1.tmp[,c(12,1:11)] 
colnames(data.fig1) <- c("Name", "PF1", "PF2", "PF3", "PF4", "PF5", "PF6", "PF7", "PF8", "PF9", "PF10", "BAB")


# CAPM
data.fig1 %>% filter(Name == "CAPM alpha") %>% select(-BAB) %>%
  pivot_longer(-Name, values_to = "alpha", names_to = "Portfolio") %>%
  ggplot(aes(x=reorder(Portfolio, -alpha), y=alpha, fill="lightblue")) +
  geom_col(width=0.7,color='darkblue',fill='steelblue')+
  labs(title="Alphas of beta sorted Portfolios", subtitle = "CAPM", x="Portfolio", y="Alpha")+
  theme_bw()+
  theme(legend.position="none")


# Sharpe Ratio
data.fig1 %>% filter(Name == "Sharpe ratio") %>% select(-BAB) %>%
  pivot_longer(-Name, values_to = "SR", names_to = "Portfolio") %>%
  ggplot(aes(x=reorder(Portfolio, -SR), y=SR, fill="lightblue")) +
  geom_col(width=0.7,color='darkblue',fill='steelblue')+
  labs(title="Sharpe Ratios of Decile Portfolios", subtitle = "-", x="Portfolio", y="Sharpe Ratio")+
  theme_bw()+
  theme(legend.position="none")



### 3.3. TED Spread ----------------------------------------------------------------------------------------------------

# Basic scatter plot
Plot_Julian <- TED.data %>% select(TED.lag, BAB, delta.TED) %>% pivot_longer(-BAB, names_to = "Variables", values_to = "Returns")
Plot_Julian$Variables <- as.factor(Plot_Julian$Variables)
ggplot(Plot_Julian, aes(x=BAB, y=Returns, color=Variables, shape=Variables)) +
  geom_point()+
  theme_bw()+
  geom_smooth(method="lm", se=FALSE)+
  scale_shape_manual(values=c(16, 17))+ 
  scale_color_manual(values=c('blue', 'red'))+
  labs(title="Scatterplot", subtitle = "BAB vs lag TED & change TED", y="lag TED & change TED", x="BAB Return")


Plot_Luca <- Output.raw3 %>% filter(Portfolio == "BAB") %>% select(-Date)
ggplot(Plot_Luca, aes(x=Mkt.rf, y=Return)) +
  geom_point(color='blue')+
  theme_bw()+
  geom_smooth(method="lm", se=FALSE, color='red')+
  labs(title="Scatterplot", subtitle = "BAB vs Market Premium", y="BAB Return", x="Market Premium")
