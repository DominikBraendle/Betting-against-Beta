
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
###        2.3.5.2. Three-Factor alpha                                           ######################################
###        2.3.5.3. Four-Factor alpha                                            ######################################
###        2.3.5.4. Five-Factor alpha                                            ######################################
###     2.3.6. Output Table (Table 3)                                            ######################################
### 3. Graphical Analysis                                                        ######################################
###   3.1. BAB Factor (Return and Desity)                                        ######################################
###   3.2. Comparing Portfolio Performance                                       ######################################
###   3.3. TED Spread                                                            ######################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################


# Define Working Directory
setwd("C:/Users/Administrator/Documents/GitHub/Master_Finance_Betting against Beta")
getwd()


#######################################################################################################################
##### 1. Prepare Dataset ----------------------------------------------------------------------------------------------
#######################################################################################################################


#### 1.1. Import Data -------------------------------------------------------------------------------------------------

library(readxl)
library(tidyverse)

### 1.1.1. SP500 constituents for each month between 1990-01-01 and 2020-09-01
constituents_import   <- read_xlsx("data/SP500_RI_data.xlsx", sheet = "Constituents")
constituents_import_2 <- constituents_import[-1,-1]

constituents_vec <- as.vector(as.matrix(constituents_import_2))
constituents     <- unique(constituents_vec) #Unique constituents vector
constituents_tbl <- as.tibble(constituents)  #Unique constituents tibble



### 1.1.2 RI data for each constituent between 1990-01-01 and 2020-09-01
RI_import   <- read_xlsx("data/SP500_RI_data.xlsx", sheet = "TS_daily")
#save(RI_import, file="data/RData/RI_Import.RData")
#load("data/RData/RI_Import.RData")
RI_import_2 <- RI_import %>% dplyr::rename(Date = "Name")



### 1.1.3 MV data for each constituent between 1990-01-01 and 2020-09-01 -> not needed here!! (we have no survivorship bias and need not to calculate a market proxy)
#MV_import   <- read_xlsx("data/SP500_MV_data.xlsx", sheet = "TS_daily")
#save(MV_import, file="data/MV_Import.RData")
#load("data/MV_Import.RData")
#MV_import_2 <- MV_import %>% dplyr::rename(Date = "Name")



### 1.1.4. SP500 Index between 1990-01-01 and 2020-09-01
GSPC_import <- read_xlsx("data/SP500_Index.xlsx", sheet = "SP500_daily")



### 1.1.5. 3 months TBILL between 1990-01-01 and 2020-09-01
TBill_import <- read_xlsx("data/TBS3M.xlsx", sheet = "Daily")



### 1.1.6 Fama French Data from FFdownload package
library(FFdownload)
tempf <- tempfile(fileext = ".RData")
inputlist <- c("F-F_Research_Data_5_Factors_2x3","F-F_Momentum_Factor")
FFdownload(output_file = tempf, inputlist=inputlist)
load(tempf)

## Monthly - Factors
library(timetk)
FF5.m.raw <- FFdownload$`x_F-F_Research_Data_5_Factors`$monthly$Temp2 %>%
  tk_tbl(rename_index="Date") %>%
  mutate(Date=as.Date(Date, frac=1)) %>%
  dplyr::filter(Date >= "1990-01-01")

Mom.m.raw <- FFdownload$`x_F-F_Momentum_Factor`$monthly$Temp2 %>%
  tk_tbl(rename_index="Date") %>%
  mutate(Date=as.Date(Date, frac=1)) %>%
  dplyr::filter(Date >= "1990-01-01")

factors.m <- FF5.m.raw %>% left_join(Mom.m.raw, by="Date")

## Daily - Factors
FF5.d.raw <- FFdownload$`x_F-F_Research_Data_5_Factors`$daily$Temp2 %>%
  tk_tbl(rename_index="Date") %>%
  mutate(Date=as.Date(Date, frac=1)) %>%
  dplyr::filter(Date >= "1990-01-01")

Mom.d.raw <- FFdownload$`x_F-F_Momentum_Factor`$daily$Temp2 %>%
  tk_tbl(rename_index="Date") %>%
  dplyr::mutate(Date=as.Date(Date, frac=1)) %>%
  dplyr::filter(Date >= "1990-01-01")

factors.d <- FF5.d.raw %>% left_join(Mom.d.raw, by="Date")



### 1.1.7. LIBOR 1990-01-01 and 2020-09-01
LIBOR_import <- read_xlsx("data/LIBOR.xlsx", sheet = "FRED Graph")






#### 1.2. Arrange Datasets for Analysis -------------------------------------------------------------------------------

### 1.2.1 Get names and codes
names <- colnames(RI_import_2)[-1]                                            # get stock names
codes <- as.vector(RI_import_2[1,-1]) %>% as.character()                      # get stock codes
names_code <- as.tibble(cbind(names, codes))                                  # Combine names and codes into a tibble
colnames(names_code) <- c("Instrument","Code")                                # Rename column names
library(stringi)
names_code$Code <- names_code$Code %>%
  stri_replace_all_fixed("(RI)", "")                                          # Replace code structure
names_code$Instrument <- names_code$Instrument %>%
  stri_replace_all_fixed(" - TOT RETURN IND", "") 



### 1.2.2 Arrange RI dataset
RI_raw <- RI_import_2[-1,]
# Generate right date format and make all prices numeric values 
RI_raw2 <- RI_raw %>%
  mutate(Date=as.Date(as.numeric(Date), origin = "1899-12-30")) %>%           # Generate right date format
  mutate_if(is.character, as.numeric) %>%                                     # Make all RI's numeric values
  pivot_longer(-Date, names_to = "Instrument", values_to = "RI") %>%          # Bring it into a long format
  mutate(Instrument = str_replace(Instrument, " - TOT RETURN IND", "")) %>%   # Replace instrument structure
  left_join(names_code, by="Instrument")                                      # Add Instrument codes

library(tidyquant)
RI_raw3 <- RI_raw2 %>% mutate(Date_mon = as.yearmon(Date))                    # Add year-mon column
RI_raw4 <- RI_raw3[,c(1,5,2,4,3)]                                             # Arrange column order



### 1.2.3 Arrange MV dataset -> not needed here!! (we have no survivorship bias and need not to calculate a market proxy)
#MV_raw <- MV_import_2[-1,]
# Generate right date format and make all prices numeric values 
#MV_raw2 <- MV_raw %>%
#mutate(Date=as.Date(as.numeric(Date), origin = "1899-12-30")) %>%            # Generate right date format
#mutate_if(is.character, as.numeric) %>%                                      # Make all MV's numeric values
#pivot_longer(-Date, names_to = "Instrument", values_to = "MV") %>%           # Bring it into a long format
#mutate(Instrument = str_replace(Instrument, " - MARKET VALUE", "")) %>%      # Replace instrument structure
#left_join(names_code, by="Instrument")                                       # Add Instrument codes

#MV_raw3 <- MV_raw2 %>% mutate(Date_mon = as.yearmon(Date))                   # Add year-mon column
#MV_raw4 <- MV_raw3[,c(1,5,2,4,3)]                                            # Arrange column order



### 1.2.4 Arrange Constituents dataset
library(lubridate)
constituents_raw <- constituents_import %>%
  mutate(Date = as_date(Date), Date_mon = as.yearmon(Date)) %>%
  dplyr::arrange(desc(row_number()))

constituents_raw2 <- constituents_raw %>%
  select(Date, Date_mon) %>%
  left_join(constituents_raw, by=c("Date", "Date_mon")) %>%
  mutate_if(is.numeric, as.character)
colnames(constituents_raw2) <- c("Date", "Date_mon",1:506)

constituents_raw3 <- constituents_raw2 %>%
  pivot_longer(-c(Date, Date_mon), names_to = "Flag", values_to = "Code") %>%
  na.omit() %>%
  filter(Code != "NA")



### 1.2.5 Arrange SP500 Index dataset
GSPC.raw <- GSPC_import %>% mutate(Date = as_date(Date))


### 1.2.6 Arrange TBILL dataset
TBill.raw <- TBill_import %>%
  select(Date, TB3m) %>%
  mutate(Date = as.Date(Date), TB3m.m = ((1+(TB3m/100))^(1/3))-1, TB3m.d = ((1+(TB3m/100))^(1/90))-1)


### 1.2.7 Arrange LIBORdataset
LIBOR.raw <- LIBOR_import %>% mutate(Date = as.Date(Date), LIBOR=LIBOR/100)




#### 1.3. Create Dataset for Analysis ---------------------------------------------------------------------------------

SP500_raw <- RI_raw4 %>%
  left_join(constituents_raw3, by=c("Date", "Date_mon","Code")) %>%             # Join constituents
  left_join(GSPC.raw, by ="Date") %>%                                           # Join GSPC
  left_join(TBill.raw %>% select(Date, TB3m.d, TB3m.m), by="Date") %>%          # Join TBILL rate
  left_join(factors.d, by ="Date") %>%                                          # Add factor dataset
  filter(Date <= "2020-09-01")                                                  # Filter dates <= "2020-09-01"

save(SP500_raw, file="data/RData/SP500_d.RData")                                # Save and clean environment
load("data/SP500_d.RData")                                                      # load again


# Bring monthly Flag to daily Flag
SP500_raw2 <- SP500_raw %>%
  group_by(Instrument) %>%
  mutate(prev_val = Flag, next_val = Flag) %>%
  fill(prev_val, .direction = "down") %>%
  fill(next_val, .direction = "up")

SP500_raw3 <- SP500_raw2 %>%
  mutate(value = ifelse(prev_val == next_val, prev_val, Flag),
         prev_val = as.numeric(prev_val),
         next_val = as.numeric(next_val),
         value = as.numeric(value))

SP500_raw4 <- SP500_raw3 %>%
  mutate(value_2 = ifelse(prev_val != "NA" & next_val != "NA", next_val, NA), Flag = value_2) %>%
  select(-prev_val, -next_val, -value, -value_2)

# Flag which is not NA defines the assets which are listed in the index
SP500_raw5 <- SP500_raw4 %>% filter(Flag != "NA")


library(roll)
SP500_data <- SP500_raw5 %>%
  dplyr::mutate(Return = log(RI)-lag(log(RI)),                                     # Asset return
         SP500.ret = log(SP500)-lag(log(SP500)),                                   # Index return
         Inst.rf = Return-TB3m.d,                                                  # Excess return asset
         Mkt.rf = SP500.ret - TB3m.d) %>%                                          # Excess return market
  dplyr::mutate(Mkt.RF = Mkt.RF/100,                                               # Great Attention: Kenneth French Data are in [%]
                SMB = SMB/100,                                                     
                HML=HML/100,
                RMW=RMW/100,
                CMA=CMA/100,
                Mom=Mom/100,
                RF=RF/100) %>%
  filter(Mkt.RF != "NA")                                                           # Each day were Mkt.RF is not available is seen as weekend -> Remark:"Mkt.RF" comes from Kenneth French

  
save(SP500_data, file="data/RData/SP500_data.RData")
load("data/RData/SP500_data.RData")





#######################################################################################################################
##### 2. Calculations -------------------------------------------------------------------------------------------------
#######################################################################################################################

# Dataset for the first while loop (Beta calculation) 
SP500_data_w1 <- SP500_data %>% mutate(Inst.RF=Return-RF) %>% select(Date, Instrument, Mkt.RF, Inst.RF)
# TBill rate for the second while loop (BAB calculation) 
TBill <- SP500_data %>% ungroup() %>% expand(Date) %>% left_join(SP500_data %>% ungroup() %>% select(Date, RF), by="Date")


### 2.1. Calculate BAB Factor -----------------------------------------------------------------------------------------

library(tidyverse)
library(tidyquant)
library(lubridate)
library(roll)

## Do parallel computing
library(parallel)
detectCores()
library(doParallel)
registerDoParallel(cores = 4)


### 2.1.1 While-loop 1: Betas and PF weights

## Define parameters
roll  <- 60                                             # Rolling Window 5 Years -> 1825 days (5*365)
rebal <- 1                                              # Rebalance each 30 days (~monthly)
ult.startp <- "1990-01-01"                              # First day of Portfolio Optimization
ult.endp   <- "2020-09-01"                              # Last day of Portfolio Optimization
w <- 0.6                                                # Weight for beta shrinkage
beta.xs <- 1                                            # Assumption for cross sectional beta (not 100% correct in this case -> only SP500 and not the whole market)

startp <- as.Date(ult.startp)                           # Insample start point
endp   <- as.Date(ult.startp) %m+% months(roll) - 1     # Insample end point

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
  data.is <- SP500_data_w1 %>%
    filter(Date>=startp,Date<=endp) %>%
    mutate(corr     = roll_cor(Inst.RF, Mkt.RF, width=1250, min_obs = 750),
           sd.inst  = roll_sd(Inst.RF, width = 250, min_obs = 120),
           sd.index = roll_sd(Mkt.RF, width = 250, min_obs = 120),
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
    dplyr::mutate(wH = k*max(diff,0),
           wL = k*min(diff,0),
           wL = wL*(-1),
           beta.weighted = ifelse(wH != 0, wH*beta, wL*beta)) %>%
    ungroup() %>%
    mutate(decile = ntile(beta.s, 10))
  
  
  ## Assign into long and short portfolio
  short.instruments <- data.beta.2 %>% filter(wH != 0) %>% .$Instrument
  long.instruments  <- data.beta.2 %>% filter(wL != 0) %>% .$Instrument
  
  beta.short <- data.beta.2 %>% filter(wH != 0) %>% ungroup() %>% summarise(beta = sum(beta.weighted)) %>% .$beta
  beta.long  <- data.beta.2 %>% filter(wL != 0) %>% ungroup() %>% summarise(beta = sum(beta.weighted)) %>% .$beta

  
  ## Calculate weight of long and short portfolios
  beta.H <- 1/beta.short
  beta.L <- 1/beta.long
  
  
  ## Store calculation results
  beta.H.list[[i]]      <- beta.H
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

save(beta.L.list, file="data/While_1/beta_L.RData")
save(beta.H.list, file="data/While_1/beta_H.RData")
save(weights.long.list, file="data/While_1/weights_long.RData")
save(weights.short.list, file="data/While_1/weights_short.RData")
save(names.long.list, file="data/While_1/names_long.RData")
save(names.short.list, file="data/While_1/names_short.RData")

save(names.PF1.list, file="data/While_1/names.PF1.list.RData")
save(names.PF2.list, file="data/While_1/names.PF2.list.RData")
save(names.PF3.list, file="data/While_1/names.PF3.list.RData")
save(names.PF4.list, file="data/While_1/names.PF4.list.RData")
save(names.PF5.list, file="data/While_1/names.PF5.list.RData")
save(names.PF6.list, file="data/While_1/names.PF6.list.RData")
save(names.PF7.list, file="data/While_1/names.PF7.list.RData")
save(names.PF8.list, file="data/While_1/names.PF8.list.RData")
save(names.PF9.list, file="data/While_1/names.PF9.list.RData")
save(names.PF10.list, file="data/While_1/names.PF10.list.RData")

save(beta.ex1.list, file="data/While_1/beta.ex1.list.RData")
save(beta.ex2.list, file="data/While_1/beta.ex2.list.RData")
save(beta.ex3.list, file="data/While_1/beta.ex3.list.RData")
save(beta.ex4.list, file="data/While_1/beta.ex4.list.RData")
save(beta.ex5.list, file="data/While_1/beta.ex5.list.RData")
save(beta.ex6.list, file="data/While_1/beta.ex6.list.RData")
save(beta.ex7.list, file="data/While_1/beta.ex7.list.RData")
save(beta.ex8.list, file="data/While_1/beta.ex8.list.RData")
save(beta.ex9.list, file="data/While_1/beta.ex9.list.RData")
save(beta.ex10.list, file="data/While_1/beta.ex10.list.RData")



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
SP500_data_w2 <- SP500_data %>% select(Date, Instrument, Return) %>% complete(nesting(Instrument), Date)

j <- 1
while(endp <= as.Date(ult.endp)){
  cat("Calculating BAB of",as.character(startp),"to",as.character(endp),"!\n")
  weights.long  <- weights.long.list[[j]]
  weights.short <- weights.short.list[[j]] 
  names.long    <- names.long.list[[j]]
  names.short   <- names.short.list[[j]]
  beta.L        <- beta.L.list[[j]]
  beta.H        <- beta.H.list[[j]]
  
  
  return.low.oos <- SP500_data_w2 %>%
    filter(Date>=startp,Date<=endp, Instrument %in% names.long) %>%
    left_join(weights.long, by="Instrument") %>%
    mutate(Return.w = Return * wL)
  
  pf.long.ret <- return.low.oos %>% summarise(ret = sum(Return.w)) %>% summarise(PF = sum(ret)) %>% .$PF
  
  
  return.high.oos <- SP500_data_w2 %>%
    filter(Date>=startp,Date<=endp, Instrument %in% names.short) %>%
    left_join(weights.short, by="Instrument") %>%
    mutate(Return.w = Return * wH)
  
  pf.short.ret <- return.high.oos %>% summarise(ret = sum(Return.w)) %>% summarise(PF = sum(ret)) %>% .$PF
  
  Date.rf <- return.high.oos %>% ungroup() %>% select(Date)
  n <- nrow(Date.rf)
  Date.rf <- Date.rf[n,] %>% .$Date
  
  rf <- TBill %>% filter(Date %in% as.Date(Date.rf)) %>% select(RF) %>% .$RF
  rf <- rf[[1]]
  
  leverage.low  <- beta.L
  leverage.high <- beta.H
  
  BAB <- leverage.low*(pf.long.ret-rf) - leverage.high*(pf.short.ret-rf)
  BAB.list[[j]] <- BAB
  
  j <- j+1
  startp <- as.Date(startp) %m+% months(rebal)
  endp <- as.Date(startp) %m+% months(rebal) - 1
}

save(BAB.list, file="data/While_2/BAB.RData")
load("data/While_2/BAB.RData")



### 2.1.3 While-loop 3: Dates of Performance Measurement -> quick and dirty approach

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

save(date.list, file="data/While_3/date.list.RData")
load("data/While_3/date.list.RData")


Dates <- as.tibble(as.vector(unlist(date.list)))
colnames(Dates) <- "Date"
Dates <- Dates %>% mutate(Date = as.Date(as.numeric(Date)))

BAB <- as.vector(unlist(BAB.list))
sum(BAB)
Dates$BAB <- BAB 
Dates$BAB.ann <- Dates$BAB*12




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
SP500_data_w4 <- SP500_data %>% mutate(RF=RF/100, Inst.rf=Return-RF) %>% select(Date, Instrument, Inst.rf, Mkt.RF) %>% complete(nesting(Instrument), Date)
SP500_data_w4 <- SP500_data_w1 %>% select(Date, Instrument, Mkt.RF, Inst.RF) %>% complete(nesting(Instrument), Date)

i <- 1
while(endp <= as.Date(ult.endp)){
  cat("Calculating Portfolio Returns from",as.character(startp),"to",as.character(endp),"!\n")
  PF.data <- SP500_data_w4 %>% filter(Date>=startp,Date<=endp) %>% ungroup()
  
  PF1.data <- PF.data %>% filter(Instrument %in% names.PF1.list[[i]])
  n.inst <- nrow(PF1.data %>% dplyr::count(Instrument))
  PF1.perf <- PF1.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return

  PF2.data <- PF.data %>% filter(Instrument %in% names.PF2.list[[i]])
  n.inst <- nrow(PF2.data %>% dplyr::count(Instrument))
  PF2.perf <- PF2.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF3.data <- PF.data %>% filter(Instrument %in% names.PF3.list[[i]])
  n.inst <- nrow(PF3.data %>% dplyr::count(Instrument))
  PF3.perf <- PF3.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF4.data <- PF.data %>% filter(Instrument %in% names.PF4.list[[i]])
  n.inst <- nrow(PF4.data %>% dplyr::count(Instrument))
  PF4.perf <- PF4.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF5.data <- PF.data %>% filter(Instrument %in% names.PF5.list[[i]])
  n.inst <- nrow(PF5.data %>% dplyr::count(Instrument))
  PF5.perf <- PF5.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF6.data <- PF.data %>% filter(Instrument %in% names.PF6.list[[i]])
  n.inst <- nrow(PF6.data %>% dplyr::count(Instrument))
  PF6.perf <- PF6.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF7.data <- PF.data %>% filter(Instrument %in% names.PF7.list[[i]])
  n.inst <- nrow(PF7.data %>% dplyr::count(Instrument))
  PF7.perf <- PF7.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF8.data <- PF.data %>% filter(Instrument %in% names.PF8.list[[i]])
  n.inst <- nrow(PF8.data %>% dplyr::count(Instrument))
  PF8.perf <- PF8.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF9.data <- PF.data %>% filter(Instrument %in% names.PF9.list[[i]])
  n.inst <- nrow(PF9.data %>% dplyr::count(Instrument))
  PF9.perf <- PF9.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  
  PF10.data <- PF.data %>% filter(Instrument %in% names.PF10.list[[i]])
  n.inst <- nrow(PF10.data %>% dplyr::count(Instrument))
  PF10.perf <- PF10.data %>% group_by(Instrument) %>% dplyr::summarise(Return = sum(Inst.RF)) %>% mutate(Return.w = Return/n.inst) %>% summarise(Return = sum(Return.w)) %>% .$Return
  

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

save(PF1.list, file="data/While_4/PF1.list.RData")
save(PF2.list, file="data/While_4/PF2.list.RData")
save(PF3.list, file="data/While_4/PF3.list.RData")
save(PF4.list, file="data/While_4/PF4.list.RData")
save(PF5.list, file="data/While_4/PF5.list.RData")
save(PF6.list, file="data/While_4/PF6.list.RData")
save(PF7.list, file="data/While_4/PF7.list.RData")
save(PF8.list, file="data/While_4/PF8.list.RData")
save(PF9.list, file="data/While_4/PF9.list.RData")
save(PF10.list, file="data/While_4/PF10.list.RData")



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



## 2.3.2 Calculate mean (excess returns) and t-stats
BAB.tmp <- t.test(Output.raw$BAB*100)
BAB.ret <- as.numeric(BAB.tmp$estimate)
BAB.t   <- as.numeric(BAB.tmp$statistic)

PF1.tmp <- t.test(Output.raw$PF1*100)
PF1.ret <- as.numeric(PF1.tmp$estimate)
PF1.t   <- as.numeric(PF1.tmp$statistic)

PF2.tmp <- t.test(Output.raw$PF2*100)
PF2.ret <- as.numeric(PF2.tmp$estimate)
PF2.t   <- as.numeric(PF2.tmp$statistic)

PF3.tmp <- t.test(Output.raw$PF3*100)
PF3.ret <- as.numeric(PF3.tmp$estimate)
PF3.t   <- as.numeric(PF3.tmp$statistic)

PF4.tmp <- t.test(Output.raw$PF4*100)
PF4.ret <- as.numeric(PF4.tmp$estimate)
PF4.t   <- as.numeric(PF4.tmp$statistic)

PF5.tmp <- t.test(Output.raw$PF5*100)
PF5.ret <- as.numeric(PF5.tmp$estimate)
PF5.t   <- as.numeric(PF5.tmp$statistic)

PF6.tmp <- t.test(Output.raw$PF6*100)
PF6.ret <- as.numeric(PF6.tmp$estimate)
PF6.t   <- as.numeric(PF6.tmp$statistic)

PF7.tmp <- t.test(Output.raw$PF7*100)
PF7.ret <- as.numeric(PF7.tmp$estimate)
PF7.t   <- as.numeric(PF7.tmp$statistic)

PF8.tmp <- t.test(Output.raw$PF8*100)
PF8.ret <- as.numeric(PF8.tmp$estimate)
PF8.t   <- as.numeric(PF8.tmp$statistic)

PF9.tmp <- t.test(Output.raw$PF9*100)
PF9.ret <- as.numeric(PF9.tmp$estimate)
PF9.t   <- as.numeric(PF9.tmp$statistic)

PF10.tmp <- t.test(Output.raw$PF10*100)
PF10.ret <- as.numeric(PF10.tmp$estimate)
PF10.t   <- as.numeric(PF10.tmp$statistic)

## Mean (excess returns) and t-stats vector
exc.ret <- c(PF1.ret,PF2.ret,PF3.ret,PF4.ret,PF5.ret,PF6.ret,PF7.ret,PF8.ret,PF9.ret,PF10.ret,BAB.ret)
exc.t   <- c(PF1.t,PF2.t,PF3.t,PF4.t,PF5.t,PF6.t,PF7.t,PF8.t,PF9.t,PF10.t,BAB.t)



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

# Important Note: If neccessary go back to 1.1.6 an run code lines 61 to 95
Output.raw3 <- Output.raw2 %>%
  filter(Portfolio != "BAB.ann") %>%
  left_join(factors.m, by="Date") %>%
  mutate(Mkt.RF = Mkt.RF/100,
         RF = RF/100,
         SMB= SMB/100,
         HML=HML/100,
         Mom=Mom/100,
         RMW = RMW/100,
         CMA = CMA/100)

vola.tmp <- Output.raw3 %>% group_by(Portfolio) %>% dplyr::summarise(vola = sd(Return))
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

# 2.3.5.1. CAPM alpha
reg.CAPM <- Output.raw3 %>% group_by(Portfolio) %>%
  nest(-Portfolio) %>%
  mutate(fit = map(data, ~lm(Return~Mkt.RF, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied)

CAPM.data.tmp <- reg.CAPM %>% filter(term =="(Intercept)") %>% select(Portfolio, estimate, statistic)
CAPM.data.tmp2 <- CAPM.data.tmp[2:11,]
CAPM.data.tmp3 <- CAPM.data.tmp[1,]
CAPM.data <- rbind(CAPM.data.tmp2,CAPM.data.tmp3)

CAPM.beta.tmp <- reg.CAPM %>% filter(term =="Mkt.RF") %>% select(Portfolio, estimate, statistic)
CAPM.beta.tmp2 <- CAPM.beta.tmp[2:11,]
CAPM.beta.tmp3 <- CAPM.beta.tmp[1,]
CAPM.beta <- rbind(CAPM.beta.tmp2,CAPM.beta.tmp3)


# 2.3.5.2. Three-Factor alpha
reg.FF3 <- Output.raw3 %>%
  nest(-Portfolio) %>%
  mutate(fit = map(data, ~lm(Return~Mkt.RF+SMB+HML, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied)

FF3.data.tmp <- reg.FF3 %>% filter(term =="(Intercept)") %>% select(Portfolio, estimate, statistic)
FF3.data.tmp2 <- FF3.data.tmp[2:11,]
FF3.data.tmp3 <- FF3.data.tmp[1,]
FF3.data <- rbind(FF3.data.tmp2,FF3.data.tmp3)


# 2.3.5.3. Four-Factor alpha
reg.CF4 <- Output.raw3 %>%
  nest(-Portfolio) %>%
  mutate(fit = map(data, ~lm(Return~Mkt.RF+SMB+HML+Mom, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied)

CF4.data.tmp <- reg.CF4 %>% filter(term =="(Intercept)") %>% select(Portfolio, estimate, statistic)
CF4.data.tmp2 <- CF4.data.tmp[2:11,]
CF4.data.tmp3 <- CF4.data.tmp[1,]
CF4.data <- rbind(CF4.data.tmp2,CF4.data.tmp3)


# 2.3.5.4. Five-Factor alpha
reg.FF5 <- Output.raw3 %>%
  nest(-Portfolio) %>%
  mutate(fit = map(data, ~lm(Return~Mkt.RF+SMB+HML+RMW+CMA, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied)

FF5.data.tmp <- reg.FF5 %>% filter(term =="(Intercept)") %>% select(Portfolio, estimate, statistic)
FF5.data.tmp2 <- FF5.data.tmp[2:11,]
FF5.data.tmp3 <- FF5.data.tmp[1,]
FF5.data <- rbind(FF5.data.tmp2,FF5.data.tmp3)




## 2.3.6. Output Table (Table 3)
library(xtable)
table_3 <- matrix(NA, 14, 11)
colnames(table_3) <- c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","BAB")
rownames(table_3) <- c("Excess return","t-stat","CAPM alpha", "t-stat", "Three-factor alpha", "t-stat", "Four-factor alpha", "t-stat", "Five-factor alpha", "t-stat", "Beta (ex ante)", "Beta (realized)", "Volatility", "Sharpe ratio")

table_3[1,]  <- exc.ret
table_3[2,]  <- exc.t
table_3[3,]  <- round(CAPM.data$estimate*100,2)
table_3[4,]  <- round(CAPM.data$statistic,2)
table_3[5,]  <- round(FF3.data$estimate*100,2)
table_3[6,]  <- round(FF3.data$statistic,2)
table_3[7,]  <- round(CF4.data$estimate*100,2)
table_3[8,]  <- round(CF4.data$statistic,2)
table_3[9,]  <- round(FF5.data$estimate*100,2)
table_3[10,] <- round(FF5.data$statistic,2)
table_3[11,] <- round(beta.ex,2)
table_3[12,] <- round(CAPM.beta$estimate,3)
table_3[13,] <- round(vola.2$vola.y,2)
table_3[14,] <- round(vola.2$SR,2)

# Plot Table
table_3 %>% xtable() %>% 
  kableExtra::kable(digits=3, booktabs = TRUE,
                    caption = "SP500: returns 1995-2020") %>%
  kableExtra::kable_styling(position = "center",bootstrap_options = c("striped", "hover", "condensed"),
                            latex_options = c("striped","HOLD_position","HOLD_position"), font_size = 9,
                            full_width = TRUE)

# Export Table
library(rio)
export(table_3, "Outputs/Table3.xlsx")




### 2.3. TED Spread --------------------------------------------------------------------------------------------------------------
LIBOR.import <- read_xlsx("data/Libor3M_USD.xlsx", sheet = "TS_daily")
LIBOR.raw <- LIBOR.import %>% mutate(Date = as.Date(Date), LIBOR = as.numeric(LIBOR)) %>% filter(Date <= "2020-09-15")

TBS3m <- read_xlsx("data/TBS3M.xlsx", sheet = "Monthly")
TBS3m.raw <- TBS3m %>% mutate(Date = as.Date(Date))

TED.data.raw <- TBS3m.raw
TED.data.raw$LIBOR <- LIBOR.raw$LIBOR

TED.data.raw2 <- TED.data.raw %>%
  mutate(TED=LIBOR-TB3m,
         TED.lag = lag(TED)) %>%
  filter(Date >= "1995-01-01", Date <= "2020-08-31")
TED.data.raw2$BAB <- Output.raw$BAB

TED.data <- TED.data.raw2 %>% mutate(BAB.lag = lag(BAB), delta.TED = TED-lag(TED))

scatter.smooth(x=TED.data$BAB,
               y=TED.data$TED.lag,
               main="Lagged TED Spread",
               xlab="BAB Factor",
               ylab="TED.lag")

lagged.TED <- lm(BAB ~ TED.lag, data=TED.data) 
summary(lagged.TED)
library(tidyverse)
change.TED <- lm(BAB ~ delta.TED, data=TED.data %>% na.omit()) 
summary(change.TED)

## Covid-Impact Analysis
lagged.TED <- lm(BAB ~ TED.lag, data=TED.data %>% filter(Date <= "2013-01-01")) 
summary(lagged.TED)
library(tidyverse)
change.TED <- lm(BAB ~ delta.TED, data=TED.data %>% filter(Date <= "2013-01-01") %>% na.omit()) 
summary(change.TED)
change.TED <- lm(BAB ~ delta.TED+TED.lag, data=TED.data %>% filter(Date <= "2013-01-01") %>% na.omit()) 
summary(change.TED)


TED.data %>%
  gather(key,value, LIBOR, TB3m, TED) %>%
  ggplot(aes(x=Date, y=value, colour=key)) +
  geom_line()+
  labs(title="TED-Spread", subtitle = "1995-01-01 to 2020-09-01", x="Date", y="TED Spread (LIBOR-TB3m)")+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  theme_bw()+
  My_Theme_BAB


# Basic scatter plot
Plot_Julian <- TED.data %>% select(TED.lag, BAB, delta.TED) %>% pivot_longer(-BAB, names_to = "Variables", values_to = "Returns")
Plot_Julian$Variables <- as.factor(Plot_Julian$Variables)
ggplot(Plot_Julian, aes(x=BAB, y=Returns, color=Variables, shape=Variables)) +
  geom_point()+
  theme_bw()+
  geom_smooth(method="lm", se=FALSE)+
  scale_shape_manual(values=c(16, 17))+ 
  scale_color_manual(values=c('blue', 'red'))+
  My_Theme_BAB+
  labs(title="Scatterplot", subtitle = "BAB vs lag TED & change TED", y="lag TED & change TED", x="BAB Return")



#######################################################################################################################
##### 3. Graphical Analysis -------------------------------------------------------------------------------------------
#######################################################################################################################

### 3.1. BAB Factor ---------------------------------------------------------------------------------------------------

## BAB Return Plot
ggplot(data = Output.raw3 %>% filter(Portfolio == "BAB"), aes(x = Date, y = BAB))+
  geom_line(color = "blue", size = 0.7)+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  theme_bw()+
  geom_rect(xmin = as.numeric(ymd("2000-03-01")), 
            xmax = as.numeric(ymd("2002-10-01")), 
            ymin = -0.5, ymax = 0.5, 
            fill = 'grey', alpha = 0.01)+
  geom_rect(xmin = as.numeric(ymd("2007-12-01")), 
            xmax = as.numeric(ymd("2009-06-01")), 
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




### 3.2. Comparing Portfolios -----------------------------------------------------------------------------------------

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
cum.Mkt.RF <- factors.m %>% filter(Date >="1995-01-01", Date <= "2020-09-01") %>% select(Date, Mkt.RF) %>% mutate(Mkt.RF = cumprod(1 + Mkt.RF/100))
cum.SMB    <- factors.m %>% filter(Date >="1995-01-01", Date <= "2020-09-01") %>% select(Date, SMB) %>% mutate(SMB = cumprod(1 + SMB/100))
cum.HML    <- factors.m %>% filter(Date >="1995-01-01", Date <= "2020-09-01") %>% select(Date, HML) %>% mutate(HML = cumprod(1 + HML/100))
cum.RMW    <- factors.m %>% filter(Date >="1995-01-01", Date <= "2020-09-01") %>% select(Date, RMW) %>% mutate(RMW = cumprod(1 + RMW/100))
cum.CMA    <- factors.m %>% filter(Date >="1995-01-01", Date <= "2020-09-01") %>% select(Date, CMA) %>% mutate(CMA = cumprod(1 + CMA/100))
cum.Mom    <- factors.m %>% filter(Date >="1995-01-01", Date <= "2020-09-01") %>% select(Date, Mom) %>% mutate(Mom = cumprod(1 + Mom/100))

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
cum.factors$Mkt.RF <- cum.Mkt.RF$Mkt.RF
cum.factors$SMB <- cum.SMB$SMB
cum.factors$HML <- cum.HML$HML
cum.factors$RMW <- cum.RMW$RMW
cum.factors$CMA <- cum.CMA$CMA
cum.factors$Mom <- cum.Mom$Mom


## Plot cummulative Returns
cum.factors %>%
  gather(key,value, BAB, PF1, PF2, PF3, PF4, PF5, PF6, PF7, PF8, PF9, PF10,
         Mkt.RF, SMB, HML, RMW, CMA, Mom) %>% group_by(Portfolio) %>%
  ggplot(aes(x=Date, y=value, colour=key)) +
  geom_line()+
  labs(title="Cumulative Portfolio Returns", subtitle = "1995-01-01 to 2020-09-01", x="Date", y="Cumulative Returns")+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  scale_y_continuous(breaks = c(-1:13), limits = c(-1,13))+
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


# FF3
data.fig1 %>% filter(Name == "Three-factor alpha") %>% select(-BAB) %>%
  pivot_longer(-Name, values_to = "alpha", names_to = "Portfolio") %>%
  ggplot(aes(x=reorder(Portfolio, -alpha), y=alpha, fill="lightblue")) +
  geom_col(width=0.7,color='darkblue',fill='steelblue')+
  labs(title="Alphas of beta sorted Portfolios", subtitle = "Fama-French 3 Factor Model", x="Portfolio", y="Alpha")+
  theme_bw()+
  theme(legend.position="none")


# CF4
data.fig1 %>% filter(Name == "Four-factor alpha") %>% select(-BAB) %>%
  pivot_longer(-Name, values_to = "alpha", names_to = "Portfolio") %>%
  ggplot(aes(x=reorder(Portfolio, -alpha), y=alpha, fill="lightblue")) +
  geom_col(width=0.7,color='darkblue',fill='steelblue')+
  labs(title="Alphas of beta sorted Portfolios", subtitle = "Carhart-Fama-French 4 Factor Model", x="Portfolio", y="Alpha")+
  theme_bw()+
  theme(legend.position="none")


# FF5
data.fig1 %>% filter(Name == "Five-factor alpha") %>% select(-BAB) %>%
  pivot_longer(-Name, values_to = "alpha", names_to = "Portfolio") %>%
  ggplot(aes(x=reorder(Portfolio, -alpha), y=alpha, fill="lightblue")) +
  geom_col(width=0.7,color='darkblue',fill='steelblue')+
  labs(title="Alphas of beta sorted Portfolios", subtitle = "Fama-French 5 Factor Model", x="Portfolio", y="Alpha")+
  theme_bw()+
  theme(legend.position="none")


# Sharpe Ratio
data.fig1 %>% filter(Name == "Sharpe ratio") %>% select(-BAB) %>%
  pivot_longer(-Name, values_to = "SR", names_to = "Portfolio") %>%
  ggplot(aes(x=reorder(Portfolio, -SR), y=SR, fill="lightblue")) +
  geom_col(width=0.7,color='darkblue',fill='steelblue')+
  labs(title="Sharpe Ratios of Decile Portfolios", subtitle = "1995-01-01 to 2020-09-01", x="Portfolio", y="Sharpe Ratio")+
  theme_bw()+
  theme(legend.position="none")




### 3.3. TED Spread ---------------------------------------------------------------------------------------------------

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
