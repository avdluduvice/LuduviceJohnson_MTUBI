# -------------------------------------------------------------------------
# Title: Means-Tested Transfers, Asset Limits, and Universal Basic Income
# Purpose: Replication codes for FRBC's Economic Commentary
# File: Script with data transformations, weighting, and calculations
# Authors: André  Victor D. Luduvice and Cornelius Johnson
# Cleveland, July 20th, 2022
# -------------------------------------------------------------------------


# Data Transformations ----------------------------------------------------
pu2018$pid <- paste0(pu2018$ssuid, pu2018$shhadid, pu2018$pnum) #ssuid and sshadid uniquely identify households, adding pnum uniquely identifies persons


# Deflating ---------------------------------------------------------------
# Adjusting values to 2018 dollars
cpi <- fread(paste0(getwd(),"/Data/BLS_CPI-U_1947_2021.csv")) # US Bureau of Labor Statistics extracted Sep 24, 2021, CUSR0000SA0 

cpi <- cpi %>% select(Year, Period, Label, Value) # dropping series ID
cpi <- cpi %>% mutate(Year = as.factor(Year))

cpi <- cpi %>% filter(Year == 2017 | Year == 2018) # our data is from 2017, our reference month is Jan 2018

cpi <- cpi %>% mutate(                        # recoding to match monthcode in pu2018
  Period = case_when(
    Period == "M01" ~ 1,
    Period == "M02" ~ 2,
    Period == "M03" ~ 3,
    Period == "M04" ~ 4,
    Period == "M05" ~ 5,
    Period == "M06" ~ 6,
    Period == "M07" ~ 7,
    Period == "M08" ~ 8, 
    Period == "M09" ~ 9, 
    Period == "M10" ~ 10, 
    Period == "M11" ~ 11, 
    Period == "M12" ~ 12
  ) 
)

cpi$Jan2018 <- cpi[Year == 2018 & Period == 1]$Value # adding new variable, Jan2018, which is index value at that time
cpi$deflator <- cpi$Value/cpi$Jan2018 # defining our base year-month

cpi_2017 <- cpi %>% filter(Year == 2017) %>% select(Period, deflator) 

pu2018 <- merge(pu2018, cpi_2017, by.x = "monthcode", by.y = "Period") # merging deflator with SIPP data

# deflating flow variables
pu2018 <- pu2018 %>%  
  mutate(
    tpearn = tpearn/deflator,
    thtotinc = thtotinc/deflator, 
    tptrninc = tptrninc/deflator,
    tga_amt = tga_amt/deflator,
    tsnap_amt = tsnap_amt/deflator,
    tssi_amt = tssi_amt/deflator,
    ttanf_amt = ttanf_amt/deflator,
    twic_amt = twic_amt/deflator
  )


# Annualizing -------------------------------------------------------------

#agg income and cash transfers to annual level, agg earnings to household and annual levels
pu2018 <- pu2018 %>% 
  group_by(ssuid) %>% 
  mutate(
    tptrninc_annual_hh = sum(tptrninc, na.rm = T), 
    tga_amt_annual_hh = sum(tga_amt, na.rm = T), 
    tsnap_amt_annual_hh = sum(tsnap_amt, na.rm = T),
    tssi_amt_annual_hh = sum(tssi_amt, na.rm = T), 
    ttanf_amt_annual_hh = sum(ttanf_amt, na.rm = T), 
    twic_amt_annual_hh = sum(twic_amt, na.rm = T), 
    thhearn_annual = sum(tpearn, na.rm = T)
  )


# earnings are at the person level, aggregation is a bit different
pu2018 <- pu2018 %>%
  group_by(pid) %>%
  mutate(tpearn_annual = sum(tpearn))

# keeping only one reference person per household of either type
pu2018_hh <- as.data.table(pu2018)[erelrpe == 1 | erelrpe == 2]

# Aggregating household level data to annual level - since we only have one obs per hh, ssuid will be enough
pu2018_hh <- pu2018_hh %>%
  group_by(ssuid) %>%
  mutate(thtotinc_annual = sum(thtotinc, na.rm = T))

# Creating a 'resources' variable - in the defined concept of the assets testing for the programs
pu2018_hh <- pu2018_hh %>%
  mutate(thhresources = thval_bank + thval_bond + thval_oth + thval_re + thval_ret + thval_rent  + thval_stmf)

#Selecting householders with an observation in the final month of surveying
dataGraphs <- as.data.table(pu2018_hh)[(erelrpe == 1 | erelrpe == 2) & monthcode == 12]

# Trim by income
dataGraphs <- dataGraphs[thtotinc_annual <= quantile(dataGraphs$thtotinc_annual, probs = income_trim)]

# Unweighted Quantiles -----------------------------------------------------------
if (!wgt_quants) {
  # Wealth 
  wealth_quints <- quantile(dataGraphs$thnetworth, probs = (0:5)/5)
  wealth_decs <- quantile(dataGraphs$thnetworth, probs = (0:10)/10)
  
  # Assignment - Quintile
  dataGraphs[thnetworth <= wealth_quints[[2]], wealth_quintile := "1st Quintile"] #0-20
  dataGraphs[thnetworth <= wealth_quints[[3]] & thnetworth > wealth_quints[[2]], wealth_quintile := "2nd Quintile"] #21-40
  dataGraphs[thnetworth <= wealth_quints[[4]] & thnetworth > wealth_quints[[3]], wealth_quintile := "3rd Quintile"] #41-60
  dataGraphs[thnetworth <= wealth_quints[[5]] & thnetworth > wealth_quints[[4]], wealth_quintile := "4th Quintile"] #61-80
  dataGraphs[thnetworth > wealth_quints[[5]], wealth_quintile := "5th Quintile"] #81-100
  
  # Assignment - Decile
  dataGraphs[thnetworth <= wealth_decs[[2]], wealth_decile := "1st Decile"] #0-10
  dataGraphs[thnetworth <= wealth_decs[[4]] & thnetworth > wealth_decs[[3]], wealth_decile := "3rd Decile"] #21-30
  dataGraphs[thnetworth <= wealth_decs[[5]] & thnetworth > wealth_decs[[4]], wealth_decile := "4th Decile"] #31-40
  dataGraphs[thnetworth <= wealth_decs[[6]] & thnetworth > wealth_decs[[5]], wealth_decile := "5th Decile"] #41-50
  dataGraphs[thnetworth <= wealth_decs[[7]] & thnetworth > wealth_decs[[6]], wealth_decile := "6th Decile"] #51-60
  dataGraphs[thnetworth <= wealth_decs[[8]] & thnetworth > wealth_decs[[7]], wealth_decile := "7th Decile"] #61-70
  dataGraphs[thnetworth <= wealth_decs[[9]] & thnetworth > wealth_decs[[8]], wealth_decile := "8th Decile"] #71-80
  dataGraphs[thnetworth <= wealth_decs[[10]] & thnetworth > wealth_decs[[9]], wealth_decile := "9th Decile"] #81-90
  dataGraphs[thnetworth > wealth_decs[[10]], wealth_decile := "10th Decile"] #91-100
  
  # Resources 
  resources_quints <- quantile(dataGraphs$thhresources, probs = (0:5)/5)
  resources_decs <- quantile(dataGraphs$thhresources, probs = (0:10)/10)
  
  # Assignment - Quintile
  dataGraphs[thhresources <= resources_quints[[2]], resources_quintile := "1st Quintile"] #0-20
  dataGraphs[thhresources <= resources_quints[[3]] & thhresources > resources_quints[[2]], resources_quintile := "2nd Quintile"] #21-40
  dataGraphs[thhresources <= resources_quints[[4]] & thhresources > resources_quints[[3]], resources_quintile := "3rd Quintile"] #41-60
  dataGraphs[thhresources <= resources_quints[[5]] & thhresources > resources_quints[[4]], resources_quintile := "4th Quintile"] #61-80
  dataGraphs[thhresources > resources_quints[[5]], resources_quintile := "5th Quintile"] #81-100
  
  # Assignment - Decile
  dataGraphs[thhresources <= resources_decs[[2]], resources_decile := "1st Decile"] #0-10
  dataGraphs[thhresources <= resources_decs[[3]] & thhresources > resources_decs[[2]], resources_decile := "2nd Decile"] #11-20
  dataGraphs[thhresources <= resources_decs[[4]] & thhresources > resources_decs[[3]], resources_decile := "3rd Decile"] #21-30
  dataGraphs[thhresources <= resources_decs[[5]] & thhresources > resources_decs[[4]], resources_decile := "4th Decile"] #31-40
  dataGraphs[thhresources <= resources_decs[[6]] & thhresources > resources_decs[[5]], resources_decile := "5th Decile"] #41-50
  dataGraphs[thhresources <= resources_decs[[7]] & thhresources > resources_decs[[6]], resources_decile := "6th Decile"] #51-60
  dataGraphs[thhresources <= resources_decs[[8]] & thhresources > resources_decs[[7]], resources_decile := "7th Decile"] #61-70
  dataGraphs[thhresources <= resources_decs[[9]] & thhresources > resources_decs[[8]], resources_decile := "8th Decile"] #71-80
  dataGraphs[thhresources <= resources_decs[[10]] & thhresources > resources_decs[[9]], resources_decile := "9th Decile"] #81-90
  dataGraphs[thhresources > resources_decs[[10]], resources_decile := "10th Decile"] #91-100
  
  # Household Income 
  thinc_quints <- quantile(dataGraphs$thtotinc_annual, probs = (0:5)/5)
  thinc_decs <- quantile(dataGraphs$thtotinc_annual, probs = (0:10)/10)
  
  # Assignment - Quintile
  dataGraphs[thtotinc_annual <= thinc_quints[[2]], thinc_quintile := "1st Quintile"] #0-20
  dataGraphs[thtotinc_annual <= thinc_quints[[3]] & thtotinc_annual > thinc_quints[[2]], thinc_quintile := "2nd Quintile"] #21-40
  dataGraphs[thtotinc_annual <= thinc_quints[[4]] & thtotinc_annual > thinc_quints[[3]], thinc_quintile := "3rd Quintile"] #41-60
  dataGraphs[thtotinc_annual <= thinc_quints[[5]] & thtotinc_annual > thinc_quints[[4]], thinc_quintile := "4th Quintile"] #61-80
  dataGraphs[thtotinc_annual > thinc_quints[[5]], thinc_quintile := "5th Quintile"] #81-100
  
  # Assignment - Decile
  dataGraphs[thtotinc_annual <= thinc_decs[[2]], thinc_decile := "1st Decile"] #0-10
  dataGraphs[thtotinc_annual <= thinc_decs[[3]] & thtotinc_annual > thinc_decs[[2]], thinc_decile := "2nd Decile"] #11-20
  dataGraphs[thtotinc_annual <= thinc_decs[[4]] & thtotinc_annual > thinc_decs[[3]], thinc_decile := "3rd Decile"] #21-30
  dataGraphs[thtotinc_annual <= thinc_decs[[5]] & thtotinc_annual > thinc_decs[[4]], thinc_decile := "4th Decile"] #31-40
  dataGraphs[thtotinc_annual <= thinc_decs[[6]] & thtotinc_annual > thinc_decs[[5]], thinc_decile := "5th Decile"] #41-50
  dataGraphs[thtotinc_annual <= thinc_decs[[7]] & thtotinc_annual > thinc_decs[[6]], thinc_decile := "6th Decile"] #51-60
  dataGraphs[thtotinc_annual <= thinc_decs[[8]] & thtotinc_annual > thinc_decs[[7]], thinc_decile := "7th Decile"] #61-70
  dataGraphs[thtotinc_annual <= thinc_decs[[9]] & thtotinc_annual > thinc_decs[[8]], thinc_decile := "8th Decile"] #71-80
  dataGraphs[thtotinc_annual <= thinc_decs[[10]] & thtotinc_annual > thinc_decs[[9]], thinc_decile := "9th Decile"] #81-90
  dataGraphs[thtotinc_annual > thinc_decs[[10]], thinc_decile := "10th Decile"] #91-100
}


# Weighted Quantiles ------------------------------------------------------
if (wgt_quants) {
  # Wealth
  wealth_quints <- wtd.quantile(dataGraphs$thnetworth, weights = dataGraphs$wpfinwgt, probs = (0:5)/5)
  wealth_decs <- wtd.quantile(dataGraphs$thnetworth, weights = dataGraphs$wpfinwgt, probs = (0:10)/10)
  
  # Assignment - Quintile 
  dataGraphs[thnetworth <= wealth_quints[[2]], wealth_quintile := "1st Quintile"] #0-20
  dataGraphs[thnetworth <= wealth_quints[[3]] & thnetworth > wealth_quints[[2]], wealth_quintile := "2nd Quintile"] #21-40
  dataGraphs[thnetworth <= wealth_quints[[4]] & thnetworth > wealth_quints[[3]], wealth_quintile := "3rd Quintile"] #41-60
  dataGraphs[thnetworth <= wealth_quints[[5]] & thnetworth > wealth_quints[[4]], wealth_quintile := "4th Quintile"] #61-80
  dataGraphs[thnetworth > wealth_quints[[5]], wealth_quintile := "5th Quintile"] #81-100
  
  # Assignment - Decile 
  dataGraphs[thnetworth <= wealth_decs[[2]], wealth_decile := "1st Decile"] #0-10
  dataGraphs[thnetworth <= wealth_decs[[3]] & thnetworth > wealth_decs[[2]], wealth_decile := "2nd Decile"] #11-20
  dataGraphs[thnetworth <= wealth_decs[[4]] & thnetworth > wealth_decs[[3]], wealth_decile := "3rd Decile"] #21-30
  dataGraphs[thnetworth <= wealth_decs[[5]] & thnetworth > wealth_decs[[4]], wealth_decile := "4th Decile"] #31-40
  dataGraphs[thnetworth <= wealth_decs[[6]] & thnetworth > wealth_decs[[5]], wealth_decile := "5th Decile"] #41-50
  dataGraphs[thnetworth <= wealth_decs[[7]] & thnetworth > wealth_decs[[6]], wealth_decile := "6th Decile"] #51-60
  dataGraphs[thnetworth <= wealth_decs[[8]] & thnetworth > wealth_decs[[7]], wealth_decile := "7th Decile"] #61-70
  dataGraphs[thnetworth <= wealth_decs[[9]] & thnetworth > wealth_decs[[8]], wealth_decile := "8th Decile"] #71-80
  dataGraphs[thnetworth <= wealth_decs[[10]] & thnetworth > wealth_decs[[9]], wealth_decile := "9th Decile"] #81-90
  dataGraphs[thnetworth > wealth_decs[[10]], wealth_decile := "10th Decile"] #91-100
  
  # Resources 
  resources_quints <- wtd.quantile(dataGraphs$thhresources, weights = dataGraphs$wpfinwgt, probs = (0:5)/5)
  resources_decs <- wtd.quantile(dataGraphs$thhresources, weights = dataGraphs$wpfinwgt, probs = (0:10)/10)
  
  # Assignment - Quintile
  dataGraphs[thhresources <= resources_quints[[2]], resources_quintile := "1st Quintile"] #0-20
  dataGraphs[thhresources <= resources_quints[[3]] & thhresources > resources_quints[[2]], resources_quintile := "2nd Quintile"] #21-40
  dataGraphs[thhresources <= resources_quints[[4]] & thhresources > resources_quints[[3]], resources_quintile := "3rd Quintile"] #41-60
  dataGraphs[thhresources <= resources_quints[[5]] & thhresources > resources_quints[[4]], resources_quintile := "4th Quintile"] #61-80
  dataGraphs[thhresources > resources_quints[[5]], resources_quintile := "5th Quintile"] #81-100
  
  # Assignment - Decile 
  dataGraphs[thhresources <= resources_decs[[2]], resources_decile := "1st Decile"] #0-10
  dataGraphs[thhresources <= resources_decs[[3]] & thhresources > resources_decs[[2]], resources_decile := "2nd Decile"] #11-20
  dataGraphs[thhresources <= resources_decs[[4]] & thhresources > resources_decs[[3]], resources_decile := "3rd Decile"] #21-30
  dataGraphs[thhresources <= resources_decs[[5]] & thhresources > resources_decs[[4]], resources_decile := "4th Decile"] #31-40
  dataGraphs[thhresources <= resources_decs[[6]] & thhresources > resources_decs[[5]], resources_decile := "5th Decile"] #41-50
  dataGraphs[thhresources <= resources_decs[[7]] & thhresources > resources_decs[[6]], resources_decile := "6th Decile"] #51-60
  dataGraphs[thhresources <= resources_decs[[8]] & thhresources > resources_decs[[7]], resources_decile := "7th Decile"] #61-70
  dataGraphs[thhresources <= resources_decs[[9]] & thhresources > resources_decs[[8]], resources_decile := "8th Decile"] #71-80
  dataGraphs[thhresources <= resources_decs[[10]] & thhresources > resources_decs[[9]], resources_decile := "9th Decile"] #81-90
  dataGraphs[thhresources > resources_decs[[10]], resources_decile := "10th Decile"] #91-100
  
  # Household Income
  thinc_quints <- wtd.quantile(dataGraphs$thtotinc_annual, weights = dataGraphs$wpfinwgt, probs = (0:5)/5)
  thinc_decs <- wtd.quantile(dataGraphs$thtotinc_annual, weights = dataGraphs$wpfinwgt, probs = (0:10)/10)
  
  # Assignment - Quintile 
  dataGraphs[thtotinc_annual <= thinc_quints[[2]], thinc_quintile := "1st Quintile"] #0-20
  dataGraphs[thtotinc_annual <= thinc_quints[[3]] & thtotinc_annual > thinc_quints[[2]], thinc_quintile := "2nd Quintile"] #21-40
  dataGraphs[thtotinc_annual <= thinc_quints[[4]] & thtotinc_annual > thinc_quints[[3]], thinc_quintile := "3rd Quintile"] #41-60
  dataGraphs[thtotinc_annual <= thinc_quints[[5]] & thtotinc_annual > thinc_quints[[4]], thinc_quintile := "4th Quintile"] #61-80
  dataGraphs[thtotinc_annual > thinc_quints[[5]], thinc_quintile := "5th Quintile"] #81-100
  
  # Assignment - Decile 
  dataGraphs[thtotinc_annual <= thinc_decs[[2]], thinc_decile := "1st Decile"] #0-10
  dataGraphs[thtotinc_annual <= thinc_decs[[3]] & thtotinc_annual > thinc_decs[[2]], thinc_decile := "2nd Decile"] #11-20
  dataGraphs[thtotinc_annual <= thinc_decs[[4]] & thtotinc_annual > thinc_decs[[3]], thinc_decile := "3rd Decile"] #21-30
  dataGraphs[thtotinc_annual <= thinc_decs[[5]] & thtotinc_annual > thinc_decs[[4]], thinc_decile := "4th Decile"] #31-40
  dataGraphs[thtotinc_annual <= thinc_decs[[6]] & thtotinc_annual > thinc_decs[[5]], thinc_decile := "5th Decile"] #41-50
  dataGraphs[thtotinc_annual <= thinc_decs[[7]] & thtotinc_annual > thinc_decs[[6]], thinc_decile := "6th Decile"] #51-60
  dataGraphs[thtotinc_annual <= thinc_decs[[8]] & thtotinc_annual > thinc_decs[[7]], thinc_decile := "7th Decile"] #61-70
  dataGraphs[thtotinc_annual <= thinc_decs[[9]] & thtotinc_annual > thinc_decs[[8]], thinc_decile := "8th Decile"] #71-80
  dataGraphs[thtotinc_annual <= thinc_decs[[10]] & thtotinc_annual > thinc_decs[[9]], thinc_decile := "9th Decile"] #81-90
  dataGraphs[thtotinc_annual > thinc_decs[[10]], thinc_decile := "10th Decile"] #91-100
}

dataGraphs$wealth_quintile <- factor(dataGraphs$wealth_quintile, 
                                     levels = c("1st Quintile", "2nd Quintile", 
                                                "3rd Quintile", "4th Quintile", 
                                                "5th Quintile"))

dataGraphs$wealth_decile <- factor(dataGraphs$wealth_decile, 
                                   levels = c("1st Decile", "2nd Decile", "3rd Decile", 
                                              "4th Decile", "5th Decile", "6th Decile", 
                                              "7th Decile", "8th Decile", "9th Decile", 
                                              "10th Decile"))

dataGraphs$resources_decile <- factor(dataGraphs$resources_decile, 
                                      levels = c("1st Decile", "2nd Decile", "3rd Decile", 
                                                 "4th Decile", "5th Decile", "6th Decile", 
                                                 "7th Decile", "8th Decile", "9th Decile", 
                                                 "10th Decile"))

dataGraphs$resources_quintile <- factor(dataGraphs$resources_quintile, 
                                        levels = c("1st Quintile", "2nd Quintile", 
                                                   "3rd Quintile", "4th Quintile", 
                                                   "5th Quintile"))

dataGraphs$thinc_decile <- factor(dataGraphs$thinc_decile, 
                                  levels = c("1st Decile", "2nd Decile", "3rd Decile", 
                                             "4th Decile", "5th Decile", "6th Decile", 
                                             "7th Decile", "8th Decile", "9th Decile", 
                                             "10th Decile"))

dataGraphs$thinc_quintile <- factor(dataGraphs$thinc_quintile, 
                                    levels = c("1st Quintile", "2nd Quintile", 
                                               "3rd Quintile", "4th Quintile", 
                                               "5th Quintile"))

# EITC Value Imputation ---------------------------------------------------
  dataGraphs <- dataGraphs %>% 
  group_by(ssuid) %>% 
  mutate(
    eitc_value = case_when(
      #     
      eeitc == 1 & thhearn_annual <= 0 & thinc_ast > 3450 ~ 0, 
      #     
      #     # Equation: k = slope. k_in = phase in slope, k_out = phase out slope, x1 = min income for max credit, x2 = phase out start, x3 = phase out final
      #     
      #     # 0 qualifying children, k_in = 7.65%, k_out = 7.65%, x1 = 6610, x2 = 8270, x3 = 14880
      eeitc == 1 & efstatus != 2 & rhnumu18 == 0 & thhearn_annual > 0 & thhearn_annual < 6610 & thinc_ast <= 3450  ~ min((7.65/100)*thhearn_annual, 506),
      eeitc == 1 & efstatus != 2 & rhnumu18 == 0 & thhearn_annual > 0 & 6610 <= thhearn_annual & thhearn_annual < 8270 & thinc_ast <= 3450 ~ 506,
      eeitc == 1 & efstatus != 2 & rhnumu18 == 0 & thhearn_annual > 0 & 8270 <= thhearn_annual & thhearn_annual <= 14880 & thinc_ast <= 3450 ~ max((506-(7.65/100)*(thhearn_annual-8270)), 0),
      #     
      #     # 1 qualifying child, k_in = 34%, k_out = 15.98%, x1 = 9920, x2 = 18190, x3 = 39296
      eeitc == 1 & efstatus != 2 & rhnumu18 == 1 & thhearn_annual > 0 & thhearn_annual < 9920 & thinc_ast <= 3450  ~ min((34/100)*thhearn_annual, 3373), 
      eeitc == 1 & efstatus != 2 & rhnumu18 == 1 & thhearn_annual > 0 & 9920 <= thhearn_annual & thhearn_annual < 18190 & thinc_ast <= 3450 ~ 3373,
      eeitc == 1 & efstatus != 2 & rhnumu18 == 1 & thhearn_annual > 0 & 18190 <= thhearn_annual & thhearn_annual <= 39296 & thinc_ast <= 3450 ~ max((3373-(15.98/100)*(thhearn_annual-18190)), 0),
      #     
      #     # 2 qualifying children, k_in = 40%, k_out = 21.06%, x1 = 13930, x2 = 18190, x3 = 44648
      eeitc == 1 & efstatus != 2 & rhnumu18 == 2 & thhearn_annual > 0 & thhearn_annual < 13930 & thinc_ast <= 3450  ~ min((40/100)*thhearn_annual, 5572), 
      eeitc == 1 & efstatus != 2 & rhnumu18 == 2 & thhearn_annual > 0 & 13930 <= thhearn_annual & thhearn_annual < 18190 & thinc_ast <= 3450 ~ 5572,
      eeitc == 1 & efstatus != 2 & rhnumu18 == 2 & thhearn_annual > 0 & 18190 <= thhearn_annual & thhearn_annual <= 44648 & thinc_ast <= 3450 ~ max((5572 - (21.06/100)*(thhearn_annual-18190)), 0),
      #     
      #     # 3+ qualifying children, k_in = 45%, k_out = 21.06%, x1 = 13930, x2 = 18190, x3 = 47955
      eeitc == 1 & efstatus != 2 & rhnumu18 >= 3 & thhearn_annual > 0 & thhearn_annual < 13930 & thinc_ast <= 3450  ~ min((45/100)*thhearn_annual, 6269), 
      eeitc == 1 & efstatus != 2 & rhnumu18 >= 3 & thhearn_annual > 0 & 13930 <= thhearn_annual & thhearn_annual < 18190 & thinc_ast <= 3450 ~ 6269,
      eeitc == 1 & efstatus != 2 & rhnumu18 >= 3 & thhearn_annual > 0 & 18190 <= thhearn_annual & thhearn_annual <= 47955 & thinc_ast <= 3450 ~ max((6269 - (21.06/100)*(thhearn_annual-18190)), 0),
      #     
      #     
      #     # efstatus == 2 indicates married filing jointly. This results in an increase in maximum AGI and in x2 and x3 by $5550
      #     
      #     # 0 qualifying children, k_in = 7.65%, k_out = 7.65%, x1 = 6610, x2 = 8270, x3 = 20430
      eeitc == 1 & efstatus == 2 & rhnumu18 == 0 & thhearn_annual > 0 & thhearn_annual < 6610 & thinc_ast <= 3450  ~ min((7.65/100)*thhearn_annual, 506),
      eeitc == 1 & efstatus == 2 & rhnumu18 == 0 & thhearn_annual > 0 & 6610 <= thhearn_annual & thhearn_annual < 8270 & thinc_ast <= 3450 ~ 506,
      eeitc == 1 & efstatus == 2 & rhnumu18 == 0 & thhearn_annual > 0 & 13820 <= thhearn_annual & thhearn_annual <= 20430 & thinc_ast <= 3450 ~ max((506 - (7.65/100)*(thhearn_annual-13820)), 0),
      #     
      #     # 1 qualifying child, k_in = 34%, k_out = 15.98%, x1 = 9920, x2 = 18190, x3 = 44846
      eeitc == 1 & efstatus == 2 & rhnumu18 == 1 & thhearn_annual > 0 & thhearn_annual < 9920 & thinc_ast <= 3450  ~ min((34/100)*thhearn_annual, 3373), 
      eeitc == 1 & efstatus == 2 & rhnumu18 == 1 & thhearn_annual > 0 & 9920 <= thhearn_annual & thhearn_annual < 18190 & thinc_ast <= 3450 ~ 3373,
      eeitc == 1 & efstatus == 2 & rhnumu18 == 1 & thhearn_annual > 0 & 23740 <= thhearn_annual & thhearn_annual <= 44846 & thinc_ast <= 3450 ~ max((3373 - (15.98/100)*(thhearn_annual-23740)), 0),
      #     
      #     # 2 qualifying children, k_in = 40%, k_out = 21.06%, x1 = 13930, x2 = 18190, x3 = 50198
      eeitc == 1 & efstatus == 2 & rhnumu18 == 2 & thhearn_annual > 0 & thhearn_annual < 13930 & thinc_ast <= 3450  ~ min((40/100)*thhearn_annual, 5572), 
      eeitc == 1 & efstatus == 2 & rhnumu18 == 2 & thhearn_annual > 0 & 13930 <= thhearn_annual & thhearn_annual < 18190 & thinc_ast <= 3450 ~ 5572,
      eeitc == 1 & efstatus == 2 & rhnumu18 == 2 & thhearn_annual > 0 & 23740 <= thhearn_annual & thhearn_annual <= 50198 & thinc_ast <= 3450 ~ max((5572 - (21.06/100)*(thhearn_annual-23740)), 0),
      #     
      #     # 3+ qualifying children, k_in = 45%, k_out = 21.06%, x1 = 13930, x2 = 18190, x3 = 53505
      eeitc == 1 & efstatus == 2 & rhnumu18 >= 3 & thhearn_annual > 0 & thhearn_annual < 13930 & thinc_ast <= 3450  ~ min((45/100)*thhearn_annual, 6269), 
      eeitc == 1 & efstatus == 2 & rhnumu18 >= 3 & thhearn_annual > 0 & 13930 <= thhearn_annual & thhearn_annual < 18190 & thinc_ast <= 3450 ~ 6269,
      eeitc == 1 & efstatus == 2 & rhnumu18 >= 3 & thhearn_annual > 0 & 23740 <= thhearn_annual & thhearn_annual <= 53505 & thinc_ast <= 3450 ~ max((6269 - (21.06/100)*(thhearn_annual-23740)), 0),
      #     
      #     eeitc == 2 ~ 0, 
      TRUE ~ 0
    )
  )

# Calibration (Adjust values in AL_CJ_2022_Replication_Master.R for desired trim levels) ----------------
dataGraphs <- as.data.table(dataGraphs)
  # Trim by wealth
  dataGraphs <- dataGraphs[thnetworth <= quantile(dataGraphs$thnetworth, probs = wealth_trim)]
  # Trim by age
  dataGraphs <- dataGraphs[tage <= quantile(dataGraphs$tage, probs = age_trim)]
  # Trim by SSI Type
  if (ssi_trim) {
    dataGraphs <- dataGraphs[((tsnap_amt_annual_hh == 0 & tssi_amt_annual_hh != 0 & ttanf_amt_annual_hh == 0 & eitc_value == 0) & tage >= 65) 
                             | !(tsnap_amt_annual_hh == 0 & tssi_amt_annual_hh != 0 & ttanf_amt_annual_hh == 0 & eitc_value == 0)]
  }
  # Checking households with negative earnings
  if (nonneg_earnings) {
    dataGraphs <- dataGraphs[thhearn_annual >= 0]
  }

# Measuring Recipients ----------------------------------------------------
  # Creating 'total transfers' variable - in our defined concept of SNAP+TANF+SSI+EITC
  dataGraphs <- dataGraphs %>%
    mutate(thhtransfers_annual = tsnap_amt_annual_hh + tssi_amt_annual_hh + ttanf_amt_annual_hh + eitc_value)
  
  # Creating 'total transfers' variable - in our defined concept of SNAP+TANF+SSI without EITC
  dataGraphs <- dataGraphs %>%
    mutate(thhtransfers_annual_trim = tsnap_amt_annual_hh + tssi_amt_annual_hh + ttanf_amt_annual_hh)
  
  # Creating a dummy transfer recipient variable
  dataGraphs <- dataGraphs %>%
    mutate(recipient = ifelse(thhtransfers_annual > 0, 1, 0))
  
# Weighting ---------------------------------------------------------------
vars <- c("thhearn_annual", "tsnap_amt_annual_hh", "tssi_amt_annual_hh", 
          "ttanf_amt_annual_hh", "eitc_value", "thhtransfers_annual", 
          "thhtransfers_annual_trim", "thnetworth", "thhresources", "thtotinc_annual")
vars <- unique(c(vars, debt, assets))

# Weighted Means ----------------------------------------------------------
if (wgt_transforms) {
  dataGraphs_wth_q_mean <- dataGraphs[,lapply(.SD, wtd.mean, weights = wpfinwgt), 
                                      by = .(wealth_quintile, recipient), 
                                      .SDcols = vars]
  
  dataGraphs_wth_d_mean <- dataGraphs[,lapply(.SD, wtd.mean, weights = wpfinwgt), 
                                      by = .(wealth_decile, recipient), 
                                      .SDcols = vars]
  
  dataGraphs_res_q_mean <- dataGraphs[,lapply(.SD, wtd.mean, weights = wpfinwgt), 
                                      by = .(resources_quintile, recipient), 
                                      .SDcols = vars]
  
  dataGraphs_res_d_mean <- dataGraphs[,lapply(.SD, wtd.mean, weights = wpfinwgt), 
                                      by = .(resources_decile, recipient), 
                                      .SDcols = vars]
  
  dataGraphs_thinc_q_mean <- dataGraphs[,lapply(.SD, wtd.mean, weights = wpfinwgt), 
                                        by = .(thinc_quintile, recipient), 
                                        .SDcols = vars]
  
  dataGraphs_thinc_d_mean <- dataGraphs[,lapply(.SD, wtd.mean, weights = wpfinwgt), 
                                        by = .(thinc_decile, recipient), 
                                        .SDcols = vars]
}

if (!wgt_transforms) {
  dataGraphs_wth_q_mean <- dataGraphs[,lapply(.SD, mean), 
                                      by = .(wealth_quintile, recipient), 
                                      .SDcols = vars]
  
  dataGraphs_wth_d_mean <- dataGraphs[,lapply(.SD, mean), 
                                      by = .(wealth_decile, recipient), 
                                      .SDcols = vars]
  
  dataGraphs_res_q_mean <- dataGraphs[,lapply(.SD, mean), 
                                      by = .(resources_quintile, recipient), 
                                      .SDcols = vars]
  
  
  dataGraphs_res_d_mean <- dataGraphs[,lapply(.SD, mean), 
                                      by = .(resources_decile, recipient), 
                                      .SDcols = vars]
  
  
  dataGraphs_thinc_q_mean <- dataGraphs[,lapply(.SD, mean), 
                                        by = .(thinc_quintile, recipient), 
                                        .SDcols = vars]
  
  
  dataGraphs_thinc_d_mean <- dataGraphs[,lapply(.SD, mean), 
                                        by = .(thinc_decile, recipient), 
                                        .SDcols = vars]
}

dataGraphs_wth_q_mean <- arrange(dataGraphs_wth_q_mean, wealth_quintile) # ordered by wealth quintile
dataGraphs_wth_d_mean <- arrange(dataGraphs_wth_d_mean, wealth_decile)   # ordered by wealth decile
dataGraphs_res_q_mean <- arrange(dataGraphs_res_q_mean, resources_quintile) # ordered by resources quintile
dataGraphs_res_d_mean <- arrange(dataGraphs_res_d_mean, resources_decile)   # ordered by resources decile
dataGraphs_thinc_q_mean <- arrange(dataGraphs_thinc_q_mean, thinc_quintile) # ordered by income quintile
dataGraphs_thinc_d_mean <- arrange(dataGraphs_thinc_d_mean, thinc_decile)   # ordered by income decile

# Weighted Medians --------------------------------------------------------
if (wgt_transforms) {
  dataGraphs_wth_q_median <- dataGraphs[,lapply(.SD, wtd.quantile, weights = wpfinwgt, probs = 0.5), 
                                        by = .(wealth_quintile, recipient), 
                                        .SDcols = vars]
  
  dataGraphs_wth_d_median <- dataGraphs[,lapply(.SD, wtd.quantile, weights = wpfinwgt, probs = 0.5), 
                                        by = .(wealth_decile, recipient), 
                                        .SDcols = vars]
  
  dataGraphs_res_q_median <- dataGraphs[,lapply(.SD, wtd.quantile, weights = wpfinwgt, probs = 0.5), 
                                        by = .(resources_quintile, recipient), 
                                        .SDcols = vars]
  
  dataGraphs_res_d_median <- dataGraphs[,lapply(.SD, wtd.quantile, weights = wpfinwgt, probs = 0.5), 
                                        by = .(resources_decile, recipient), 
                                        .SDcols = vars]
  
  dataGraphs_thinc_q_median <- dataGraphs[,lapply(.SD, wtd.quantile, weights = wpfinwgt, probs = 0.5), 
                                          by = .(thinc_quintile, recipient), 
                                          .SDcols = vars]
  
  dataGraphs_thinc_d_median <- dataGraphs[,lapply(.SD, wtd.quantile, weights = wpfinwgt, probs = 0.5), 
                                          by = .(thinc_decile, recipient), 
                                          .SDcols = vars]
}

if (!wgt_transforms) {
  dataGraphs_wth_q_median <- dataGraphs[,lapply(.SD, quantile, probs = 0.5, na.rm = T), 
                                        by = .(wealth_quintile, recipient), 
                                        .SDcols = vars]
  
  dataGraphs_wth_d_median <- dataGraphs[,lapply(.SD, quantile, probs = 0.5, na.rm = T), 
                                        by = .(wealth_decile, recipient), 
                                        .SDcols = vars]
  
  dataGraphs_res_q_median <- dataGraphs[,lapply(.SD, quantile, probs = 0.5, na.rm = T), 
                                        by = .(resources_quintile, recipient), 
                                        .SDcols = vars]
  
  dataGraphs_res_d_median <- dataGraphs[,lapply(.SD, quantile, probs = 0.5, na.rm = T), 
                                        by = .(resources_decile, recipient), 
                                        .SDcols = vars]
  
  dataGraphs_thinc_q_median <- dataGraphs[,lapply(.SD, quantile, probs = 0.5, na.rm = T), 
                                          by = .(thinc_quintile, recipient), 
                                          .SDcols = vars]
  
  dataGraphs_thinc_d_median <- dataGraphs[,lapply(.SD, quantile, probs = 0.5, na.rm = T), 
                                          by = .(thinc_decile, recipient), 
                                          .SDcols = vars]
  
}

dataGraphs_wth_q_median <- arrange(dataGraphs_wth_q_median, wealth_quintile) # ordered by wealth quintile
dataGraphs_wth_d_median <- arrange(dataGraphs_wth_d_median, wealth_decile)   # ordered by wealth decile
dataGraphs_res_q_median <- arrange(dataGraphs_res_q_median, resources_quintile) # ordered by resources quintile
dataGraphs_res_d_median <- arrange(dataGraphs_res_d_median, resources_decile)   # ordered by resources decile
dataGraphs_thinc_q_median <- arrange(dataGraphs_thinc_q_median, thinc_quintile) # ordered by income quintile
dataGraphs_thinc_d_median <- arrange(dataGraphs_thinc_d_median, thinc_decile)   # ordered by income decile