# -------------------------------------------------------------------------
# Title: Means-Tested Transfers, Asset Limits, and Universal Basic Income
# Purpose: Replication codes for FRBC's Economic Commentary
# File: Master script for running replication package
# Authors: Andr?  Victor D. Luduvice and Cornelius Johnson
# Cleveland, July 20th, 2022
# -------------------------------------------------------------------------


# Startup -----------------------------------------------------------------
library(data.table) # used for general data manipulation and transformations
library(tidyverse)  # used for general data manipulation and transformations
library(readxl)     # required to read in CPI inflation data
library(Hmisc)      # required to weight quantiles and weight transforms
library(vtable)     # required to generate summary table

# Directory and Calibration -----------------------------------------------
setwd("filepath/LuduviceJohnson_MTUBI-main")   # add filepath to directory

income_trim <- 0.999  # choice of trimming of sample on household income dimension
wealth_trim <- 0.95   # choice of trimming of sample on net worth dimension
age_trim <- 1         # choice of trimming of sample on age income dimension
ssi_trim <- T         # choice of trimming of sample on SSI dimension
wgt_quants <- T       # choice for using sample weights on quantiles computation
nonneg_earnings <- T  # choice for only keeping households with non negative earnings
wgt_transforms <- T   # choice of sample weighting


# creating appropriate path to paste figures
Chart_Path <- paste0(getwd(), "/Charts/")
dir.create(paste0(getwd(), Chart_Path))

# Data Input --------------------------------------------------------------

# identifiers
id_dem <- c("shhadid", "spanel", "ssuid", "swave", "pnum", "monthcode", "wpfinwgt", "rfamref", "erelrpe")
# demographic characteristics
id_dem <- c(id_dem, "tage", "tage_ehc", "tdob_byear", "esex", "erace", "eeduc", "rged", "tceb")
# number of people and children
id_dem <- c(id_dem, "rhnumper", "rhnumu18")
# income and earnings 
earnings_inc <- c("thtotinc", "thinc_ast", "thincpov", "thinc_bank", "thinc_bond", "thinc_oth", "thinc_rent", "thinc_stmf", "tptotinc", "tpearn")
# cash transfers
cash_trnf <- c("tptrninc", "tga_amt", "tsnap_amt", "tssi_amt", "ttanf_amt", "twic_amt")
# eitc
eeitc_sts <- c("eeitc", "efstatus")
# asset values
assets <- c("thnetworth", "thval_ast", "thval_bank", "thval_bond", "thval_bus", 
            "thval_esav", "thval_home", "thval_oth", "thval_re", "thval_rent")
assets <- c(assets, "thval_ret", "thval_rmu", "thval_stmf", "thval_veh")
# debt values
debt <- c("thdebt_ast", "thdebt_bus", "thdebt_cc", "thdebt_ed", "thdebt_home", 
          "thdebt_ot", "thdebt_re", "thdebt_rent", "thdebt_sec", "thdebt_usec", 
          "thdebt_veh", "tmed_amt")
# equity values
equity <- c("theq_bus", "theq_home", "theq_re", "theq_rent", "theq_veh")

# reading data from assigned directory
# download data at: https://www2.census.gov/programs-surveys/sipp/data/datasets/2018/pu2018_csv.zip
pu2018 <- fread(paste0(getwd(),"/Data/pu2018.csv"), select = c(id_dem, earnings_inc, cash_trnf, eeitc_sts, assets, debt, equity))

# a few type adjustments
pu2018$ssuid <- as.character(pu2018$ssuid)
pu2018$shhadid <- as.character(pu2018$shhadid)
pu2018$spanel <- as.character(pu2018$spanel)
pu2018$eeduc <- as.numeric(pu2018$eeduc)
pu2018$theq_veh <- as.numeric(pu2018$theq_veh)

# Data Transformations ----------------------------------------------------
source(paste0(getwd(),"/Scripts/AL_CJ_2022_Replication_Transformations.R"), local = TRUE)

# Figures 1 - 4 -----------------------------------------------------------
source(paste0(getwd(),"/Scripts/AL_CJ_2022_Replication_Figures.R"), local = TRUE)

# Tables A1-A2 and Figures A1-A2.2 ------------------------------------------
source(paste0(getwd(),"/Scripts/AL_CJ_2022_Replication_Appendix.R"), local = TRUE)

# Figure A3 ---------------------------------------------------------------
# Since this figure is based upon a different calibration, it requires a new script
source(paste0(getwd(),"/Scripts/AL_CJ_2022_Replication_Appendix_A3.R"), local = TRUE)
