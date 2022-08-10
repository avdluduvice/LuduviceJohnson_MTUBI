# LuduviceJohnson_MTUBI

## Overview

This repository contains all code and data associated with the FRBC's Economic Commentary "Means-Tested Transfers, Asset Limits, and Universal Basic Income" by André Victor D. Luduvice and Cornelius Johnson.

The data and code in this replication package constructs the data transformations and calculations from two data sources (U.S. Census SIPP, BLS CPI-U) using R. The Master script and Transformation script run the replication package and implement all data transformation and calculations required for the analysis. The Figures and  Appendix sripts generate the figures and tables. The Table1_Documentation folder includes documentation for Table 1 of the Commentary, including PDFs for static access to resources accessed from URLs.

## Recommended Citation

Luduvice, André Victor D., and Cornelius Johnson. 2022. "Means-Tested Transfers, Asset Limits, and Universal Basic Income." Economic Commentary, no. 2022-10 (August). https://doi.org/10.26509/frbc-ec-202210.


## Contents

- Charts and Tables:
  - These folders will be automatically generated by the R code and will house the output figures and tables.

- Data:
  - `BLS_CPI-U_1947_2021.csv`: CPI-U index to be merged with the SIPP 2018 data. 
  - `pu2018.csv`: first wave of the SIPP 2018. Raw data used in the analysis.    

- Scripts:
  - `AL_CJ_2022_Replication_Master.R`: master script for running the replication package..
  - `AL_CJ_2022_Repliation_Transformation.R`: script to implement all data transformation and calculations required for the analysis.
  - `AL_CJ_2022_Replication_Figures.R`: script to generate Figures 1 to 4.
  - `AL_CJ_2022_Replication_Appendix.R`: script to generate Tables A.1 and A.2 and Figures A.1 and A.2 of the appendix.
  - `AL_CJ_2022_Replication_Appendix_A3.R`: script to generate Figure A.3 of the appendix.
  
- Table1_Documentation:
  - `Documentation_Table1.pdf`: step-by-step explanation of calculations in Table 1.
  - `Annual_Statistical_Supplement_2017.pdf`: main reference for TANF numbers.
  - `Earned_Income_and_Earned_Income_Tax_Credit_(EITC)_Tables_Internal_Revenue_Service.pdf`: main reference for EITC numbers.
  - `OASDI_and_SSI_Program_Rates_&_Limits_2017.pdf`: main reference for SSI numbers.
  - `SNAP_Fiscal_Year_2017_Cost_Of_Living_Adjustments.pdf`: main reference for SNAP numbers.

## Data Availability

All data are publicly available.

This paper uses data from the U.S. Census Bureau, Survey of Income and Program Participation (SIPP) 2018. The raw data is from the 2018 the first wave of SIPP. Data can be freely downloaded from https://www2.census.gov/programs-surveys/sipp/data/datasets/2018/pu2018_csv.zip. Note: the SIPP data 
file is not provided. Save the file in the directory as /Data/pu2018.csv.

This paper uses raw data from the U.S. Bureau of Labor Statistics. The raw data is the seasonally adjusted CPI-U index, series ID: CUSR0000SA0. Data can be freely downloaded from https://data.bls.gov/timeseries/CUSR0000SA0&output. Change output options to include years 1947 - 2021, and the "Original Data Value" view of the data. Note: the raw CPI-U index data file is provided in the Data folder as BLS_CPI-U_1947_2021.csv and was extracted on Sep 24, 2021. 


## License

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[cc-by]: http://creativecommons.org/licenses/by/4.0/

