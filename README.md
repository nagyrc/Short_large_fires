This code was prepared by Dr. R. Chelsea Nagy for the article "Human-related ignitions increase the number of large wildfires across U.S. ecoregions" by R. Chelsea Nagy, Emily Fusco, Bethany Bradley, John T. Abatzoglou, and Jennifer Balch.  This article was accepted for publication in the journal Fire on 22 January 2018.  

This project consists of several R scripts used on various pieces of the analyses.  In this repository, you will find the following R scripts:
1) 1_create_folders: this creates some folders to store raw data and derived results
2) 2_get_data: this pulls some of the datasources and puts them into the folders
3) 3_creating_monthly_wind_fm: this creates the monthly averages of wind speed and fuel moisture across the 23 year record from 1992-2015
4) 4_Short_preprocessing: this is where the new Short data is pulled in and merged with other data (biomass, biophysical setting (vegetation class), wind speed, fuel moisture, and ecoregions) needed for the project and subset to the large fires.
5) 5_veg: this includes the analyses of large fires with biomass and biophysical setting
6) 6_extract_fm_wind_monthly: this includes the analyses of large fires with mean monthly wind speed and mean monthly fuel moisture
7) 7_script-shortyseasons_lev3: this includes the analysis of seasonality of large fires
8) 8_sensitivity_analysis: this includes analyses of large fires as defined as the 5% and 20% of largest fires by ecoregion (as opposed to the 10% threshold used in all other analyses here)


Contact info for Dr. Nagy:
nagyrc@gmail.com; chelsea.nagy@colorado.edu
Earth Lab, University of Colorado, Boulder

The data can be downloaded here: https://zenodo.org/record/_____ . All of the code can be found at https://github.com/nagyrc/Short_large_fires . 

