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



The following files were created in these 8 scripts above:

bps_stats_all.csv: this is the results of fires of all sizes by biophysical setting or vegetation class; fields in this dataset include: GROUPVEG= biophycial setting or vegetation class; hnobsall= number of human fires of all sizes; lnobsall= number of lightning fires of all sizes; ratio= the ratio of human:lightning fires of all sizes by vegetation class

bps_stats_lrg.csv: this is the results of large fires by biophysical setting or vegetation class; fields in this dataset include: GROUPVEG= biophycial setting or vegetation class; hnobslrg= number of large human fires; lnobslrg= number of large lightning fires; ratio= the ratio of large human:lightning fires by vegetation class

fm_ws_monthly_ecn.csv: this is the mean monthly wind speed and fuel moisture by ecoregion; fields in this dataset include: NA_L3CODE= the level 3 ecoregion code from the CEC; fm.mean= mean monthly 100-h dead fuel moisture (%); Wind.mean= mean monthly wind speed (m/s)

diff_fm_NA_L3CODE_10_monthly.csv: this is calculating the difference in mean monthly fuel moisture between large human and lightning-caused fires within an ecoregion; fields in this dataset include: NA_L3CODE= the level 3 ecoregion code from the CEC; Human= mean monthly 100-h dead fuel moisture (%) for large human-caused fires; Lightning= mean monthly 100-h dead fuel moisture (%) for large lightning-caused fires; diff_fm10= the difference in fuel moisture between large human minus large lightning-caused fires within an ecoregion; NA_L3NAME= the level 3 ecoregion name from CEC

diff_wind_NA_L3CODE_10_monthly.csv: this is calculating the difference in mean monthly wind speed between large human and lightning-caused fires within an ecoregion; fields in this dataset include: NA_L3CODE= the level 3 ecoregion code from the CEC; Human= mean monthly wind speed (m/s) for large human-caused fires; Lightning= mean monthly wind speed (m/s) for large lightning-caused fires; diff_windspeed= the difference in wind speed between large human minus large lightning-caused fires within an ecoregion; NA_L3NAME= the level 3 ecoregion name from CEC

firehasum_ecn_top_ten_Short_update.csv: this file includes summary statistics for the large fires by ecoregion; fields in this dataset include: NA_L3NAME= the level 3 ecoregion code from the CEC; FIRE_SIZE_ha.length= number of large fires by ecoregion; FIRE_SIZE_ha.mean= mean large fire size by ecoregion; FIRE_SIZE_ha.sd= standard deviation of large fire size by ecoregion; FIRE_SIZE_ha.median= median large fire size by ecoregion; FIRE_SIZE_ha.min= minimum large fire size by ecoregion; FIRE_SIZE_ha.max= maximum large fire size by ecoregion; NA_L1NAME= the level 1 ecoregion name from the CEC; NA_L3CODE= the level 3 ecoregion code from the CEC

firestats_ecn_top_ten_Short_update_hl.csv

burned_area_hl.csv

meddoyhl.csv

non_light_season.csv

seasonal_corr_human_only.csv

perh_large20.csv

perh_large5.csv
