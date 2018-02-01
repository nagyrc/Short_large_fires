This code was prepared by Dr. R. Chelsea Nagy for the article "Human-related ignitions increase the number of large wildfires across U.S. ecoregions" by R. Chelsea Nagy, Emily Fusco, Bethany Bradley, John T. Abatzoglou, and Jennifer Balch. This article was accepted for publication in the journal Fire on 22 January 2018.

This project consists of several R scripts used on various pieces of the analyses. In this repository, you will find the following R scripts:

1_create_folders: this creates some folders to store raw data and derived results
2_get_data: this pulls some of the datasources and puts them into the folders
3_creating_monthly_wind_fm: this creates the monthly averages of wind speed and fuel moisture across the 23 year record from 1992-2015
4_Short_preprocessing: this is where the new Short data is pulled in and merged with other data (biomass, biophysical setting (vegetation class), wind speed, fuel moisture, and ecoregions) needed for the project and subset to the large fires.
5_veg: this includes the analyses of large fires with biomass and biophysical setting
6_extract_fm_wind_monthly: this includes the analyses of large fires with mean monthly wind speed and mean monthly fuel moisture
7_script-shortyseasons_lev3: this includes the analysis of seasonality of large fires
8_sensitivity_analysis: this includes analyses of large fires as defined as the 5% and 20% of largest fires by ecoregion (as opposed to the 10% threshold used in all other analyses here)

---
Contact info for Dr. Nagy: nagyrc@gmail.com; chelsea.nagy@colorado.edu Earth Lab, University of Colorado, Boulder

The data can be downloaded here: https://zenodo.org/record/_____ . All of the code can be found at https://github.com/nagyrc/Short_large_fires .

---

The following data files were created in these 8 scripts above:

bps_stats_all.csv: this is the results of fires of all sizes by biophysical setting or vegetation class; fields in this dataset include: GROUPVEG= biophycial setting or vegetation class; hnobsall= number of human fires of all sizes; lnobsall= number of lightning fires of all sizes; ratio= the ratio of human:lightning fires of all sizes by vegetation class

bps_stats_lrg.csv: this is the results of large fires by biophysical setting or vegetation class; fields in this dataset include: GROUPVEG= biophycial setting or vegetation class; hnobslrg= number of large human fires; lnobslrg= number of large lightning fires; ratio= the ratio of large human:lightning fires by vegetation class

fm_ws_monthly_ecn.csv: this is the mean monthly wind speed and fuel moisture by ecoregion; fields in this dataset include: NA_L3CODE= the level 3 ecoregion code from the CEC; fm.mean= mean monthly 100-h dead fuel moisture (%); Wind.mean= mean monthly wind speed (m/s)

diff_fm_NA_L3CODE_10_monthly.csv: this is calculating the difference in mean monthly fuel moisture between large human and lightning-caused fires within an ecoregion; fields in this dataset include: NA_L3CODE= the level 3 ecoregion code from the CEC; Human= mean monthly 100-h dead fuel moisture (%) for large human-caused fires; Lightning= mean monthly 100-h dead fuel moisture (%) for large lightning-caused fires; diff_fm10= the difference in fuel moisture between large human minus large lightning-caused fires within an ecoregion; NA_L3NAME= the level 3 ecoregion name from CEC

diff_wind_NA_L3CODE_10_monthly.csv: this is calculating the difference in mean monthly wind speed between large human and lightning-caused fires within an ecoregion; fields in this dataset include: NA_L3CODE= the level 3 ecoregion code from the CEC; Human= mean monthly wind speed (m/s) for large human-caused fires; Lightning= mean monthly wind speed (m/s) for large lightning-caused fires; diff_windspeed= the difference in wind speed between large human minus large lightning-caused fires within an ecoregion; NA_L3NAME= the level 3 ecoregion name from CEC

firehasum_ecn_top_ten_Short_update.csv: this file includes summary statistics for the large fires by ecoregion; fields in this dataset include: NA_L3NAME= the level 3 ecoregion code from the CEC; FIRE_SIZE_ha.length= number of large fires by ecoregion; FIRE_SIZE_ha.mean= mean large fire size by ecoregion; FIRE_SIZE_ha.sd= standard deviation of large fire size by ecoregion; FIRE_SIZE_ha.median= median large fire size by ecoregion; FIRE_SIZE_ha.min= minimum large fire size by ecoregion; FIRE_SIZE_ha.max= maximum large fire size by ecoregion; NA_L1NAME= the level 1 ecoregion name from the CEC; NA_L3CODE= the level 3 ecoregion code from the CEC

firestats_ecn_top_ten_Short_update_hl.csv: this file includes summary statistics for large human- and lightning-caused fires by ecoregion; fields in this dataset include: NA_L3NAME= the level 3 ecoregion code from the CEC; hnobs= number of large human-caused fires; lnobs= number of large lightning-caused fires; hameanh= mean fire size of large human-caused fires; hameanl= mean fire size of large lightning-cuased fires; hamedh= median fire size of large human-caused fires; hamedl= median fire size of large lightning-caused fires; totfires= total number of large fires by ecoregion; perh= percent of human-caused fires by ecoregion; ecn= ecoregion code (abbreviation of NA_L3CODE)

burned_area_hl.csv: this file calculates the total area burned of large human and lightning fires by ecoregion; fields in this dataset include: NA_L3NAME= the level 3 ecoregion code from the CEC; FIRE_SIZE_ha.sum.Human= total burned area (ha) of large human-caused fires; FIRE_SIZE_ha.sum.Lightning= total burned area (ha) of large lightning-caused fires

meddoyhl.csv: this file reports the median day of year for large human- and lightning-caused fires by ecoregion; fields in this dataset include: NA_L3NAME= the level 3 ecoregion code from the CEC; DISCOVERY_DOY.median.Human= the median discovery day of year for large human-caused fires; DISCOVERY_DOY.median.Lightning= the median discovery day of year for large lightning-caused fires; diff= the difference in the median discovery day of year between large human- and lightning-caused fires

non_light_season.csv: this reports the number of large fires that fell outside of the lightning-fire season; fields in this dataset include: NA_L3CODE= the level 3 ecoregion code from the CEC; clean_id.length= the number of large fires that occurred outside of the lightning-fire season by ecoregion

seasonal_corr_human_only.csv: this datasheet contains the results from the correlation analysis between the number of large fires by day of year and the number of fires of all sizes by day of year for the human-caused fires only; fields include: corr= the correlation coefficient; NA_L3CODE= the level 3 ecoregion code from the CEC

perh_large20.csv: this file contains summary statistics from large fires as defined by the largest 20% of fires by ecoregion; fields in this dataset include: NA_L3CODE= the level 3 ecoregion code from the CEC; hnobs= the number of large human-caused fires; lnobs= the number of large lightning-caused fires; totfires= the total number of large fires by ecoregion; perh20= the percent of large human-caused fires by ecoregion

perh_large5.csv: this file contains summary statistics from large fires as defined by the largest 5% of fires by ecoregion; fields in this dataset include: NA_L3CODE= the level 3 ecoregion code from the CEC; hnobs= the number of large human-caused fires; lnobs= the number of large lightning-caused fires; totfires= the total number of large fires by ecoregion; perh5= the percent of large human-caused fires by ecoregion

---

Original data came from:

Commission for Environmental Cooperation. Ecological regions of north america – levels i, ii, and iii: Montreal, quebec, canada, scale 1:10,000,000; 2006. Available on: https://www.epa.gov/eco-research/ecoregions-north-america (access on 25 January 2018).

Short, K.C. Spatial wildfire occurence data for the united states, 1992-2015 [fpa_fod_20170508]. In Forest Service Research Data Archive, 4th Edition ed.; Fort Collins, CO, 2017 Available on: https://www.fs.usda.gov/rds/archive/Product/RDS-2013-0009.4/ (access on 25 January 2018).

Abatzoglou, J.T. Development of gridded surface meteorological data for ecological applications and modelling. International Journal of Climatology 2013, 33, 121-131.

USGS EROS. Landfire.Hi_130bps. Landfire biophysical settings (landfire 2012). LF 1.3.0 ed.; 2010. Available on: http://www.landfire.gov (access on 25 January 2018).

Kellndorfer, J.; Walker, W.; LaPoint, E.; Bishop, J.; Cormier, T.; Fiske, G.; Hoppus, M.; Kirsch, K.; Westfall, J. Nacp aboveground biomass and carbon baseline data (nbcd 2000), u.S.A., 2000. DAAC, O., Ed. Oak Ridge, TN, 2012. Available on: https://daac.ornl.gov/NACP/guides/NBCD_2000.html (access on 25 January 2018).