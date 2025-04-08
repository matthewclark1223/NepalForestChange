# R code and data repository for 'Community Management and Wellbeing Increase the Resilience of Nepali Forests to Climate Change'

The code and data are provided here to allow readers to reproduce the analyses and figures in the manuscript. 

There are four categories of scripts used in this paper: 1) Data acquisition and cleaning (including precipitation anomaly calculation); 2) Spatial processes for creating and summarizing loss/gap patches; 3) Statistical analyses; and 4) Ancillary scripts for various supplemental analyses and figures. 

## Scripts:
### Data acquisition and cleaning
* MatchFUG_Shp.R - Use fuzzy matching to match VDC names from the GADM database to the sheet provided by ForestAction Nepal
* Baseline_VDC_Precip.R - Use the Google Earth Engine connection with R to pull baseline (1980s) precipitation data
* GEE_VDC_Precip.R - Use the Google Earth Engine connection with R to pull VDC precipitation data for the study period
* Deprivation.R - Calculate deprivation summary statistics by VDC
* Migration.R - Calculate migration summary statistics by VDC
* Population.R - Calculate population summary statistics by VDC
* RoadDensity.R - Calculate road density summary statistics by VDC
* Slope.R - Calculate slope summary statistics by VDC
* MergePoltCovariates.R - Merge and plot all covariates by vdc
* FullSummaryDataset.R - Compile full summary dataset
### Spatial processes for creating and summarizing loss/gap patches
* LossDistToEdgeLoop.R - Make loss patches and calculate their size and distance to forest edge using a loop
* GapsDistanceToEdgeLoop.R - Make loss patches and calculate their size and distance to forest edge using a loop
* GapsDistanceToEdgeParallel.R - Redundant with 'GapsDistanceToEdgeLoop.R' but much faster
* GapsDistancesCheckComplete.R - Double check that all of the VDCs are included in the Gaps data. Useful if the loops in the above scripts get interrupted.
* LossDistancesSizesSummary.R - Summary stats and figures for loss patch observations
* GapsDistancesSizesSummary.R - Summary stats and figures for gap patch observations
### Statistical analyses
* StatisticalModelZeroInflated.R - Code for running and plotting the output of the statistical models assessing drivers of forest loss and gaps
* SummaryStatsTable.R - Code for creating summary stats of forest change inside VDCs with and without community forestry (shown in table 1 of the manuscript)
* ModelCompare.R - Code for sensitivity checks on regression models
* ConditionalExpectationsStatistics.R - Code for calculating the expected values (and associated uncertainty) of model outputs given changes in the certain predictors
### Ancillary scripts
* DagCode.R - Code for making DAG figures. Also includes 'daggity' style code for use on https://www.dagitty.net/
* Equations.rmd - R markdown document for making the equations shown in the paper using Latex
* StatsFigures - Some figures shown in the supplemental

## Data:
* BaselineVDCPrecipitation.csv - Daily precipitation for each VDC 1981 - 1990. THIS FILE IS TOO LARGE FOR GITHUB CREATE YOURELF WITH THE PROVIDED CODE OR CONTACT THE AUTHOR FOR ACCESS
* VDCPrecipitation.csv - Daily precipitation for each VDC 2018 - 2023. THIS FILE IS TOO LARGE FOR GITHUB CREATE YOURELF WITH THE PROVIDED CODE OR CONTACT THE AUTHOR FOR ACCESS
* Nepal_NationalParks.shp - All IUCN category II protected areas in Nepal. Compiled from the World Protected Areas Database.
* gadm41_NPL_4.shp - Administrative boundaries for Nepal
* FUG_Final.csv - Forest user groups from ForestActionNepal
* Nepal_fug.shp - Forest user groups matched to GADM administrative boundaries
* ForestChange.tif - Compressed raster showing forest losses (contact Matt Clark for non-compressed tiles). THIS FILE IS TOO LARGE FOR GITHUB CREATE YOURELF WITH THE PROVIDED CODE OR CONTACT THE AUTHOR FOR ACCESS
* LC_Classification.tif - Compressed raster showing land cover (contact Matt Clark for non-compressed tiles). THIS FILE IS TOO LARGE FOR GITHUB CREATE YOURELF WITH THE PROVIDED CODE OR CONTACT THE AUTHOR FOR ACCESS
* LC_Classification_SIMPLE.tif - Land cover raster simplified just to show non-forest. THIS FILE IS TOO LARGE FOR GITHUB CREATE YOURELF WITH THE PROVIDED CODE OR CONTACT THE AUTHOR FOR ACCESS
* povmap-grdi-v1.tif - Deprivation raster. THIS FILE IS TOO LARGE FOR GITHUB CREATE YOURELF WITH THE PROVIDED CODE OR CONTACT THE AUTHOR FOR ACCESS
* Migration_2000_2019_5yrSum.tif - Human migration raster
* landscan-global-2022.tif - Human population raster
* hotosm_npl_roads_lines_shp.shp - Roads shapefile
* NepalSlope.tif - Topographic slope raster
* Migration_by_VDC.shp - Migration summarized to VDC
* Population2022_by_VDC.shp - Population in 2022 summarized to VDC
* Population2015_by_VDC.shp - Population in 2015 summarized to VDC
* road_density_by_VDC.shp - Road density summarized to VDC
* Deprivation_by_VDC.shp - Deprivation summarized to VDC
* Slope_by_VDC.shp - Slope summarized to VDC
* AllForestGaps.shp - All forest gap patches
* OutcomeData_AllSlope.csv - Gap and loss data by VDC without observations at steep topographic slopes removed
* AllForestLosses.shp - All forest loss patches
* PrecedingGaps.shp - All forest gaps not overlapping with losses
* VDCPROUTnew2.csv - Precipitation anomaly measure calculated using functional halfspace depth
* FullSummaryDataset.csv - Full dataset with all predictor and outcome variables summarized at the VDC level


Note that we include many intermediate datasets. Users can produce these themselves using the provided scripts, but we give them here as the code in its entirety takes weeks (or months depending on methods used) to run. With these intermediate datasets users can, for example, easily run just the statistical models.  

Also note that the statistical models require the STAN statistical program to be installed on the user's machine. If STAN is not installed, follow the link below:
STAN: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

There are many R packages that must also be installed. All packages are loaded in the provided scripts. 

**Matt Clark, Adam M. Sykulski, 09 April 2025**
