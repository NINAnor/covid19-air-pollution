## COVID-19 lockdowns cause global air pollution declines with implications for public health risk

Data and scripts in support of published manuscript:  https://www.medrxiv.org/content/10.1101/2020.04.10.20060673v1

This is an Rproject with a complex workflow that requires manual transfer of data between local PC, Google Earth Engine and Amazon Web Services. Therefore it is not a seamless workflow and anyone interested in comonents of the analysis are encouraged to contact Zander Venter: zander.venter@nina.no

Due to GitHub data file limitations, there are three CSV files that have been truncated to save space. Please contact Zander Venter for acces to original files if needed.

This applies to files:
- "./DATA/Stations/openaq/city_day_agg.csv"
- "./DATA/Stations/openaq/city_day_agg_cleaned.csv"
- "./DATA/Stations/Climate/gfsDaily_allcities_updated.csv"

The scripts should be run in the following order:
1. Setup.R
2. Lockdown_prepare.R
3. Station_prepare.R
4. Sateillte_analysis.R
5. Station_analysis.R
6. Mobility_vs_pollution.R
7. Health_burden.R 

The "SQL_queries.R" file contains SQL queries that are run on Athena AWS to fetch data from OpenAQ. See here fore more: https://registry.opendata.aws/openaq/
