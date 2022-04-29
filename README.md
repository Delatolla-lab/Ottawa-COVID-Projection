# Ottawa COVID-19 projections

Projections for daily new COVID-19 cases and hospitalizations for residents in the Ottawa area. 

Case projections are created using the [Bayesian latent variable model](https://epiforecasts.io/EpiNow2/). The figures in this repository, however, can be used to present projections for other models.

# To build the website

1. Follow the instructions here to install blogdown and hugo https://bookdown.org/yihui/blogdown/installation.html
2. Once installed, run `blogdown::serve_site()` and navigate to http://127.0.0.1:4450/613-COVID/ to view the site. This command will also continuously watch your files for changes so that the website will be automatically rebuilt and refreshed whenever you change any of the files.

## Updates

See [changelog](https://github.com/Big-Life-Lab/Ottawa-COVID-Projection/blob/master/content/en/change-log.Rmd) for updates.

## Descriptions of Folders and files in the Repo:

- `.github/workflows`: Configuration files for GitHub Actions.

- `/R/case_proj_performance.R`: This script runs a loop of projections through previous case data to generate a file of historic projections to be used to compare with observed case data.

- `/R/dataset_creation.R`: This file pulls covid-19 data from Open Ottawa and prepares a dataset for visualization and projection simulations.

- `/R/epinow_functions.R`: This file contains the functions for running the Epinow model for making projections on cases, hospitalization and waste waster data. It also contains the function for creating plots of the projections.

- `/R/historic_forecasts.R`: This function creates a dataset that pulls data from historic projections and combines it with observed data. Function defaults to pull 7th and 14th day projections, but users can select other dates. Function defaults to median as projection variable of interest, but users can change to mean.

- `/R/hosp_proj_performance.R`: This script runs a loop of projections through previous hosp data to generate a file of historic projections to be used to compare with observed hosp data.

- `/R/lag_replacement.R`: This function calculates expected values based on rate of change for that period and the length of that period. It also contains functions for calculating doubling time, growth rate for range of data.

- `/R/localization.R`: This function will use the data in the Covid Localization - Home Page.csv files to create the html output text from the Rmd files in the content folder for the 613Covid website.

- `/R/observed_data.R`: This function will create plotly interactive plots for the wastewater signal, cases, hospitalizations for both observed data and projected data for validation of forecasted data with actual data along with error bands or error bars. 

- `/R/open_ottawa_scripts.R`: This contains functions to extract dataframe from JSON objects and extracs adjusted cases by episode date. It cleans the case data and test data and merge it.

- `/R/short_term_hosp_projections.R`: It runs epinow short term projections on hospital data using last 120 days of historic data for training.

- `/R/short_term_projections.R`: It runs epinow short term projections on the case data.

- `/R/short_term_ww_projections.R`: It runs epinow short term projections on the waste water data.

- `/R/wastewater.R`: This contains function to clean the wastewater data both wide and long and merge it into covid case data.

- `/content/en/model.Rmd`: creates the https://613covid.ca/model/#projection-models website

- `/content/en/hosp-historic-performance.Rmd`: This creates plots to evaluate the performance of the model to validate the hospital data by comparing the forecasted data with the actual data.

- `/content/en/more.Rmd`: This generates all the websites under the More Tab on 613covid website such as https://613covid.ca/more/#use

- `/content/en/short-proj-dev.Rmd`: This calls functions to create the <b>development page</b> for the hospital projections for Ottawa.

- `/content/en/wastewater.Rmd`: This calls functions to generate the wastewater visualization page on 613covid site: https://613covid.ca/wastewater/

- `/content/en/wastewater-dev.Rmd`: This calls functions for creating plots of wastewater projections and viral signal with plots of different variants of concerns in development page.

- `/content/en/ww-historic-performance.Rmd`: This calls functions for validating the performance of the model in forecasting wastewater signal for different time intervals.

- `/content/en/contact.Rmd`: This generates the website: https://613covid.ca/contact/

- `/content/en/change-log.Rmd`: This generates the website: https://613covid.ca/change-log/

- `/content/en/_index.Rmd`: This generates all the homepage of the 613Covid website: https://613covid.ca/#

- `/Model Validation/ww_projections_development/R/cases_7_and_14_days_forecast.R`: Checks performance evaluation of the epinow model on cases data by validating 7th and 14th day of forecasted data against the actual data by running projections across different intervals of data.

- `/Model Validation/ww_projections_development/R/cases_forecast_sept_present.R`: Makes epinow projections on cases data using historic data from September 2021 to current time point.

- `/Model Validation/ww_projections_development/R/cases_hosp_ww_forecast_dec_present.R`: Makes epinow projections on cases, waste water, and hospital data using historic data from December 2021 to current time point.

- `/Model Validation/ww_projections_development/R/cases_hosp_ww_forecast_jan_present.R`: Makes epinow projections on cases, waste water, and hospital data using historic data from January 2021 to current time point.

- `/Model Validation/ww_projections_development/R/cases_hosp_ww_forecast_jul21_present.R`: Makes epinow projections on cases, waste water, and hospital data using historic data from July 2021 to currentt time point.

- `/Model Validation/ww_projections_development/R/cases_hosp_ww_forecast_nov_present.R`: Makes epinow projections on cases, waste water, and hospital data using historic data from November 2021 to current time point.

- `/Model Validation/ww_projections_development/R/hosp_historic_proj_different_Zero_delay_performance.R`: Validates performance of the epinow model on hospital data using different intervals of data for training using Zero reporting delay period.

- `/Model Validation/ww_projections_development/R/hosp_historic_proj_different_delays_performance.R`: Validates performance of the epinow model on hospital data using different intervals of data and different reporting delay periods of 0, 10, and 14 days delay.

- `/Model Validation/ww_projections_development/R/hospital_7_and_14_days_forecast.R`: Checks performance evaluation of the epinow model on hospital data by validating 7th and 14th day of forecasted data against the actual data by running projections across different intervals of data.

- `/Model Validation/ww_projections_development/R/hospital_census_forecast_sept_present.R`: Makes epinow projections on the hospital data from September 2021 to current time point.

- `/Model Validation/ww_projections_development/R/cases_hosp_ww_forecast_jul_present.R`: Makes epinow projections on the cases, hospital, and waste water data from July 2020 to current time point.

- `/Model Validation/ww_projections_development/R/cases_forecast_sept_present.R`: Makes epinow projections on the cases data from September 2021 to current time.

- `/Model Validation/ww_projections_development/R/ww_forecast_sept_present.R`: Makes epinow projections on the waste water data from September 2021 to current time.

- `/Model Validation/ww_projections_development/R/ww_historic_proj_performance.R`: Checks performance evaluation of the epinow model on the waste water data by validation the forecasted data against actual data using different intervals of historic data for training.

- `/Model Validation/ww_projections_development/R/ww_start_date_performance.R`: Makes epinow projections on the waste water data using 1, 3, 6,and 12 months of historic data.

- `/Model Validation/ww_projections_development/plot_observed_predicted_hospital.Rmd`: creates plots of observed vs predicted hospital data from epinow model for all the 3 reporting delay period and the different interval of data.

- `/Model Validation/ww_projections_development/plot_observed_predicted_hospital_delay_0.Rmd`: creates plots of observed vs predicted hospital data from epinow model for Zero reporting delay period and the different intervals of data.

- `/Model Validation/ww_projections_development/plots_model_evaluation.Rmd`: Makes projection plots on hospital, waste water, and cases data for both Rt and reported cases. Merges the projections from hospital, waste water and cases data. 

- `/Model Validation/ww_projections_development/plots_model_evaluation_dec_present.Rmd`: Makes projection plots on hospital, waste water, and cases data for both Rt and reported cases. Uses historic data from December 2021 for projections. Merges the hospital, wastewater, and cases data.

- `/Model Validation/ww_projections_development/plots_model_evaluation_jan_present.Rmd`: Makes projection plots on hospital, waste water, and cases data for both Rt and reported cases. Uses historic data from January 2021 for projections. Merges the hospital, wastewater, and cases data.

- `/Model Validation/ww_projections_development/plots_model_evaluation_jul21_present.Rmd`: Makes projection plots on hospital, waste water, and cases data for both Rt and reported cases. Uses historic data from July 2021 for projections. Merges the hospital, wastewater, and cases data.

- `/Model Validation/ww_projections_development/plots_model_evaluation_nov_present.Rmd`: Makes projection plots on hospital, waste water, and cases data for both Rt and reported cases. Uses historic data from November 2021 for projections. Merges the hospital, wastewater, and cases data.

- `/Model Validation/ww_projections_development/plots_model_evaluation_sep_present.Rmd`: Makes projection plots on hospital, waste water, and cases data for both Rt and reported cases. Uses historic data from September 2021 for projections. Merges the hospital, wastewater, and cases data.

- `/Model Validation/ww_projections_development/ww-testing.Rmd`: This script runs epinow projections on wastewater data for testing purpose and makes projection plots for infections, Rt, and reported cases.

- `/Model Validation/ww_projections_development/ww_proj_performance.Rmd`: This script checks the validation performance on waste water data.

- `/Model Validation/ww_projections_development/Data/`: Stores all the Rdata files generated from the R scripts in /Model Validation/ww_projections_development/R/ folder.

- `/Model Validation/case_performance_proj.Rmd`: Examines historic case projections with case data using different intervals of case data.

- `/Model Validation/epinow-hosp-ww-assessment.Rmd`: Estimates hospitalizations against case data and cases against waste water data.

- `/Model Validation/proj_comparisons.Rmd`: Makes plots of projections vs actual values for cases and hospital data from all the short term forecasts files in /Data/Historic Projections folder.

- `/Model Validation/short-term_assessment.Rmd`: Assess short term projections on the Ottawa cases, hospitalization data. 

- `docs/, layouts/, static/, themes/, config.toml`: Data folders and files used to configure & render 613covid.ca.

- `renv/, renv.lock`: Files used to configure R project environment for automated runs.
