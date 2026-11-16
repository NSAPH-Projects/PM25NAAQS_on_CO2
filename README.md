# The impact of PM2.5 National Ambient Air Quality Standards on CO2 emissions
This repository contains data and codes to reproduce the results in the manuscript "The impact of PM2.5 National Ambient Air Quality Standards on CO2 emissions" by Veronica Ballerini, Marina Bottomley, Michelle L. Bell and Francesca Dominici.

To reproduce all results, run files from 01a to 09a.

A detailed description of the files follows.

- 00_data_visualization.R: this script is directly recalled in 02_electricpower.R and reproduces Figure S1 in the Supplementary Materials
- 00_modified_library.R: this script is directly recalled in 02_electricpower.R and modifies the CausalArima package to fix the seed for the bootstrap
- 00_modified_plot.R: this script is directly recalled in 03_additional_figures.R and modifies the CausalArima package to customize the plots of the CausalArima package 
- 01_electricpowerdata.R: This code builds the main dataset used to produce results in the manuscript.
  - inputs: co2_source.xlsx (directly downloaded from the EIA website and saved in the wd); all files in the "covariates" folder
  - output: final_data_electricpower.RData
- 02_electricpower.R: this script reproduces Figs. 3a and 3b, in-text results (section "Counterfactual analysis"), and Table S1 in the Supplementary Materials.
  - inputs: final_data_electricpower.RData; 00_modified_plot.R; 00_modified_library.R; 00_data_visualization.R
  - outputs: tables/TableS1.docx; tables/Table_electricpower.rds; plots/Fig3a; plots/Fig3b
- 03_additional_figures.R: This code reproduces Fig. 2 and 4 in the manuscript. To run immediately after 02_electricpower.R
  - outputs: plots/Fig2.jpeg; plots/Fig4.jpeg
- 04_heterogeneity.R: This code reproduces Fig. 5 and meta-regression results reported in-text in the manuscript (section "Investigation into alternative policies targeting CO2 emissions during the evaluation period"). To run after 02_electricpower.R.R
  - inputs: StateBindingClass.csv; Science_Table.xlsx
  - output: plots/Fig5.jpeg; main_results/meta_analysis_moderator_20052009.rds; main_results/meta_analysis_moderator_20052012.rds; main_results/meta_analysis_moderator_20052014.rds
- 07a_naturalgas_data.R: this code builds the dataset about CO2 emissions from natural gas used to produce results in the Supplementary Materials (testing the assumption that the observed reduction in CO2 emissions is not due to the new natural gas shifting).
  - inputs: co2_source.xlsx (directly downloaded from the EIA website and saved in the wd); all files in the "covariates" folder
  - outputs: final_data_naturalgas.RData
- 07b_naturalgas.R: This code identifies the states for which there is a significant increase in CO2 emissions from natural gas; this information in used to test the assumption that the overall observed reduction in CO2 emissions is not due to the natural gas shifting (Supplementary Materials).
  - inputs: final_data_naturalgas.RData; 00_modified_library.R
  - outputs: increase_naturalgas.txt; supplementary_results/naturalgasshifting_20052012.rds; supplementary_results/naturalgasshifting_20052014.rds; supplementary_results/naturalgasshifting_rel_20052009.rds; supplementary_results/naturalgasshifting_rel_20052012.rds; supplementary_results/naturalgasshifting_rel_20052014.rds; supplementary_results/Table_naturalgas.rds
- 07c_naturalgasshifting.R: This code reproduces Fig. S8 and in-text results of the section "Natural gas shifting" in the Supplementary Materials.
  - inputs: increase_naturalgas.txt; main_analysis.RData
  - outputs: plots/FigS8.jpeg; main_results/meta_analysis_20052009_naturalgasshifting.rds; main_results/meta_analysis_20052012_naturalgasshifting.rds; main_results/meta_analysis_20052014_naturalgasshifting.rds; main_results/meta_analysis_rel_20052009_naturalgasshifting.rds; main_results/meta_analysis_rel_20052012_naturalgasshifting.rds; main_results/meta_analysis_rel_20052014_naturalgasshifting.rds
- 08a_Population: This code reproduces results in Table S9 (line "Population") in the Supplementary Materials.
  - inputs: final_data.RData; 00_modified_library.R
  - outputs: tables/TableS9_Population.rds; supplementary_results/population_20052009.rds; supplementary_results/population_20052012.rds; supplementary_results/population_20052014.rds; supplementary_results/population_rel_20052009.rds; supplementary_results/population_rel_20052012.rds; supplementary_results/population_rel_20052014.rds
- 08b_Temperature: This code reproduces results in Table S9 (line "Temperature") in the Supplementary Materials.
  - inputs: final_data.RData; 00_modified_library.R
  - outputs: tables/TableS9_Temperature.rds; supplementary_results/temperature_20052009.rds; supplementary_results/temperature_20052012.rds; supplementary_results/temperature_20052014.rds; supplementary_results/temperature_rel_20052009.rds; supplementary_results/temperature_rel_20052012.rds; supplementary_results/temperature_rel_20052014.rds
- 08c_Precipitation: This code reproduces results in Table S9 (line "Precipitation") in the Supplementary Materials.
  - inputs: final_data.RData; 00_modified_library.R
  - outputs: tables/TableS9_Precipitation.rds; supplementary_results/precipitation_20052009.rds; supplementary_results/pprecipitation_20052012.rds; supplementary_results/precipitation_20052014.rds; supplementary_results/precipitation_rel_20052009.rds; supplementary_results/precipitation_rel_20052012.rds; supplementary_results/precipitation_rel_20052014.rds
- 08d_OilPrices: This code reproduces results in Table S9 (line "Oil Prices") in the Supplementary Materials.
  - inputs: final_data.RData; 00_modified_library.R
  - outputs: tables/TableS9_OilPrices.rds
- 08e_GDP: This code reproduces results in Table S9 (line "GDP") in the Supplementary Materials.
  - inputs: final_data.RData; 00_modified_library.R
  - outputs: tables/TableS9_GDP.rds
- 09a_noanticipation.R: This code reproduces results in Table S8 in the Supplementary Materials.
  - inputs: final_data.RData; 00_modified_library.R
  - outputs: tables/TableS8.rds
