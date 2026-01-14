# The impact of PM2.5 National Ambient Air Quality Standards on CO2 emissions
This repository contains data and codes to reproduce the results in the manuscript "The impact of PM2.5 National Ambient Air Quality Standards on CO2 emissions" by Veronica Ballerini, Marina Bottomley, Michelle L. Bell and Francesca Dominici.

To reproduce all results, run files from 01a to 09a.

A detailed description of the files follows.

- 00_data_visualization.R: this script is directly recalled in 01b_main_analysis.R and reproduces Figure S1 in the Supplementary Materials
- 00_modified_plot.R and 00_modified_library.R: these scripts are directly recalled in 01b_main_analysis.R and modify the CausalArima package to customize plots and fix the seed for the bootstrap, respectively
- 01a_data.R: This code builds the main dataset used to produce results in the manuscript.
  - inputs: co2_source.xlsx (directly downloaded from the EIA website and saved in the wd); all files in the "covariates" folder
  - output: final_data.RData
- 01b_main_analysis.R: this script reproduces Figs. 2 and 3, results of the meta-analysis reported in text in the manuscript (section "Counterfactual analysis"), Table S1 and Table S2 (line "Total") in the Supplementary Materials.
  - inputs: final_data.RData; 00_modified_plot.R; 00_modified_library.R; 00_data_visualization.R
  - outputs: tables/TableS1; tables/TableS2_total.rds; plots/Fig3; main_results/meta_analysis_20052009.rds; main_results/meta_analysis_20052012.rds; main_results/meta_analysis_20052014.rds; main_results/meta_analysis_rel_20052009.rds; meta_analysis_rel_20052014.rds
- 01c_meta-analysis.R: This code reproduces Fig. 5 and meta-regression results' reported in text in the manuscript (section "Investigation into alternative policies targeting CO2 emissions during the evaluation period"). To run immediately afer 01a_main_analysis.R
  - inputs: StateBindingClass.csv; Science_Table.xlsx
  - output: plots/Fig5; main_results/meta_analysis_moderator_20052009.rds; main_results/meta_analysis_moderator_20052012.rds; main_results/meta_analysis_moderator_20052014.rds
