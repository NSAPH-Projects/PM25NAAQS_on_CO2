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
  - outputs: tables/TableS1; tables/TableS2_total.rds; plots/Fig3; main_results/meta_analysis_20052009.rds; main_results/meta_analysis_20052012.rds; main_results/meta_analysis_20052014.rds; main_results/meta_analysis_rel_20052009.rds; meta_analysis_rel_20052014.rds; all plots in the folder "plots/state-series"
- 01c_meta-analysis.R: This code reproduces Fig. 5 and meta-regression results' reported in text in the manuscript (section "Investigation into alternative policies targeting CO2 emissions during the evaluation period"). To run immediately afer 01a_main_analysis.R
  - inputs: StateBindingClass.csv; Science_Table.xlsx
  - output: plots/Fig5; main_results/meta_analysis_moderator_20052009.rds; main_results/meta_analysis_moderator_20052012.rds; main_results/meta_analysis_moderator_20052014.rds
- 01d_additional_figures.R: This code reproduces Fig. 4 in the manuscript and Fig. S7 in the Supplementary Materials. To run immediately after 01c_meta-analysis.R
  - inputs: Science_Table.xlsx
  - outputs: plots/Fig4; plots/FigS7
- 02a_electricpower_data.R, 03a_industrial_data.R, 04a_transportation_data.R, 05a_commercial_data.R, 06a_residential_data.R: These codes build the sector specific datasets used in the manuscript.
  - inputs: co2_source.xlsx (directly downloaded from the EIA website and saved in the wd); all files in the "covariates" folder
  - output: final_data_electricpower.RData; final_data_industrial.RData; final_data_transportation.RData; final_data_commercial.RData; final_data_residential.RData, respectively
- 02b_electricpower.R: this script reproduces results of the analysis by sector reported in text, and Figure S2 and Table S2 (line "Electric power") and Table S3 in the Supplementary Materials.
  - inputs: final_data_electricpower.RData; 00_modified_plot.R; 00_modified_library.R; 00_data_visualization.R
  - outputs: tables/TableS2_electricpower.rds; tables/TableS3; plots/FigS2; main_results/meta_analysis_20052009_electricpower.rds; main_results/meta_analysis_20052012_electricpower.rds; main_results/meta_analysis_20052014_electricpower.rds; main_results/meta_analysis_rel_20052009_electricpower.rds; meta_analysis_rel_20052014_electricpower.rds
- 03b_industrial.R: this script reproduces results of the analysis by sector reported in text, and Figure S3 and Table S2 (line "Industrial") and Table S4 in the Supplementary Materials.
  - inputs: final_data_industrial.RData; 00_modified_plot.R; 00_modified_library.R; 00_data_visualization.R
  - outputs: tables/TableS2_electricpower.rds; tables/TableS4; plots/FigS3; main_results/meta_analysis_20052009_industrial.rds; main_results/meta_analysis_20052012_industrial.rds; main_results/meta_analysis_20052014_industrial.rds; main_results/meta_analysis_rel_20052009_industrial.rds; meta_analysis_rel_20052014_industrial.rds
- 04b_transportation.R: this script reproduces results of the analysis by sector reported in text, and Figure S4 and Table S2 (line "Transportation") and Table S5 in the Supplementary Materials.
  - inputs: final_data_transportation.RData; 00_modified_plot.R; 00_modified_library.R; 00_data_visualization.R
  - outputs: tables/TableS2_transportation.rds; tables/TableS5; plots/FigS4; main_results/meta_analysis_20052009_transportation.rds; main_results/meta_analysis_20052012_transportation.rds; main_results/meta_analysis_20052014_transportation.rds; main_results/meta_analysis_rel_20052009_transportation.rds; meta_analysis_rel_20052014_transportation.rds
- 05b_commercial.R: this script reproduces results of the analysis by sector reported in text, and Figure S5 and Table S2 (line "Commercial") and Table S6 in the Supplementary Materials.
  - inputs: final_data_commercial.RData; 00_modified_plot.R; 00_modified_library.R; 00_data_visualization.R
  - outputs: tables/TableS2_commercial.rds; tables/TableS6; plots/FigS5; main_results/meta_analysis_20052009_commercial.rds; main_results/meta_analysis_20052012_commercial.rds; main_results/meta_analysis_20052014_commercial.rds; main_results/meta_analysis_rel_20052009_commercial.rds; meta_analysis_rel_20052014_commercial.rds
- 06b_residential.R: this script reproduces results of the analysis by sector reported in text, and Figure S6 and Table S2 (line "Residential") and Table S7 in the Supplementary Materials.
  - inputs: final_data_residential.RData; 00_modified_plot.R; 00_modified_library.R; 00_data_visualization.R
  - outputs: tables/TableS2_residential.rds; tables/TableS7; plots/FigS6; main_results/meta_analysis_20052009_residential.rds; main_results/meta_analysis_20052012_residential.rds; main_results/meta_analysis_20052014_residential.rds; main_results/meta_analysis_rel_20052009_residential.rds; meta_analysis_rel_20052014_residential.rds
