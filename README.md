# tent-census_socius

Repository for Snedker, McKinney, & Lanfear "A tent census: How counting tents 
informs an understanding of unsheltered homelessness in Seattle"

This repository contains all code used to generate the results found in the 
paper. Reproduction data are not available due to identifiability concerns with
the tent census data, as well as inability to redistribute many of the publicly
available datasets used in this project. See `data/data-info.txt` for details
on acquiring the publicly-accessible data used in this project.

Repository structure:

* `data/`: Placeholder folders for data
   * `data-info.txt`: Documents data sources used in this project. Note: Some 
   unused data sources are listed.
* `fig/`: Output of figures for paper
* `syntax/`: All code used in the project
   * `processing/`: Code for processing all data
   * `tables/`: Code to generate tables in the paper
   * `maps/`: Code to generate maps
   * `01_data-processing.R`: Runs all processing scripts in order
   * `project-functions.R`: Core functions used in multiple scripts