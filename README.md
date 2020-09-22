# Explaining Gender Differences in Career Trajectories Across 30 Academic Fields
This repository contains the code and data needed to replicate the analyses presented in the paper *Explaining Gender Differences in Career Trajectories Across 30 Academic Fields* by Larremore, Joseph, Hannak, and Cimpian.  The one thing that is not provided is the raw data from ORCID.  For this, you'll need to head over to the [public ORCID dumps](https://support.orcid.org/hc/en-us/articles/360006897394-How-do-I-get-the-public-data-file-).  You can place the resulting data in the ```data/raw``` directory to get started!

Then, run the scripts in ```code/data_processing``` to generate the files we use for our analyses. These analyses are carried out using the code in the ```code/analysis``` directory.  Details on all files in the repository are below.  

Please open an issue if you have questions!


# Code 
All code used for this project can be found in the ```code``` directory of this repository.  There are three subdirectories. Code in the ```data_processing``` directory takes the raw ORCID data and runs the preprocessing steps described in the Supplement of the paper.  The primary outputs of this code are two CSV files (both gzipped!), ```cleaned_singlematchtofield_affiliations.csv.gz``` and ```all_switches_w_samefield.csv.gz```, which describe the affiliations and field switches, respectively, on which our primary analyses are carried out.  These analyses are performed in R, using the code in the ```analysis``` directory.  Finally, we include code for generating the data for the simulation experiment and code for creating the network flow images in the paper in the ```python``` subdirectory.  Code and supplementary files in each of these directories are described in more detail below.

Python code has been tested primarily in python 3.6.
## Data Processing

### Primary Code Files
- ```extract_affiliations.ipynb``` - Takes as input data from an [AWS sync](https://github.com/ORCID/public-data-sync/blob/master/sync.py) of the ORCID data. Outputs a gender-tagged, cleaned listing of each affiliation to a set of potential fields. Note that the AWS version of the data is very difficult to work with. We encourage others to use the [public ORCID dumps](https://support.orcid.org/hc/en-us/articles/360006897394-How-do-I-get-the-public-data-file-), and provide code in ```parse_raw_orcid_dump.py``` to process the JSON version of these dumps. See the code for further details.
- ```identify_single_affiliations_and_switches.ipynb``` - Takes as input the affiliations outputted by ```extract_affiliations.ipynb``` and identifies only those affiliations that match a single field in our survey data. Then, computes field switches using our field switch algorithm described in the paper.

### Supplementary Code and Data Files
- ```black-list-roots.txt``` - A list of regular expressions used for the blacklist
- ```field_matching.py``` - Utility code for implementing our algorithm to match strings of text to academic fields
- ```blacklist.txt``` - Terms (not regular expressions) used to identify fields in the blacklist
- ```parse_raw_orcid_dump.py``` - Code to help clean and extract information from raw ORCID dumps



## Analysis

Primary statistical analyses for the paper are carried out here. See ```sessionInfo.txt``` for details on the R environment used. You can also click on the ```analysis.Rproj``` file to easily open up the code in RStudio.

### Primary Code Files
- ```regressions.R``` - Regression analyses carried out in the paper, as well as the calculation of various basic statistics reported.  Images are output to the top level ```img``` directory.
- ```robustness_results.R```- Regression analyses for the simulation experiment detailed in the paper

### Supplementary Code and Data Files
- ```analysis.Rproj``` - Convenience for RStudio users
- ```country_to_region.tsv``` - A mapping of countries listed by ORCID users to regions of the world.
- ```util.R``` - Utility R code for carrying out regressions and loading data



## Python

### Primary Code Files
- ```make_redblue_network_flow_figures.ipynb``` - Code for the construction of Figure 1. 
- ```make_synthetic_biased_data.ipynb``` - Code to generate simulated data for robustness analysis

### Supplementary Code and Data Files
- ```network_viz``` - Output directory for network images
- ```synthetic``` - Simulation data used in the paper
- ```figures``` - Output directory for simulation code

### Cultural consenses estimates of associations between name and gender
- `data_processing/BayesCCM` contains functions in `cct.py` which can be called in a demonstration notebook in `CulturalConsensusDemo.ipynb`. Due to the large file size of the training data, *training data file has been zipped* so after cloning the repository, one **must** unzip `aggregated_master.json.zip` into the same folder as the .zip file. 

# Data
The ```data``` directory contains data that is used (```processed``` and ```raw```, although see note below on raw) or generated and then used (```output```) by our code.

## Output

- ```switch_counts_2019_12_12.csv``` - Counts of field switches, used for Figure 1 generation
- ```field_counts_2019_12_12.csv``` - Counts of field affiliations, used for simulation robustness checks
- ```branching_name_consensus.csv``` - Data on names mapped to gender
- ```all_switches_w_samefield.csv.gz``` - All switches between fields that we identified
- ```cleaned_singlematchtofield_affiliations.csv.gz``` - All affiliations that we were able to match to a single field in our survey data.

## Processed
- ```field_match_test_sheet.csv``` - Data to test the field matching algorithm
- ```survey.csv``` - Survey data for the 30 fields used in the paper
- ```gre_data_kenny_namefix.csv``` - GRE scores for the fields, used for GRE math robustness check
- ```survey_kenny_header_fieldname_edits.csv``` - Same as ```survey.csv```, but with field names that are easier to process
- ```translated_fieldnames.csv``` - Data generated by Google Translate to map all fields to English
- ```survey_names.csv``` - Same as ```survey.csv```, but with field names that are easier to process for certain analyses

## Raw
ORCID provides raw data directly. Please see above for how to access it!
