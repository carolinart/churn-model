# Credit card Churn

## Starting 

# Clean environment
rm(list=ls())
gc()
# Disable scientific notation
options(scipen=999)

# Change prompt
options(prompt="CHURN_TC> ", continue=" ") 

# Load utilities functions (change wd, auxiliary scripts...)
source("scripts/utiles.R")

# Set up paths
set_environment() 

# Execute orquestador which handles the process to execute.
source(os.path.join("scripts", "orquestador.R"))
