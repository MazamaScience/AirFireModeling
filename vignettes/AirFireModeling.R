## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
# Load the library
library(AirFireModeling)

# Set the data download directory 
setModelDataDir('~/Data/Bluesky')

# Load a single model run
bs <- bluesky_load(modelRun = 20191228, model = 'CANSAC-4km')

bs 

