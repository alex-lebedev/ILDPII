#####################################
# Control script for ILDPII_Pilot2: #
#####################################




# Run RW for Atlas, 2016 data (80 trials, 3 reversals) and Pilot2 data (80 trials, 3 reversalss)
source('/Users/alebedev/GitHub/ILDPII/Pilot_2/ILDPII_Pilot2_main.R')

# Generate EVs based on estimated parameters:
source('/Users/alebedev/GitHub/ILDPII/Pilot_2/generateEVs.R')

# Run hybrid RW model for Atlas, 2016 data (first 40 trials, 1 reversal), Atlas, 2018 data (1 reversal), and Pilot2 data (first 40 trials, 5 subjects)
source('/Users/alebedev/GitHub/ILDPII/Pilot_2/ILDPII_Pilot2_main_first40.R')