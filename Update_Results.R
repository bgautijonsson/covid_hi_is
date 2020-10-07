source("Make_Stan_Data.R")
source("Make_IGLGM_Model.R")
source("Make_Model_Preds.R")
source("Make_EpiEstim_Q_Model.R")
# source("Make_Figures.R")

# Make_IGLGM_Model(warmup = 500, iter = 500, threads_per_chain = 2, chains = 4)

# Make_Model_Preds()

Make_EpiEstim_Q_Model(warmup = 500, iter = 500, chains = 4)


# Make_Figures()

    