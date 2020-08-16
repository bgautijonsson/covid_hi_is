source("Make_Stan_Data.R")  
source("Make_IGLGM_Model.R")
source("Make_Model_Preds.R")
source("Make_RW_SIR_Model.R")
source("Make_Figures.R")

Make_IGLGM_Model(warmup = 100, iter = 100, threads_per_chain = 2)

Make_Model_Preds()

Make_RW_SIR_Model(warmup = 100, iter = 100)

Make_Figures()
