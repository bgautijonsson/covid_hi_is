source("Make_Stan_Data.R")  
source("Make_IGLGM_Model.R")
source("Make_Model_Preds.R")
source("Make_RW_SIR_Model.R")
source("Make_EpiEstim_Model.R")
source("Make_Figures.R")

Make_IGLGM_Model(warmup = 500, iter = 500, threads_per_chain = 2)

Make_Model_Preds(fit_date = Sys.Date() - 1)

Make_RW_SIR_Model(warmup = 500, iter = 500)

Make_EpiEstim_Model()

Make_Figures()

    