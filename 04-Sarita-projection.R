

nOM <- 5

library(snowfall)
sfInit(TRUE, nOM)

tictoc::tic()
SMSE_list <- sfLapply(seq_len(nOM), function(i) {
  require(salmonMSE)

  SOM <- readRDS(file.path("SOM", paste0("SOM", i, ".rds")))
  SMSE <- salmonMSE(SOM)

  saveRDS(SMSE, file = file.path("SMSE", paste0("Sarita", i, ".rds")))

  invisible()
})
sfStop()
tictoc::toc()

SOM <- readRDS("SOM/SOM_base_3sim.rds")
SMSE <- salmonMSE(SOM, convert = FALSE)
#saveRDS(SMSE, file = "SMSE/Sarita_base2.rds")

#lhg_name <- c("Early Smalls", "Late Larges")
#rs_name <- c("Fed Fry", "Smolt 0+")
#report(SMSE, dir = "SMSE", filename = "Sarita_base2")
