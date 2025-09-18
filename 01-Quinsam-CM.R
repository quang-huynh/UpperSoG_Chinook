
library(tidyverse)
library(readxl)

# Quinsam CWT recovery
rec <- readxl::read_excel(
  file.path("data", "Quinsam", "2025-02-17-QuinsamChinook_Analyses_2005-2024.xlsx"),
  sheet = "Expanded"
) %>%
  mutate(is_catch = TotCatch > 0, is_esc = Escape > 0)


# CWT by release strategy, removing fed fry (only traditionals: seapen 0+ and smolt 0+)
cwt_rs <- rec %>%
  filter(RELEASE_STAGE_NAME %in% c("Seapen 0+", "Smolt 0+")) %>%
  mutate(RS = "Seapen/Smolt 0+") %>%
  summarise(
    n_catch = sum(TotCatch),
    n_esc = sum(Escape),
    .by = c(Age, BROOD_YEAR, RS)
  )


# Quinsam - CWT releases
rel <- readxl::read_excel(
  file.path("data", "Quinsam", "2025-02-17-QuinsamChinook_Analyses_2005-2024.xlsx"),
  sheet = "Releases"
)

rel_rs <- rel %>%
  filter(RELEASE_STAGE_NAME %in% c("Smolt 0+", "Seapen 0+")) %>%
  mutate(RS = "Seapen/Smolt 0+") %>%
  summarise(n_CWT = sum(TaggedNum) - sum(ShedTagNum), .by = c(BROOD_YEAR, RS))


# Fit model ----
full_table <- expand.grid(
  BROOD_YEAR = seq(min(cwt_rs$BROOD_YEAR), 2023), # 2005 - 2023
  Age = seq(1, 5),
  RS = c( "Seapen/Smolt 0+")
) %>%
  left_join(cwt_rs)

cwt_catch <- reshape2::acast(full_table, list("BROOD_YEAR", "Age", "RS"), value.var = "n_catch", fill = 0)
cwt_esc <- reshape2::acast(full_table, list("BROOD_YEAR", "Age", "RS"), value.var = "n_esc", fill = 0)

cwt_rel <- left_join(
  full_table %>% filter(Age == 1) %>% select(BROOD_YEAR, RS),
  rel_rs
) %>%
  reshape2::acast(list("BROOD_YEAR", "RS"), fill = 0)

# Change this to Quinsam escapement, currently Sarita esc
esc <- readr::read_csv("data/R-OUT_infilled_indicators_escapement_timeseries.csv") %>%
  filter(river == "sarita_river") %>%
  arrange(year) %>%
  right_join(
    full_table %>% filter(Age == 1, RS == "Seapen/Smolt 0+") %>% select(BROOD_YEAR),
    by = c("year" = "BROOD_YEAR")
  )


# Data object for model
Ldyr <- dim(cwt_esc)[1]
Nages <- 5

mat <- c(0, 0.1, 0.4, 0.95, 1) #Could change mat (mean of prior) to align with Quinsam estimates in Walters and Korman (2024)
vulPT <- c(0, 0.075, 0.9, 0.9, 1) #Could change vulPT (mean of prior) to align with Quinsam estimates in Walters and Korman (2024)
vulT <- rep(0, Nages)

M_CTC <- -log(1 - c(0.9, 0.3, 0.2, 0.1, 0.1)) # CTC 23-06 p.9; CWT Exploitation Rate analyses

fec_Quinsam <- c(0, 0,800, 2000, 2500) # Walters and Korman (2024); Filipovic et al. (in revision) RPA.
# Eggs/total spawner (not female spawner)
# age-6 fecundity = 3000 not used

d <- list(
  Nages = Nages,
  Ldyr = Ldyr,
  lht = 1,
  n_r = 2,
  cwtrelease = cwt_rel,
  cwtesc = round(cwt_esc),
  cwtcatPT = round(cwt_catch),
  cwtcatT = NULL,
  bvulPT = vulPT,
  bvulT = vulT,
  RelRegFPT = rep(1, Ldyr),
  RelRegFT = rep(1, Ldyr),
  mobase = M_CTC,
  bmatt = mat,
  hatchsurv = 0.5, #Walters and Korman (2024); 1 used for WCVI Chinook
  gamma = 0.8,
  ssum = 1, # ppn female. Fecundity is eggs/total spawner, so this is set to 1.
  fec = fec_Quinsam,
  r_matt = 2,
  obsescape = esc$escapement,
  propwildspawn = rep(1, Ldyr),
  hatchrelease = rep(0, Ldyr + 1), #to be udpated
  finitPT = 0.8, # Walters and Korman (2024)
  finitT = 0.8,  # Walters and Korman (2024)
  cwtExp = 1
)

# Fix these parameters
map <- list()

# Fix maturity
#map$sd_matt <- factor(rep(NA, Nages-2)) # Not estimating year-specific maturity
#map$logit_matt <- factor(rep(NA, Ldyr * (Nages - 2)))

# Fix additional age-1 M
#map$moadd <- factor(NA)

# Fix age-1 density-independent M deviates
#map$wto <- factor(rep(NA, Ldyr))
#map$wto_sd <- factor(NA)

# Fix density dependent egg-smolt M deviates
#map$wt <- factor(rep(NA, Ldyr))
#map$wt_sd <- factor(NA)

# Fix observation error of Sarita escapement (needed, otherwise model can't separate process from obs error)
map$lnE_sd <- factor(NA)

start <- list(log_so = log(3 * max(d$obsescape)))

# Fit with sampling rate = 1
fit <- fit_CM(d, start = start, map = map, do_fit = TRUE)
samp <- sample_CM(fit, chains = 4, cores = 4)
saveRDS(samp, file = "CM/Quinsam_CM_09.17.25.rds")

samp <- readRDS(file = "CM/Quinsam_CM_09.17.25.rds")
report <- salmonMSE:::get_report(samp)
d <- salmonMSE:::get_CMdata(samp@.MISC$CMfit)
#shinystan::launch_shinystan(samp)

rs_names <- c("Smolt 0+")
salmonMSE::report_CM(
  samp,
  rs_names = rs_names, name = "Quinsam CWT (trial)", year = unique(full_table$BROOD_YEAR),
  dir = "CM", filename = "Quinsam_09.17"
)
