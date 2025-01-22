
library(tidyverse)
library(salmonMSE) # Need January 17, 2025 version


#### Data ----
# Sarita escapement (1979 - 2023)
esc <- readr::read_csv("data/R-OUT_infilled_indicators_escapement_timeseries.csv") %>%
  filter(river == "sarita_river") %>%
  arrange(year)

# CWT data (1973 - 2021)
dat <- readr::read_csv("data/RBT_data_wfisheries.csv")
#problems(dat)
dat[c(1013, 2273), ]




#### Process data ----

# Full matrix of ages (1-5) and years (1979 - 2021)
full_matrix <- expand.grid(
  BroodYear = 1979:2021,
  Age = 1:5
) %>%
  as.data.frame()
full_year <- data.frame(BroodYear = 1979:2021)

# Escapement
esc_sarita <- filter(esc, year %in% full_year$BroodYear)

# CWT releases by tag code
rel <- dat %>%
  summarise(CWTMark1Count = unique(CWTMark1Count), .by = c(TagCode, BroodYear)) %>%
  arrange(BroodYear, TagCode)

# Annual CWT releases
rel_annual <- rel %>%
  summarise(rel = sum(CWTMark1Count), .by = BroodYear) %>%
  right_join(full_year)

# CWT escapement by brood year, age
cwt_esc <- dat %>%
  filter(fishery_type == "escapement") %>%
  summarise(n = sum(AdjustedEstimatedNumber), .by = c(BroodYear, Age)) %>%
  right_join(full_matrix, by = c("BroodYear", "Age")) %>%
  reshape2::acast(list("BroodYear", "Age"), value.var = "n", fill = 0)

# Preterminal CWT
cwt_pt <- dat %>%
  filter(fishery_type == "pre-terminal", Age < 7) %>%
  summarise(n = sum(AdjustedEstimatedNumber), .by = c(BroodYear, Age)) %>%
  right_join(full_matrix, by = c("BroodYear", "Age")) %>%
  reshape2::acast(list("BroodYear", "Age"), value.var = "n", fill = 0)

# Terminal CWT
cwt_t <- dat %>%
  filter(fishery_type == "terminal") %>%
  summarise(n = sum(AdjustedEstimatedNumber), .by = c(BroodYear, Age)) %>%
  right_join(full_matrix, by = c("BroodYear", "Age")) %>%
  reshape2::acast(list("BroodYear", "Age"), value.var = "n", fill = 0)



# Data object for model
Ldyr <- nrow(cwt_esc)
Nages <- 5

mat <- c(0, 0.1, 0.4, 0.95, 1)
vulPT <- c(0, 0.075, 0.9, 0.9, 1)
vulT <- vulPT
#M_CTC <- -log(1 - c(0.9, 0.3, 0.2, 0.1, 0.1))
d <- list(
  Nages = Nages,
  Ldyr = Ldyr,
  lht = 1,
  hatchsurv = 1,
  gamma = 1,
  ssum = 0.5,
  finitPT = 0.8,
  finitT = 0.8,
  bmatt = mat,
  fec = c(0, 87, 1153, 2780, 2700),
  bvulPT = vulPT,
  bvulT = vulT,
  #mobase = M_CTC,
  mobase = c(3, rep(0.3, 4)),
  cwtrelease = rel_annual$rel,
  cwtesc = round(cwt_esc),
  cwtcatPT = round(cwt_pt),
  cwtcatT = round(cwt_t),
  RelRegFPT = rep(1, Ldyr),
  RelRegFT = rep(1, Ldyr),
  obsescape = esc_sarita$escapement,
  propwildspawn = rep(1, Ldyr),
  hatchrelease = rep(0, Ldyr + 1),
  cwtExp = 1
  #covariate = matrix(1, d$Ldyr, 1)
)


# Fix these parameters
map <- list()

# Fix maturity
#map$sd_matt <- factor(rep(NA, Nages-2)) # Not estimating year-specific maturity
#map$logit_matt <- factor(rep(NA, Ldyr * (Nages - 2)))

# Fix additional age-1 M
#map$moadd <- factor(NA)

# Fix age-1 density-independent M deviates
map$wto <- factor(rep(NA, Ldyr))
map$wto_sd <- factor(NA)

# Fix density dependent egg-smolt M deviates
#map$wt <- factor(rep(NA, Ldyr))
#map$wt_sd <- factor(NA)

# Fix observation error of Sarita escapement (needed, otherwise model can't separate process from obs error)
map$lnE_sd <- factor(NA)

start <- list(log_so = log(5 * max(d$obsescape)))

fit <- fit_CM(d, start = start, map = map, do_fit = TRUE)

samp <- sample_CM(fit, chains = 2, cores = 2)
#saveRDS(samp, file = "CM/Sarita_RBT_CM_01.17.25.rds")
#samp <- readRDS("CM/Sarita_RBT_CM_01.17.25.rds")
salmonMSE:::reportCM(samp, dir = "CM", filename = "Sarita_Ex", year = full_year$BroodYear, name = "Sarita (RBT CWT)")

shinystan::launch_shinystan(samp)

# Look at gradients
data.frame(
  par = fit$opt$par %>% names(),
  gr = fit$obj$gr()[1, ]
) %>%
  filter(abs(gr) > 0.1)

