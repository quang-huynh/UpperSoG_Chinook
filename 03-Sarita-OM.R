
# Make OM
library(tidyverse)
library(salmonMSE)

maxage <- 5
nsim <- 100
nyears <- 2
proyears <- 50
n_g <- 2

# Load exploitation rate model - Sarita (with Robertson CWT traditionals)
ERM_Sarita <- readRDS("CM/Sarita_RBT_CM_05.20.25.rds")
report_RBT <- salmonMSE:::get_report(ERM_Sarita)

# Take maturity average from the 6 most recent brood years (2013-2018)
matt_dev <- readRDS("CM/Sarita_maturity.rds")
matt_avg <- sapply(matt_dev, function(x) {
  apply(x$matt[seq(9, 14), , ], 2:3, mean)
}, simplify = "array")

set.seed(24)
sim_samp <- sample(seq(1, length(report_RBT)), nsim)

# Load exploitation rate model - Quinsam CWT (fed fry + traditionals)
ERM_Quinsam <- readRDS("CM/Sarita_RBT_CM_05.20.25.rds")
report_Q <- salmonMSE:::get_report(ERM_Quinsam)

### Natural mortality - NOS ----
Mjuv_NOS <- array(0, c(nsim, maxage, nyears + proyears, n_g))

# Survival from CTC 23-06 p. 9 for ages 2+
M_CTC <- -log(1 - c(0.9, 0.3, 0.2, 0.1, 0.1))

# Age 1 value by life cycle group
# Life cycle groups only differ in age 1 survival (first year of life in marine environment) from life cycle spreadsheet
surv1 <- 1 - c(0.996, 0.95)
Mjuv_NOS[, 1, , 1] <- -log(surv1[1])
Mjuv_NOS[, 1, , 2] <- -log(surv1[2])

# Age 2-5
Mjuv_NOS[, 2, , ] <- M_CTC[2]
Mjuv_NOS[, 3, , ] <- M_CTC[3]
Mjuv_NOS[, 4, , ] <- M_CTC[4]
Mjuv_NOS[, 5, , ] <- M_CTC[5]

### Maturity ----
# Assume identical maturity from hypothesized fed fry maturity
p_mature <- array(0, c(nsim, maxage, nyears + proyears))
p_mature[] <- matt_avg[, 1, sim_samp] %>% t() %>%
  array(c(nsim, maxage, nyears + proyears))

fec_Sarita <- c(0, 1500, 3000, 3600, 4600) # See Res Doc, but set age 2 fecundity arbitrarily equal to half of age 3

Bio <- new(
  "Bio",
  maxage = maxage,
  n_g = n_g,
  p_LHG = c(0.95, 0.05),
  p_mature = p_mature,
  #SRrel = "BH",
  #kappa = 1.0001,
  #capacity_smolt = 1e12,
  #phi = 1,
  Mjuv_NOS = Mjuv_NOS,
  p_female = 0.4,
  fec = fec_Sarita,
  s_enroute = 1
)

### Harvest, fishery vulnerability ----
vulPT <- sapply(report_RBT[sim_samp], getElement, "vulPT")
vulT <- sapply(report_RBT[sim_samp], getElement, "vulT")

#matplot(vulPT, typ = 'l')
#matplot(vulT, typ = 'l')
#g <- salmonMSE:::CM_ER(report_RBT, brood = FALSE, type = "PT", year1 = 1979, at_age = FALSE)
#g <- salmonMSE:::CM_ER(report_RBT, brood = FALSE, type = "T", year1 = 1979, at_age = FALSE)
#g <- salmonMSE:::CM_ER(report_RBT, brood = FALSE, type = "all", year1 = 1979, at_age = FALSE)

Harvest <- new(
  "Harvest",
  u_preterminal = 0.35,
  u_terminal = 0.05,
  MSF_PT = FALSE,
  MSF_T = TRUE,
  release_mort = c(0, 0),
  vulPT = t(vulPT),
  vulT = t(vulT)
)


### Hatchery ----
n_r <- 2

# Natural mortality HOS
Mjuv_HOS <- array(0, c(nsim, maxage, nyears + proyears, n_r))

# Age 1 survival
#g <- salmonMSE:::CM_M(report_RBT)
#g <- salmonMSE:::CM_M(report_Q)

surv1_HOS <- 1 - c(0.969, 0.969) # 0.969 from life cycle table, assuming traditional survival is 2x of small
Mjuv_HOS[, 1, , 1] <- -log(surv1_HOS[1])
Mjuv_HOS[, 1, , 2] <- -log(surv1_HOS[2])

# Age 2 survival
Mjuv_HOS[, 2, , ] <- M_CTC[2]
Mjuv_HOS[, 3, , ] <- M_CTC[3]
Mjuv_HOS[, 4, , ] <- M_CTC[4]
Mjuv_HOS[, 5, , ] <- M_CTC[5]

p_mature_RS <- array(0, c(nsim, maxage, nyears + proyears, n_r)) # Traditionals mature earlier
p_mature_RS[] <- array(matt_avg[, , sim_samp], c(maxage, n_r, nsim, nyears + proyears)) %>%
  aperm(c(3, 1, 4, 2))

# Sarita releases
sarita_rel <- readxl::read_excel(
  "data/Sarita/2025-05-08_Sarita_ReleaseRep_2000-2024.xlsx",
  sheet = "Actual Release"
)

# 2023 releases
sarita_rel %>%
  summarise(marked = sum(), n = sum(TotalRelease), .by = c(RELEASE_STAGE_NAME, BROOD_YEAR)) %>%
  filter(BROOD_YEAR == 2023)

Hatchery <- new(
  "Hatchery",
  n_r = n_r,
  n_yearling = c(292853, 288745), # Sarita smalls and traditionals in 2023
  n_subyearling = c(0, 0),
  s_prespawn = 0.85,  # Hatchery data, Sarita AHA inputs (Sarita CN AHA inputs.xlsx)
  s_egg_smolt = 0.9,  # Assumed 10 percent mortality shortly after release
  s_egg_subyearling = 1,
  Mjuv_HOS = Mjuv_HOS,
  p_mature_HOS = p_mature_RS,
  stray_external = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 100), maxage, 2),
  gamma = 0.8,  # HSRG standard, Sarita AHA inputs
  m = 1,
  pmax_esc = 1,
  pmax_NOB = 0.50,     # Arbitrary
  ptarget_NOB = 0.11,  # Hatchery data, Sarita AHA inputs
  phatchery = 0.75,    # Arbitrary
  premove_HOS = 0.21,  # Sarita AHA inputs
  fec_brood = fec_Sarita, #rep(3625, maxage), # Hatchery data, Sarita AHA inputs
  fitness_type = c("none", "none")
)

### Historical object ----
# Specify the initial spawners from 2023 escapement of 3000 spawners (Res Doc)
# Conditioning model suggests hatchery dominant system, arbitrarily setting pHOS = 2/3
#Historical <- new(
#  "Historical",
#  HistSpawner_NOS = 1/3 * 3000,
#  HistSpawner_HOS = 2/3 * 3000
#)

# Calculate HistNjuv_NOS (which return year?)
nyears_cm <- 45
Njuv_NOS <- sapply(report_RBT[sim_samp],
                   function(x) x$N[nyears_cm + 1, , 1],
                   simplify = "array")
Njuv_HOS <- sapply(report_RBT[sim_samp], function(x) x$N[nyears_cm + 1, , 2])

# Assume 50-50 ratio of spawners by life history group and release strategy
NOS <- sapply(report_RBT[sim_samp], function(x) x$syear[seq(nyears_cm - nyears + 1, nyears_cm), , 1],
              simplify = "array") %>%
  aperm(c(3, 2, 1))
HOS <- sapply(report_RBT[sim_samp], function(x) x$syear[seq(nyears_cm - nyears + 1, nyears_cm), , 2],
              simplify = "array") %>%
  aperm(c(3, 2, 1))
#colSums(NOS)
#colSums(HOS)
#colSums(HOS)/(colSums(NOS) + colSums(HOS))
#colSums(NOS) + colSums(HOS)

NOS_g <- sapply(1:Bio@n_g, function(g) {
  0.5 * NOS
}, simplify = "array")

HOS_r <- sapply(1:Hatchery@n_r, function(r) {
  0.5 * HOS
}, simplify = "array")

# Get F
FPT <- sapply(report_RBT[sim_samp], function(x) x$FPT[seq(nyears_cm - nyears + 1, nyears_cm)])
FT <- sapply(report_RBT[sim_samp], function(x) x$FT[seq(nyears_cm - nyears + 1, nyears_cm)])

Njuv_NOS <- sapply(1:Bio@n_g, function(g) {
  N <- sapply(report_RBT[sim_samp], getElement, "N", simplify = "array")

  Njuv <- array(0, c(nsim, maxage, nyears + 1))
  Njuv[, 1, ] <- Bio@p_LHG[g] * t(N[seq(nyears_cm - nyears + 1, nyears_cm + 1), 1, 1, ]) # Age - 1
  Njuv[, -1, 1] <- 0.5 * t(N[nyears_cm - nyears + 1, -1, 1, ]) # Year 1

  for (y in seq(2, nyears + 1)) {
    surv <- exp(-t(vulPT) * FPT[y-1, ] - Bio@Mjuv_NOS[, , y-1, g])
    Njuv[, seq(2, maxage), y] <- Njuv[, seq(1, maxage - 1), y-1] * surv[, seq(1, maxage - 1)] *
      (1 - Bio@p_mature[, seq(1, maxage - 1), y-1])
  }
  return(Njuv)
}, simplify = 'array')

Njuv_HOS <- sapply(1:Hatchery@n_r, function(r) {
  N <- sapply(report_RBT[sim_samp], getElement, "N", simplify = "array")

  Njuv <- array(0, c(nsim, maxage, nyears + 1))
  Njuv[, 1, ] <- 0.5 * t(N[seq(nyears_cm - nyears + 1, nyears_cm + 1), 1, 2, ]) # Age - 1
  Njuv[, -1, 1] <- 0.5 * t(N[nyears_cm - nyears + 1, -1, 2, ]) # Year 1

  for (y in seq(2, nyears + 1)) {
    surv <- exp(-t(vulPT) * FPT[y-1, ] - Hatchery@Mjuv_HOS[, , y-1, r])
    Njuv[, seq(2, maxage), y] <- Njuv[, seq(1, maxage - 1), y-1] * surv[, seq(1, maxage - 1)] *
      (1 - Hatchery@p_mature_HOS[, seq(1, maxage - 1), y-1, r])
  }
  return(Njuv)
}, simplify = 'array')

Historical <- new(
  "Historical",
  HistSpawner_NOS = NOS_g,
  HistSpawner_HOS = HOS_r,
  HistNjuv_NOS = Njuv_NOS,
  HistNjuv_HOS = Njuv_HOS,
  HistFPT = array(t(FPT), c(nsim, nyears, 2)),
  HistFT = array(t(FT), c(nsim, nyears, 2))
)




### Habitat ----
fry_surv <- read.csv("data/Sarita/fry_surv.csv")
fry_surv_year <- read.csv("data/Sarita/fry_surv_year.csv")

#g <- ggplot(fry_surv_year, aes(year, x)) +
#  geom_point() +
#  geom_line() +
#  expand_limits(y = 0) +
#  labs(x = "Year", y = "Fry/spawner")

# Average conditions 2017-2023 (x = environmental variable, y = fry/spawner)
#mean(fry_surv_year$x)

get_eggfry_surv <- function(env_series, seed = 342) {
  set.seed(seed)

  fps_sim <- lapply(1:nsim, function(x) {
    data.frame(
      year = seq(1, proyears),
      x = env_series
    ) %>%
      mutate(Simulation = .env$x)
  }) %>%
    bind_rows() %>%
    left_join(fry_surv) %>%
    mutate(fps = rlnorm(nrow(.), log(median), sd_lower)) %>%
    mutate(fpe = fps/3900/Bio@p_female)

  return(fps_sim)

}
env_series <- rep(60, proyears)

sim_surv <- get_eggfry_surv(env_series)
#fps <- reshape2::acast(sim_surv, list("Simulation", "year"), value.var = "fps")
fpe <- reshape2::acast(sim_surv, list("Simulation", "year"), value.var = "fpe")
#matplot(t(fps), typ = 'l')

#png("figures/Sarita_envvar.png", height = 4, width = 6, res = 400, units = "in")
#par(mar = c(5, 4, 1, 1))
#matplot(t(fpe[1:3, ]), typ = 'l', ylab = "Egg-fry survival", xlab = "Projection  year", ylim = c(0, 0.15), lty = 1)
#dev.off()

Habitat <- new(
  "Habitat",
  use_habitat = TRUE,
  fry_rel = "BH",
  fry_prod = 1,
  fry_capacity = Inf,
  fry_sdev = fpe
)


SOM <- new("SOM",
           Name = "Sarita base, 2 LHG, 2 RS",
           nsim = nsim,
           nyears = nyears,
           proyears = proyears,
           seed = 1,
           Bio = Bio,
           Habitat = Habitat,
           Hatchery = Hatchery,
           Harvest = Harvest,
           Historical = Historical)
saveRDS(SOM, "SOM/SOM1.rds")

# Scenario 2 - 90% fed fry
SOM2 <- SOM
SOM2@Hatchery@n_yearling <- c(0.9, 0.1) * sum(Hatchery@n_yearling)
saveRDS(SOM2, "SOM/SOM2.rds")


# Scenario 3 - 90% traditionals
SOM3 <- SOM
SOM3@Hatchery@n_yearling <- c(0.1, 0.9) * sum(Hatchery@n_yearling)
saveRDS(SOM3, "SOM/SOM3.rds")


# Scenario 4 - high fry/spawner
env_series_high <- rep(80, proyears)
sim_surv2 <- get_eggfry_surv(env_series_high)
fpe2 <- reshape2::acast(sim_surv2, list("Simulation", "year"), value.var = "fpe")

#matplot(t(fpe2), typ = 'l', ylab = "Egg-fry survival", xlab = "Projection  year", ylim = c(0, 0.4))


SOM4 <- SOM
SOM4@Habitat@fry_sdev <- fpe2
saveRDS(SOM4, "SOM/SOM4.rds")


# Scenario 5 - 50% pNOB
SOM5 <- SOM
SOM5@Hatchery@ptarget_NOB <- 0.5
saveRDS(SOM5, "SOM/SOM5.rds")



