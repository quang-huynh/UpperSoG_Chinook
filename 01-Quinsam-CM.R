
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
  # mutate(RS = "Seapen/Smolt 0+") %>%
  summarise(
    n_catch = sum(TotCatch),
    n_esc = sum(Escape),
    .by = c(Age, BROOD_YEAR)#, RS)
  )


# Quinsam - CWT releases
rel <- readxl::read_excel(
  file.path("data", "Quinsam", "2025-02-17-QuinsamChinook_Analyses_2005-2024.xlsx"),
  sheet = "Releases"
)

rel_rs <- rel %>%
  filter(RELEASE_STAGE_NAME %in% c("Smolt 0+", "Seapen 0+")) %>%
  # mutate(RS = "Seapen/Smolt 0+") %>%
  summarise(n_CWT = sum(TaggedNum) - sum(ShedTagNum), .by = c(BROOD_YEAR))#, RS))

# Set up matrices
full_table <- expand.grid(
  BROOD_YEAR = seq(min(cwt_rs$BROOD_YEAR), 2023), # 2005 - 2023
  Age = seq(1, 5)#6)
  # RS = c( "Seapen/Smolt 0+")
) %>%
  left_join(cwt_rs)

cwt_catch <- reshape2::acast(full_table, list("BROOD_YEAR", "Age"), value.var = "n_catch", fill = 0)
cwt_esc <- reshape2::acast(full_table, list("BROOD_YEAR", "Age"), value.var = "n_esc", fill = 0)

cwt_rel <- left_join(
  full_table %>% filter(Age == 1) %>% select(BROOD_YEAR),# RS),
  rel_rs
) %>%
  reshape2::acast(list("BROOD_YEAR"), fill = 0)

# Total hatchery releases, across all facilities, all release types
rel_Quinsam.x <- readxl::read_excel(
  file.path("data", "Quinsam", "2025-07-23-Quinsam_Chinook_Releases_1970-2024.xlsx"),
  sheet = "Actual Release"
)

rel_Quinsam <- rel_Quinsam.x %>%
  summarise(n_rel = sum(TotalRelease), .by = c(BROOD_YEAR)) %>%
  arrange(BROOD_YEAR)

# Crop to years with CWT data PLUS an extra year (to confirm)
full_year <- data.frame(BROOD_YEAR= seq(min(cwt_rs$BROOD_YEAR),
                                        max(cwt_rs$BROOD_YEAR) + 1))
rel_Quinsam <- left_join(full_year, rel_Quinsam, by = "BROOD_YEAR")
rel_Quinsam$n_rel[is.na(rel_Quinsam$n_rel)] <- 0

# Escapement time-series
pop <- "Quinsam" #Campbell, Adam, Nimpkish, Salmon

if(pop %in% c("Adam", "Nimpkish", "Salmon")) {
  esc <- readxl::read_excel(
    file.path("data", "SOG_N_Escapement-Salmon_Adam_Nimpkish.xlsx"),
    sheet = "Data") %>%
    filter(str_starts(Description, pop)) %>%
    rename(year = "Analysis Year") %>%
    rename(escapement="Max Estimate") %>%
    select (year, escapement) %>%
    right_join(
      full_table %>% filter(Age == 1) %>% select(BROOD_YEAR),
      by = c("year" = "BROOD_YEAR")
    )
}

if(pop %in% c("Quinsam", "Campbell")){
  pop.cap <- str_to_upper(pop)
  esc <- readxl::read_excel(
    file.path("data", "Quinsam", "fsar-sog-cn-cq-nuseds.xlsx"),
    sheet = "Data") %>%
    filter(StAD_Use == 1) %>%
    rename(WaterbodyName = "Waterbody Name") %>%
    filter(str_starts(WaterbodyName, pop.cap)) %>%
    rename(year = "Analysis Year") %>%
    rename(escapement="Max Estimate") %>%
    select (year, escapement) %>%
    right_join(
      full_table %>% filter(Age == 1) %>% select(BROOD_YEAR),
      by = c("year" = "BROOD_YEAR")
    )

}

# Data object for model
Ldyr <- dim(cwt_esc)[1]
Nages <- 5#6

mat <- c(0, 0.1, 0.4, 0.95, 1) # from WCVI = c(0, 0.1, 0.4, 0.95, 1)
vulPT <- c(0, 0.075, 0.9, 0.9, 1)#  from WCVI = c(0, 0.075, 0.9, 0.9, 1)
vulT <- rep(0, Nages)

M_CTC <- -log(1 - c(0.9, 0.3, 0.2, 0.1, 0.1)) # CTC 23-06 p.9; CWT Exploitation Rate analyses

fec_Quinsam <- c(0, 0, 800, 2000, 2500) # Walters and Korman (2024) removing age6=3000; Filipovic et al. (in revision) RPA.
# Eggs/total spawner (not female spawner)

d <- list(
  Nages = Nages,
  Ldyr = Ldyr,
  lht = 1,
  n_r = 1,
  cwtrelease = as.vector(cwt_rel),
  cwtesc = array(round(cwt_esc), c(Ldyr, Nages, 1)),
  cwtcatPT = array(round(cwt_catch), c(Ldyr, Nages, 1)),
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
  obsescape = esc$escapement,
  propwildspawn = rep(1, Ldyr),
  hatchrelease = rel_Quinsam$n_rel, #rep(0, Ldyr + 1),
  finitPT = 0.8, # Walters and Korman (2024)
  finitT = 0.8,  # Walters and Korman (2024)
  cwtExp = 0.1 # Sarita used 1 #Walters and Korman (2024) used 0.1
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
samp <- sample_CM(fit, chains = 4, cores = 4, iter = 10000, thin = 5)
saveRDS(samp, file = "CM/Quinsam_CM_09.29.25.rds")

samp <- readRDS(file = "CM/Quinsam_CM_09.29.25.rds")
report <- salmonMSE:::get_report(samp)
d <- salmonMSE:::get_CMdata(samp@.MISC$CMfit)
#shinystan::launch_shinystan(samp)

rs_names <- c("Smolt 0+")
salmonMSE::report_CM(
  samp,
  rs_names = rs_names, name = "Quinsam", year = unique(full_table$BROOD_YEAR),
  dir = "CM", filename = "Quinsam_09.29"
)
