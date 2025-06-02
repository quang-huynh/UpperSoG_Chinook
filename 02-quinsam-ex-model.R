
library(tidyverse)
library(readxl)

# Quinsam CWT recovery
rec <- readxl::read_excel(
  file.path("data", "Quinsam", "2025-02-17-QuinsamChinook_Analyses_2005-2024.xlsx"),
  sheet = "Expanded"
) %>%
  mutate(is_catch = TotCatch > 0, is_esc = Escape > 0)

#table(rec$is_catch, rec$is_esc)
#filter(rec, is_catch & is_esc) %>% View()

# Catch area names
#cnames <- colnames(rec)
#data.frame(
#  Name = cnames[grepl("1|2|3", cnames)]
#) %>%
#  readr::write_csv(file = "data/Quinsam/Quinsam_fisheries.csv")


# CWT recoveries
#cwt <- summarise(rec,
#                 n_catch = sum(TotCatch),
#                 n_esc = sum(Escape),
#                 .by = c(Age, BROOD_YEAR))
#
#g <- ggplot(cwt, aes(BROOD_YEAR, n_catch)) +
#  facet_wrap(vars(Age), scales = "free_y") +
#  geom_line() +
#  geom_point() +
#  expand_limits(y = 0)
#g
#
#g <- ggplot(cwt, aes(BROOD_YEAR, n_esc)) +
#  facet_wrap(vars(Age), scales = "free_y") +
#  geom_line() +
#  geom_point() +
#  expand_limits(y = 0)
#g

# CWT by release strategy
cwt_rs <- rec %>%
  mutate(RS = ifelse(RELEASE_STAGE_NAME == "Fed Fry", "Fed Fry", "Seapen/Smolt 0+")) %>%
  summarise(
    n_catch = sum(TotCatch),
    n_esc = sum(Escape),
    .by = c(Age, BROOD_YEAR, RS)
  )

g <- cwt_rs %>%
  filter(!is.na(Age), Age %in% seq(2, 5)) %>%
  mutate(Age = paste("Age", Age)) %>%
  ggplot(aes(BROOD_YEAR, n_catch, colour = RS)) +
  facet_grid(vars(RS), vars(Age), scales = "free_y") +
  geom_line() +
  geom_point() +
  labs(x = "Brood Year", y = "CWT catch") +
  expand_limits(y = 0) +
  theme(legend.position = "bottom")
ggsave("figures/Quinsam_CWT_catch.png", g, height = 3.5, width = 6)

g <- cwt_rs %>%
  filter(!is.na(Age), Age %in% seq(2, 5)) %>%
  mutate(Age = paste("Age", Age)) %>%
  ggplot(aes(BROOD_YEAR, n_esc)) +
  facet_grid(vars(RS), vars(Age), scales = "free_y") +
  geom_line() +
  geom_point() +
  labs(x = "Brood Year", y = "CWT escapement", colour = "Release Strategy") +
  expand_limits(y = 0) +
  theme(legend.position = "bottom")
ggsave("figures/Quinsam_CWT_esc.png", g, height = 3.5, width = 6)


# Quinsam - CWT releases
rel <- readxl::read_excel(
  file.path("data", "Quinsam", "2025-02-17-QuinsamChinook_Analyses_2005-2024.xlsx"),
  sheet = "Releases"
)

rel_rs <- rel %>%
  filter(RELEASE_STAGE_NAME %in% c("Fed Fry", "Smolt 0+", "Seapen 0+")) %>%
  mutate(RS = ifelse(RELEASE_STAGE_NAME == "Fed Fry", "Fed Fry", "Seapen/Smolt 0+")) %>%
  summarise(n_CWT = sum(TaggedNum) - sum(ShedTagNum), .by = c(BROOD_YEAR, RS))

g <- ggplot(rel_rs, aes(BROOD_YEAR, n_CWT)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(RS), ncol = 1, scales = "free_y") +
  labs(x = "Brood year", y = "CWT releases") +
  theme(legend.position = "bottom") +
  expand_limits(y = 0)
ggsave("figures/Quinsam_CWT_rel.png", g, height = 5, width = 4)


# Fit model ----
full_table <- expand.grid(
  BROOD_YEAR = seq(min(cwt_rs$BROOD_YEAR), 2023), # 2005 - 2023
  Age = seq(1, 5),
  RS = c("Fed Fry", "Seapen/Smolt 0+")
) %>%
  left_join(cwt_rs)

cwt_catch <- reshape2::acast(full_table, list("BROOD_YEAR", "Age", "RS"), value.var = "n_catch", fill = 0)
cwt_esc <- reshape2::acast(full_table, list("BROOD_YEAR", "Age", "RS"), value.var = "n_esc", fill = 0)

cwt_rel <- left_join(
  full_table %>% filter(Age == 1) %>% select(BROOD_YEAR, RS),
  rel_rs
) %>%
  reshape2::acast(list("BROOD_YEAR", "RS"), fill = 0)

# Sarita esc
esc <- readr::read_csv("data/R-OUT_infilled_indicators_escapement_timeseries.csv") %>%
  filter(river == "sarita_river") %>%
  arrange(year) %>%
  right_join(
    full_table %>% filter(Age == 1, RS == "Fed Fry") %>% select(BROOD_YEAR),
    by = c("year" = "BROOD_YEAR")
  )


# Data object for model
Ldyr <- dim(cwt_esc)[1]
Nages <- 5

mat <- c(0, 0.1, 0.4, 0.95, 1)
vulPT <- c(0, 0.075, 0.9, 0.9, 1)
vulT <- rep(0, Nages)

M_CTC <- -log(1 - c(0.9, 0.3, 0.2, 0.1, 0.1))

#fec_Cowichan <- c(0, 87, 1153, 2780, 2700)
fec_Sarita <- c(0, 3000, 3000, 3600, 4600) # See Res Doc
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
  hatchsurv = 1,
  gamma = 0.8,
  ssum = 0.4,
  fec = fec_Sarita,
  r_matt = 2,
  obsescape = esc$escapement,
  propwildspawn = rep(1, Ldyr),
  hatchrelease = rep(0, Ldyr + 1),
  finitPT = 0.8,
  finitT = 0.8,
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
saveRDS(samp, file = "CM/Quinsam_CM_05.14.25.rds")

samp <- readRDS(file = "CM/Quinsam_CM_05.14.25.rds")
report <- salmonMSE:::get_report(samp)
#shinystan::launch_shinystan(samp)

#g1 <- salmonMSE:::CM_maturity(report, d, year1 = min(full_table$BROOD_YEAR), r = 1) +
#  ggtitle("Quinsam Fed Fry")
#g2 <- salmonMSE:::CM_maturity(report, d, year1 = min(full_table$BROOD_YEAR), r = 2) +
#  ggtitle("Quinsam Smolt 0+")

# Fit to cwt catch
g <- salmonMSE:::CM_fit_CWTcatch(report, d, year1 = 2005, rs_names = rs_names)
ggsave("figures/Quinsam_CWTfit_catch.png", g, height = 4, width = 6)

g <- salmonMSE:::CM_fit_CWTesc(report, d, year1 = 2005, rs_names = rs_names)
ggsave("figures/Quinsam_CWTfit_esc.png", g, height = 4, width = 6)

# Exploitation rate and releases
g1 <- salmonMSE:::CM_CWTrel(d$cwtrelease, year1 = 2005, rs_names = rs_names)
g2 <- salmonMSE:::CM_CWT_ER(report, type = "all", year1 = 2005, rs_names = rs_names)
g <- ggpubr::ggarrange(g1, g2, ncol = 1)
ggsave("figures/Quinsam_CWT_ER.png", g, height = 6, width = 4)

salmonMSE:::reportCM(
  samp,
  rs_names = rs_names, name = "Sarita (Quinsam CWT)", year = unique(full_table$BROOD_YEAR),
  dir = "CM", filename = "Quinsam_05.14"
)

# Predict proportion of escapement
# 2017
g <- sapply(report, function(i) {
  apply(i$ebrood[13, , ], 2, function(x) x/sum(x))
}, simplify = "array") %>%
  apply(1:2, median) %>%
  reshape2::melt() %>%
  mutate(Age = factor(Var1, 5:1), `Release Strategy` = factor(rs_names[Var2], rs_names)) %>%
  ggplot(aes(`Release Strategy`, value, fill = Age)) +
  geom_col(colour = "grey20") +
  labs(y = "Proportion escapement")
ggsave("figures/Quinsam_CWT_esc_prop.png", g, height = 4, width = 4)

# Compare maturity from Quinsam and RBT
samp_RBT <- readRDS(file = "CM/Sarita_RBT_CM_05.20.25.rds")
report_RBT <- salmonMSE:::get_report(samp_RBT)

g_RBT <- salmonMSE:::CM_maturity(report_RBT, d = salmonMSE:::get_CMdata(samp_RBT@.MISC$CMfit),
                                 year1 = 1979, rs_names = "RBT Smolt 0+", annual = TRUE) %>%
  getElement("data") %>%
  filter(Year >= 2005)

rs_names <- c("Quinsam Fed Fry", "Quinsam Smolt 0+")
col_pal <- scales::pal_hue()(2) %>% c("black") %>% structure(names = c(rs_names, "RBT Smolt 0+"))
g <- salmonMSE:::CM_maturity(report, d, year1 = 2005, rs_names = rs_names, annual = TRUE) +
  geom_point(data = g_RBT) +
  geom_line(data = g_RBT) +
  geom_ribbon(data = g_RBT, aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.25, colour = NA) +
  scale_colour_manual(values = col_pal) +
  scale_fill_manual(values = col_pal)
ggsave("figures/Quinsam_CWT_maturity.png", g, height = 5.5, width = 6)

# Apply difference in Quinsam maturity to RBT to model Sarita
#x <- report[[1]]
#y <- report_RBT[[1]]

calc_Sarita_fedfry_matt <- function(x, y, type = c("logit", "ratio")) { # x = report Quinsam, y = report RBT
  type <- match.arg(type)

  # Fed fry/traditional (< 1 means fed fry mature later)
  if (type == "logit") {
    matt_dev <- qlogis(x$matt[, , 1]/x$matt[, , 2])
  } else {
    matt_dev <- x$matt[, , 1]/x$matt[, , 2]
  }
  i <- seq(1979, 2023) %in% seq(2005, 2023)

  matt_new <- array(0, dim(x$matt)) # 1 = Fed fry, 2 = Smolt 0+
  matt_new[, , 2] <- y$matt[i, , 1] # Traditionals

  # Fed fry
  if (type == "logit") {
    matt_new[, , 1] <- plogis(qlogis(y$matt[i, , 1]) + matt_dev)
  } else {
    matt_new[, , 1] <- y$matt[i, , 1] * matt_dev
  }
  matt_new[is.na(matt_new)] <- 0

  return(list(matt = matt_new))
}

matt_Sarita <- Map(calc_Sarita_fedfry_matt, x = report, y = report_RBT, type = "ratio")
saveRDS(matt_Sarita, file = "CM/Sarita_maturity.rds")
matt_Sarita <- readRDS("CM/Sarita_maturity.rds")

rs_names <- c("Sarita Fed Fry (hypothesized)", "RBT Smolt 0+")
col_pal <- c("magenta", "black") %>% structure(names = rs_names)
g <- salmonMSE:::CM_maturity(
  matt_Sarita, list(n_r = 2, bmatt = rep(0, 5), Nages = 5),
  year1 = 2005, rs_names = rs_names, annual = TRUE) +
  scale_colour_manual(values = col_pal) +
  scale_fill_manual(values = col_pal)
ggsave("figures/Sarita_maturity.png", g, height = 5.5, width = 6)

# Take average from last 6 complete brood years
#matt_avg <- sapply(matt_Sarita, function(x) {
#  apply(x$matt[seq(9, 14), , ], 2:3, mean)
#}, simplify = "array")

#matplot(matt_avg[, 1, ], typ = 'l')  # Fed Fry
#matlines(matt_avg[, 2, ], typ = 'l') # Traditionals

