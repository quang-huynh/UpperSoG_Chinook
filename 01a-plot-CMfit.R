
# These plots are all available in the markdown report

# Get MCMC values
samp <- readRDS("CM/Sarita_RBT_CM_04.28.25_MCTC.rds")
report <- salmonMSE:::get_report(samp)

# Get data
d <- salmonMSE:::get_CMdata(samp@.MISC$CMfit)

# Plot maturity
g <- salmonMSE:::CM_maturity(report, d, 1979, brood = FALSE, annual = TRUE) +
  ggtitle("Calendar year maturity")
ggsave("figures/Sarita_maturity_calendar.png", g, height = 8, width = 6)

g <- salmonMSE:::CM_maturity(report, d, 1979, brood = TRUE, annual = TRUE) +
  ggtitle("Brood year maturity")
ggsave("figures/Sarita_maturity_brood.png", g, height = 8, width = 6)


# Plot F
g1 <- salmonMSE:::CM_F(report, year1 = 1979)
g2 <- salmonMSE:::CM_F(report, PT = FALSE, year1 = 1979)

g3 <- salmonMSE:::CM_CYER(report, type = "PT", year1 = 1979, at_age = FALSE)
g4 <- salmonMSE:::CM_CYER(report, type = "T", year1 = 1979, at_age = FALSE)

g5 <- salmonMSE:::CM_BYER(report, type = "PT", year1 = 1979, at_age = FALSE)
g6 <- salmonMSE:::CM_BYER(report, type = "T", year1 = 1979, at_age = FALSE)

g7 <- salmonMSE:::CM_CYER(report, type = "all", year1 = 1979, at_age = FALSE)
g8 <- salmonMSE:::CM_BYER(report, type = "all", year1 = 1979, at_age = FALSE)

#g <- ggpubr::ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, ncol = 2, nrow = 4,
#                       labels = paste0("(", 1:8, ")"))
#ggsave("figures/Sarita_ex.png", g, height = 9, width = 6)

g <- ggpubr::ggarrange(g3, g5, g4, g6, g7, g8, ncol = 2, nrow = 3,
                       labels = paste0("(", LETTERS[1:6], ")"))
ggsave("figures/Sarita_ex.png", g, height = 7, width = 6)


# Compare exploitation rates to CTC model
ctc <- readr::read_csv("data/rbt_er.csv")[, -1] %>%
  rename(Year = brood) %>%
  mutate(Age = paste("Age", age))

g <- salmonMSE:::CM_BYER(report, type = "PT", year1 = 1979, at_age = TRUE) +
  facet_wrap(vars(Age), scales = "free_y") +
  geom_line(data = ctc, aes(y = preterminal_er), colour = "red") +
  guides(colour = guide_legend(title = "Model"))
ggsave("figures/Sarita_ex_PT_CTC.png", g, height = 4, width = 6)

g <- salmonMSE:::CM_BYER(report, type = "T", year1 = 1979, at_age = TRUE) +
  facet_wrap(vars(Age), scales = "free_y") +
  geom_line(data = ctc, aes(y = terminal_er), colour = "red")
ggsave("figures/Sarita_ex_T_CTC.png", g, height = 4, width = 6)















report_MPD <- samp@.MISC$CMfit$obj$report()
plot(1:d$Nages, report_MPD$vulPT, typ = "o")
plot(1:d$Nages, report_MPD$vulT, typ = "o")

matplot(report_MPD$mo, typ = "o")

full_year <- seq(1979, 2023)

SAMtool::plot_composition(
  full_year,
  obs = d$cwtesc,
  fit = report_MPD$ebrood,
  annual_yscale = 'raw'
)

SAMtool::plot_composition(
  full_year,
  obs = d$cwtcatPT,
  fit = report_MPD$cbroodPT,
  annual_yscale = 'raw'
)

SAMtool::plot_composition(
  full_year,
  obs = report_MPD$matt,
  annual_yscale = 'raw'
)

SAMtool::plot_composition(
  full_year,
  obs = d$cwtcatT,
  fit = report_MPD$cbroodT,
  annual_yscale = 'raw'
)

par(mar = c(5, 4, 1, 1), oma = rep(0, 4), mfrow = c(1, 1))
plot(full_year, d$obsescape, typ = 'o')
lines(full_year, exp(report_MPD$logpredesc), col = 2, typ = 'o')

plot(full_year, report_MPD$egg, typ = 'o')
plot(full_year, report_MPD$N[1:d$Ldyr, 1, 1], typ = 'o', ylab = "Smolt production")

plot(full_year, report_MPD$FPT, typ = 'o')
plot(full_year, report_MPD$FT, typ = 'o')

# SRR
plot(report_MPD$egg, report_MPD$N[1:d$Ldyr + 1, 1, 1])

memin <- report_MPD$memin
memax <- report_MPD$memax
log_cr <- memax - memin
exp(log_cr)

salmonMSE:::CM_Megg(report, 1979, surv = FALSE)
salmonMSE:::CM_Megg(report, 1979, surv = TRUE)

