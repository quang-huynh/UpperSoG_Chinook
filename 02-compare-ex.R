



# Get MCMC values
samp <- readRDS("CM/Sarita_RBT_CM_01.17.25_age5.rds")
report <- salmonMSE:::get_report(samp)

# Plot maturity
CM_maturity <- function(report, d, year1, annual = FALSE, type = c("calendar", "brood")) {
  type <- match.arg(type)

  if (type == "calendar") {
    matt <- sapply(report, getElement, "matt", simplify = 'array')
  } else {
    matt <- sapply(report, function(i) {
      mbrood <- matrix(NA, d$Ldyr, d$Nages)
      for (t in 1:(d$Ldyr)) {
        for (a in 1:d$Nages) {
          if (t+a-1 <= d$Ldyr) {
            mbrood[t, a] <- i$matt[t+a-1, a]
          }
        }
      }
      return(mbrood)
    }, simplify = "array")
  }
  matt_q <- apply(matt, 1:2, quantile, probs = c(0.025, 0.5, 0.975), na.rm = TRUE) %>%
    reshape2::melt() %>%
    mutate(Year = Var2 + year1 - 1) %>%
    rename(Age = Var3)

  bmatt <- data.frame(Age = 1:6, value = d$bmatt)

  if (annual) {
    g <- matt_q %>%
      reshape2::dcast(Age + Var2 + Year ~ Var1) %>%
      ggplot(aes(Age, `50%`)) +
      geom_line() +
      geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.2) +
      facet_wrap(vars(Year)) +
      theme(panel.spacing = unit(0, "in")) +
      #geom_hline(data = bmatt, linetype = 2, aes(yintercept = value, colour = factor(Age))) +
      labs(x = "Age", y = "Proportion mature")
  } else {
    g <- matt_q %>%
      reshape2::dcast(Age + Var2 + Year ~ Var1) %>%
      ggplot(aes(Year, `50%`, fill = factor(Age), colour = factor(Age))) +
      geom_line() +
      geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.2) +
      geom_hline(data = bmatt, linetype = 2, aes(yintercept = value, colour = factor(Age))) +
      labs(x = "Year", y = "Proportion mature", colour = "Age", fill = "Age")
  }

  g

}

g <- CM_maturity(report, d, 1979, TRUE) +
  ggtitle("Calendar year maturity")
ggsave("figures/Sarita_maturity_calendar.png", g, height = 8, width = 6)

g <- CM_maturity(report, d, 1979, TRUE, "brood") +
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

g <- ggpubr::ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, ncol = 2, nrow = 4,
                       labels = paste0("(", 1:8, ")"))
ggsave("figures/Sarita_ex.png", g, height = 9, width = 6)


# Compare exploitation rates to CTC model

ctc <- readr::read_csv("data/rbt_er.csv")[, -1] %>%
  rename(Year = brood) %>%
  mutate(Age = paste("Age", age))

g <- salmonMSE:::CM_BYER(report, type = "PT", year1 = 1979, at_age = TRUE) +
  facet_wrap(vars(Age), scales = "free_y") +
  geom_line(data = ctc, aes(y = preterminal_er), colour = "red")
ggsave("figures/Sarita_ex_PT_CTC.png", g, height = 4, width = 6)

g <- salmonMSE:::CM_BYER(report, type = "T", year1 = 1979, at_age = TRUE) +
  facet_wrap(vars(Age), scales = "free_y") +
  geom_line(data = ctc, aes(y = terminal_er), colour = "red")
ggsave("figures/Sarita_ex_T_CTC.png", g, height = 4, width = 6)

