

library(salmonMSE)

gr <- local({

  gr <- expand.grid(
    Ctarget = c(750, 1000, 1250),
    pNOB_target = c(0.5, 0.75, 1)
  ) %>%
    mutate(scenario = paste(Ctarget, "C, pNOB =", pNOB_target), ref = "grid")

  g2 <- data.frame(
    Ctarget = 1000,
    pNOB_target = 0.5,
    scenario = c("90% Traditionals from (2)", "90% Fed Fry from (2)", "High Surv. from (2)", "HiSurvNoHarvHatch", "NoHarv.NoHatch."),
    ref = "additional"
  )
  gr <- rbind(gr, g2) %>%
    mutate(scenario = paste0("(", 1:nrow(.), ") ", scenario))
  gr[-c(13, 14), ]
})

name <- gr$scenario
nOM <- nrow(gr)

SMSE_list <- lapply(1:nrow(gr), function(i) {
  SMSE <- readRDS(file.path("SMSE", paste0("Sarita", i, ".rds")))
  return(SMSE)
})


#### Plot maturity and vulnerability ----
png("figures/SMSE/maturity_vul.png", height = 6, width = 3, units = "in", res = 400)
par(mfrow = c(3, 1), mar = c(5, 4, 1, 1))

SOM <- SMSE_list[[1]]@Misc$SOM
salmonMSE:::plot_Mjuv_RS(SOM@Hatchery[[1]]@p_mature_HOS[, , 1, ],
                         RS_names = c("Fed Fry", "Traditionals"), ylab = "Proportion mature")

salmonMSE:::plot_SOM(SOM@Harvest[[1]], "vulPT",
                     type = "age", nsim = SOM@nsim, maxage =  SOM@Bio[[1]]@maxage,
                     nyears = SOM@nyears, proyears = SOM@proyears,
                     ylab = "Juvenile fishery vulnerability")

salmonMSE:::plot_SOM(SOM@Harvest[[1]], "vulT",
                     type = "age", nsim = SOM@nsim, maxage =  SOM@Bio[[1]]@maxage,
                     nyears = SOM@nyears, proyears = SOM@proyears,
                     ylab = "Terminal fishery vulnerability")

dev.off()

#### Plot time series ----
.ts_fn <- function(SMSE, name, var) {
  require(salmonMSE)

  if (var == "Brood") {
    out <- apply(SMSE@NOB + SMSE@HOB, 3, quantile, c(0.025, 0.5, 0.975))
  } else {
    out <- plot_statevar_ts(SMSE, var, figure = FALSE, quant = TRUE)
  }

  reshape2::melt(out) %>%
    rename(Year = Var2) %>%
    mutate(name = name) %>%
    reshape2::dcast(Year + name ~ Var1)
}

ts_fn <- function(SMSE_list, name, var) {
  d <- Map(.ts_fn, SMSE = SMSE_list, name = name, MoreArgs = list(var = var)) %>%
    bind_rows() %>%
    filter(!is.na(`50%`), `50%` > 0) %>%
    mutate(var = .env$var)

  g <- ggplot(d, aes(Year, `50%`, colour = name, fill = name)) +
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.25, colour = NA, linetype = 2) +
    geom_line() +
    labs(x = "Year", y = var, colour = "Scenario", fill = "Scenario")
  g
}


g1 <- ts_fn(SMSE_list, name, var = "Total Spawners") +
  coord_cartesian(ylim = c(0, 8000)) +
  guides(colour = guide_legend(ncol = 2), fill = guide_legend(ncol = 2))

g2 <- ts_fn(SMSE_list, name, var = "pHOS_effective") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(y = expression(pHOS[eff]))

#g2 <- ts_fn(SMSE_list, name, var = "NOS") +
#  coord_cartesian(ylim = c(0, 5000))

#g2 <- ts_fn(SMSE_list, name, var = "HOS") +
#  coord_cartesian(ylim = c(0, 3000))

g3 <- ts_fn(SMSE_list, name, var = "PNI")

g4 <- ts_fn(SMSE_list, name, var = "p_wild") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(y = "pWILD")

g5 <- ts_fn(SMSE_list, name, var = "Brood") +
  coord_cartesian(ylim = c(300, 600))

g6 <- ts_fn(SMSE_list, name, var = "pNOB") +
  coord_cartesian(ylim = c(0, 1))

g <- ggpubr::ggarrange(g1, g2, g3, g4, g5, g6, ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")
ggsave("figures/SMSE/ts.png", g, height = 7, width = 6)

#### Plot results by individual scenario -----
png("figures/SMSE/spawners_prop.png", height = 6, width = 6, units = "in", res = 400)
par(mfrow = c(3, 3), mar = c(5, 4, 1, 1))
for (i in 1:nOM) {
  plot_spawners(SMSE_list[[i]])
  box()
  title(name[i])
}
dev.off()

png("figures/SMSE/spawners.png", height = 6, width = 6, units = "in", res = 400)
par(mfrow = c(3, 3), mar = c(5, 4, 1, 1))
for (i in 1:nOM) {
  plot_spawners(SMSE_list[[i]], prop = FALSE, ylim = c(0, 8000))
  title(name[i])
}
dev.off()

#par(mfrow = c(3, 2))
#for (i in 1:nOM) {
#  plot_escapement(SMSE_list[[i]], ylim = c(0, 1))
#  title(name[i])
#}

#par(mfrow = c(3, 2))
#for (i in 1:nOM) {
#  plot_fitness(SMSE_list[[i]], ylim = c(0, 1))
#  title(name[i])
#}

#par(mfrow = c(3, 2))
#for (i in 1:nOM) {
#  plot_fishery(SMSE_list[[i]], ylim = c(0, 3500))
#  title(name[i])
#}

png("figures/SMSE/RS_HOS.png", height = 6, width = 6, units = "in", res = 400)
par(mfrow = c(3, 3), mar = c(5, 4, 1, 1))
for (i in 1:nOM) {
  plot_RS(SMSE_list[[i]], var = "HOS", type = "abs", name = c("Fed Fry", "Traditionals"), ylim = c(0, 7000))
  title(name[i])
}
dev.off()

png("figures/SMSE/RS_Rel.png", height = 6, width = 6, units = "in", res = 400)
par(mfrow = c(3, 3), mar = c(5, 4, 1, 1))
for (i in 1:nOM) {
  plot_RS(SMSE_list[[i]], var = "Smolt", type = "abs", name = c("Fed Fry", "Traditionals"))
  title(name[i])
}
dev.off()

png("figures/SMSE/RS_Esc.png", height = 6, width = 6, units = "in", res = 400)
par(mfrow = c(3, 3), mar = c(5, 4, 1, 1))
for (i in 1:nOM) {
  plot_RS(SMSE_list[[i]], var = "Esc", type = "abs", name = c("Fed Fry", "Traditionals"), ylim = c(0, 7000))
  title(name[i])
}
dev.off()

png("figures/SMSE/LHG_NOS.png", height = 6, width = 6, units = "in", res = 400)
par(mfrow = c(3, 3), mar = c(5, 4, 1, 1))
for (i in 1:nOM) {
  plot_LHG(SMSE_list[[i]], var = "NOS", type = "abs", name = c("Early Smalls", "Late Larges"), ylim = c(0, 1500))
  title(name[i])
}
dev.off()

png("figures/SMSE/LHG_Smolt.png", height = 6, width = 6, units = "in", res = 400)
par(mfrow = c(3, 3), mar = c(5, 4, 1, 1))
for (i in 1:nOM) {
  plot_LHG(SMSE_list[[i]], var = "Smolt", type = "abs", name = c("Early Smalls", "Late Larges"), ylim = c(0, 7e5))
  title(name[i])
}
dev.off()


#### Performance metrics ----
y <- 29

PNI <- sapply(SMSE_list, function(x) x@PNI[, 1, y]) %>%
  reshape2::melt() %>%
  rename(Simulation = Var1, PNI = value) %>%
  mutate(scenario = name[Var2])
NOS <- sapply(SMSE_list, function(x) rowSums(x@NOS[, 1, , y])) %>%
  reshape2::melt() %>%
  rename(Simulation = Var1, NOS = value) %>%
  mutate(scenario = name[Var2])

Fitness <- sapply(SMSE_list, function(x) x@fitness[, 1, 1, y]) %>%
  reshape2::melt() %>%
  rename(Simulation = Var1, Fitness = value) %>%
  mutate(scenario = name[Var2])

TS <- sapply(SMSE_list, function(x) {
  TS_a <- x@NOS[, 1, , y] + x@HOS[, 1, , y]
  rowSums(TS_a)
}) %>%
  reshape2::melt() %>%
  rename(Simulation = Var1, Spawners = value) %>%
  mutate(scenario = name[Var2])

MA <- sapply(SMSE_list, function(x) {
  TS_a <- x@NOS[, 1, , y] + x@HOS[, 1, , y]
  MA <- apply(TS_a, 1, function(w) weighted.mean(x = 1:5, w = w))
  return(MA)
}) %>%
  reshape2::melt() %>%
  rename(Simulation = Var1, `Mean age` = value) %>%
  mutate(scenario = name[Var2])
p_wild <- sapply(SMSE_list, function(x) x@p_wild[, , y]) %>%
  reshape2::melt() %>%
  rename(Simulation = Var1, pWILD = value) %>%
  mutate(scenario = name[Var2])

pHOSeff <- sapply(SMSE_list, function(x) x@pHOS_effective[, , y]) %>%
  reshape2::melt() %>%
  rename(Simulation = Var1, pHOSeff = value) %>%
  mutate(scenario = name[Var2])

pNOBeff <- sapply(SMSE_list, function(x) x@pNOB[, , y]) %>%
  reshape2::melt() %>%
  rename(Simulation = Var1, pNOBeff = value) %>%
  mutate(scenario = name[Var2])

Brood <- sapply(SMSE_list, function(x) x@NOB[, , y] + x@HOB[, , y]) %>%
  reshape2::melt() %>%
  rename(Simulation = Var1, Brood = value) %>%
  mutate(scenario = name[Var2])

K <- sapply(SMSE_list, function(x) {
  vars <- c("KPT_NOS", "KPT_HOS", "KT_NOS", "KT_HOS")
  val <- lapply(vars, function(i) slot(x, i))
  K <- Reduce("+", val)[, , y]
  return(K)
}) %>%
  reshape2::melt() %>%
  rename(Simulation = Var1, `Total Catch` = value) %>%
  mutate(scenario = name[Var2])

KT <- sapply(SMSE_list, function(x) {
  vars <- c("KT_NOS", "KT_HOS")
  val <- lapply(vars, function(i) slot(x, i))
  K <- Reduce("+", val)[, , y]
  return(K)
}) %>%
  reshape2::melt() %>%
  rename(Simulation = Var1, `ESSR Catch` = value) %>%
  mutate(scenario = name[Var2])

P_Sgen <- sapply(SMSE_list, function(x, Sgen = 250) {
  val <- rowSums(x@NOS[, 1, , y]) >= Sgen
  mean(val)
})

P_Smsy85 <- sapply(SMSE_list, function(x, SMSY = 560) {
  val <- rowSums(x@NOS[, 1, , y]) >= 0.85 * SMSY
  mean(val)
})

val_sim <- list(PNI, #NOS,
                TS, pHOSeff, p_wild, MA, Fitness, Brood, KT, pNOBeff) %>%
  Reduce(left_join, .) %>%
  select(!Var2) %>%
  reshape2::melt(id.vars = c("Simulation", "scenario")) %>%
  summarise(m = mean(value),
            median = median(value, na.rm = TRUE),
            lwr = quantile(value, 0.25, na.rm = TRUE),
            upr = quantile(value, 0.75, na.rm = TRUE),
            .by = c(scenario, variable)) %>%
  left_join(gr, by = "scenario") %>%
  mutate(scenario = factor(scenario, rev(name)))


plot_dotplot <- function(val_sim) {
  g <- val_sim %>%
    ggplot(aes(scenario, median, ymin = lwr, ymax = upr, shape = factor(Ctarget), colour = factor(pNOB_target))) +
    facet_wrap(vars(variable), scales = "free_x", strip.position = "top") +
    geom_point() +
    geom_linerange() +
    coord_flip()

  g
}
g <- plot_dotplot(val_sim) +
  scale_shape_manual(values = c(1, 4, 16)) +
  #theme(strip.placement = "outside") +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(ncol = 1), shape = guide_legend(ncol = 1)) +
  labs(x = NULL, y = NULL, shape = "Catch target", colour = "pNOB target")
ggsave("figures/SMSE/performance_metrics.png", g, width = 6, height = 7)



val_prob <- data.frame(
  scenario = name,
  P_250 = P_Sgen,
  P_476 = P_Smsy85
) %>%
  reshape2::melt(id.var = "scenario") %>%
  rename(median = value)

d <- val_sim %>%
  select(scenario, variable, median) %>%
  rbind(val_prob)

plot_table <- function(df, padding = 0.52) {

  d <- df %>%
    mutate(txt = signif(median, 3)) %>%
    mutate(val_rel = median/max(median),
           val_0_1 = (median - min(median)) / (max(median) - min(median)),
           .by = variable)

  g <- ggplot(d, aes(variable, scenario)) +
    geom_tile(aes(fill = val_rel), alpha = 0.6, color = "white") +
    geom_text(aes(label = txt), size = ggplot2::rel(3)) +
    guides(fill = "none") +
    labs(x = NULL, y = NULL) +
    coord_cartesian(
      expand = FALSE,
      xlim = range(as.numeric(d$variable)) + c(-padding, padding),
      ylim = range(as.numeric(d$scenario)) + c(-padding - 0.01, padding + 0.01)
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(color = "grey10", angle = 90),
      strip.placement = "outside",
      strip.background = element_blank()
    ) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(labels = levels(d$scenario)) +
    scale_fill_gradient2(low = "deeppink", high = "green4", mid = "white", limits = c(0, 1), midpoint = 0.5)
  g
}
g <- plot_table(d)
ggsave("figures/SMSE/performance_table_full.png", g, width = 7, height = 3.5)

d_sens <- d %>%
  filter(!variable %in% c("P_250", "P_476"), scenario %in% name[c(2, 10:12)]) %>%
  mutate(scenario = factor(scenario, levels = rev(name[c(2, 10:12)])))
g <- plot_table(d_sens)
ggsave("figures/SMSE/performance_table_sens.png", g, width = 5.5, height = 2)

g <- salmonMSE::plot_tradeoff(
  val_sim %>% filter(variable == "Spawners", ref == "grid") %>% select(lwr, median, upr) %>% as.matrix(),
  val_sim %>% filter(variable == "PNI", ref == "grid") %>% select(lwr, median, upr) %>% as.matrix(),
  val_sim %>% filter(variable == "Spawners", ref == "grid") %>% pull(Ctarget) %>% factor(),
  val_sim %>% filter(variable == "Spawners", ref == "grid") %>% pull(pNOB_target) %>% factor(),
  xlab = "Total spawners",
  ylab = "PNI",
  x1lab = "Catch target",
  x2lab = "pNOB target"
) +
  scale_shape_manual(values = c(1, 4, 16))
ggsave("figures/SMSE/tradeoff_PNI.png", g, width = 5, height = 3)

# PNI decision table
PNI_dt <- val_sim %>% filter(variable == "PNI", ref == "grid")
g <- salmonMSE::plot_decision_table(
  x = PNI_dt$Ctarget,
  y = PNI_dt$pNOB_target,
  z = PNI_dt$median,
  title = "Median PNI",
  xlab = "ESSR catch target",
  ylab = "pNOB target"
)
ggsave("figures/SMSE/decisiontable_PNI.png", g, width = 3, height = 3)
