

library(salmonMSE)

#### Load results ----
nOM <- 5
name <- paste0("(", 1:nOM, ") ", c("Base", "90% Fed Fry", "90% Trads", "High fry/spawner", "High pNOB"))

SMSE_list <- lapply(seq_len(nOM), function(i) {
  SMSE <- readRDS(file.path("SMSE", paste0("Sarita", i, ".rds")))
  return(SMSE)
})

#### Plot time series ----
.ts_fn <- function(SMSE, name, var) {
  require(salmonMSE)

  out <- plot_statevar_ts(SMSE, var, figure = FALSE, quant = TRUE)

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


g <- ts_fn(SMSE_list, name, var = "Total Spawners") +
  coord_cartesian(ylim = c(0, 4000))

g1 <- ts_fn(SMSE_list, name, var = "NOS") +
  coord_cartesian(ylim = c(0, 3000)) +
  guides(colour = guide_legend(ncol = 2), fill = guide_legend(ncol = 2))

g2 <- ts_fn(SMSE_list, name, var = "HOS") +
  coord_cartesian(ylim = c(0, 3000))

g3 <- ts_fn(SMSE_list, name, var = "PNI")

g4 <- ts_fn(SMSE_list, name, var = "p_wild") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(y = "PWILD")


g <- ggpubr::ggarrange(g1, g2, g3, g4, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
ggsave("figures/SMSE/ts.png", g, height = 5, width = 6)

#### Plot results by individual scenario -----
png("figures/SMSE/spawners_prop.png", height = 6, width = 6, units = "in", res = 400)
par(mfrow = c(3, 2), mar = c(5, 4, 1, 1))
for (i in 1:nOM) {
  plot_spawners(SMSE_list[[i]])
  box()
  title(name[i])
}
dev.off()

png("figures/SMSE/spawners.png", height = 6, width = 6, units = "in", res = 400)
par(mfrow = c(3, 2), mar = c(5, 4, 1, 1))
for (i in 1:nOM) {
  plot_spawners(SMSE_list[[i]], prop = FALSE, ylim = c(0, 3500))
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
par(mfrow = c(3, 2), mar = c(5, 4, 1, 1))
for (i in 1:nOM) {
  plot_RS(SMSE_list[[i]], var = "HOS", type = "abs", name = c("Fed Fry", "Traditionals"), ylim = c(0, 9000))
  title(name[i])
}
dev.off()

png("figures/SMSE/RS_Rel.png", height = 6, width = 6, units = "in", res = 400)
par(mfrow = c(3, 2), mar = c(5, 4, 1, 1))
for (i in 1:nOM) {
  plot_RS(SMSE_list[[i]], var = "Smolt", type = "abs", name = c("Fed Fry", "Traditionals"))
  title(name[i])
}
dev.off()

png("figures/SMSE/RS_Esc.png", height = 6, width = 6, units = "in", res = 400)
par(mfrow = c(3, 2), mar = c(5, 4, 1, 1))
for (i in 1:nOM) {
  plot_RS(SMSE_list[[i]], var = "Esc", type = "abs", name = c("Fed Fry", "Traditionals"), ylim = c(0, 9000))
  title(name[i])
}
dev.off()

png("figures/SMSE/LHG_NOS.png", height = 6, width = 6, units = "in", res = 400)
par(mfrow = c(3, 2), mar = c(5, 4, 1, 1))
for (i in 1:nOM) {
  plot_LHG(SMSE_list[[i]], var = "NOS", type = "abs", name = c("Early Smalls", "Late Larges"), ylim = c(0, 2000))
  title(name[i])
}
dev.off()

png("figures/SMSE/LHG_Smolt.png", height = 6, width = 6, units = "in", res = 400)
par(mfrow = c(3, 2), mar = c(5, 4, 1, 1))
for (i in 1:nOM) {
  plot_LHG(SMSE_list[[i]], var = "Smolt", type = "abs", name = c("Early Smalls", "Late Larges"), ylim = c(0, 7e5))
  title(name[i])
}
dev.off()


#### Performance metrics ----
y <- 49

PNI <- sapply(SMSE_list, function(x) x@PNI[, 1, y]) %>%
  reshape2::melt() %>%
  rename(Simulation = Var1, PNI = value) %>%
  mutate(scenario = name[Var2])
NOS <- sapply(SMSE_list, function(x) rowSums(x@NOS[, 1, , y])) %>%
  reshape2::melt() %>%
  rename(Simulation = Var1, NOS = value) %>%
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
  rename(Simulation = Var1, PWILD = value) %>%
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
  rename(Simulation = Var1, `Catch Term.` = value) %>%
  mutate(scenario = name[Var2])

P_Sgen <- sapply(SMSE_list, function(x) {
  val <- rowSums(x@NOS[, 1, , y]) > 250
  mean(val)
})

P_Smsy85 <- sapply(SMSE_list, function(x) {
  val <- rowSums(x@NOS[, 1, , y]) > 0.85*560
  mean(val)
})

val_sim <- list(PNI, NOS, TS, MA, p_wild, K, KT) %>%
  Reduce(left_join, .) %>%
  select(!Var2) %>%
  reshape2::melt(id.vars = c("Simulation", "scenario")) %>%
  summarise(m = mean(value),
            median = median(value),
            lwr = quantile(value, 0.025),
            upr = quantile(value, 0.975),
            .by = c(scenario, variable)) %>%
  mutate(scenario = factor(scenario, rev(name)))

g <- val_sim %>%
  ggplot(aes(scenario, median, ymin = lwr, ymax = upr)) +
  facet_wrap(vars(variable), scales = "free_x", strip.position = "top") +
  geom_point() +
  geom_linerange() +
  coord_flip() +
  #theme(strip.placement = "outside") +
  labs(x = NULL, y = NULL)
ggsave("figures/SMSE/performance_metrics.png", g, width = 6, height = 4)

val_prob <- data.frame(
  scenario = name,
  P_250 = P_Sgen,
  P_476 = P_Smsy85
) %>%
  reshape2::melt() %>%
  rename(median = value)

d <- val_sim %>%
  select(scenario, variable, median) %>%
  rbind(val_prob) %>%
  mutate(txt = signif(median, 3)) %>%
  mutate(val_rel = median/max(median),
         val_0_1 = (median - min(median)) / (max(median) - min(median)),
         .by = variable)

padding <- 0.52
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
    axis.text.x = element_text(color = "grey10"),
    strip.placement = "outside",
    strip.background = element_blank()
  ) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(labels = levels(d$scenario)) +
  scale_fill_gradient2(low = "deeppink", high = "green4", mid = "white", limits = c(0, 1), midpoint = 0.5)
ggsave("figures/SMSE/performance_table.png", g, width = 7, height = 2)

g <- salmonMSE::plot_tradeoff(
  val_sim %>% filter(variable == "Spawners") %>% select(lwr, median, upr) %>% as.matrix(),
  val_sim %>% filter(variable == "PWILD") %>% select(lwr, median, upr) %>% as.matrix(),
  name,
  factor(rep(1, length(name))),
  xlab = "Spawners",
  ylab = "PWILD",
  x1lab = "Scenario",
  x2lab = NULL
) +
  guides(shape = "none") +
  expand_limits(x = 0)
ggsave("figures/SMSE/tradeoff.png", g, width = 5, height = 3)
