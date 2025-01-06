library(tidyverse)

dat <- readr::read_csv("data/RBT_data_wfisheries.csv")
#problems(dat)
dat[c(1013, 2273), ]


# Preterminal = US and Canada
# Terminal = Canada
table(dat$fishery_type, dat$country)
table(dat$fishery_type, dat$ERA_fishery_name)


unique(dat$TagCode) %>% length()
range(dat$ExpansionFactor %>% as.numeric(), na.rm = TRUE)
dat$ExpansionFactor %>% as.numeric() %>% is.na() %>% mean()

# Escapement by run year
esc_run <- dat %>%
  filter(fishery_type == "escapement") %>%
  summarise(n = sum(AdjustedEstimatedNumber), .by = c(RunYear, Age))

#g <- esc_run %>%
#  ggplot(aes(Age, n)) +
#  facet_wrap(vars(RunYear)) +
#  geom_point() +
#  geom_line() +
#  ggtitle("Escapement at age by run year")

g <- esc_run %>%
  ggplot(aes(RunYear, n)) +
  geom_col(width = 1, colour = "black", fill = "grey80", linewidth = 0.1) +
  facet_wrap(vars(Age)) +
  ggtitle("Escapement at age by run year")
ggsave("figures/esc.png", g, width = 6, height = 4)

esc_annual <- esc_run %>%
  summarise(n = sum(n), .by = RunYear)
g <- esc_annual %>%
  ggplot(aes(RunYear, n)) +
  geom_line() +
  geom_point() +
  labs(y = "CWT escapement")
ggsave("figures/esc_total.png", g, width = 4, height = 3)


# Escapement by brood year
esc_brood <- dat %>%
  filter(fishery_type == "escapement") %>%
  summarise(n = sum(AdjustedEstimatedNumber), .by = c(BroodYear, Age))

g <- esc_brood %>%
  ggplot(aes(BroodYear, n)) +
  geom_col(width = 1, colour = "black", fill = "grey80") +
  facet_wrap(vars(Age)) +
  ggtitle("Escapement at age")

#g <- esc_brood %>%
#  ggplot(aes(Age, n)) +
#  facet_wrap(vars(BroodYear)) +
#  geom_point() +
#  geom_line() +
#  ggtitle("Escapement at age by brood year")

g <- esc_brood %>%
  ggplot(aes(BroodYear, n)) +
  geom_col(width = 1, colour = "black", fill = "grey80", linewidth = 0.1) +
  facet_wrap(vars(Age)) +
  ggtitle("CWT Escapement at age")

# Preterminal catch - by run year and country
pt_country <- dat %>%
  filter(fishery_type == "pre-terminal") %>%
  summarise(n = n(), .by = c(RunYear, Age, country))

g <- pt_country %>%
  ggplot(aes(RunYear, n)) +
  geom_col(width = 1, colour = "black", aes(fill = country), linewidth = 0.1) +
  #facet_wrap(vars(Age)) +
  facet_wrap(vars(Age), scales = "free_y") +
  ggtitle("CWT preterminal catch at age") +
  labs(fill = NULL) +
  #coord_trans(y = "sqrt") +
  theme(legend.position = "bottom")
ggsave("figures/pt_fishery.png", g, width = 6, height = 5.5)


# Terminal catch
term <- dat %>%
  filter(fishery_type == "terminal") %>%
  summarise(n = sum(AdjustedEstimatedNumber), .by = c(RunYear, Age))

g <- term %>%
  ggplot(aes(RunYear, n)) +
  geom_col(width = 1, colour = "black", fill = "grey80", linewidth = 0.1) +
  facet_wrap(vars(Age), scales = "free_y") +
  ggtitle("CWT terminal catch at age")
ggsave("figures/term_fishery.png", g, width = 6, height = 4)
