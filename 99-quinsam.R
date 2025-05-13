
library(tidyverse)
library(readxl)

# Quinsam - CWT releases
rel <- readxl::read_excel(
  file.path("data", "Quinsam", "2025-02-17-QuinsamChinook_Analyses_2005-2024.xlsx"),
  sheet = "Releases"
)

cwt_rel <- rel %>%
  filter(RELEASE_STAGE_NAME %in% c("Fed Fry", "Smolt 0+", "Seapen 0+")) %>%
  mutate(RS = ifelse(RELEASE_STAGE_NAME == "Fed Fry", "Fed Fry", "Seapen/Smolt 0+")) %>%
  summarise(n_CWT = sum(TaggedNum) - sum(ShedTagNum), .by = c(BROOD_YEAR, RS))

g <- ggplot(cwt_rel, aes(BROOD_YEAR, n_CWT)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(RS), ncol = 1, scales = "free_y") +
  labs(x = "Brood year", y = "CWT releases") +
  theme(legend.position = "bottom") +
  expand_limits(y = 0)
ggsave("figures/Quinsam_CWT_rel.png", g, height = 5, width = 4)

# Quinsam recovery
rec <- readxl::read_excel(
  file.path("data", "Quinsam", "2025-02-17-QuinsamChinook_Analyses_2005-2024.xlsx"),
  sheet = "Expanded"
) %>%
  mutate(is_catch = TotCatch > 0, is_esc = Escape > 0)

table(rec$is_catch, rec$is_esc)
filter(rec, is_catch & is_esc) %>% View()

# Catch area names
#cnames <- colnames(rec)
#data.frame(
#  Name = cnames[grepl("1|2|3", cnames)]
#) %>%
#  readr::write_csv(file = "data/Quinsam/Quinsam_fisheries.csv")


# CWT recoveries
cwt <- summarise(rec,
                 n_catch = sum(TotCatch),
                 n_esc = sum(Escape),
                 .by = c(Age, BROOD_YEAR))

g <- ggplot(cwt, aes(BROOD_YEAR, n_catch)) +
  facet_wrap(vars(Age), scales = "free_y") +
  geom_line() +
  geom_point() +
  expand_limits(y = 0)
g

g <- ggplot(cwt, aes(BROOD_YEAR, n_esc)) +
  facet_wrap(vars(Age), scales = "free_y") +
  geom_line() +
  geom_point() +
  expand_limits(y = 0)
g

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

