
library(tidyverse)
library(readxl)

# Quinsam - CWT releases
rel <- readxl::read_excel(
  file.path("data", "Quinsam", "2025-02-17-QuinsamChinook_Analyses_2005-2024.xlsx"),
  sheet = "Releases"
)

cwt_rel <- rel %>%
  summarise(n_CWT = sum(TaggedNum) - sum(ShedTagNum), .by = c(BROOD_YEAR, RELEASE_STAGE_NAME))

g <- ggplot(cwt_rel, aes(BROOD_YEAR, n_CWT, colour = RELEASE_STAGE_NAME)) +
  geom_point() +
  geom_line()

# Quinsam recovery
rec <- readxl::read_excel(
  file.path("data", "Quinsam", "2025-02-17-QuinsamChinook_Analyses_2005-2024.xlsx"),
  sheet = "Expanded"
) %>%
  mutate(is_catch = TotCatch > 0, is_esc = Escape > 0)

table(rec$is_catch, rec$is_esc)
filter(rec, is_catch & is_esc) %>% View()

# Catch area names
cnames <- colnames(rec)
data.frame(
  Name = cnames[grepl("1|2|3", cnames)]
) %>%
  readr::write_csv(file = "Quinsam_fisheries.csv")


# CWT recoveries
cwt <- summarise(rec,
                 n_catch = sum(TotCatch),
                 n_esc = sum(Escape),
                 .by = c(Age, BROOD_YEAR))

g <- ggplot(cwt, aes(BROOD_YEAR, n_catch)) +
  facet_wrap(vars(Age)) +
  geom_line() +
  geom_point()
g

g <- ggplot(cwt, aes(BROOD_YEAR, n_esc)) +
  facet_wrap(vars(Age)) +
  geom_line() +
  geom_point()
g

# CWT by release strategy
cwt_rs <- summarise(rec,
                    n_catch = sum(TotCatch),
                    n_esc = sum(Escape),
                    .by = c(Age, BROOD_YEAR, RELEASE_STAGE_NAME))

g <- ggplot(cwt_rs, aes(BROOD_YEAR, n_catch, colour = RELEASE_STAGE_NAME)) +
  facet_wrap(vars(Age)) +
  geom_line() +
  geom_point()
g

g <- ggplot(cwt_rs, aes(BROOD_YEAR, n_esc, colour = RELEASE_STAGE_NAME)) +
  facet_wrap(vars(Age)) +
  geom_line() +
  geom_point()
g

# Escapement Fed Fry
cwt_rs %>% filter(RELEASE_STAGE_NAME == "Fed Fry", !is.na(Age)) %>%
  reshape2::acast(list("BROOD_YEAR", "Age"), value.var = "n_esc", fill = 0)

# Escapement Traditionals
cwt_rs %>% filter(RELEASE_STAGE_NAME != "Fed Fry", !is.na(Age)) %>%
  reshape2::acast(list("BROOD_YEAR", "Age"), value.var = "n_esc", fill = 0, fun.agg = sum)

# Catch Fed Fry
cwt_rs %>% filter(RELEASE_STAGE_NAME == "Fed Fry", !is.na(Age)) %>%
  reshape2::acast(list("BROOD_YEAR", "Age"), value.var = "n_catch", fill = 0)

# Catch Traditionals
cwt_rs %>% filter(RELEASE_STAGE_NAME != "Fed Fry", !is.na(Age)) %>%
  reshape2::acast(list("BROOD_YEAR", "Age"), value.var = "n_catch", fill = 0, fun.agg = sum)


