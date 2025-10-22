# Plot of hatchery releases

# Total hatchery releases from Quinsamand Campbell release sites, all release types
hatchrel.1 <- readxl::read_excel(
  file.path("data", "Quinsam", "2025-07-23-Quinsam_Chinook_Releases_1970-2024.xlsx"),
  sheet = "Actual Release"
)

# Suggestion from Brendan Zoehner, SEP, to include all Quinsam sites and Cold
# Creek. Discovery Pass and Orange Pt are seapen releases, likely impacting
# Quinsam and Campbell (8 Oct 2025)
rel_Quinsam <- hatchrel.1 %>%
  filter(str_starts(RELEASE_SITE_NAME, "Quinsam") |
           str_starts(RELEASE_SITE_NAME, "Cold") |
           str_starts(RELEASE_SITE_NAME, "Discovery") |
           str_starts(RELEASE_SITE_NAME, "Orange")) %>%
  summarise(n_rel = sum(TotalRelease), .by = c(BROOD_YEAR)) %>%
  arrange(BROOD_YEAR)%>%
  mutate(pop = "Quinsam")



# Suggestion from Brendan Zoehner, SEP, to include all Campbell sites,
# Elk R Chanel sites and Second Is (8 Oct 2025)
rel_Campbell <- hatchrel %>%
  filter(str_starts(RELEASE_SITE_NAME, "Campbell") |
           str_starts(RELEASE_SITE_NAME, "Elk") |
           str_starts(RELEASE_SITE_NAME, "Second")) %>% # |
  # str_starts(RELEASE_SITE_NAME, "Discovery") |
  # str_starts(RELEASE_SITE_NAME, "Orange")) %>%
  summarise(n_rel = sum(TotalRelease), .by = c(BROOD_YEAR)) %>%
  arrange(BROOD_YEAR)%>%
  mutate(pop = "Campbell")

# Load hatchery releases for Salmon
hatchrel.2 <- readxl::read_excel(
  file.path("data", "Quinsam", "2025-10-08-SalmonRiverChinookReleases-AllYears.xlsx"),
  sheet = "Actual Release"
)

rel_Salmon <- hatchrel.2 %>%
  filter(str_starts(RELEASE_SITE_NAME, "Salmon R/JNST") |
           str_starts(RELEASE_SITE_NAME, "Salmon R Up/JNST")) %>%
  summarise(n_rel = sum(TotalRelease), .by = c(BROOD_YEAR)) %>%
  arrange(BROOD_YEAR)%>%
  mutate(pop = "Salmon")

# Load hatchery releases for Nimpkish(Woss)
hatchrel.3 <- readxl::read_excel(
  file.path("data", "Quinsam", "2025-07-23-NimpkishWoss_Chinook_Releases_1970-2024.xlsx"),
  sheet = "Actual Release"
)

rel_Woss <- hatchrel.3 %>%
  filter(str_starts(RELEASE_SITE_NAME, "Woss R") |
           str_starts(RELEASE_SITE_NAME, "Woss Lk") |
           str_starts(RELEASE_SITE_NAME, "Nimpkish R") |
           str_starts(RELEASE_SITE_NAME, "Nimpkish R Up")) %>%
  summarise(n_rel = sum(TotalRelease), .by = c(BROOD_YEAR)) %>%
  arrange(BROOD_YEAR) %>%
  mutate(pop = "Woss")

hatchrel_combined <- bind_rows(rel_Quinsam, rel_Campbell, rel_Salmon, rel_Woss)

# Crop to years with CWT recovery data from Quinsam \
rec <- readxl::read_excel(
  file.path("data", "Quinsam", "2025-02-17-QuinsamChinook_Analyses_2005-2024.xlsx"),
  sheet = "Expanded"
) %>%
  mutate(is_catch = TotCatch > 0, is_esc = Escape > 0)


# CWT by release strategy, removing fed fry (only traditionals: seapen 0+ and smolt 0+)
startYear <- rec %>%
  filter(RELEASE_STAGE_NAME %in% c("Seapen 0+", "Smolt 0+")) %>%
  summarize(start=min(BROOD_YEAR))
lastYear <- rec %>%
  filter(RELEASE_STAGE_NAME %in% c("Seapen 0+", "Smolt 0+")) %>%
  summarize(last=max(BROOD_YEAR))

hatchrel_combined <- hatchrel_combined %>%
  mutate(IsUsed = ifelse(BROOD_YEAR < startYear$start |
                           BROOD_YEAR >lastYear$last, "no", "yes")) %>%
  rename(year = BROOD_YEAR) %>%
  rename(releases = n_rel)

hatchrel_combined$releases[is.na(hatchrel_combined$releases)] <- 0

g <- hatchrel_combined %>%
  ggplot(aes(year, releases)) +
  geom_line() +
  labs(x = "Year", y = "Hatchery releases (Seapen 0+, Smolt 0+") +
  facet_wrap(vars(pop), scales = "free_y", ncol = 3) +
  expand_limits(y = 0) +
  xlim(1985,2024) +
  geom_point(aes(col = IsUsed)) +
  theme(legend.position = 'bottom')

ggsave("figures/total_hatchery_releases.png", g, height = 6, width = 7)
