

library(tidyverse)

# To update with Upper SoG escapement (Quinsam, Campbell, Adam, Salmon, Nimpkish) data once available


# Load the Excel file
dfa <- readxl::read_excel(
  file.path("data", "SOG_N_Escapement-Salmon_Adam_Nimpkish.xlsx"),
  sheet = "Data")
# Define the prefixes to match
prefixes <- c("Adam", "Nimpkish", "Salmon")

# Filter rows where Description starts with any of the prefixes (case-insensitive)
filtered_dfa <- dfa %>%
  filter(str_starts(Description, prefixes[1]) |
           str_starts(Description, prefixes[2]) |
           str_starts(Description, prefixes[3])) %>%
  mutate(Description = case_when( str_starts(Description, prefixes[1]) ~ prefixes[1],
    str_starts(Description, prefixes[2]) ~ prefixes[2],
    str_starts(Description, prefixes[3]) ~ prefixes[3])) %>%
  rename(year = "Analysis Year") %>%
  rename(escapement="Max Estimate") %>%
  rename(pop="Description") %>%
  mutate(IsUsed = ifelse(year < 2005, "no", "yes")) %>%
  select(year, escapement, pop, IsUsed)



# Load the Excel file
dfb <- readxl::read_excel(
  file.path("data", "Quinsam", "fsar-sog-cn-cq-nuseds.xlsx"),
  sheet = "Data")
# Define the prefixes to match
prefixes <- c("Quinsam", "Campbell")

# Filter rows where Description starts with any of the prefixes (case-insensitive)
filtered_dfb <- dfb %>%
  rename(WaterbodyName = "Waterbody Name") %>%
  filter(str_starts(str_to_lower(WaterbodyName), str_to_lower(prefixes[1])) |
           str_starts(str_to_lower(WaterbodyName), str_to_lower(prefixes[2]))) %>%
  mutate(WaterbodyName = case_when( str_starts(str_to_lower(WaterbodyName), str_to_lower(prefixes[1])) ~ prefixes[1],
                                  str_starts(str_to_lower(WaterbodyName), str_to_lower(prefixes[2])) ~ prefixes[2])) %>%
  rename(year = "Analysis Year") %>%
  rename(escapement="Max Estimate") %>%
  rename(pop="WaterbodyName") %>%
  mutate(IsUsed = ifelse(year < 2005, "no", "yes")) %>%
  select(year, escapement, pop, IsUsed)

filtered_df <- bind_rows(filtered_dfa, filtered_dfb)

  # right_join(
  #   full_table %>% filter(Age == 1) %>% select(BROOD_YEAR),
  #   by = c("year" = "BROOD_YEAR")
  # )

g <- filtered_df %>%
  ggplot(aes(year, escapement)) +
  geom_line() +
  labs(x = "Year", y = "Total escapement") +
  #   scale_shape_manual(values = c(16, 1)) +
  facet_wrap(vars(pop), scales = "free_y", ncol = 3) +
  expand_limits(y = 0) +
  xlim(1985,2024) +
  geom_point(aes(col = IsUsed)) +
  theme(legend.position = 'bottom')

ggsave("figures/total_escapement.png", g, height = 6, width = 7)

# esc <- readr::read_csv("data/R-OUT_infilled_indicators_escapement_timeseries.csv")
#
# g <- esc %>%
#   mutate(river = gsub("_", " ", river) %>% stringr::str_to_title()) %>%
#   mutate(`Infill?` = ifelse(is.na(infill), "No", "Yes")) %>%
#   ggplot(aes(year, escapement)) +
#   geom_line() +
#   geom_point(aes(shape = `Infill?`)) +
#   labs(x = "Year", y = "Total escapement") +
#   scale_shape_manual(values = c(16, 1)) +
#   facet_wrap(vars(river), scales = "free_y", ncol = 3) +
#   expand_limits(y = 0) +
#   theme(legend.position = 'bottom')
# ggsave("figures/total_escapement.png", g, height = 9, width = 7)
