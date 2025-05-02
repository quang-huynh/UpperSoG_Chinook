
library(tidyverse)

# Total Sarita ESSR
return <- readr::read_csv(
  file.path("data", "ESSR", "River_Returns_2025_04_28_Sarita.csv")
)

ret <- return %>%
  summarise(N4 = sum(`04Excess To Spawning Req (ESSR)`, na.rm = TRUE),
            N3 = sum(`03Given To First Nations (FSC)`, na.rm = TRUE),
            .by = `Recovery Year`) %>%
  mutate(ESSR_catch = N4 + N3)

plot(N ~ `Recovery Year`, ret, typ = 'o')


# Clipped fish - per Lian's email
clip <- tibble::tibble(
  `Recovery Year` = seq(2020, 2024),
  N_noclip = c(26, 5, 0, 38, 3)
) %>% left_join(ret, by = "Recovery Year") %>%
  mutate(N_clip = ESSR_catch - N_noclip)

# CWT sampling (2023 not available)
samp_2020 <- readr::read_csv(
  file.path("data", "ESSR", "Sarita_ESSR_CWT_Batch_BatchId_7414_2020.csv")
)
samp_2021 <- readr::read_csv(
  file.path("data", "ESSR", "Sarita_ESSR_CWT_Batch_BatchId_7641_2021.csv")
)
samp_2022 <- readr::read_csv(
  file.path("data", "ESSR", "Sarita_ESSR_CWT_Batch_BatchId_7951_2022a.csv")
)
samp_2022_hfn <- readr::read_csv(
  file.path("data", "ESSR", "Sarita_HFN_ESSR_CWT_Batch_BatchId_7887_2022b.csv")
)
samp_2024 <- readr::read_csv(
  file.path("data", "ESSR", "Sarita_ESSR_CWT_Batch_BatchId_2024.csv")
)

samp <- rbind(
  samp_2020,
  samp_2021,
  samp_2022,
  samp_2022_hfn,
  samp_2024
)

#unique(samp$Status)
#unique(samp$TagCondition)
#table(samp$Status, samp$TagCondition)


# ESSR summary with total catch, number of clipped fish, and CWTs
cwt <- samp %>%
  summarise(CWT_samp = n(),
            n_CWT = sum(TagCondition == "Normal"),
            .by = `RecoveryYear`)

essr <- ret %>%
  left_join(clip) %>%
  left_join(cwt, by = c("Recovery Year" = "RecoveryYear")) %>%
  filter(`Recovery Year` >= 2020)

essr %>%
  filter(`Recovery Year` >= 2020) %>%
  readr::write_csv(file = "Sarita_ESSR_CWT.csv")
