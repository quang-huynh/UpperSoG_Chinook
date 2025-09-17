

library(tidyverse)

# To update with Quinsam escapement data once available


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
