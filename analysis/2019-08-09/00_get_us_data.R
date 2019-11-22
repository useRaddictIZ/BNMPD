data_all <- haven::read_dta("./data/uspp_aggregated_final.dta")
states_all <- unique(data_all$state)
years_all <- unique(data_all$year)
save_cum_cap <- rep(list(list()), times = length(states_all))
for (i in 1:length(states_all)) {
  save_cum_cap[[i]] <- as.vector(as.data.frame(data_all %>%
    filter(state == states_all[i], year == years_all[1]) %>% select(TOTALGEN)))
}
