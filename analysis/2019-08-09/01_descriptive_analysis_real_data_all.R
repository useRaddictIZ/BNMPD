########## DESCRIPTIVE DATA ANALYSIS FOR ALL STATES ENERGY TIME SERIES #########
# data_all_states <- readxl::read_xlsx("data/raw/TS_EIA_Consumption_renamed.xlsx")
data_all <- haven::read_dta("./data/uspp_aggregated_final.dta")
data_share_absolute <- data_all %>%
  select(state, year, CLEIB, NGEIB, PAEIB, HYEGB, NUEGB, renewables) %>%
  mutate(sum_absolute = CLEIB + NGEIB + PAEIB + HYEGB + NUEGB + renewables) %>%
  mutate(CLEIB_share = CLEIB/sum_absolute) %>%
  mutate(NGEIB_share = NGEIB/sum_absolute) %>%
  mutate(PAEIB_share = PAEIB/sum_absolute) %>%
  mutate(HYEGB_share = HYEGB/sum_absolute) %>%
  mutate(NUEGB_share = NUEGB/sum_absolute) %>%
  mutate(renewables_share = renewables/sum_absolute) %>%
  mutate(sum_relative = CLEIB_share + NGEIB_share + PAEIB_share + HYEGB_share +
           NUEGB_share + renewables_share)
data_shares <- data_share_absolute %>%
  select(state, year, CLEIB_share, NGEIB_share, PAEIB_share, HYEGB_share, NUEGB_share, renewables_share)
# data_shares_US <-
# plot naming -------------------------------------------------------------
data_prices <- data_all %>%
  select(state, year, cleid, ngeid, dfeid)
prices_to_plot <- names(data_prices)[3:5]
names_prices <- c("coal_price", "gas_price", "oil_price")
names_states <- unique(data_prices$state)
num_states <- length(names_states)
plot_list <- rep(list(list()), times = num_states)
# all energy prices jointly -----------------------------------------------
for (i in 1:num_states) {
  plot_list[[i]] <- generate_plot_ts(data_prices,
                                     name_state = names_states[i],
                                     ts = prices_to_plot,
                                     names_ts = names_prices)
  name <- paste0("results/descriptive/uspp_aggregated_final_plots/",
                 names_states[i], "_03_prices.pdf")
  ggsave(name, plot = plot_list[[i]],  width = 8.27, height = 11.69, units = "in")
  print(paste0(round(i/num_states, digits = 4)*100, "%", "for price plots!"))
}
# number of cols/variables minus 4 variables: state, year, sum_absolute,
# sum_relative to be divided by 2 because we have absolute mwatts and shares
# as variables present in the dataset
# num_shares <- (ncol(data_shares) - 4)/2
# plot naming -------------------------------------------------------------
energy_fractions <- names(data_shares)[3:8]
names_energy_fractions <- c("coal", "gas", "oil", "hydro", "nuclear", "renewable")
names_states <- unique(data_shares$state)
num_states <- length(names_states)
plot_list <- rep(list(list()), times = num_states)
# all energy shares jointly -----------------------------------------------
for (i in 1:num_states) {
  plot_list[[i]] <- generate_plot_ts(data_shares,
                                     name_state = names_states[i],
                                     ts = energy_fractions,
                                     names_ts = names_energy_fractions)
  name <- paste0("results/descriptive/uspp_aggregated_final_plots/",
                 names_states[i], "_02_shares.pdf")
  ggsave(name, plot = plot_list[[i]],  width = 8.27, height = 11.69, units = "in")
  print(paste0(round(i/num_states, digits = 4)*100, "%", "for share plots!"))
}
# all energy shares and coal, gas and oil prices jointly -------------------
# joining all data --------------------------------------------------------
data_shares_prices <- full_join(data_shares, data_prices)
for (i in 1:num_states) {
  plot_list[[i]] <- generate_plot_ts_both(data_shares_prices,
                                          name_state = names_states[i],
                                          ts_1 = energy_fractions,
                                          ts_2 = prices_to_plot,
                                          names_ts = names_energy_fractions)
  name <- paste0("results/descriptive/uspp_aggregated_final_plots/",
                 names_states[i], "_01_both.pdf")
  ggsave(name, plot = plot_list[[i]],  width = 8.27, height = 11.69, units = "in")
  print(paste0(round(i/num_states, digits = 4)*100, "%", "for both plots!"))
}
#
#
#
#
#
# %>%
#   select(State:Sum) %>%
#   filter(Sum != 0) %>%
#   drop_na(CLEIB) %>%
#   mutate_at(vars(CLEIB, NGEIB, PAEIB, HYEGB, NUEGB, WWEIB, GEEGB, SOEGB, WYEGB),
#             as.double)
# grouping with renewables = WWEIB + GEEGB + SOEGB + WYEGB
# data_mwatts_merged <- data_mwatts %>%
#   mutate(renewables = WWEIB + GEEGB + SOEGB + WYEGB) %>%
#   select(State, Year_t, CLEIB, NGEIB, PAEIB, HYEGB, NUEGB, renewables, Sum)
#
#
#
#
#
# # test if true rowsums of megawatts are equal to data column "Sum":
# # test_mat <- as.matrix(data_mwatts_merged[, 3:8])
# # identical(rowSums(test_mat), data_mwatts_merged$Sum)
# data_mwatts_merged_shares <- data_mwatts_merged %>%
#   mutate(CLEIB_share = CLEIB/Sum) %>%
#   mutate(NGEIB_share = NGEIB/Sum) %>%
#   mutate(PAEIB_share = PAEIB/Sum) %>%
#   mutate(HYEGB_share = HYEGB/Sum) %>%
#   mutate(NUEGB_share = NUEGB/Sum) %>%
#   mutate(renewables_share = renewables/Sum) %>%
#   mutate(Sum_share = CLEIB_share + NGEIB_share + PAEIB_share +
#            HYEGB_share + NUEGB_share + renewables_share)
# # test if true rowsums of megawatt shares are equal to data column "Sum_share":
# # test_mat <- as.matrix(data_mwatts_merged_shares[, 10:15])
# # identical(rowSums(test_mat), data_mwatts_merged_shares$Sum_share)
# # IT'S NOT! Bugfix in dplyr!!! Ayway, for our purposes it's fine as we use the
# # true shares which indeed sum to 1. It's the Sum_share column that has the
# # overflow problem
