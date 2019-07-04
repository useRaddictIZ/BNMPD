########## DESCRIPTIVE DATA ANALYSIS of CALIFORNIA ENERGY TIME SERIES ##########
source("R/helper/00_helper_data_analyze_plots.R")
source("R/helper/00_helper_lib_load.R.R")
data_all_states <- readxl::read_xlsx("data/raw/TS_EIA_Consumption_renamed.xlsx")
data_mwatts <- data_all_states %>%
  select(State, Year_t, everything()) %>%
  select(State:Sum) %>%
  filter(Sum != 0) %>%
  drop_na(CLEIB) %>%
  mutate_at(vars(CLEIB, NGEIB, PAEIB, HYEGB, NUEGB, WWEIB, GEEGB, SOEGB, WYEGB),
            as.double)
# grouping with renewables = WWEIB + GEEGB + SOEGB + WYEGB
data_mwatts_merged <- data_mwatts %>%
  mutate(renewables = WWEIB + GEEGB + SOEGB + WYEGB) %>%
  select(State, Year_t, CLEIB, NGEIB, PAEIB, HYEGB, NUEGB, renewables, Sum)
num_shares <- ncol(data_mwatts_merged) - 3
# test if true rowsums of megawatts are equal to data column "Sum":
# test_mat <- as.matrix(data_mwatts_merged[, 3:8])
# identical(rowSums(test_mat), data_mwatts_merged$Sum)
data_mwatts_merged_shares <- data_mwatts_merged %>%
  mutate(CLEIB_share = CLEIB/Sum) %>%
  mutate(NGEIB_share = NGEIB/Sum) %>%
  mutate(PAEIB_share = PAEIB/Sum) %>%
  mutate(HYEGB_share = HYEGB/Sum) %>%
  mutate(NUEGB_share = NUEGB/Sum) %>%
  mutate(renewables_share = renewables/Sum) %>%
  mutate(Sum_share = CLEIB_share + NGEIB_share + PAEIB_share +
           HYEGB_share + NUEGB_share + renewables_share)
# test if true rowsums of megawatt shares are equal to data column "Sum_share":
# test_mat <- as.matrix(data_mwatts_merged_shares[, 10:15])
# identical(rowSums(test_mat), data_mwatts_merged_shares$Sum_share)
# IT'S NOT! Bugfix in dplyr!!! Ayway, for our purposes it's fine as we use the
# true shares which indeed sum to 1. It's the Sum_share column that has the
# overflow problem
data_energy_ts <- data_mwatts_merged_shares
dim(data_energy_ts)
# plot naming -------------------------------------------------------------
energy_fractions <- names(data_energy_ts)[10:15]
names_energy_fractions <- names(data_energy_ts)[3:8]
names_states <- unique(data_energy_ts$State)
num_states <- length(names_states)
plot_list <- rep(list(list()), times = num_states)
# all energy types jointly ------------------------------------------------
for (i in 1:num_states) {
  plot_list[[i]] <- generate_plot_ts_shares(data_energy_ts,
                                            name_state = names_states[i],
                                            energy_types = energy_fractions,
                                            names_energy_types = names_energy_fractions)
  name <- paste0("results/analysis_data_plots/TS_EIA_Consumption_renamed/",
                 names_states[i], ".pdf")
  ggsave(name, plot = plot_list[[i]])
  print(paste0(round(i/num_states, digits = 4)*100,"%"))
}
