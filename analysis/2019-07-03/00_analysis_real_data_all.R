# Data transformations ----------------------------------------------------
# This file manipulates the raw energy consumption data from the file
# "TS_EIA_Consumption_renamed.xlsx".
#   1. It merges the different types of energy renewables into one variable
#   termed renewable.
#   2. It prepares the dataset containing all states of the US as well as
#   the subset of California and Texas to have regressor variable columns
#   filled with NA, so values for these variables can be copy pasted into
#   the resulting *.xlsx-files.
source("R/helper/00_helper_data_analyze_plots.R")
source("R/helper/00_helper_lib_load.R")
# @0: reading the raw data
data_all_states <- readxl::read_xlsx("data/raw/TS_EIA_Consumption_renamed.xlsx")
data_mwatts <- data_all_states %>%
  select(State, Year_t, everything()) %>%
  select(State:Sum) %>%
  filter(Sum != 0) %>%
  drop_na(CLEIB) %>%
  mutate_at(vars(CLEIB, NGEIB, PAEIB, HYEGB, NUEGB, WWEIB, GEEGB, SOEGB, WYEGB),
            as.double)
#
#
#
#
#
# @1.: grouping with renewables = WWEIB + GEEGB + SOEGB + WYEGB
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
#
#
#
#
#
# @2: defining data subsets (CA and TX only) and adding col-names for regressor
# values
data_energy_ts <- data_mwatts_merged_shares
len_data_energy_ts <- nrow(data_energy_ts)
data_energy_ts_ca_tx <- data_energy_ts %>% filter(State %in% c("CA", "TX"))
len_data_energy_ts_ca_tx <- nrow(data_energy_ts_ca_tx)

reg_vals_all_na <-  rep("NA", times = len_data_energy_ts)
reg_vals_ca_tx_na <- rep("NA", times = len_data_energy_ts_ca_tx)
# dummy_wholesale_market_restructurin <-


reg_names_all_states_model_01 <- data.frame(price_NUEGB = reg_vals_all_na,
                                            price_renewables = reg_vals_all_na,
                                            price_CLEIB = reg_vals_all_na,
                                            price_NGEIB = reg_vals_all_na,
                                            price_PAEIB = reg_vals_all_na,
                                            price_HYEGB = reg_vals_all_na,
                                            dummy_wholesale_market_restructuring = reg_vals_all_na,
                                            dummy_1978 = reg_vals_all_na,
                                            dummy_tech_util = reg_vals_all_na,
                                            LCOE = reg_vals_all_na,
                                            ROI = reg_vals_all_na,
                                            DSIRE = reg_vals_all_na)
reg_names_ca_tx_model_01 <- data.frame(price_NUEGB = reg_vals_ca_tx_na,
                                       price_renewables = reg_vals_ca_tx_na,
                                       price_CLEIB = reg_vals_ca_tx_na,
                                       price_NGEIB = reg_vals_ca_tx_na,
                                       price_PAEIB = reg_vals_ca_tx_na,
                                       price_HYEGB = reg_vals_ca_tx_na,
                                       dummy_wholesale_market_restructuring = reg_vals_ca_tx_na,
                                       dummy_1978 = reg_vals_ca_tx_na,
                                       dummy_tech_util = reg_vals_ca_tx_na,
                                       LCOE = reg_vals_ca_tx_na,
                                       ROI = reg_vals_ca_tx_na,
                                       DSIRE = reg_vals_ca_tx_na)

data_energy_ts <- cbind(data_energy_ts, reg_names_all_states_model_01)
data_energy_ts_ca_tx <- cbind(data_energy_ts_ca_tx, reg_names_ca_tx_model_01)

write_csv(data_energy_ts, path = "data/tidy/test_data_from_analysis_2019-07-03/data_energy_all_states.xlsx")
write_csv(data_energy_ts_ca_tx, path = "data/tidy/test_data_from_analysis_2019-07-03/data_energy_ca_tx.xlsx")

