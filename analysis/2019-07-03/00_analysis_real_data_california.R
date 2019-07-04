########## DESCRIPTIVE DATA ANALYSIS of CALIFORNIA ENERGY TIME SERIES ##########
source("R/helper/00_helper_data_analyze_plots.R")
source("R/helper/00_helper_lib_load.R.R")
data_california <- readxl::read_xlsx("data/raw/TS_EIA_Consumption_California_only.xlsx")
data_mwatts <- data_california %>% select(State:Sum)
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
energy_fractions_names <- names(data_energy_ts)[3:8]
# all energy types jointly ------------------------------------------------
par(mfrow = c(1, 1))
matplot(x = data_energy_ts[, "Year_t"],
        y = data_energy_ts[, energy_fractions], type = "l",
        main = "energy fractions",
        xlab = "year",
        ylab = "all shares",
        col = rainbow(num_shares))
legend("topright", title = "energy types", fill = rainbow(num_shares),
       energy_fractions_names)
# individual energy types -------------------------------------------------
par(mfrow = c(3, 2))
for (i in 1:num_shares) {
  matplot(data_energy_ts[, energy_fractions[i]], type = "l",
          main = energy_fractions_names[i],
          ylab = "individual share",
          col = rainbow(num_shares)[i])
}

