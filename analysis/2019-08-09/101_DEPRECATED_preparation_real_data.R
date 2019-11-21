########## DESCRIPTIVE DATA ANALYSIS of CALIFORNIA ENERGY TIME SERIES ##########
rm(list = ls())
source_all <- function() {
  dir <- paste0(getwd(),"/R/")
  file_names <- paste0(dir, list.files(dir, recursive = TRUE))
  invisible(sapply(file_names, source))
}
source_all()
dir_main <- "~/Dropbox/research/usa_energy"
dir_dest <- file.path(dir_main, "02-data/tidy")
dir_src  <- file.path(dir_main, "02-data/raw")

data_raw <- readxl::read_xlsx(file.path(dir_src,
                                        "TS_EIA_Consumption_renamed.xlsx"),
                              na = "NA") # na = "NA" treats any char "NA" as a
                                         # non-available data point
data_mwatts <- data_raw %>% select(Year_t, State,
                                   CLEIB, NGEIB, PAEIB, HYEGB,
                                   NUEGB, WWEIB, GEEGB, SOEGB,
                                   WYEGB) %>%  drop_na()
data_mwatts <- data_mwatts %>%
  mutate_at(vars(CLEIB, NGEIB, PAEIB, HYEGB, NUEGB, WWEIB, GEEGB, SOEGB, WYEGB),
            as.double) %>%
  mutate_at(vars(CLEIB, NGEIB, PAEIB, HYEGB, NUEGB, WWEIB, GEEGB, SOEGB, WYEGB),
            function(x){return(round(x/1, digits = 0))}) %>%
  mutate(renewables = WWEIB + GEEGB + SOEGB + WYEGB) %>%
  select(Year_t, State, CLEIB, NGEIB, PAEIB, HYEGB, NUEGB, renewables) %>%
  mutate(Sum = rowSums(cbind(CLEIB, NGEIB, PAEIB, HYEGB, NUEGB, renewables)))

num_shares <- ncol(data_mwatts) - 3 # -3 because of Year_t, State, and Sum vars
# test if true rowsums of megawatts are equal to data column "Sum":
test_mat <- as.matrix(data_mwatts[, 3:8])
identical(rowSums(test_mat), data_mwatts$Sum)
# generate shares from raw megawatts and merge
data_mwatts_merged_shares <- data_mwatts %>%
  mutate(CLEIB_share = CLEIB/Sum) %>%
  mutate(NGEIB_share = NGEIB/Sum) %>%
  mutate(PAEIB_share = PAEIB/Sum) %>%
  mutate(HYEGB_share = HYEGB/Sum) %>%
  mutate(NUEGB_share = NUEGB/Sum) %>%
  mutate(renewables_share = renewables/Sum) %>%
  mutate(Sum_share = CLEIB_share + NGEIB_share + PAEIB_share +
           HYEGB_share + NUEGB_share + renewables_share)
################################################################################
################################################################################
################################################################################
# test if true rowsums of megawatt shares are equal to data column "Sum_share":
# test_mat <- as.matrix(data_mwatts_merged_shares[, 10:15])
# test_mat_summed <- rowSums(test_mat)
# identical(rowSums(test_mat), data_mwatts_merged_shares$Sum_share)
# IT'S NOT! Bugfix in dplyr!!! Ayway, for our purposes it's fine as we use the
# true shares which indeed sum to 1. It's the Sum_share column that has the
# overflow problem. Check the following
# ID_problems <- which(rowSums(test_mat) != data_mwatts_merged_shares$Sum_share)
# print(test_mat_summed[ID_problems[1]], digits = 20)
# print(data_mwatts_merged_shares$Sum_share[ID_problems[1]], digits = 20)
################################################################################
################################################################################
################################################################################
data_shares_ALL <- data_mwatts_merged_shares
data_mwatts_ALL <- data_mwatts
dim(data_mwatts_ALL)
dim(data_shares_ALL)
# Generating data WITH shares for CA and TX
data_shares_CA <- data_shares_ALL %>% filter(State == "CA")
dim(data_shares_CA)
data_shares_TX <- data_shares_ALL %>% filter(State == "TX")
dim(data_shares_TX)
# Generating data WITHOUT shares for CA and TX
data_mwatts_CA <- data_mwatts %>% filter(State == "CA")
dim(data_mwatts_CA)
data_mwatts_TX <- data_mwatts %>% filter(State == "TX")
dim(data_mwatts_TX)
# merging with regressors
# generating data with price regressors:
data_prices <- readxl::read_xlsx(file.path(dir_src, "r_eiasep_real.xlsx"))
data_prices <- data_prices %>% rename(Year_t = year)
data_prices <- data_prices %>% rename(State = state)
# merging mwatts with price regressors
data_mwatts_prices_all <- inner_join(data_mwatts_ALL,
                                     data_prices,
                                     by = c("Year_t", "State"))
data_mwatts_prices_CA <- inner_join(data_mwatts_CA,
                                    data_prices,
                                    by = c("Year_t", "State"))
data_mwatts_prices_TX <- inner_join(data_mwatts_TX,
                                    data_prices,
                                    by = c("Year_t", "State"))
# merging mwatts and shares with price regressors
data_mwatts_shares_prices_ALL <- inner_join(data_shares_ALL,
                                           data_prices,
                                           by = c("Year_t", "State"))
data_mwatts_shares_prices_CA <- inner_join(data_shares_CA,
                                           data_prices,
                                           by = c("Year_t", "State"))
data_mwatts_shares_prices_TX <- inner_join(data_shares_TX,
                                           data_prices,
                                           by = c("Year_t", "State"))
# saving data -------------------------------------------------------------
# @SHARES
# dir_dest_shares <- file.path(dir_dest, "descriptive-analysis/data-used")
# pALL <- file.path(dir_dest_shares, "data_shares_ALL.xlsx")
# pCA  <- file.path(dir_dest_shares, "data_shares_CA.xlsx")
# pTX  <- file.path(dir_dest_shares, "data_shares_TX.xlsx")
# writexl::write_xlsx(x = data_shares_ALL, path = pALL)
# writexl::write_xlsx(x = data_shares_CA,  path = pCA)
# writexl::write_xlsx(x = data_shares_TX,  path = pTX)
# View(data_shares_prices_TX)
# View(data_shares_prices_CA)
# @MWATTS -----------------------------------------------------------------
dir_dest_mwatts <- file.path(dir_dest, "CA-TX-estimation")
dir_dest_mwatts_dep_var <- file.path(dir_dest_mwatts, "01-dependent-var-mwatts")
dir_dest_mwatts_reg <- file.path(dir_dest_mwatts, "02-regressors")
dir_dest_mwatts_final <- file.path(dir_dest_mwatts, "03-merged-data-final")

pth_dep <- file.path(dir_dest_mwatts_dep_var, "data_mwatts_ALL.xlsx")
writexl::write_xlsx(x = data_mwatts_ALL, path = pth_dep)
pth_dep <- file.path(dir_dest_mwatts_dep_var, "data_mwatts_CA.xlsx")
writexl::write_xlsx(x = data_mwatts_CA, path = pth_dep)
pth_dep <- file.path(dir_dest_mwatts_dep_var, "data_mwatts_TX.xlsx")
writexl::write_xlsx(x = data_mwatts_TX, path = pth_dep)

pth_REG <- file.path(dir_dest_mwatts_reg, "data_prices.xlsx")
writexl::write_xlsx(x = data_prices, path = pth_REG)

pth_ALL <- file.path(dir_dest_mwatts_final, "data_mwatts_prices_ALL.xlsx")
pth_CA  <- file.path(dir_dest_mwatts_final, "data_test_01_CA.xlsx")
pth_TX  <- file.path(dir_dest_mwatts_final, "data_test_01_TX.xlsx")
writexl::write_xlsx(x = data_mwatts_prices_all, path = pth_ALL)
writexl::write_xlsx(x = data_mwatts_prices_CA, path = pth_CA)
writexl::write_xlsx(x = data_mwatts_prices_TX, path = pth_TX)

