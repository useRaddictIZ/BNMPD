########## DESCRIPTIVE DATA ANALYSIS of CALIFORNIA ENERGY TIME SERIES ##########
source_all <- function() {
  dir <- paste0(getwd(),"/R/")
  file_names <- paste0(dir, list.files(dir, recursive = TRUE))
  invisible(sapply(file_names, source))
}
source_all()
num_shares <- 6
data_CA <- readxl::read_xlsx("data/tidy/shares_mwatts_prices/data_test_01_CA_unscaled.xlsx")
data_CA <- as.data.frame(data_CA)
# plot naming -------------------------------------------------------------
energy_fractions <- as.data.frame(data_CA %>% select(CLEIB:renewables))
energy_fractions_names <- data_CA %>% select(CLEIB:renewables) %>% names()
# all energy types jointly ------------------------------------------------
par(mfrow = c(1, 1))
matplot(x = data_CA[, "Year_t"],
        y = energy_fractions, type = "l",
        main = "energy fractions",
        xlab = "year",
        ylab = "all shares",
        col = rainbow(num_shares))
legend("topright", title = "energy types", fill = rainbow(num_shares),
       energy_fractions_names)
# individual energy types -------------------------------------------------
par(mfrow = c(3, 2))
for (i in 1:num_shares) {
  matplot(energy_fractions[, i], type = "l",
          main = energy_fractions_names[i],
          ylab = "individual share",
          col = rainbow(num_shares)[i])
}

