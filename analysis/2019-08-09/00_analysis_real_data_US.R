############## DESCRIPTIVE DATA ANALYSIS of US ENERGY TIME SERIES ##############
# sum data transformations and checking -----------------------------------
data_energy_ts <- read_xls("./data/raw/us_energy.xls")
data_energy_ts <- data_energy_ts[1:44,]

dim(data_energy_ts)
names(data_energy_ts)
energy_fractions <- c("coal", "oil", "gas", "nuclear", "renewable")


# check: all fractions 100% -----------------------------------------------
rowSums(data_energy_ts[, energy_fractions])
# plot naming -------------------------------------------------------------
energy_cols      <- c("(black)", "(red)", "(green)", "(blue)", "(turquoise)")
plot_title <- paste0("Energy fractions: ",
                     paste(energy_fractions, energy_cols,
                           collapse = ", ", sep = " "),
                     collapse = "")
plot_ylab  <- paste("y", "_alpha", 1:5, sep = "", collapse = ", ")


# all energy types jointly ------------------------------------------------
matplot(data_energy_ts[, energy_fractions], type = "l",
        main = plot_title,
        ylab = plot_ylab)

# individual energy types -------------------------------------------------
matplot(data_energy_ts[, energy_fractions[1]], type = "l",
        main = energy_fractions[1],
        ylab = "y_alpha1")
matplot(data_energy_ts[, energy_fractions[2]], type = "l",
        main = energy_fractions[2],
        ylab = "y_alpha2")
matplot(data_energy_ts[, energy_fractions[3]], type = "l",
        main = energy_fractions[3],
        ylab = "y_alpha3")
matplot(data_energy_ts[, energy_fractions[4]], type = "l",
        main = energy_fractions[4],
        ylab = "y_alpha4")
matplot(data_energy_ts[, energy_fractions[5]], type = "l",
        main = energy_fractions[5],
        ylab = "y_alpha5")
