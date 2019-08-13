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
