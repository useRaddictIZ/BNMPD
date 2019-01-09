# elaborating on to-dos form three commits before (commit ID: cd76b14a44....)

# I. prepare for use to run the analysis with energy data:
# - run with constant and see whether everything works fine (constant without
#   any price regressors as a start in the empirical part: project 2019-01-09)
# - test non-logarithmic version in case empirical variation in the data doesn't
#   match variation produced by logarithmic version of the state processes

# II. add data anlalysis plots - weight/likelihood diagnostic tools: automatic
# plotting of weights/likelihoods at various state values after generating
# simulated values; similar to the state plotting part; should contain
# multivariate plots in 3dims too i.e. 4 pairwise plots of xa_t, xb_t, xp_t,
# xq_t

# III. add documentation pipeline - make a proper Rmarkdown document which is
# able to incorporate the results with all figs in doc/fig/.... - make proper
# tables in LaTeX for the Rmarkdown document - make a comparison between ideal
# Gibbs run and PGAS to see difference in estimation quality, in particular test
# for the remaining pairwise states to be sampled correctly and then try 3
# states; experiment with different number of particles and mcmc run-lengths ->
# put the result into an Rmarkdown doc - make a test with a constant for ideal
# Gibbs and PGAS to see difference in estimation quality and put the result into
# an Rmarkdown doc (seperate folder from 2018-11-30)
