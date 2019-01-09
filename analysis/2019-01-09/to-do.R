# elaborating on to-dos form three commits before (commit ID: cd76b14a44....)

# I. prepare for use to run the analysis with energy data:
# + T = 44 convergence check: works well with 100 particles but might get 1000
#   to check reliability of convergence and quality of parameter inference
# - D = 5: implement for D=5 (currently D = 4) and see the necessary number of
#   particles (as above)
# - run with dataset:
#   - start with intercept only (finish testing of intercept version in project
#     2018-11-31 first!)
#   - include regressors subsequently: with linear effects
#   - include regressors subsequently: with spline effects
#   - run logarithmic and non-logarithmic versions to see what performs better
#     (finish testing non-logarithmic version in projec 2018-11-31 first!)
