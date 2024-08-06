# OWSA


# Darth has OWSA tornado diagram code
install.packages("remotes")
# Install development version from GitHub
devtools::install_github("DARTH-git/dampack")

# See this website!
# https://rdrr.io/github/DARTH-git/dampack/man/owsa_tornado.html

my_owsa_params_range <- data.frame(pars = c("u_trtA", "c_trtA", "hr_S1S2_trtB", "r_HD"),
                                   min = c(0.9, 9000, 0.3, 0.001),
                                   max = c(1,   24000, 0.9, 0.003))

library(dampack)

l_owsa_det <- run_owsa_det(params_range = my_owsa_params_range,
                           params_basecase = my_params_basecase,
                           nsamp = 100,
                           FUN = simulate_strategies,
                           outcomes = c("Cost", "QALY", "LY", "NMB"),
                           strategies = c("No_Treatment", "Treatment_A", "Treatment_B"),
                           progress = FALSE)


owsa_tornado(
  owsa,
  return = c("plot", "data"),
  txtsize = 12,
  min_rel_diff = 0,
  col = c("full", "bw"),
  n_y_ticks = 8,
  ylim = NULL,
  ybreaks = NULL
)