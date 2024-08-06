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


# Load necessary libraries
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Function to perform sensitivity analysis and create a tornado diagram
sensitivity_analysis <- function(base_case, min_values, max_values, model_function) {
  # Check that input lengths match
  if (length(base_case) != length(min_values) || length(base_case) != length(max_values)) {
    stop("Base case, min_values, and max_values must have the same length")
  }
  
  # Number of parameters
  n_params <- length(base_case)
  
  # Data frame to store results
  results <- data.frame(Parameter = names(base_case),
                        Base = rep(NA, n_params),
                        Min = rep(NA, n_params),
                        Max = rep(NA, n_params),
                        stringsAsFactors = FALSE)
  
  # Base case result
  base_result <- model_function(base_case)
  
  # Calculate results for min and max values
  for (i in 1:n_params) {
    # Base case for current parameter
    current_values <- base_case
    
    # Min value result
    current_values[i] <- min_values[i]
    min_result <- model_function(current_values)
    
    # Max value result
    current_values[i] <- max_values[i]
    max_result <- model_function(current_values)
    
    # Store results
    results$Base[i] <- base_result
    results$Min[i] <- min_result
    results$Max[i] <- max_result
  }
  
  # Calculate differences
  results$MinDiff <- abs(results$Base - results$Min)
  results$MaxDiff <- abs(results$Base - results$Max)
  
  # Sort results by maximum difference
  results <- results[order(-pmax(results$MinDiff, results$MaxDiff)), ]
  
  # Plot tornado diagram
  ggplot(results, aes(x = reorder(Parameter, -pmax(MinDiff, MaxDiff)))) +
    geom_bar(aes(y = Base), stat = "identity", fill = "grey", width = 0.4) +
    geom_errorbar(aes(ymin = Min, ymax = Max), width = 0.2, color = "blue") +
    coord_flip() +
    labs(title = "Tornado Diagram",
         x = "Parameters",
         y = "Effect on Model Outcome") +
    theme_minimal()
}

# Example usage
# Define a simple cost-effectiveness model function
example_model <- function(params) {
  # Assume the model returns net monetary benefit (NMB)
  # NMB = Effectiveness * Willingness-to-pay - Cost
  effectiveness <- params["effectiveness"]
  cost <- params["cost"]
  wtp <- params["wtp"] # willingness-to-pay threshold
  
  nmb <- effectiveness * wtp - cost
  return(nmb)
}

# Define base case parameters
base_case <- c(effectiveness = 0.8, cost = 5000, wtp = 50000)

# Define minimum values for parameters
min_values <- c(effectiveness = 0.7, cost = 4000, wtp = 40000)

# Define maximum values for parameters
max_values <- c(effectiveness = 0.9, cost = 6000, wtp = 60000)

# Run the sensitivity analysis and plot the tornado diagram
sensitivity_analysis(base_case, min_values, max_values, example_model)
