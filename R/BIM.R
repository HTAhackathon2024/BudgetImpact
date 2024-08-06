# Budget Impact Models


# If you need to install a package, add it here. 
# install.packages("")

# Remember to load here 
#library()

################################
#    Environment set up        #
################################

# Set working directory
# setwd()

################################
#           Inputs             #
################################
# Years for the budget
base_year = 2024
time_horizon = 5
years = seq(base_year,base_year + time_horizon)


################################
#     Patient population      #
################################
# 

prevalent_patients = rep(100,time_horizon)

incident_patients = rep(10,time_horizon)

number_of_patients_with_condition = prevalent_patients+incident_patients

mortality_rate_in_patients_with_condition = rep(0.05,time_horizon)

net_number_of_patients = 
  number_of_patients_with_condition*(1-mortality_rate_in_patients_with_condition)

patients_eligible_for_the_new_intervention_under_the_license = 
  rep(0.40,time_horizon)

number_of_patients_potentially_treatable_under_license = 
  net_number_of_patients * 
  patients_eligible_for_the_new_intervention_under_the_license

sub_population_of_eligible_patient_population_under_consideration = rep(0.8,time_horizon)

eligible_patients_treated_with_intervention = 
  number_of_patients_potentially_treatable_under_license*sub_population_of_eligible_patient_population_under_consideration

# Market shares
market_share_without_intervention = 
  matrix(1, nrow = 1, ncol = time_horizon)

market_share_with_intervention = 
  matrix(0.5, 
         nrow = 1+nrow(market_share_without_intervention), 
         ncol = time_horizon,
         byrow = TRUE)

summary_of_population_without_new_intervention = eligible_patients_treated_with_intervention * market_share_without_intervention

summary_of_population_with_new_intervention = eligible_patients_treated_with_intervention * market_share_with_intervention


################################
#     Cost      #
################################
cost_PAS =  matrix(1,
                   nrow = 1+nrow(market_share_without_intervention), 
                   ncol = time_horizon,
                   byrow = TRUE)

cost_acquisition = matrix(c(rep(1000,time_horizon), 
                            rep(800,time_horizon)),
                          nrow = 1+nrow(market_share_without_intervention), 
                          ncol = time_horizon,
                          byrow = TRUE)

cost_admin = matrix(c(rep(100,time_horizon), 
                      rep(100,time_horizon)),
                    nrow = 1+nrow(market_share_without_intervention), 
                    ncol = time_horizon,
                    byrow = TRUE)

cost_total = cost_acquisition * cost_PAS + cost_admin

################################
#     Results      #
################################
gross_drug_budget_impact = summary_of_population_with_new_intervention * cost_total


# For loop for world without intervention

for(j in 1:length(marketshares_noI))
{
  for(i in 1:length(Years))
  {
    market_shares_no_intervention[j, i] <- marketshares_noI[[j]][i]
  }}


# For loop for world with intervention

for(j in 1:length(marketshares_I))
{
  for(i in 1:length(Years))
  {
    market_shares_with_intervention[j, i] <- marketshares_I[[j]][i]
    
  }}




# Costs


intervention_costs<-1000
comparator_costs <-  c(1500,2000)





################################
#        Calculations          #
################################




# World with intervention calculations
world_with_intervention <- matrix(0, nrow = length(Years), ncol = 1)
world_without_intervention <- matrix(0, nrow = length(Years), ncol = 1)

for (i in 1:length(Years)) {
  world_with_intervention[i] <- sum(
    market_shares_with_intervention[, i] * c(intervention_costs, comparator_costs)
  )
  
  world_without_intervention[i] <- sum(
    market_shares_no_intervention[, i] * comparator_costs
  )
}

# Gross budget is the cost of the new drug over the 5 years
Gross_budget <- sum(market_shares_with_intervention[1, ] * intervention_costs)

# Net budget is the difference between the world with and without the intervention
Net_budget <- sum(world_with_intervention) - sum(world_without_intervention)









################################
#           Results            #
################################

Gross_budget
Net_budget



################################
#      Tornado diagram         #
################################


