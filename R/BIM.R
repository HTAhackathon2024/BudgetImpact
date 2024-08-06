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
patient_population = function(time_horizon,
                              prevalent_patients,
                              incident_patients,
                              mortality_rate_in_patients_with_condition,
                              patients_eligible_for_the_new_intervention_under_the_license,
                              sub_population_of_eligible_patient_population_under_consideration
                              ){
    prevalent_patients = rep(prevalent_patients,time_horizon)
    incident_patients = rep(incident_patients,time_horizon)
    number_of_patients_with_condition = prevalent_patients+incident_patients
    net_number_of_patients = number_of_patients_with_condition*(1-mortality_rate_in_patients_with_condition)
    number_of_patients_potentially_treatable_under_license = net_number_of_patients * 
    patients_eligible_for_the_new_intervention_under_the_license
    eligible_patients_treated_with_intervention = number_of_patients_potentially_treatable_under_license*sub_population_of_eligible_patient_population_under_consideration
  
  res = list(prevalent_patients = prevalent_patients,
             incident_patients = incident_patients,
             number_of_patients_with_condition = number_of_patients_with_condition,
             net_number_of_patients = net_number_of_patients,
             number_of_patients_potentially_treatable_under_license = number_of_patients_potentially_treatable_under_license,
             eligible_patients_treated_with_intervention = eligible_patients_treated_with_intervention)
  
  return(res)
}

patient_population(time_horizon = 5,
                   prevalent_patients = 110,
                   incident_patients = 20,
                   mortality_rate_in_patients_with_condition = 0.10,
                   patients_eligible_for_the_new_intervention_under_the_license = 0.45, 
                   sub_population_of_eligible_patient_population_under_consideration = 0.9
)


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

number_of_patients_who_died = 
  number_of_patients_with_condition *
  mortality_rate_in_patients_with_condition *
  patients_eligible_for_the_new_intervention_under_the_license *
  sub_population_of_eligible_patient_population_under_consideration *
  market_share_with_intervention


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

cost = cost_acquisition * cost_PAS + cost_admin

total_cost_of_intervention =  
  (summary_of_population_with_new_intervention * cost) +
  (0.5 * cost * number_of_patients_who_died)

total_cost_over_time_horizon = rowSums(total_cost_of_intervention)

################################
#     Results      #
################################
gross_drug_budget_impact = summary_of_population_with_new_intervention * cost_total
