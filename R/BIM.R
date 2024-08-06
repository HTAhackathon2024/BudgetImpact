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

country<-c("Ireland","UK","Canada")

# Years for the budget
Years<-list("2024","2025","2026","2027","2028")
# Should we use base_year<-2024 then add n_years<-5 somehow?

# Intervention
n_interventions <-  1
intervention_names <-  c("Intervention")

# Comparators 

n_comparators <-  2
comparator_names <-  c("Comp1","Comp2")


# Prevalent Patients


# Incident Patients


# Mortality Rate


# Eligibility license


# Eligibility consideration



# Market shares

# Creating empty space for the matrix calculations to add in the values
market_shares_no_intervention <-  array(dim = c(n_comparators, length(Years)), 
                                        dimnames = list(comparator_names,Years))

market_shares_with_intervention <-  array(dim = c(n_interventions+n_comparators, length(Years)), 
                                          dimnames = list(c(intervention_names,comparator_names),Years))

# Market shares inputs

marketshares_noI <- list(
  comp1 = c(0.1, 0.15, 0.2, 0.25, 0.3),
  comp2 = c(0.9, 0.85, 0.8, 0.75, 0.7)
)

marketshares_I <- list(
  int1 = c(0.1, 0.15, 0.15, 0.15, 0.2),
  comp1 = c(0.1, 0.1, 0.1, 0.1, 0.1),
  comp2 = c(0.9, 0.85, 0.85, 0.85, 0.7)
)


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


