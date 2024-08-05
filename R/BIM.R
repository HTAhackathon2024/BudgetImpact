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







################################
#           Results            #
################################




years<-base_year
prevalence<-matrix(0,
                   nrow=n_comparators+1,
                   ncol=n_years,
                   dimnames = c())



incidence<-matrix(0,
                   nrow=n_comparators+1,
                   ncol=n_years,
                   dimnames = c())

costs<-matrix(0,
                   nrow=n_comparators+1,
                   ncol=n_years,
                   dimnames = c())
