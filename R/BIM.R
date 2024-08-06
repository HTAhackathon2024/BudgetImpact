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
#      Patient Flow Chart       #
################################

library(grid)
library(Gmisc)

grid.newpage()
# set some parameters to use repeatedly
leftx <- .25
midx <- .5
rightx <- .75
width <- .4
gp <- gpar(fill = "lightgrey")

# population <- 4000
# prevalent <- 3800
# incident <- 2000
# r_mortality <- 0.02
# licensce <- 1800
# consideration <- 0.98

# create boxes
(box_total <- boxGrob(paste("Population\n N =", population), 
                  x=midx, y=.9, box_gp = gp, width = width))
(box_pre <- boxGrob(paste("Prevalant patients\n N =", prevalent),
                  x=midx, y=.75, box_gp = gp, width = width))
# connect boxes like this
connectGrob(box_total, box_pre, "v")

(box_inc <- boxGrob(paste("Incident patients\n N =", incident),
                  x=midx, y=.6, box_gp = gp, width = width))
connectGrob(box_pre, box_inc, "v")

(box_mort <- boxGrob(paste("Discontinuation\n N =", r_mortality*incident), 
                 x=rightx, y=.5, box_gp = gp, width = .25, height = .05))
connectGrob(box_total, box_mort, "-")
(box_lic <- boxGrob(paste("Patients eligible under the licensce\n N = ", licensce), 
               x=midx, y=.4 , box_gp = gp, width = width))
connectGrob(box_inc, box_lic, "v")
(box_cons <- boxGrob(paste("Patients eligible under consideration\n N = ", consideration), 
               x=midx, y=.2, box_gp = gp, width = width))
connectGrob(box_lic, box_cons, "v")





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


