library(grid)
library(Gmisc)



grid.newpage()
# set some parameters to use repeatedly
leftx <- .25
midx <- .5
rightx <- .75
width <- .4
gp <- gpar(fill = "lightgrey")

population <- 4000
prevalent <- 3800
incident <- 2000
r_mortality <- 0.02
licensce <- 1800
consideration <- 0.98

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



















