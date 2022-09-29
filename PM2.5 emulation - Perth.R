#################################################
# Emulation for GMR Perth and Surrounding areas #
#################################################

setwd('C:/ctm/em-public')

### load relevant R libraries
library(DiceKriging)
library(sensitivity)
library(ggplot2)
library(cowplot)
library(rlang)
library(ggtext)
library(grid)
library(paletteer)
library(hrbrthemes)

# Load additional R functions
source('./EmodFit.R')
source('./misc functions.R')

### load training data (CTM outputs) for PM2.5 - GMR Perth (unweighted by population)
data(res_gccunwgt)

### Fit emulator for GMR Perth
pm25_t = EmodFit(myvar  = 'pm25d', res = res_gccunwgt, myloc = 'Greater Perth', areaType = 'GCC_NAME16', molconv = 1)
pm25_t$p # present validation data (Figure 4)  
assoc(pm25_t$EmModel, mylaby = 'Delta PM2.5 (ugm-3)') # present associations (Figure 5)
myVC(pm25_t$EmModel) # Carry out sensitivity analyses and present main effects and interactions

#######################
### SCENARIO ANALYSES #
#######################

# Define scenarios based on setting parameters in the emulator at selected levels
scendat = data.frame(myscen = paste("EV", seq(6), sep=""),
                     descr = c("Diesel - BAU",
                               "Diesel - renewables",
                               "Petrol - BAU",
                               "Petrol - renewables",
                               "All vehicles - BAU",
                               "All vehicles - renewables"),
                     X1 = c(0.379, 0.379, 0.64, 0.64, 0, 0),
                     X2 = c(0.94, 0.94, 0.10, 0.10, 0, 0),
                     X3 = c(0.08, 0.08, 0.94, 0.94, 0, 0),
                     X4 = c(0.95, 0.95, 0.08, 0.08, 0, 0) ,
                     X5 = c(0.513, 0.513, 0.517, 0.517, 0, 0),
                     X6 = c(0.94, 0.94, 0.13, 0.13, 0, 0),
                     X7 = c(1.07, 0, 1.2, 0, 1.3, 0),
                     X8 = c(1.07, 0, 1.2, 0, 1.3, 0))

# net difference for renewables - Greater Perth
-0.45174514--0.56122372
# 0.1094786
# net difference for renewables - Surrounding regions
0.02518582--0.12990284
# 0.1550887

### load SA4 and SA3 emulators
data(sa4_ems)
data(sa3_ems)

myvar = 'pm25d_wgt'
regs = c('Perth - Inner', 'Perth - North East', 'Perth - North West',
         'Perth - South East', 'Perth - South West', 'Mandurah')
area_ems = sa4_ems
tab = EmPred(areatype = 'sa4', scendat=scendat, regs=regs, myvar=myvar)
tab$pop = c(170, 251, 535, 488, 403, 97)
taba = rbind(tab, data.frame(regs = "Greater Perth",
                             EV1 = sum(tab$EV1*tab$pop)/sum(tab$pop),
                             EV2 = sum(tab$EV2*tab$pop)/sum(tab$pop),
                             EV3 = sum(tab$EV3*tab$pop)/sum(tab$pop),
                             EV4 = sum(tab$EV4*tab$pop)/sum(tab$pop),
                             EV5 = sum(tab$EV5*tab$pop)/sum(tab$pop),
                             EV6 = sum(tab$EV6*tab$pop)/sum(tab$pop),
                             pop = sum(tab$pop)))

regs = c('Bunbury', 'Manjimup', 'Augusta - Margaret River - Busselton')
area_ems = sa3_ems
tab = EmPred(areatype = 'sa3', scendat=scendat, regs=regs, myvar=myvar)
tab$pop = c(102, 23, 51)
tabb = rbind(tab, data.frame(regs = "SA total", 
                             EV1 = sum(tab$EV1*tab$pop)/sum(tab$pop),
                             EV2 = sum(tab$EV2*tab$pop)/sum(tab$pop),
                             EV3 = sum(tab$EV3*tab$pop)/sum(tab$pop),
                             EV4 = sum(tab$EV4*tab$pop)/sum(tab$pop),
                             EV5 = sum(tab$EV5*tab$pop)/sum(tab$pop),
                             EV6 = sum(tab$EV6*tab$pop)/sum(tab$pop),
                             pop = sum(tab$pop)))

tab4 = rbind(taba, tabb)
#write.xlsx(tab4, file = './output/tab4.xlsx') # output file to separate excel table



