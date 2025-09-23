#### Wavelet and Fourier Analysis
#### Data at this point has been QCd and Straightened 

## Here is where we will remove outlines still bad 
## We will then calculate how many we have from each class 

library(shapeR)
library(dplyr)

# Read in the corrected data 
load("/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/otoOutlines_Aligned.RData")

# These are just the outlines so now we need to extract the coefficients. 

# First, smooth the contours 
shape<- smoothout(outlinesonly, n = 100)

#for, now remove the one problematic one, 2021_yk_416 
shape<-remove.outline(shape, "YK", "2021_yk_416")
shape<-remove.outline(shape, "YK", "2021_yk_434")

#Generate Coefficients 
shape = generateShapeCoefficients(shape)

#Save the coefficients as an rdata file
save(shape, file = "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/otoOutlines_Smoothed_Coefficients.RData")
