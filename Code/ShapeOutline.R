# ###From the top 
# install.packages("shapeR")
library(shapeR)
library(here)
# shape<- shapeR("C:/Users/makhl/Desktop/Research Repos/Integrated_OtoShapeAnalysis/Data/OtoPhotos","FISH.CSV")
# shape<- detect.outline(shape, threshold = .2, write.outline.w.org = TRUE)
# save(shape, file = "C:/Users/makhl/Desktop/Research Repos/Integrated_OtoShapeAnalysis/Data/oto_outlinesOnly.RData")
# 
# # outlines extracted 09262025

# ok, now lets load those in (so that this script can be used if they're already extracted)

# load(here("Data", "oto_outlinesOnly.RData"))
# 
# shape <- smoothout(shape, n = 100) # Smoothout 
# 
# # remove problematic 
# shape<-remove.outline(shape,"KK","2024_kk_163")
# shape<-remove.outline(shape,"NK","2019_nk_206")
# 
# shape<- generateShapeCoefficients(shape) # Generate Shape Coefficients 
# 
# #Save the coefficients
# save(shape,file = "C:/Users/makhl/Desktop/Research Repos/Integrated_OtoShapeAnalysis/Data/oto_coefficients.RData")

# Read in 
load(here("Data", "oto_coefficients.RData"))

# Remove .jpg extension from picname column
shape@master.list.org$picname <- gsub("\\.jpg$", "", shape@master.list.org$picname)

#Enrich master list 
shape<- enrich.master.list(shape)

# Plot the average 
plotWaveletShape(shape, "pop")

#Estimate outline reconstruction
est.list = estimate.outline.reconstruction(shape)
outline.reconstruction.plot(est.list, max.num.harmonics = 15)





