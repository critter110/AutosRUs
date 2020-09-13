#read in data from csv
MechaMPG <- read.csv("MechaCar_mpg.csv", check.names = F,stringsAsFactors = F)
#Perform multiple regression and get summary statistics
lm(MechaMPG$"mpg" ~ MechaMPG$"vehicle length" + MechaMPG$"vehicle weight" + MechaMPG$"spoiler angle" + MechaMPG$"ground clearance", data=MechaMPG)
summary(lm(MechaMPG$"mpg" ~ MechaMPG$"vehicle length" + MechaMPG$"vehicle weight" + MechaMPG$"spoiler angle" + MechaMPG$"ground clearance", data=MechaMPG))

#suspension coil data read and summary table creation
susCoilData <- read.csv("Suspension_Coil.csv", check.names = F,stringsAsFactors = F)
psiSummary <- susCoilData %>% group_by(Manufacturing_Lot) %>% summarize(MeanPSI=mean(PSI), Median_PSI=median(PSI), Variance=var(PSI), std_Dev=sd(PSI))

#test for normalcy in suspension data
shapiro.test(susCoilData$PSI)
# This gave a p-value of 6.011e-11 which means that the data is not normally distributed
# This means we cannot perform a t test on the data