# Author:       David Simpson
# Title:        Guacamole Experiment
# Class:        W 4768 Experimental Research
# Created:      10 March 2019
# Edited:       26 March 2019
# Adapted From: Professor Donald Green - "Drying wood practicum 2018 - with 5 day outcomes.R"
# References:   (1) Gerber, Alan S. and Donald P. Green. 2012. Field Experiments: Design, Analysis, 
#               and Interpretation. New York: W.W. Norton.
#               (2) {ri} R Documentation
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Outline:
# Guacamole Recipe given in Pre-Analysis Plan
# Research Question: Does removing jalapeno seeds change the observed spice level
#                    in guacamole that contains jalapenos.
# Treatment == Guacamole with jalapeno seeds; 
# Control   == Guacamole without jalapeno seeds
# Measure   == 5 point "spice" scale measured by taste tester
# (1) Testers are "blindly" given a sample of the baseline guacamole 
# Pre-treatment measure is a tester spice scoring of the baseline guacamole
# (2) Testers are then "blindly" and randomly given either a sample of the treatment or control 
# Outcome measure is a tester spice socring of the random guacamole
# Estimation model: Outcome Score = a + b(treatment) + c(Baseline-Score) + u
# Test is one-tailed: 
# Blocking: I expect to have only three taste testers, so I will block on tester
################################################################################################
setwd("/Users/dsimp/Dropbox/ColumbiaPoliSci/02.Spring2019/03_Experiments/02_ExperimentProject/Analysis")
rm(list=ls())
library(ri)
library(stargazer)
library("kableExtra")
library("tidyr")
library("foreign")
library("ggplot2")
set.seed(1234567)

#########################
## SECTION 1 - PRE-ANALYSIS
#########################

N = 30 # Total number of subjects i
n = 10 # Number of subjects per block
m = 5  # Number of treated subjects per block

guac <- data.frame("Observation"=1:N)          # Create data frame with 30 observations
guac$Block <- c(rep(1,10),rep(2,10),rep(3,10)) # Create block variable - for 3 blocks

#########################
# Randomization Procedure
#########################
# Complete Random Assignment within each block
# Per block, 0 < m=5 < 10
guac$Treatment[1:10] <- ifelse(1:n %in% sample(1:10, m), 1, 0)       # Random Assignment of treatment for Block 1
guac$Treatment[11:20] <- ifelse(11:20 %in% sample(11:20, m), 1, 0)   # Random Assignment of treatment for Block 2
guac$Treatment[21:30] <- ifelse(21:30 %in% sample(21:30, m), 1, 0)   # Random Assignment of treatment for Block 3

table(guac$Treatment) # Show total treatment vs control. Confirms 15 in each status

#########################
## Random Assignment Table (To report in pre-analysis plan)
#########################
guac$Type<- "Spicy"                         # Variable to describe the treatment type (Spicy as default)
guac$Type[guac$Treatment==0] <- "Moderate"  # Change Type to "Moderate" for obsevations with Treatment == 0
kable(guac, "html", booktabs = T, caption="Random Assignment Table") %>%       # Generate Random Assignment Table 
  kable_styling(latex_options = "striped")  

#########################
## Adminstrative Table (Make a Table to easily administer the experiment)
#########################
# Note: There will be 60 total tastings (30 baseline and 30 treatment or control)
# Note: As such each individual will taste 10 baseline servings, 5 treatment serving, and 5 control servings
# Note: Each taste tester will taste and rate a baseline guacamole serving before tasting and rating each treatment or control guac 

admin <- data.frame("Order"=1:60,"Type"=1:60) # Destination data frame for the adminsitration data

# Loop to insert a baseline observation between each treatment or control serving
counter <- c(1:60)
count <- 0
for (i in 1:60){
    if((counter[i] %% 2)==0){
      print(paste(i,"even"))
      count = i/2
      print(count)
      admin$Type[i] = guac$Treatment[count]+1
    } else{
      print(paste(i,"odd"))
      admin$Type[i]=0
       }
}

admin$Name <- "Baseline"                  # Describe the batch identity of Baseline servings
admin$Name[admin$Type==1] <- "Control"    # Describe the batch identity of Control servings
admin$Name[admin$Type==2] <- "Treatment"  # Describe the batch identity of Treatment servings
admin <- cbind(admin[1:20,c(1,3)],admin[21:40,3],admin[41:60,3])  # Organize data by administration rounds. Assistant will adminster 20 rounds 
colnames(admin) = c("Order", "Block 1","Block 2","Block 3")       # Rename Columns

kable(admin, "html", booktabs = T, caption = "Experiment Administration Table") %>%  # Generate Administration Table 
  kable_styling(latex_options = "striped")

############################################
## SECTION 2 - EXPERIMENTAL RESULTS ANALYSIS
############################################

results <- read.csv("Results.csv") # Read in Google Survey Results
glimpse(results) #60 Observations

#########################
## Prepare Data
#########################
# Note: Every 20 observations is a different block (eg. Block 1: 1-20, Block 2: 21-40, Block 3: 41-60)
# Note: Within each block, observations alternate between Baseline servings (odd numbered) and randomly assigned servings (even-numbered)
# Note: Want to create a data set where each observation has the block assignment, treatment assignment, 
#       prior baseline spice score, and outcome ratings
# Note: Will add results data to the "guac" data set

baseline <- results[(results$Number %% 2)>0,1:9]  # Baseline pre-test scores are odd numbered
guac$BaseSpice <- baseline$Spicy                  # Add baseline spice score "BaseSpice" to the guac dataset

outcomes <- results[(results$Number %% 2)==0,1:9] # Treatment and Control outcome measures are even numbered
guac<-cbind(guac,outcomes[,3:9])                  # Add outcomes measures to the guac dataset
guac$SpicyDiff <- guac$Spicy - guac$BaseSpice     # Create Differenced Outcome Variable

#########################
## Data Summary
#########################
kable(guac, "html", booktabs = T, caption = "Data Table") %>%  # Generate Data Table 
  kable_styling(latex_options = "striped")

# Summary Data Destination Data Frame
summ<-data.frame("Statistic"=1:6,"BaseSpice"=1:6,"Garlic"=1:6,
                 "Salt"=1:6,"Spicy"=1:6,"Lime"=1:6,
                 "Onion"=1:6,"Tomato"=1:6,"Overall"=1:6,"SpicyDiff"=1:6)
summ$Statistic <- c("Mean(Control)","Var(Control)","Mean(Treatment)","Var(Treatment)","Mean(All)","Var(All)")

# Loop to create summary statistics
for (j in 1:4){    # For loop identify data group to summarize 
  print(paste("Block ",j," Round"))
  if (j<4){        # If statement to create data group and variance adjustments
    guac_summ<-guac[guac$Block==j,]
    adj1<-4/5      # Within block | treatment or control adjustment (obs = 5)
    adj2<-9/10     # Wihtin block total adjustment (obs =10)
  } else{
    guac_summ<-guac
    adj1<-14/15   # Total sample | treatment or control adjustment (obs = 15)
    adj2<-29/30   # Total sample (obs =30)
  }
  for (i in 5:13){  # For loop to create summary stats
    print(names(guac_summ[i]))
    summ[1,i-3]<-mean(guac_summ[guac_summ$Treatment==0,i])
    summ[2,i-3] <-adj1*var(guac_summ[guac_summ$Treatment==0,i])
    summ[3,i-3]<-mean(guac_summ[guac_summ$Treatment==1,i])
    summ[4,i-3] <-adj1*var(guac_summ[guac_summ$Treatment==1,i])
    summ[5,i-3]<-mean(guac_summ[,i])
    summ[6,i-3] <-adj2*var(guac_summ[,i])
    }                # End for loop
  
  summ[,2:10]<-round(summ[,2:10],2)
  print(summ)
  if (j==1){         # If statement to store data summary
    summ1<-summ
  } else if (j==2){
    summ2<-summ
  } else if (j==3){
    summ3<-summ
  } else {
    summall<-summ
  }                  # End If Statment
  
} # End Summary for loop

# Tables With All Data
kable(summ1, "html", booktabs = T, caption = "Block 1 Data Summary Table") %>%  # Generate Data Summary 
  kable_styling(latex_options = "striped")
kable(summ2, "html", booktabs = T, caption = "Block 2 Data Summary Table") %>%  # Generate Data Summary 
  kable_styling(latex_options = "striped")
kable(summ3, "html", booktabs = T, caption = "Block 3 Data Summary Table") %>%  # Generate Data Summary 
  kable_styling(latex_options = "striped")
kable(summall, "html", booktabs = T, caption = "All Blocks Data Summary Table") %>%  # Generate Data Summary 
  kable_styling(latex_options = "striped")

# Tables With Key Measures
kable(summ1[,c(1,2,5,9,10)], "html", booktabs = T, caption = "Block 1 Data Summary Table") %>%  # Generate Data Summary 
  kable_styling(latex_options = "striped")
kable(summ2[,c(1,2,5,9,10)], "html", booktabs = T, caption = "Block 2 Data Summary Table") %>%  # Generate Data Summary 
  kable_styling(latex_options = "striped")
kable(summ3[,c(1,2,5,9,10)], "html", booktabs = T, caption = "Block 3 Data Summary Table") %>%  # Generate Data Summary 
  kable_styling(latex_options = "striped")
kable(summall[,c(1,2,5,9,10)], "html", booktabs = T, caption = "All Blocks Data Summary Table") %>%  # Generate Data Summary 
  kable_styling(latex_options = "striped")


ggplot(guac,aes(x=BaseSpice,color=factor(Block),fill=factor(Block)))+ 
  geom_histogram(alpha=0.5,bins=20)+
  #geom_vline(data=mu, aes(xintercept=grp.mean), linetype="dashed",show.legend = FALSE)+
  facet_grid(factor(Treatment)~factor(Block))+
  labs(title="Baseline Spice Score Results",x="Spice Score by Block",y="Count by Treatment Status",fill=" ",color=" ",
       subtitle="  Q: On the following scale, please rate your opinion about the spicy level in the guacamole:\
       1 = Very Weak, 2 = Weak, 3 = Balanced, 4 = Strong, 5 = Very Strong")+
  theme_classic()+
  theme(legend.position="none")

#mu<-ddply(data3,"year",summarise,grp.mean=mean(turnout)) #Df of Mean Stats
ggplot(guac,aes(x=Spicy,color=factor(Block),fill=factor(Block)))+ 
  geom_histogram(alpha=0.5,bins=20)+
  facet_grid(factor(Treatment)~factor(Block))+
  labs(title="Outcome Spice Score Results",x="Spice Score by Block",y="Count by Treatment Status",fill=" ",color=" ",
       subtitle="  Q: On the following scale, please rate your opinion about the spicy level in the guacamole:\
       1 = Very Weak, 2 = Weak, 3 = Balanced, 4 = Strong, 5 = Very Strong")+
  theme_classic()+
  theme(legend.position="none")

#mu<-ddply(data3,"year",summarise,grp.mean=mean(turnout)) #Df of Mean Stats
ggplot(guac,aes(x=Overall,color=factor(Block),fill=factor(Block)))+ 
  geom_histogram(alpha=0.5,bins=20)+
  #geom_vline(data=mu, aes(xintercept=grp.mean), linetype="dashed",show.legend = FALSE)+
  facet_grid(factor(Treatment)~factor(Block))+
  labs(title="Outcome Overall Taste Score Results",x="Spice Score by Block",y="Count by Treatment Status",fill=" ",color=" ",
       subtitle="  Q: On the following scale, please rate your opinion about the overall taste level of the 
       guacamole: 1 = Terrible, 2 = Poor, 3 = So-So, 4 = Good, 5 = Great")+
  theme_classic()+
  theme(legend.position="none")

#########################
## Balance Check
#########################

# Balance Check with Main Covariate the Baseline Spice Variable 
summary(b1<-lm(BaseSpice~Treatment+factor(Block),data=guac))         # Balance Check - Spice

stargazer(b1,
          header = FALSE,
          title = "Balance Check for BaseSpice",
          type = "text",
          omit.stat=c("LL","ser","f"),
          #table.placement = "!htbp",
          #column.labels =c("1950","1951","1952"),
          omit = "Block",
          add.lines = list(c("Blocked","Yes")),
          style = "ajps"
)

# Balance Check with Other Baseline Covariates 
summary(lm(baseline$Garlic~Treatment+factor(Block),data=guac))  # Balance Check - Garlic
summary(lm(baseline$Salt~Treatment+factor(Block),data=guac))    # Balance Check - Salt
summary(lm(baseline$Lime~Treatment+factor(Block),data=guac))    # Balance Check - Lime
summary(lm(baseline$Onion~Treatment+factor(Block),data=guac))   # Balance Check - Onion
summary(lm(baseline$Tomato~Treatment+factor(Block),data=guac))  # Balance Check - Tomato
summary(lm(baseline$Overall~Treatment+factor(Block),data=guac)) # Balance Check - Overall

#########################
## Analyze Main Outcome Measures with Regression
#########################

mean(guac$BaseSpice) # Average Baseline Spice = 1.366667
mean(guac$Spicy)     # Average Spice Level of Moderate and Spicy Guac = 2.633333
mean(guac$Spicy[guac$Treatment==0]) # Average Control Guac Spice = 2.333333
mean(guac$Spicy[guac$Treatment==1]) # Average Treated Guac Spice = 2.933333

summary(lm(Spicy~Treatment+factor(Block),data=guac))
summary(lm(Spicy~Treatment+factor(Block)+BaseSpice,data=guac))
summary(lm(SpicyDiff~Treatment+factor(Block),data=guac))

#########################
## Analyze Other Outcome Measures with Regression
#########################
# Overall - Slightly Better Tasting overall
summary(lm(Overall~Treatment+factor(Block),data=guac))  # Overall

# Other Ingredients - None test better
summary(c1<-lm(Garlic~Treatment+factor(Block),data=guac))   # Garlic
summary(c2<-lm(Salt~Treatment+factor(Block),data=guac))     # Salt
summary(c3<-lm(Lime~Treatment+factor(Block),data=guac))     # Lime
summary(c4<-lm(Onion~Treatment+factor(Block),data=guac))    # Onion
summary(c5<-lm(Tomato~Treatment+factor(Block),data=guac))   # Tomato

stargazer(c1,c2,c3,c4,c5,
          header = FALSE,
          title = "Other Outcome Measures",
          type = "html",
          omit.stat=c("LL","ser","f"),
          #table.placement = "!htbp",
          #column.labels =c("1950","1951","1952"),
          omit = "Block",
          add.lines = list(c("Blocked","Yes", "Yes","Yes","Yes","Yes")),
          style = "ajps"
)


#########################
## RI Reporting Table
#########################
# Note: Will Create location to store resuls from randomization inference
# Note: Collect: ATE estimate, SE, 95% CI, and P-value from one tailed test
RiReport <- data.frame("Y"=1:4,"Covariate"=1:4,"ATE"=1:4,"SE"=1:4,"CI_Lower"=1:4,"CI_Upper"=1:4,"PValue"=1:4)

RiReport$Y<-c("Spice Score","Spice Score","Spice Score Difference","Overall Score")
RiReport$Covariate <-c("Baseline Spice","NA","NA Used to Scale Y","NA")

#########################
# Randomization Inference 1 - Difference-in-Means Estimator with Baseline: Y = Spicy Level
#########################
Z <- guac$Treatment
Y <- guac$Spicy
X <- guac$BaseSpice
block <- guac$Block

perms <- genperms(Z=Z, blockvar=block,maxiter = 10000) # Permutations
probs <-  genprobexact(Z = Z, blockvar = block)        # Probability of treatment
table(probs,block)                                     # Confirm treatment probability is identical across blocks

ate <- estate(Y,Z,X,prob=probs)                        # Estimate the ATE

# Store Estimates (1)
RiReport$ATE[1] <- round(ate,4)                        # Store ATE estimate in the RiReport Table 

# Get Confidence Intervals
Ys <- genouts(Y,Z,ate=ate)                     # Generate potential outcomes under tau = ATE
distout <- gendist(Ys, perms, X=X, prob=probs) # Generate sampling dist. under sharp null
dispdist(distout,ate)                          # Display characteristics of sampling dist. for inference
mean(distout)                                  # Mean of Sampling Distribution
ate

# Store Estimates (2)
RiReport$SE[1] <- round(dispdist(distout,ate,display.plot = FALSE)$sd,4)                 # Store SE from test ATE=ATE in the RiReport Table 
RiReport$CI_Lower[1] <- round(dispdist(distout,ate,display.plot = FALSE)$quantile[1],4)  # Store CI_L from test ATE=ATE in the RiReport Table 
RiReport$CI_Upper[1] <- round(dispdist(distout,ate,display.plot = FALSE)$quantile[2],4)  # Store CI_U from test ATE=ATE in the RiReport Table 

# Test Sharp Null (No Effect for Every Unit)
Ys <- genouts(Y,Z,ate=0) # generate potential outcomes under sharp null
distout <- gendist(Ys, perms, X=X, prob=probs) # generate sampling dist. under tau = 0
dispdist(distout,ate) # display characteristics of sampling dist. for inference
sum(distout >= ate)                 # one-tailed comparison used to calculate p-value (greater than)

# Store Estimates (3)
RiReport$PValue[1] <- round(dispdist(distout,ate,display.plot = FALSE)$greater.p.value,4) # Store one tailed p-value from Sharp Null Test in the RiReport Table 

# plot results with and without covariate adjustment
resresplot(Y,Z,probs,scale=1)
resresplot(Y,Z,X,prob=probs,scale=1)

#########################
# Randomization Inference 2 - Difference-in-Means Estimator without Baseline: Y = Spicy Level
#########################
ate <- estate(Y,Z,prob=probs)    # Estimate the ATE

# Store Estimates (1)
RiReport$ATE[2] <- round(ate,4)  # Store ATE estimate in the RiReport Table 

# Get Confidence Intervals
Ys <- genouts(Y,Z,ate=ate)                # Generate potential outcomes under tau = ATE
distout <- gendist(Ys, perms, prob=probs) # Generate sampling dist. under sharp null
dispdist(distout,ate)                     # Display characteristics of sampling dist. for inference
mean(distout)                             # Mean of Sampling Distribution
ate

# Store Estimates (2)
RiReport$SE[2] <- round(dispdist(distout,ate,display.plot = FALSE)$sd,4)                 # Store SE from test ATE=ATE in the RiReport Table 
RiReport$CI_Lower[2] <- round(dispdist(distout,ate,display.plot = FALSE)$quantile[1],4)  # Store CI_L from test ATE=ATE in the RiReport Table 
RiReport$CI_Upper[2] <- round(dispdist(distout,ate,display.plot = FALSE)$quantile[2],4)  # Store CI_U from test ATE=ATE in the RiReport Table 

# Test Sharp Null (No Effect for Every Unit)
Ys <- genouts(Y,Z,ate=0) # generate potential outcomes under sharp null
distout <- gendist(Ys, perms, prob=probs) # generate sampling dist. under tau = 0
dispdist(distout,ate) # display characteristics of sampling dist. for inference
sum(distout >= ate)                 # one-tailed comparison used to calculate p-value (greater than)

# Store Estimates (3)
RiReport$PValue[2] <- round(dispdist(distout,ate,display.plot = FALSE)$greater.p.value,4) # Store one tailed p-value from Sharp Null Test in the RiReport Table 

#########################
# Randomization Inference 3 - Difference-in-Differences Estimator: Y = Spicy Level - Baseline Spice Level
#########################
Y <- guac$SpicyDiff           # Set new Outcome Variable
ate <- estate(Y,Z,prob=probs) # estimate the ATE

# Store Estimates (1)
RiReport$ATE[3] <- round(ate,4)  # Store ATE estimate in the RiReport Table 

# Get Confidence Intervals
Ys <- genouts(Y,Z,ate=ate) # generate potential outcomes under tau = ATE
distout <- gendist(Ys, perms, prob=probs) # generate sampling dist. under sharp null
dispdist(distout,ate) # display characteristics of sampling dist. for inference
mean(distout)
ate

# Store Estimates (2)
RiReport$SE[3] <- round(dispdist(distout,ate,display.plot = FALSE)$sd,4)                 # Store SE from test ATE=ATE in the RiReport Table 
RiReport$CI_Lower[3] <- round(dispdist(distout,ate,display.plot = FALSE)$quantile[1],4)  # Store CI_L from test ATE=ATE in the RiReport Table 
RiReport$CI_Upper[3] <- round(dispdist(distout,ate,display.plot = FALSE)$quantile[2],4)  # Store CI_U from test ATE=ATE in the RiReport Table 


# Test Sharp Null (No Effect for Every Unit)
Ys <- genouts(Y,Z,ate=0) # generate potential outcomes under sharp null
distout <- gendist(Ys, perms, prob=probs) # generate sampling dist. under tau = 0
dispdist(distout,ate) # display characteristics of sampling dist. for inference
sum(distout >= ate)                 # one-tailed comparison used to calculate p-value (greater than)

# Store Estimates (3)
RiReport$PValue[3] <- round(dispdist(distout,ate,display.plot = FALSE)$greater.p.value,4) # Store one tailed p-value from Sharp Null Test in the RiReport Table 

#########################
# Randomization Inference 4 - Difference-in-Means Estimator: Y = Overall Taste (no covariate)
#########################
Y <- guac$Overall           # Set new Outcome Variable
ate <- estate(Y,Z,prob=probs) # estimate the ATE

# Store Estimates (1)
RiReport$ATE[4] <- round(ate,4)  # Store ATE estimate in the RiReport Table 


# Get Confidence Intervals
Ys <- genouts(Y,Z,ate=ate) # generate potential outcomes under tau = ATE
distout <- gendist(Ys, perms, prob=probs) # generate sampling dist. under sharp null
dispdist(distout,ate) # display characteristics of sampling dist. for inference
mean(distout)
ate

# Store Estimates (2)
RiReport$SE[4] <- round(dispdist(distout,ate,display.plot = FALSE)$sd,4)                 # Store SE from test ATE=ATE in the RiReport Table 
RiReport$CI_Lower[4] <- round(dispdist(distout,ate,display.plot = FALSE)$quantile[1],4)  # Store CI_L from test ATE=ATE in the RiReport Table 
RiReport$CI_Upper[4] <- round(dispdist(distout,ate,display.plot = FALSE)$quantile[2],4)  # Store CI_U from test ATE=ATE in the RiReport Table 

# Test Sharp Null (No Effect for Every Unit)
Ys <- genouts(Y,Z,ate=0) # generate potential outcomes under sharp null
distout <- gendist(Ys, perms, prob=probs) # generate sampling dist. under tau = 0
dispdist(distout,ate) # display characteristics of sampling dist. for inference
sum(distout >= ate)                 # one-tailed comparison used to calculate p-value (greater than)

# Store Estimates (3)
RiReport$PValue[4] <- round(dispdist(distout,ate,display.plot = FALSE)$greater.p.value,4) # Store one tailed p-value from Sharp Null Test in the RiReport Table 

#########################
## Create RI Reporting Table
#########################
kable(RiReport, "html", booktabs = T, caption = "Randomization Inference Results") %>%  # Generate Administration Table 
  kable_styling(latex_options = "striped")

#########################
## Covariate Prognostic Evaluation
#########################
summary(p1<-lm(Spicy~BaseSpice+factor(Block),data=guac))
summary(p2<-lm(Spicy~BaseSpice+factor(Block),data=subset(guac,guac$Treatment==0)))
summary(p3<-lm(Spicy~BaseSpice+factor(Block),data=subset(guac,guac$Treatment==1)))

stargazer(p1,p2,p3,
          header = FALSE,
          title = "Covariate Prognostic Evaluation",
          type = "html",
          omit.stat=c("LL","ser","f"),
          #table.placement = "!htbp",
          #column.labels =c("1950","1951","1952"),
          omit = "Block",
          add.lines = list(c("Blocked","Yes", "Yes","Yes"),
                           c("Subjects","All","Control","Treatment")),
          style = "ajps"
)



