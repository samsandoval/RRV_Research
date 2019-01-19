# Code to run the RRV study
# Sam Sandoval 10/20/2018

# ______________ Section 1 : Basic Information ____________________________
##_______________        1.1 Input Variables
library(plyr)
rm(list = ls())
timesteps = 100
Num_of_traces = 1000
Reliability_Target <- 0.1
Initial_Year_Def_NoDef = 0 # If the initial year starts with No Deficit then, Initial_seed = 0, Otherwise Initial_seed = 1 

##_______________        1.2 Miscellaneous
Counter_reliability = 1
Number_of_cycles = 9
Num_of_TimeSteps = timesteps + 1
set.seed(150)
# The line below generates n number of random numbers
dat = runif(Num_of_TimeSteps*Num_of_traces, 0, 1)
# Creating the matrix that will be filled
Matrix_Reliability <- matrix(data = NA, nrow = Num_of_traces, ncol = 1)
colnames(Matrix_Reliability) <- c("Reliability")
Matrix_Count_Resilience <- matrix(data = NA, nrow = timesteps, ncol = Num_of_traces)
colnames(Matrix_Count_Resilience) <- paste("Trace", 1:Num_of_traces)
Matrix_Resilience <- matrix(data = NA, nrow = Num_of_traces, ncol = 1)
colnames(Matrix_Resilience) <- c("Resilience")
Matrix_Count_Fragility <- matrix(data = NA, nrow = timesteps, ncol = Num_of_traces)
colnames(Matrix_Count_Fragility) <- paste("Trace", 1:Num_of_traces)
Matrix_Fragility <- matrix(data = NA, nrow = Num_of_traces, ncol = 1)
colnames(Matrix_Fragility) <- c("Fragility")
Matrix_Reliability_All <- matrix(data = NA, nrow = Num_of_traces, ncol = Number_of_cycles)
Matrix_Resilience_All <- matrix(data = NA, nrow = Num_of_traces, ncol = Number_of_cycles)
Matrix_Fragility_All <- matrix(data = NA, nrow = Num_of_traces, ncol = Number_of_cycles)
# ______________ End of Section 1 _________________________________________

# ______________ Section 2: Performance Criteria __________________________
##_______________        2.1 Estimating the Deficit - No Deficit Matrix
for (Counter_reliability in 1:Number_of_cycles)
{
  Matrix_Def_NoDef <- matrix (data = dat, nrow = Num_of_TimeSteps, ncol = Num_of_traces)
  colnames(Matrix_Def_NoDef) <- paste("Trace", 1:Num_of_traces)
  # Deficit = 1, No Deficit = 0
  Matrix_Def_NoDef <- ifelse(Matrix_Def_NoDef <= Reliability_Target, 0, 1)
  Matrix_Def_NoDef[1,] <- Initial_Year_Def_NoDef
  Matrix_Total_Def <- matrix(data = colSums(Matrix_Def_NoDef ==1)- ifelse((Initial_Year_Def_NoDef==1),1,0) )
  colnames(Matrix_Total_Def) <- c("Number of Deficits")
  
  ##_______________        2.2 Reliability Matrix
  Matrix_Reliability <- ((timesteps - Matrix_Total_Def)/timesteps)
  
  ##_______________        2.3 Resilience Matrix
  for (j in 1:Num_of_traces){
    for(i in 1:timesteps)
    {Matrix_Count_Resilience[i,j] = ifelse((Matrix_Def_NoDef[i,j] == 1)&(Matrix_Def_NoDef[i+1,j]==0),1,0)}}
  Matrix_Total_Count_Resil <- matrix(data = colSums(Matrix_Count_Resilience !=0))
  for(i in 1:Num_of_traces)
  {Matrix_Resilience[i,] = ifelse((Matrix_Total_Def[i,]==timesteps),0,ifelse( (Matrix_Total_Def[i,]==0),1,ifelse((Matrix_Total_Def[i,]==1)&(Matrix_Def_NoDef[timesteps,i]==1),1,ifelse((Matrix_Total_Def[i,]==timesteps-1)&(Matrix_Def_NoDef[1,i]==0),0,Matrix_Total_Count_Resil[i,]/Matrix_Total_Def[i,]))))}
  
  ##_______________        2.3 Fragility Matrix
  for (j in 1:Num_of_traces){
    for(i in 1:timesteps)
    {Matrix_Count_Fragility[i,j] = ifelse((Matrix_Def_NoDef[i,j] == 0)&(Matrix_Def_NoDef[i+1,j]==1),1,0)}}
  Matrix_Total_Count_Fragility <- matrix(data = colSums(Matrix_Count_Fragility !=0))
  colnames(Matrix_Total_Count_Fragility) <- paste("Number of Fragility")
  Matrix_Total_No_Def <- matrix(data = colSums(Matrix_Def_NoDef ==0)- ifelse((Initial_Year_Def_NoDef==0),1,0))
  colnames(Matrix_Total_No_Def) <- c("Number of No Deficits")
  for(i in 1:Num_of_traces)
  {Matrix_Fragility[i,] = ifelse((Matrix_Total_Def[i,]==timesteps),1,ifelse( (Matrix_Total_Def[i,]==0),0,ifelse((Matrix_Total_Def[i,]==timesteps-1)&(Matrix_Def_NoDef[timesteps,i]==0),1,ifelse((Matrix_Total_Def[i,]==1)&(Matrix_Def_NoDef[1,i]==1),0,Matrix_Total_Count_Fragility[i,]/Matrix_Total_No_Def[i,]))))}
  
  ##_______________        2.4 Integrating Matrix
  Matrix_Reliability_All[,Counter_reliability] <- matrix (data = Matrix_Reliability)
  Matrix_Resilience_All[,Counter_reliability] <- matrix (data = Matrix_Resilience)
  Matrix_Fragility_All[,Counter_reliability] <- matrix (data = Matrix_Fragility) 
  
  Reliability_Target = Reliability_Target + 0.1
}

##_______________        2.5 Matrix Min and Max
Matrix_Relia_Min_Max <- matrix(data = NA, nrow = timesteps, ncol = 1)
for(i in 1:timesteps){Matrix_Relia_Min_Max[i,] = (timesteps-i)/100 }

Matrix_Resil_Min <- matrix(data = NA, nrow = timesteps, ncol = 1)
for(i in 1:timesteps){Matrix_Relia_Min_Max[i,] = (timesteps/i)/100}
Matrix_Relia_Min_Max
Matrix_Resil_Min

# ______________ Section 3 : Plotting ____________________________
plot(Matrix_Reliability_All,Matrix_Resilience_All,main="Reliability Vs Resilience",xlab="Reliability",ylab="Resilience")