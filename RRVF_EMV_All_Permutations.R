# Code to run the RRV study
# Sam Sandoval & Pablo Ortiz 10/31/2018
# ______________ Section 1 : Basic Information ____________________________
##_______________        1.1 Input Variables
#install.packages("plotly")
#install.packages("rgl")
#install.packages("plot3D")
#update.packages("ggplot2")
library(plyr)
library(plotly)
require("rgl")
require("RColorBrewer")
rm(list = ls())
timesteps = 100
Vulnerability = 20
EMV_No_Def = 100
Half_Permutations = 0
Half_TS = timesteps/2
##_______________        1.1 Estimate Number of permutations & building matrices
for (counter in 1:Half_TS){Half_Permutations <- Half_Permutations + counter}
Permutations <- Half_Permutations*2
Reliability <- matrix(data = NA, nrow = Permutations, ncol = 1)
Resilience  <- matrix(data = NA, nrow = Permutations, ncol = 1)
Fragility   <- matrix(data = NA, nrow = Permutations, ncol = 1)
EMV         <- matrix(data = NA, nrow = Permutations, ncol = 1)
EMV_Typical <- matrix(data = NA, nrow = Permutations, ncol = 1)
EMV_4       <- matrix(data = NA, nrow = Permutations, ncol = 1)
# ______________ Section 2 : Estimating RRVF & EMV ____________________________
##_______________        2.1 Upper half of Reliability
counter = 0
j = 0
Deficits = 0
for (j in 1:Half_TS){
  for (Deficits in 1:j){
    counter <- counter + 1
    Reliability [counter,] <- (1 - j/timesteps)
    Resilience [counter,] <- (Deficits/j)
    Fragility[counter,] <- (Deficits/(timesteps-j))
    EMV[counter,] <- ((Reliability[counter,]*(1-Fragility[counter,]))*(EMV_No_Def*2) + (Reliability[counter,]*Fragility[counter,])*(EMV_No_Def+(EMV_No_Def-Vulnerability)) + ((1-Reliability[counter,])*Resilience[counter,])*((EMV_No_Def-Vulnerability)+EMV_No_Def) + ((1-Reliability[counter,])*(1-Resilience[counter,]))*((EMV_No_Def-Vulnerability)*2))/2
    EMV_Typical[counter,] <- (Reliability[counter,]*EMV_No_Def)+((1-Reliability[counter,])*(EMV_No_Def-Vulnerability))
  } }
##_______________        2.1 Lower half of Reliability
a = 0; b = 0
counter <- Permutations/2
for (a in (Half_TS+1):(timesteps-1)){
  for (b in 1:(timesteps-a)) {
    counter <- counter + 1
    Reliability [counter,] <- (1 - a/timesteps)
    Resilience [counter,] <- (b/a)
    Fragility[counter,] <- (b/(timesteps-a))
    EMV[counter,] <- ((Reliability[counter,]*(1-Fragility[counter,]))*(EMV_No_Def*2) + (Reliability[counter,]*Fragility[counter,])*(EMV_No_Def+(EMV_No_Def-Vulnerability)) + ((1-Reliability[counter,])*Resilience[counter,])*((EMV_No_Def-Vulnerability)+EMV_No_Def) + ((1-Reliability[counter,])*(1-Resilience[counter,]))*((EMV_No_Def-Vulnerability)*2))/2
    EMV_Typical[counter,] <- (Reliability[counter,]*EMV_No_Def)+((1-Reliability[counter,])*(EMV_No_Def-Vulnerability))
  }
}

##_______________        2.3 EMV Trees
Number_rows <- nrow(Reliability)
aa = 1; a = 0; b=0; c=0; d=0
for (aa in 1:Number_rows){
  EMV_4[aa,] = 0
  for (a in 0:1){
    for (b in 0:1){
      for (c in 0:1){
        for (d in 0:1){
          Benefits <- (((EMV_No_Def)*(1-a)+(EMV_No_Def-Vulnerability)*(a)) + 
                       ((EMV_No_Def)*(1-b)+(EMV_No_Def-Vulnerability)*(b)) + 
                       ((EMV_No_Def)*(1-c)+(EMV_No_Def-Vulnerability)*(c)) + 
                       ((EMV_No_Def)*(1-d)+(EMV_No_Def-Vulnerability)*(d)))/4
          Probability <- ((Reliability[aa,])*(1-a)+(1-Reliability[aa,])*(a)) *
            ((1-a)*((1-Fragility[aa,])*(1-b)+(Fragility[aa,])*(b))+(a)*((Resilience[aa,])*(1-b)+(1-Resilience[aa,])*(b)))*
            ((1-b)*((1-Fragility[aa,])*(1-c)+(Fragility[aa,])*(c))+(b)*((Resilience[aa,])*(1-c)+(1-Resilience[aa,])*(c)))*
            ((1-c)*((1-Fragility[aa,])*(1-d)+(Fragility[aa,])*(d))+(c)*((Resilience[aa,])*(1-d)+(1-Resilience[aa,])*(d)))
          EMV_4[aa,] <- EMV_4[aa,] + Benefits*Probability 
        } 
        d == 0 
      } 
      c == 0 
    } 
    b == 0 
  } 
}
Performance_Matrix <- cbind(Reliability,Resilience,Fragility,EMV_Typical,EMV,EMV_4)
colnames(Performance_Matrix) <- c("Reliability","Resilience","Fragility","EMV 1","EMV 2","EMV 4")

# ______________ Section 3 : Plotting ____________________________
plot(Reliability,Fragility,main="Reliability Vs Fragility",xlab="Reliability",ylab="Fragility")
plot(Reliability,Resilience,main="Reliability Vs Resilience",xlab="Reliability",ylab="Resilience")

p <- plot_ly(x = Reliability[,], y = Resilience[,], z = EMV[,], color = EMV[,]) %>%
 # marker = list(size = 10)
 add_markers()
 # add_surface()
p

plot3d(Reliability[,], #x variable
       Resilience[,],  #y variable
       EMV[,],         #z variable
       xlab = "Reliability",
       ylab = "Resilience",
       zlab = "EMV (Expected Monetary Value)",
       col = brewer.pal(5,"Blues"),
       size = 6)


