# sensitivity-specificity_analisis-
analysis
# Sensitivity & Specificity analysis
#@jVillanueva 
# Load data 
library(readxl)
ptx <- read_excel("C:/Users/jvillanueva/Desktop/ptx.xlsx")

# Converter to numeric 
ptx$F1A <- as.numeric(factor(ptx$F1A, levels= c("S", "W")))
ptx$F1B <- as.numeric(factor(ptx$F1B, levels= c("S", "W")))
ptx$F2A <- as.numeric(factor(ptx$F2A, levels= c("S", "W")))
ptx$F2B <- as.numeric(factor(ptx$F2B, levels= c("S", "W")))
ptx$F3A <- as.numeric(factor(ptx$F3A, levels= c("S", "W")))
ptx$F3B <- as.numeric(factor(ptx$F3B, levels= c("S", "W")))
ptx$PSG1 <- as.numeric(factor(ptx$PSG1, levels= c("S", "W")))
ptx$PSG2 <- as.numeric(factor(ptx$PSG2, levels= c("S", "W")))
ptx$PSG3 <- as.numeric(factor(ptx$PSG3, levels= c("S", "W")))
#str(ptx)    

# as factor 
ptx$F1A <- as.factor(ptx$F1A)      # Threhold 20
ptx$F1B <- as.factor(ptx$F1B)      # Threhold 40
ptx$PSG1 <- as.factor(ptx$PSG1)    # Reference 

ptx$F2A <- as.factor(ptx$F2A)      # Threhold 20
ptx$F2B <- as.factor(ptx$F2B)      # Threhold 40
ptx$PSG2 <- as.factor(ptx$PSG2)

ptx$F3A <- as.factor(ptx$F3A)     # Threhold 20
ptx$F3B <- as.factor(ptx$F3B)     # Threhold 40
ptx$PSG3 <- as.factor(ptx$PSG3)
#str(ptx)

# Confusion matrix 
# TH20_ phase1
y1 <- table(ptx$F1A,ptx$PSG1)   
colnames(y1) <- c("SleepPSG", "WakePSG")
rownames(y1) <- c ("SleepAct", "WakeAct")
#y1 

# TH40_phase1
y2 <- table(ptx$F1B,ptx$PSG1)  
colnames(y2) <- c("SleepPSG", "WakePSG")
rownames(y2) <- c ("SleepAct", "WakeAct")
#y2  

# TH20_ phase2
y3 <- table(ptx$F2A,ptx$PSG2)  
colnames(y3) <- c("SleepPSG", "WakePSG")
rownames(y3) <- c ("SleepAct", "WakeAct")
# y3

# TH40_phase2
y4 <- table(ptx$F2B,ptx$PSG2)  
colnames(y4) <- c("SleepPSG", "WakePSG")
rownames(y4) <- c ("SleepAct", "WakeAct")
# y4 

# TH20_phase2
y5 <- table(ptx$F3A,ptx$PSG3)   # 20
colnames(y5) <- c("SleepPSG", "WakePSG")
rownames(y5) <- c ("SleepAct", "WakeAct")
#y5 

# TH40_phase2
y6 <- table(ptx$F3B,ptx$PSG3)   # 20
colnames(y6) <- c("SleepPSG", "WakePSG")
rownames(y6) <- c ("SleepAct", "WakeAct")
#y6 

# Compute Se and Sp

library(caret)
se1 <-  sensitivity(ptx$PSG1, ptx$F1A)*100
sp1 <-  specificity(ptx$PSG1, ptx$F1A)*100
se1B<- sensitivity(ptx$PSG1, ptx$F1B)* 100
sp1B<- specificity(ptx$PSG1, ptx$F1B)*100

se2 <- sensitivity(ptx$PSG2, ptx$F2A)*100
sp2 <-specificity(ptx$PSG2, ptx$F2A)*100
se2B <- sensitivity(ptx$PSG2, ptx$F2B)*100
sp2B <-specificity(ptx$PSG2, ptx$F2B)*100

se3 <-sensitivity(ptx$PSG3, ptx$F3A)*100
sp3 <-specificity(ptx$PSG3, ptx$F3A)*100
se3B <-sensitivity(ptx$PSG3, ptx$F3B)*100
sp3B <-specificity(ptx$PSG3, ptx$F3B)*100

# Accuracy 
Acc20_f1 <-((y1[[1]]+ y1[[4]])/(y1[[1]]+ y1[[2]]+ y1[[3]]+ y1[[4]]))*100 # TH20_ phase1
Acc40_f1 <-((y2[[1]]+ y2[[4]])/(y2[[1]]+ y2[[2]]+ y2[[3]]+ y2[[4]]))*100 # TH40_ phase1
Acc20_f2 <-((y3[[1]]+ y3[[4]])/(y3[[1]]+ y3[[2]]+ y3[[3]]+ y3[[4]]))*100 # TH20_ phase2
Acc40_f2 <-((y4[[1]]+ y4[[4]])/(y4[[1]]+ y4[[2]]+ y4[[3]]+ y4[[4]]))*100 # TH40_ phase2
Acc20_f3 <-((y3[[1]]+ y3[[4]])/(y3[[1]]+ y3[[2]]+ y3[[3]]+ y3[[4]]))*100 # TH20_ phase3
Acc40_f3 <-((y4[[1]]+ y4[[4]])/(y4[[1]]+ y4[[2]]+ y4[[3]]+ y4[[4]]))*100 # TH40_ phase3

# Summary 
gral_table <- matrix(c(se1, se2, se3, sp1, sp2, sp3,
                       Acc20_f1, Acc20_f2, Acc20_f3), ncol=3)
colnames(gral_table) <- c('Sensitivity', 'Specificity', 'Accuracy')  # TH_20
rownames(gral_table) <- c('epoch-1', 'epoch-2', 'epoch-3')
print (gral_table) 
