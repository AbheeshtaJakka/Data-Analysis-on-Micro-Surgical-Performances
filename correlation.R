micro <- read.csv("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/MicrosurgeryPerformance.csv")
micro <- micro[-16:-17,]
head(micro,1)
micro <- as.data.frame(micro)

subject1 <- data.frame(micro$Cutting.Time.1)
subject1

#forsubject1
cut1nasa <- read.csv("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/subjects/extracted/subject01/subject01/session1/Subject01_Cutting1_NASA.csv")
cut2nasa <- read.csv("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/subjects/extracted/subject01/subject01/session2/Subject01_Cutting2_NASA.csv")
cut3nasa <- read.csv("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/subjects/extracted/subject01/subject01/session3/Subject01_Cutting3_NASA.csv")
cut4nasa <- read.csv("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/subjects/extracted/subject01/subject01/session4/Subject01_Cutting4_NASA.csv")
cut5nasa <- read.csv("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/subjects/extracted/subject01/subject01/session5/Subject01_Cutting5_NASA.csv")

time <- c(595,359,487,153,245)
#time <- c(micro[1,5],micro[1,12],micro[1,19],micro[1,26],micro[1,33])
subject1 <- data.frame(time,c(cut1nasa[6,2],cut2nasa[6,2],cut3nasa[6,2],cut4nasa[6,2],cut5nasa[6,2]))
colnames(subject1) <- c("time","frustration")

subject1$MentalDemand <- c(cut1nasa[1,2],cut2nasa[1,2],cut3nasa[1,2],cut4nasa[1,2],cut5nasa[1,2])
subject1$PhysicalDemand <- c(cut1nasa[2,2],cut2nasa[2,2],cut3nasa[2,2],cut4nasa[2,2],cut5nasa[2,2])
subject1$TemporalDemand <- c(cut1nasa[3,2],cut2nasa[3,2],cut3nasa[3,2],cut4nasa[3,2],cut5nasa[3,2])
subject1$Performance <- c(cut1nasa[4,2],cut2nasa[4,2],cut3nasa[4,2],cut4nasa[4,2],cut5nasa[4,2])
subject1$Effort <- c(cut1nasa[5,2],cut2nasa[5,2],cut3nasa[5,2],cut4nasa[5,2],cut5nasa[5,2])

attach(subject1)
#model <- lm(time ~ PhysicalDemand + frustration + Performance + Effort, data = subject1)
model <- lm(time ~ ., data = subject1)
summary(model)

summary(lm(time ~., data = subject1))$coefficients

alias(model)

summary(lm(time ~ TemporalDemand + frustration))



plot(subject1$Effort,subject1$time) 
cor.test( ~ subject1$time + subject1$frustration, 
         data= subject1,
         method = "spearman",
         continuity = FALSE,
         conf.level = 0.95)

library(corrplot)
corrplot(cor(subject1), type = "upper", method = "number")


library(ggplot2)

ggplot(data = subject1, aes(x = MentalDemand, y = time)) + geom_point(shape = 15, color = "blue", size = 3) +
  geom_point(data = subject1,aes(x = PhysicalDemand, y = time), shape = 16, color = "red", size = 3) +
  geom_point(data = subject1, aes(x = TemporalDemand, y = time), shape = 17,color = "black", size = 3) +
  geom_point(data = subject1,aes(x =  Performance, y = time), shape = 18, color = "green", size = 3) +
  geom_point(data = subject1, aes(x = Effort, y = time), shape = 19,color = "yellow", size = 3) +
  geom_point(data = subject1, aes(x = frustration, y = time), shape = 21,color = "pink",fill = "pink", size = 3) 
  