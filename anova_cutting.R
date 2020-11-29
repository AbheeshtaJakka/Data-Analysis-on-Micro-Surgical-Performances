library(reshape2)

#read file
performance <- read.csv("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/MicrosurgeryPerformance.csv")
head(performance)

attach(performance)
cutting_scorer1 <- data.frame(c(1:17),Score.Cut1,Score.Cut1.1,Score.Cut1.2,Score.Cut1.3,Score.Cut1.4)
colnames(cutting_scorer1) <- c("subject","session1","session2","session3","session4","session5")

cutting_scorer1 <- cutting_scorer1[-17,]
cutting_scorer1 <- cutting_scorer1[-16,]

#calculating mean scores
cutting_scorer1$session1 <- as.numeric(as.character(cutting_scorer1$session1))
mean(cutting_scorer1$session1)
mean(cutting_scorer1$session2)
mean(cutting_scorer1$session3)
mean(cutting_scorer1$session4)
mean(cutting_scorer1$session5)


#melting the dataframe to make it easy to plot
cutting_scorer1 <- melt(cutting_scorer1, id = c("subject"))
colnames(cutting_scorer1) <- c("subjects","sessions","score")
cuttingfactor <- factor(sessions)

#performing anova where score being the dependent variable and cuttingfactor is the independent variable
analysis <- aov(score ~ cuttingfactor)
summary(analysis)

mean(cutting_scorer1[16:30,3])
ggplot(data = cutting_scorer1, aes(x = sessions,y = score)) + geom_boxplot()

plot(score ~ cuttingfactor)
