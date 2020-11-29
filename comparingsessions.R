install.packages("ggplot2")
library(ggplot2)

micro <- read.csv("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/MicrosurgeryPerformance.csv")
head(micro,1)

micro <- as.data.frame(micro)
micro <- micro[-16:-17,]
#scorer1 
avgscorecut1 <- mean(as.numeric(as.character(micro$Score.Cut1)))
avgscorecut2 <- mean(as.numeric(as.character(micro$Score.Cut1.1)))
avgscorecut3 <- mean(as.numeric(as.character(micro$Score.Cut1.2)))
avgscorecut4 <- mean(as.numeric(as.character(micro$Score.Cut1.3)))
avgscorecut5 <- mean(as.numeric(as.character(micro$Score.Cut1.4)))

#averagecuttingscoresacrosssessions
x <- data.frame(c(1,2,3,4,5),c(avgscorecut1,avgscorecut2,avgscorecut3,avgscorecut4,avgscorecut5))
colnames(x) <- c("sessions","scores")
plot(x$sessions,x$scores, xlab = "Sessions", ylab = "cuttingscores", type = "l")

#averagesuturingscoresacrosssessions
avgscoresut1 <- mean(as.numeric(as.character(micro$Score.Sut1)))
avgscoresut2 <- mean(as.numeric(as.character(micro$Score.Sut1.1)))
avgscoresut3 <- mean(as.numeric(as.character(micro$Score.Sut1.2)))
avgscoresut4 <- mean(as.numeric(as.character(micro$Score.Sut1.3)))
avgscoresut5 <- mean(as.numeric(as.character(micro$Score.Sut1.4)))

x <- data.frame(c(1,2,3,4,5),c(avgscoresut1,avgscoresut2,avgscoresut3,avgscoresut4,avgscoresut5))
colnames(x) <- c("sessions","scores")
plot(x$sessions,x$scores, xlab = "sessions", ylab = "suturing scores", type = "l")

micro <- read.csv("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/MicrosurgeryPerformance.csv")
head(micro,1)

micro <- as.data.frame(micro)
micro <- micro[-16:-17,]

for(i in 1:15)
{
  
  assign(paste("y_",micro[i,1]),c(micro[i,8],micro[i,15],micro[i,22],micro[i,29],micro[i,36]))
  
}


x = c(1:5)
data <- data.frame(x,`y_ 1`,`y_ 2`,`y_ 3`,`y_ 4`,`y_ 7`,`y_ 8`,`y_ 10`,`y_ 11`,`y_ 12`,`y_ 13`,`y_ 19`,`y_ 21`,`y_ 22`,`y_ 24`,`y_ 26`)

colnames(data) = c("sessions","s1","s2","s3","s4","s7","s8","s10","s11","s12","s13","s19","s21","s22","s24","s26")


class(data$sessions)
class(data$s1)

ggplot(data, aes(x = data$sessions, y = as.numeric(as.character(data$s1)))) + geom_point(color = "darkblue") + geom_line(color = "darkblue")+
geom_line(data = data,aes(x = data$sessions, y =as.numeric(as.character(data$s2))), color = "red")+
geom_line(data = data,aes(x = data$sessions, y = as.numeric(as.character(data$s3))), color = "red")+
  geom_line(data = data,aes(x = data$sessions, y = as.numeric(as.character(data$s4))), color = "black")+
  geom_line(data = data,aes(x = data$sessions, y = as.numeric(as.character(data$s7))), color = "yellow")+
  geom_line(data = data,aes(x = data$sessions, y = as.numeric(as.character(data$s8))), color = "green")+
  geom_line(data = data,aes(x = data$sessions, y = as.numeric(as.character(data$s21))), color = "brown")+
  geom_line(data = data,aes(x = data$sessions, y = as.numeric(as.character(data$s10))), color = "grey")+
  geom_line(data = data,aes(x = data$sessions, y = as.numeric(as.character(data$s11))), color = "pink")+
  geom_line(data = data,aes(x = data$sessions, y = as.numeric(as.character(data$s12))), color = "orange")+
  geom_line(data = data,aes(x = data$sessions, y = as.numeric(as.character(data$s13))), color = "darkred")+
  geom_line(data = data,aes(x = data$sessions, y = as.numeric(as.character(data$s19))), color = "blue")

cutting = as.matrix(data)

for(i in 2:16)
{
  
}

cutting[,2] = (as.numeric(as.character(cutting[,2])) - 6 )/ (30 - 6)
cutting[,3] = (as.numeric(as.character(cutting[,3])) - 6) / (30 - 6)

cutting <- as.data.frame(cutting)




###############33
#perspiration
#forsubject2

y2 <- c(0.00767688036095128, 0.00653486754061702, 0.00834188252502213, 0.00655255727677561,0.0060958805052671)
y1 <- c(0.00663993410602955, 0.00979478142132851, 0.00696778251036097 , 0 ,0.0121075776399281 )

micro <- read.csv("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/MicrosurgeryPerformance.csv")
micro <- micro[-16:-17,]
head(micro,1)
micro <- as.data.frame(micro)

x <- c(micro[2,8],micro[2,15],micro[2,22],micro[2,29],micro[2,36])

y2 = round(y2,digits = 4)
y1 = round(y1,digits = 4)
x <- c(1:5)

data <- data.frame(x,y1,y2)
data
plot(c(1:5),y2,type = "l")

ggplot(data, aes(x = x, y = y2)) + geom_line() +
  geom_line(data = data ,aes(x = x, y = y1))



