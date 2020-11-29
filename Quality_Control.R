install.packages("ggplot2")
library(ggplot2)
library(reshape2)
accuracyplots()

accuracyplots<-function(){
  
  headers <- names(read.csv("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/MicrosurgeryPerformance.csv",nrows=1))
  for(i in 1){
    perfdata <- read.table("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/MicrosurgeryPerformance.csv",header=TRUE,sep=",",nrows=1,col.names=headers) #,skip=i-1
    #View(perfdata)
    
    perfdata

    attach(perfdata)
    Scorer1cut <- c(Score.Cut1,Score.Cut1.1,Score.Cut1.2,Score.Cut1.3,Score.Cut1.4)
    Scorer2cut <- c(Score.Cut2,Score.Cut2.1,Score.Cut2.2,Score.Cut2.3,Score.Cut2.4)
    Scorer1sut <- c(Score.Sut1,Score.Sut1.1,Score.Sut1.2,Score.Sut1.3,Score.Sut1.4)
    Scorer2sut <- c(Score.Sut2,Score.Sut2.1,Score.Sut2.2,Score.Sut2.3,Score.Sut2.4)
    
    
    
    score1 <- matrix(ncol = 5,byrow = TRUE)
    score1 <- rbind(score1,Scorer1cut,Scorer1sut)
    score1 = score1[-1,]
    score1 = cbind(score1,c("cut","sut"),c("scorer1","scorer1"))
    colnames(score1) <- c("session1","session2","session3","session4","session5","task","scorer")
    score1 = as.data.frame(score1)
    score1 <- melt(score1, id = c("task","scorer"))# id = c("Scorer1cut","Scorer1sut"))#,id = c("scorer1cut","Scorer1sut")
    
    score2 <- matrix(ncol = 5,byrow = TRUE)
    score2 <- rbind(score2,Scorer2cut,Scorer2sut)
    score2 = score2[-1,]
    score2 = cbind(score2,c("cut","sut"),c("Scorer2","Scorer2"))
    colnames(score2) <- c("session1","session2","session3","session4","session5","task","scorer")
    score2 = as.data.frame(score2)
    score2 <- melt(score2, id = c("task","scorer"))# id = c("Scorer1cut","Scorer1sut"))#,id = c("scorer1cut","Scorer1sut")
    
    score1 <- rbind(score1,score2)
    score1
    colnames(score1) <- c("Task","Scorer","Session","Score")
    
    
      
    
   # times<-c(perfdata$Score.Cut1,perfdata$Score.Sut1,perfdata$Score.Cut2,perfdata$Score.Sut2,perfdata$Score.Cut3,perfdata$Score.Sut3,perfdata$Score.Cut4,perfdata$Score.Cut5,perfdata$Score.Sut5)
   #print(times)
    #A = matrix(times,nrow=10,ncol=1,byrow = TRUE)   
    #df=as.data.frame(A)
    #df<-cbind(df,c(rep(c("Suturing","Total"),5)))
    #df<-cbind(df,c(rep("Session1",2),rep("Session2",2),rep("Session3",2),rep("Session4",2),rep("Session5",2)))
   # colnames(df) <- c("Score","task","session")
  #  print(df)
    
    
    sub<-perfdata$ID
    
    nameOffile=paste("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/outputs/5/subject",sub,".jpg",sep="")
    jpeg(nameOffile)
    titleG=paste("Accuracy Scores for the Subject ",sub)
    print(ggplot(data = score1,aes(x = Session, y = Score,fill = Task)) + facet_grid(Scorer ~ .) +
            geom_bar(position="dodge", stat="identity") +ggtitle(titleG) + 
            theme(plot.title = element_text(hjust = 0.5)) + 
            scale_fill_manual(values=c("deeppink4","darkslategrey")))
           
    dev.off()
  }
}
