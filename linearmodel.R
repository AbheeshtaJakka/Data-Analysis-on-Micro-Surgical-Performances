#CUTTING
sub <- c(1:13,19:26)

mean_matrix = matrix(nrow = 105,ncol = 4,byrow = TRUE)

colnames(mean_matrix) = c("subsession","mean_cut","mean_sut","mean_base")


index = 1
for(sub1 in sub)
{
  for(sub2 in 1:5)
  {
    if(sub1 < 10)
    {
      file_read_path <- file.path("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/subjects/extracted",paste("subject0",sub1,"/subject0",sub1,"/session",sub2,"/","Subject0",sub1,"_Cutting",sub2,".csv",sep = ""))
      if(file.exists(file_read_path))
      { 
        # assign(paste("cutting",sub1,"session",sub2),read.csv(file_read_path))
        
        data <- read.csv(file_read_path)
        
        data = data.frame(data)
        t <- data$Time
        t <- trunc(t)
        t <- as.data.frame(t)
        is.numeric(t)
        
        data <- data.frame(data$Frame.,t,data$Perspiration)
        colnames(data) <- c("frame","t","perspiration")
        
        
        ag <- aggregate(data$perspiration~factor(data$t), FUN = mean)
        colnames(ag) <- c("frame","perspiration")
        
        #cutting_mean = mean(ag$perspiration) 
        
        x <- c(paste(sub1,"_",sub2))
        mean_matrix[index,1] = x
        mean_matrix[index,2] = mean(ag$perspiration) 
        index = index + 1
        
        # mean_matrix <- rbind(mean_matrix,c(x,mean(ag$perspiration)))
        
        assign(paste("cutting",sub1,"session",sub2),ag)

        
      } 
      else
      {
        x <- c(paste(sub1,"_",sub2))
        mean_matrix[index,1] = x
        mean_matrix[index,2] = NA 
        index = index + 1
      }
    }
    else
    {
      file_read_path <- file.path("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/subjects/extracted",paste("subject",sub1,"/subject",sub1,"/session",sub2,"/","Subject",sub1,"_Cutting",sub2,".csv",sep = ""))
      if(file.exists(file_read_path))
      { 
        #assign(paste("cutting",sub1,"session",sub2),read.csv(file_read_path))
        
        data <- read.csv(file_read_path)
        
        data = data.frame(data)
        t <- data$Time
        t <- trunc(t)
        t <- as.data.frame(t)
        is.numeric(t)
        
        data <- data.frame(data$Frame.,t,data$Perspiration)
        colnames(data) <- c("frame","t","perspiration")
        
        
        ag <- aggregate(data$perspiration~factor(data$t), FUN = mean)
        colnames(ag) <- c("frame","perspiration")
        
        x <- c(paste(sub1,"_",sub2))
        mean_matrix[index,1] = x
        mean_matrix[index,2] = mean(ag$perspiration) 
        index = index + 1
        
        assign(paste("cutting",sub1,"session",sub2),ag)
        
      }
      else
      {
        x <- c(paste(sub1,"_",sub2))
        mean_matrix[index,1] = x
        mean_matrix[index,2] = NA 
        index = index + 1
      }
    }
  }
}

#**************************************************************

mean_matrix
#SUTURING

sub <- c(1:13,19:26)

index = 1

for(sub1 in sub)
{
  for(sub2 in 1:5)
  {
    if(sub1 < 10)
    {
      file_read_path <- file.path("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/subjects/extracted",paste("subject0",sub1,"/subject0",sub1,"/session",sub2,"/","Subject0",sub1,"_Suturing",sub2,".csv",sep = ""))
      if(file.exists(file_read_path))
      { 
        # assign(paste("cutting",sub1,"session",sub2),read.csv(file_read_path))
        
        data <- read.csv(file_read_path)
        
        data = data.frame(data)
        t <- data$Time
        t <- trunc(t)
        t <- as.data.frame(t)
        is.numeric(t)
        
        data <- data.frame(data$Frame.,t,data$Perspiration)
        colnames(data) <- c("frame","t","perspiration")
        
        
        ag <- aggregate(data$perspiration~factor(data$t), FUN = mean)
        colnames(ag) <- c("frame","perspiration")
        
        x <- c(paste(sub1,"_",sub2))
        mean_matrix[index,1] = x
        mean_matrix[index,3] = mean(ag$perspiration) 
        index = index + 1
        
        assign(paste("suturing",sub1,"session",sub2),ag)
        
        
      }
      else
      {
        x <- c(paste(sub1,"_",sub2))
        mean_matrix[index,1] = x
        mean_matrix[index,2] = NA 
        index = index + 1
      }
    }
    else
    {
      file_read_path <- file.path("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/subjects/extracted",paste("subject",sub1,"/subject",sub1,"/session",sub2,"/","Subject",sub1,"_Suturing",sub2,".csv",sep = ""))
      if(file.exists(file_read_path))
      { 
        #assign(paste("cutting",sub1,"session",sub2),read.csv(file_read_path))
        
        data <- read.csv(file_read_path)
        
        data = data.frame(data)
        t <- data$Time
        t <- trunc(t)
        t <- as.data.frame(t)
        is.numeric(t)
        
        data <- data.frame(data$Frame.,t,data$Perspiration)
        colnames(data) <- c("frame","t","perspiration")
        
        
        ag <- aggregate(data$perspiration~factor(data$t), FUN = mean)
        colnames(ag) <- c("frame","perspiration")
        
        x <- c(paste(sub1,"_",sub2))
        mean_matrix[index,1] = x
        mean_matrix[index,3] = mean(ag$perspiration) 
        index = index + 1
        
        
        assign(paste("suturing",sub1,"session",sub2),ag)
        
      }
      else
      {
        x <- c(paste(sub1,"_",sub2))
        mean_matrix[index,1] = x
        mean_matrix[index,2] = NA 
        index = index + 1
      }
    }
  }
}

#***********************************************************************



#BASELINE
sub <- c(1:13,19:26)

index = 1

for(sub1 in sub)
{
  for(sub2 in 1:5)
  {
    if(sub1 < 10)
    {
      file_read_path <- file.path("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/subjects/extracted",paste("subject0",sub1,"/subject0",sub1,"/session",sub2,"/","Subject0",sub1,"_Baseline",sub2,".csv",sep = ""))
      if(file.exists(file_read_path))
      { 
        # assign(paste("cutting",sub1,"session",sub2),read.csv(file_read_path))
        
        data <- read.csv(file_read_path)
        
        data = data.frame(data)
        t <- data$Time
        t <- trunc(t)
        t <- as.data.frame(t)
        is.numeric(t)
        
        data <- data.frame(data$Frame.,t,data$Perspiration)
        colnames(data) <- c("frame","t","perspiration")
        
        
        ag <- aggregate(data$perspiration~factor(data$t), FUN = mean)
        colnames(ag) <- c("frame","perspiration")
        
        x <- c(paste(sub1,"_",sub2))
        mean_matrix[index,1] = x
        mean_matrix[index,4] = mean(ag$perspiration) 
        index = index + 1
        
        
        baseline_mean = mean(ag$perspiration)
        assign(paste("baseline",sub1,"session",sub2),ag)
        
        
      }
      else
      {
        x <- c(paste(sub1,"_",sub2))
        mean_matrix[index,1] = x
        mean_matrix[index,2] = NA 
        index = index + 1
      }
    }
    else
    {
      file_read_path <- file.path("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/subjects/extracted",paste("subject",sub1,"/subject",sub1,"/session",sub2,"/","Subject",sub1,"_Baseline",sub2,".csv",sep = ""))
      if(file.exists(file_read_path))
      { 
        #assign(paste("cutting",sub1,"session",sub2),read.csv(file_read_path))
        
        data <- read.csv(file_read_path)
        
        data = data.frame(data)
        t <- data$Time
        t <- trunc(t)
        t <- as.data.frame(t)
        is.numeric(t)
        
        data <- data.frame(data$Frame.,t,data$Perspiration)
        colnames(data) <- c("frame","t","perspiration")
        
        
        ag <- aggregate(data$perspiration~factor(data$t), FUN = mean)
        colnames(ag) <- c("frame","perspiration")
        
        x <- c(paste(sub1,"_",sub2))
        mean_matrix[index,1] = x
        mean_matrix[index,4] = mean(ag$perspiration) 
        index = index + 1
        
        
        assign(paste("baseline",sub1,"session",sub2),ag)
        
      }
      else
      {
        x <- c(paste(sub1,"_",sub2))
        mean_matrix[index,1] = x
        mean_matrix[index,2] = NA 
        index = index + 1
      }
    }
  }
}

#*************************************************************

mean_matrix 

mean_data = as.data.frame(mean_matrix,options(stringsAsFactors = FALSE))
head(mean_data)

mean_data$diff_cut = as.numeric(as.character(mean_data$mean_cut)) - as.numeric(as.character(mean_data$mean_base))
mean_data$diff_sut = as.numeric(as.character(mean_data$mean_sut)) - as.numeric(as.character(mean_data$mean_base))


head(mean_data,30)
mean_data[1:20,5]

c(mean_data[31:40,5],mean_data[46:70,5],mean_data[76:85,5],mean_data[91:95,5],mean_data[101:105,5])




########################################################

##########################################################

########################################################

############################################################

########################################################

library(reshape2)


  
  perform <- read.csv("C:/Users/Abheeshta/Desktop/2ndSEMJUNK/SMR/PROJECT/MicroSurgery/without output/MicrosurgeryPerformance.csv")
  head(perform)
  
  perform = perform[-16:-17,]
  attach(perform)
  
  
#******************SCORER1**************************
 
  #SESSION1
  subject <- c(1:4,7,8,10:13,19,21,22,24,26)
  subject
  cut <- c(Score.Cut1)
  sut <- c(Score.Sut1)
  s1 <- data.frame(subject,Age,Sex,cut,sut)
  s1 = melt(s1,id = c("subject","Age","Sex"))
  colnames(s1) <- c("Subject","Age","Sex","Task","Session1")
  s1
   mod1 <- lm(s1$Score ~ s1$Age + s1$Sex + s1$Task)
  summary(mod1)
  
  #ppcut1 <- c(mean_data[1:20,5],mean_data[31:40,5],mean_data[46:70,5],mean_data[76:85,5],mean_data[91:95,5],mean_data[101:105,5],mean_data[1:20,6],mean_data[31:40,6],mean_data[46:70,6],mean_data[76:85,6],mean_data[91:95,6],mean_data[101:105,6])
  
  
  #SESSION2
  cut <- c(Score.Cut1.1)
  sut <- c(Score.Sut1.1)
  s2 <- data.frame(Age,Sex,cut,sut)
  s2 = melt(s2,id = c("Age","Sex"))
  colnames(s2) <- c("Age","Sex","Task","Session2")
  
  #SESSION3
  cut <- c(Score.Cut1.2)
  sut <- c(Score.Sut1.2)
  s3 <- data.frame(Age,Sex,cut,sut)
  s3 = melt(s3,id = c("Age","Sex"))
  colnames(s3) <- c("Age","Sex","Task","Session3")
  
  #SESSION4
  cut <- c(Score.Cut1.3)
  sut <- c(Score.Sut1.3)
  s4 <- data.frame(Age,Sex,cut,sut)
  s4 = melt(s4,id = c("Age","Sex"))
  colnames(s4) <- c("Age","Sex","Task","Session4")
  
  #SESSION5
  cut <- c(Score.Cut1.4)
  sut <- c(Score.Sut1.4)
  s5 <- data.frame(Age,Sex,cut,sut)
  s5 = melt(s5,id = c("Age","Sex"))
  colnames(s5) <- c("Age","Sex","Task","Session5")
  
  #APPENDING ALL THE SESSIONS TO ONE DATAFRAME
  s1$Session2 <- c(s2$Session2)
  s1$Session3 <- c(s3$Session3)
  s1$Session4 <- c(s4$Session4)
  s1$Session5 <- c(s5$Session5)
  
  s1 <- melt(s1, id = c("Subject","Age","Sex","Task"))
  colnames(s1) <- c("Subject","Age","Sex","Task","Session","Score")
  
  colnames(s1) <- c("Subject","Age","Sex","Task","Session","Scorer1")
  
  common <- data.frame(s1)
  common
  
  
#********************SCORER2****************  
  
  #SESSION1
  subject <- c(1:4,7,8,10:13,19,21,22,24,26)
  cut <- c(Score.Cut2)
  sut <- c(Score.Sut2)
  s1 <- data.frame(subject,Age,Sex,cut,sut)
  s1 = melt(s1,id = c("subject","Age","Sex"))
  colnames(s1) <- c("Subject","Age","Sex","Task","Session1")

  
  #SESSION2
  cut <- c(Score.Cut2.1)
  sut <- c(Score.Sut2.1)
  s2 <- data.frame(Age,Sex,cut,sut)
  s2 = melt(s2,id = c("Age","Sex"))
  colnames(s2) <- c("Age","Sex","Task","Session2")
  
  
  #SESSION3
  cut <- c(Score.Cut2.2)
  sut <- c(Score.Sut2.2)
  s3 <- data.frame(Age,Sex,cut,sut)
  s3 = melt(s3,id = c("Age","Sex"))
  colnames(s3) <- c("Age","Sex","Task","Session3")
  
  
  #SESSION4
  cut <- c(Score.Cut2.3)
  sut <- c(Score.Sut2.3)
  s4 <- data.frame(Age,Sex,cut,sut)
  s4 = melt(s4,id = c("Age","Sex"))
  colnames(s4) <- c("Age","Sex","Task","Session4")
  
  
  #SESSION5
  cut <- c(Score.Cut2.4)
  sut <- c(Score.Sut2.4)
  s5 <- data.frame(Age,Sex,cut,sut)
  s5 = melt(s5,id = c("Age","Sex"))
  colnames(s5) <- c("Age","Sex","Task","Session5")
  
  #APPENDING ALL THE SESSIONS TO ONE DATAFRAME
  s1$Session2 <- c(s2$Session2)
  s1$Session3 <- c(s3$Session3)
  s1$Session4 <- c(s4$Session4)
  s1$Session5 <- c(s5$Session5)
  s1 <- melt(s1, id = c("Subject","Age","Sex","Task"))
  colnames(s1) <- c("Subject","Age","Sex","Task","Session","Score")
  
  colnames(s1) <- c("Subject","Age","Sex","Task","Session","Scorer2")
  
  #APPENDING TWO SCORERS TO ONE ONE DATAFRAME
  common$Scorer2 <- c(s1$Scorer2)
 
  
  
  
  ##################################################################333 
  bigmatrix = matrix(ncol = 1,byrow = TRUE)
  
  for(j in 1:5)
  {
    
    sessioncut = matrix(ncol = 1,byrow = TRUE)
    sessionsut = matrix(ncol = 1,byrow = TRUE)
    
    for(i in c(1,6,11,16,31,36,46,51,56,61,66,76,81,91,101))
    {
         i = i + (j-1)
         
      sessioncut <- rbind(sessioncut,mean_data[i,5])
      sessionsut <- rbind(sessionsut,mean_data[i,6])
      
    }
      
      sessioncut = sessioncut[-1,] 
      sessioncut = as.matrix(sessioncut)
      sessionsut = sessionsut[-1,]
      sessionsut = as.matrix(sessionsut) 
      
      print(sessioncut)
      print(sessionsut)
      
       bigmatrix = rbind(bigmatrix,sessioncut)
       bigmatrix = rbind(bigmatrix,sessionsut)
    
}
  
  bigmatrix = bigmatrix[-1,]
  
  class(bigmatrix)
  
  bigmatrix = as.data.frame(bigmatrix)
  bigmatrix 
  
  class(bigmatrix)
###########################################################################  
  common
  
  exc <- common
  exc <- melt(exc, id = c("Subject","Age","Sex","Task","Session"))
  head(exc)
  
  colnames(exc) <- c("Subject","Age","Sex","Task","Session","Scorer","Score")
  
  pp <- data.frame()
  
  pp <- c(bigmatrix)
  pp <- rep(pp,2)
  head(pp)
 
       
   exc$pp <- as.data.frame(pp)
   
   head(exc)
  # exc <- na.omit(exc)
   #colnames(common) <- c("Subject","Age","Sex","Task","Session","Scorer","Score")
  #common 

  

  attach(exc)
  model <- lm(Score ~ pp + Age + Sex + Task + Session + Scorer)
  summary(model)
  
  model <- lm(Score ~ pp + Session +  Task + Scorer + Age + Sex)
  summary(model)
  
  model <- aov(Score ~ pp + Session +  Task + Scorer + Age + Sex)
  summary(model)
  
  
  

