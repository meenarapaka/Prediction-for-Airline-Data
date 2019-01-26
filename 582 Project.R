library(RCurl)
library(RJSONIO)

#1st Milestone:JSON TO CSV

project.data = getURL("http://ist.gmu.edu/~hpurohit/courses/ait582-proj-data-spring16.json")
dataset = fromJSON(project.data)
project.data =do.call(rbind,dataset)
View(dataset)
write.csv(project.data, "project.data.csv")
head(project.data)
tail(project.data)

#deleting the extra heading

mydata= project.data
mydata= data.frame(mydata[-1,])


#spliting the data into columns
library(stringr)


out <- strsplit(as.character(mydata$DESCRIPTION),',') 
do.call(rbind, out)
mydata1=data.frame(mydata$DESCRIPTION, do.call(rbind, out))
mydata=cbind(mydata,mydata1$X1)


out <- strsplit(as.character(mydata$DESCRIPTION),';') 
do.call(rbind, out)
mydata1=data.frame(mydata$DESCRIPTION, do.call(rbind, out))
mydata=cbind(mydata,mydata1$X2)

out <- strsplit(as.character(mydata1$X1),',') 
do.call(rbind, out)
mydata1=data.frame(mydata$DESCRIPTION, do.call(rbind, out))
mydata=cbind(mydata,mydata1$X2)

mydata<- setNames(mydata, c("fare","description","success","seatclass","guests","customerid","lastname","age","firstname"))

#mydata=mydata[-c(1),]
#mydata1=mydata1[-1,]

library(stringr)
Name <- mydata1$X2
x3=str_extract(string = Name,pattern = "(Mr|Miss|Mrs|Master|Col|the Countess|Rev|Don|Dr|Major|Mme|Lady|Ms|Mlle|Capt|Jonkheer)\\.")
mydata1=cbind(mydata1,x3)
mydata=cbind(mydata,mydata1$x3)

mydata<- setNames(mydata, c("fare","description","success","seatclass","guests","customerid","lastname","age","firstname","Gender"))

mydata$Gender=ifelse(grepl("Mrs.",mydata$Gender),"Female",ifelse(grepl("Miss.",mydata$Gender),"Female",ifelse(grepl("Master.",mydata$Gender),"Male",ifelse(grepl("Mr.",mydata$Gender),"Male",ifelse(grepl("Rev.",mydata$Gender),"Other",ifelse(grepl("the Countess.",mydata$Gender),"Female",ifelse(grepl("Mme.",mydata$Gender),"Female",ifelse(grepl("Lady.",mydata$Gender),"Female",ifelse(grepl("Ms.",mydata$Gender),"Female",ifelse(grepl("Mlle.",mydata$Gender),"Female",ifelse(grepl("Col.",mydata$Gender),"Male",ifelse(grepl("Capt.",mydata$Gender),"Male",ifelse(grepl("Don.",mydata$Gender),"Other",ifelse(grepl("Major.",mydata$Gender),"Other",ifelse(grepl("Dr.",mydata$Gender),"Other",ifelse(grepl("Jonkheer.",mydata$Gender),"Male",""))))))))))))))))
mydata[[8]] <- as.numeric(as.character(mydata[[8]]))


#mydata$seatclass=ifelse(grepl("1",mydata$seatclass),"Business",ifelse(grepl("2",mydata$seatclass),"Premium Economy",ifelse(grepl("3",mydata$Gender),"Economy","")))

##Missing Age Imputation 
for(i in 1:nrow(mydata)){
  if (is.na(mydata$age[i])== T){
    mydata$age[i]=round(mean(mydata$age, na.rm =T))
  }
}
mean(mydata$age)
mydata[mydata$age == "NA"] <- mean(mydata$age)
row=nrow(mydata)

mydata$category[mydata$age > 60] <- "Elder"
mydata$category[mydata$age > 12 & mydata$age <= 60] <- "Adult"
mydata$category[mydata$age > 3 & mydata$age <= 12] <- "Child"
mydata$category[mydata$age <= 3] <- "Infants"

#for(i in 1:row)
#{  
 # ifelse (( mydata$seatclass[i] == 1)
 #{ mydata$seatclass[i]<- "business"}
  #ifelse (mydata$seatclass[i] ==2)
  #{mydata$seatclass [i] ="premium economy"})
  #ifelse(mydata$seatclass[i]==3)
  #{mydata$seatclass[i]<- "economy"}
#}

#for(i in 1:row)
#{
 # mydata$Yearborn[i]<- (as.integer(2017- mydata$Age[i]))
#}
mydata=mydata[,-9]
mydata=mydata[,-7]
mydata=mydata[,-2]

write.csv(mydata,file = "mydata.csv")



