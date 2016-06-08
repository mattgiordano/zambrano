#Matt Giordano 
#Replication Project

#Loading the data
library(foreign)
file.choose()
dataset = read.spss("/Users/matthewgiordano/Desktop/R-Studio Data/AEJApp2008-0129_data/AEJApp_2008_0129_data.sav", to.data.frame=TRUE)
# dataset=read.csv("repdata.csv")

#Removing NA observations
df=na.omit(dataset)

#Getting 1s and 0s
df$Female=ifelse(df$Female=="Yes",1,0)
df$Female=as.numeric(df$Female)
df$AfrAmer=ifelse(df$AfrAmer=="Yes",1,0)
df$HealthyMenu=ifelse(df$HealthyMenu=="Yes",1,0)
df$UnhealthyMenu=ifelse(df$UnhealthyMenu=="Yes",1,0)
df$CalInfo=ifelse(df$CalInfo=="Yes",1,0)
df$CalRef=ifelse(df$CalRef=="Yes",1,0)
df$Study2=ifelse(df$Study2=="Yes",1,0)


#Creating a dataframe for Study 2
df2=data.frame(TotalCal=numeric(590),CalInfo=numeric(590),CalRef=numeric(590),HealthyMenu=numeric(590),UnhealthyMenu=numeric(590),Female=numeric(590),Age=numeric(590),AfrAmer=numeric(590))
for (i in 1:length(df[[1]])){
  if (df["Study2"][i,]==1){
    df2[i,1]=df["TotalCal"][i,]
    df2[i,2]=df["CalInfo"][i,]
    df2[i,3]=df["CalRef"][i,]
    df2[i,4]=df["HealthyMenu"][i,]
    df2[i,5]=df["UnhealthyMenu"][i,]
    df2[i,6]=df["Female"][i,]
    df2[i,7]=df["Age"][i,]
    df2[i,8]=df["AfrAmer"][i,]
  }
}

#Getting Ride of Study 1ers
gone=which(df2["TotalCal"]==0)
df2=df2[-gone,]

#Creating a dataframe for Study 1
df1=data.frame(TotalCal=numeric(590),CalInfo=numeric(590),CalRef=numeric(590),HealthyMenu=numeric(590),UnhealthyMenu=numeric(590),Female=numeric(590),Age=numeric(590),AfrAmer=numeric(590))
for (i in 1:length(df[[1]])){
  if (df["Study2"][i,]==0){
    df1[i,1]=df["TotalCal"][i,]
    df1[i,2]=df["CalInfo"][i,]
    df1[i,3]=df["CalRef"][i,]
    df1[i,4]=df["HealthyMenu"][i,]
    df1[i,5]=df["UnhealthyMenu"][i,]
    df1[i,6]=df["Female"][i,]
    df1[i,7]=df["Age"][i,]
    df1[i,8]=df["AfrAmer"][i,]
  }
}

#Getting Rid of Study 2ers
gone1=which(df1["TotalCal"]==0)
df1=df1[-gone1,]

#Quick Check method that is alittle off at this point
test2=lm(df2$TotalCal~df2$CalInfo+df2$CalRef+df2$HealthyMenu+df2$UnhealthyMenu+df2$Female+df2$Age+df2$AfrAmer)
test1=lm(df1$TotalCal~df1$CalInfo+df1$CalRef+df1$HealthyMenu+df1$UnhealthyMenu+df1$Female+df1$Age+df1$AfrAmer)
summary(test2)
summary(test1)

#Create the b1 vector by hand
bo1=c(rep(1,276))
Y1=df1$TotalCal
model1 = list(bo1,df1$CalInfo,df1$CalRef,df1$HealthyMenu,df1$UnhealthyMenu,df1$Female,df1$Age,df1$AfrAmer)
x1=matrix(unlist(model1),byrow=FALSE,ncol=8)
b1=solve(t(x1)%*%x1)%*%(t(x1)%*%Y1)

#Creating the b2 vector by hand
bo2=c(rep(1,314))
Y2=df2$TotalCal
model2 = list(bo2,df2$CalInfo,df2$CalRef,df2$HealthyMenu,df2$UnhealthyMenu,df2$Female,df2$Age,df2$AfrAmer)
x2=matrix(unlist(model2),byrow=FALSE,ncol=8)
b2=solve(t(x2)%*%x2)%*%(t(x2)%*%Y2)

#Creating the Standard Errors for Study 1
yhat1=x1%*%b1
e1=Y1-yhat1
samvar1=sum((Y1-x1%*%b1)^2)/(nrow(x1)-ncol(x1))
xneed1=t(x1)%*%x1
xneeded1=chol(xneed1)
ss1=samvar1*chol2inv((xneeded1))
SE1=sqrt(diag(ss1))

#Creating the Standard Errors for Study 2
yhat2=x2%*%b2
e2=Y2-yhat2
samvar2=sum((Y2-x2%*%b2)^2)/(nrow(x2)-ncol(x2))
xneed2=t(x2)%*%x2
xneeded2=chol(xneed2)
ss2=samvar2*chol2inv((xneeded2))
SE2=sqrt(diag(ss2))

#Final b vector table
rowtitles=c("Constant","Calorie Info","Daily Calorie Info","Healthy Featured Menu","Unhealthy Featured Menu","Female","Age","African American")
btable=data.frame("Study_1"=b1,"Study_2"=b2,row.names=rowtitles)

#Final SE vector table
SEtable=data.frame("Study_1"=SE1,"Study_2"=SE2,row.names=rowtitles)
#View(SEtable)

#Checking Robustness with Hunger Variable
#Creating new data frame with Hunger Variable for Study 2 Data
df3=data.frame(TotalCal=numeric(590),CalInfo=numeric(590),CalRef=numeric(590),HealthyMenu=numeric(590),UnhealthyMenu=numeric(590),Female=numeric(590),Age=numeric(590),AfrAmer=numeric(590),Hunger=numeric(590))
for (i in 1:length(df[[1]])){
  if (df["Study2"][i,]==1){
    df3[i,1]=df["TotalCal"][i,]
    df3[i,2]=df["CalInfo"][i,]
    df3[i,3]=df["CalRef"][i,]
    df3[i,4]=df["HealthyMenu"][i,]
    df3[i,5]=df["UnhealthyMenu"][i,]
    df3[i,6]=df["Female"][i,]
    df3[i,7]=df["Age"][i,]
    df3[i,8]=df["AfrAmer"][i,]
    df3[i,9]=df["Hunger"][i,]
  }
}
gone3=which(df3["TotalCal"]==0)
df3=df3[-gone3,]

#Getting the Coeffecients of Study 2 with Hunger Varaible Added
bo3=c(rep(1,314))
Y3=df3$TotalCal
model3 = list(bo3,df3$CalInfo,df3$CalRef,df3$HealthyMenu,df3$UnhealthyMenu,df3$Female,df3$Age,df3$AfrAmer,df3$Hunger)
x3=matrix(unlist(model3),byrow=FALSE,ncol=9)
b3=solve(t(x3)%*%x3)%*%(t(x3)%*%Y3)

#Getting the F stat of Study 2 with Hunger Variable Added
SST2=sum((Y3-mean(Y3))^2)
yhat3=x3%*%b3
e3=Y3-yhat3
SSR2=sum((e3)^2)
SSM2=SST2-SSR2
k=8
n=nrow(df3)
degfreeresidual=n-k-1
degfreemodel=k
degfreetot=degfreeresidual+degfreemodel
MSmodel4=SSM2/degfreemodel
MSresidual4=SSR2/degfreeresidual
MStotal2=MSmodel4+MSresidual4
Fstat4=MSmodel4/MSresidual4
fprob4=qf(.99,degfreemodel,degfreeresidual)
isTRUE(Fstat4>fprob4)

#Getting the Standard Errors of Study 2 with Hunger Variable Added
yhat3=x3%*%b3
e3=Y3-yhat3
samvar3=sum((Y3-x3%*%b3)^2)/(nrow(x3)-ncol(x3))
xneed3=t(x3)%*%x3
xneeded3=chol(xneed3)
ss3=samvar3*chol2inv((xneeded3))
SE3=sqrt(diag(ss3))

#Getting the Coeffecients of Study 1 with Hunger Variable Added
df4=data.frame(TotalCal=numeric(590),CalInfo=numeric(590),CalRef=numeric(590),HealthyMenu=numeric(590),UnhealthyMenu=numeric(590),Female=numeric(590),Age=numeric(590),AfrAmer=numeric(590),Hunger=numeric(590))
for (i in 1:length(df[[1]])){
  if (df["Study2"][i,]==0){
    df4[i,1]=df["TotalCal"][i,]
    df4[i,2]=df["CalInfo"][i,]
    df4[i,3]=df["CalRef"][i,]
    df4[i,4]=df["HealthyMenu"][i,]
    df4[i,5]=df["UnhealthyMenu"][i,]
    df4[i,6]=df["Female"][i,]
    df4[i,7]=df["Age"][i,]
    df4[i,8]=df["AfrAmer"][i,]
    df4[i,9]=df["Hunger"][i,]
  }
}
gone4=which(df4["TotalCal"]==0)
df4=df4[-gone4,]

#Getting the Coeffecients of Study 1 with Hunger Varaible Added
bo4=c(rep(1,276))
Y4=df4$TotalCal
model4 = list(bo4,df4$CalInfo,df4$CalRef,df4$HealthyMenu,df4$UnhealthyMenu,df4$Female,df4$Age,df4$AfrAmer,df4$Hunger)
x4=matrix(unlist(model4),byrow=FALSE,ncol=9)
b4=solve(t(x4)%*%x4)%*%(t(x4)%*%Y4)

#Getting the F stat of Study 1 with Hunger Variable Added
SST1=sum((Y4-mean(Y4))^2)
yhat4=x4%*%b4
e4=Y4-yhat4
SSR1=sum((e4)^2)
SSM1=SST1-SSR1
k=8
n=nrow(df4)
degfreeresidual=n-k-1
degfreemodel=k
degfreetot=degfreeresidual+degfreemodel
MSmodel3=SSM1/degfreemodel
MSresidual3=SSR1/degfreeresidual
MStotal1=MSmodel3+MSresidual3
Fstat3=MSmodel3/MSresidual3
fprob3=qf(.99,degfreemodel,degfreeresidual)
isTRUE(Fstat3>fprob3)

#Getting the Standard Errors of Study 2 with Hunger Variable Added
yhat4=x4%*%b4
e4=Y4-yhat4
samvar4=sum((Y4-x4%*%b4)^2)/(nrow(x4)-ncol(x4))
xneed4=t(x4)%*%x4
xneeded4=chol(xneed4)
ss4=samvar4*chol2inv((xneeded4))
SE4=sqrt(diag(ss4))


#Final b vector table with Hunger Variable Added
rowtitles=c("Constant","Calorie Info","Daily Calorie Info","Healthy Featured Menu","Unhealthy Featured Menu","Female","Age","African American","Hunger")
bhtable=data.frame("Study_1"=b4,"Study_2"=b3,row.names=rowtitles)


testtest=lm(df4$TotalCal~df4$CalInfo+df4$CalRef+df4$HealthyMenu+df4$UnhealthyMenu+df4$UnhealthyMenu+df4$Female+df4$Age+df4$AfrAmer+df4$Hunger)


#Final SE vector table with Hunger Variable Added
SEhtable=data.frame("Study_1"=SE4,"Study_2"=SE3,row.names=rowtitles)
#View(SEhtable)

#Checking Robustness with CalInfo & CalRef Interaction Term
#Creating the Data Frame for Study 1 with the Interaction Term of CalInfo & CalRef
df5=data.frame(TotalCal=numeric(590),CalInfo=numeric(590),CalRef=numeric(590),HealthyMenu=numeric(590),UnhealthyMenu=numeric(590),Female=numeric(590),Age=numeric(590),AfrAmer=numeric(590),Interaction=numeric(590))
for (i in 1:length(df[[1]])){
  if (df["Study2"][i,]==0){
    df5[i,1]=df["TotalCal"][i,]
    df5[i,2]=df["CalInfo"][i,]
    df5[i,3]=df["CalRef"][i,]
    df5[i,4]=df["HealthyMenu"][i,]
    df5[i,5]=df["UnhealthyMenu"][i,]
    df5[i,6]=df["Female"][i,]
    df5[i,7]=df["Age"][i,]
    df5[i,8]=df["AfrAmer"][i,]
    df5[i,9]=(df["CalInfo"][i,])*(df["CalRef"][i,])
  }
}
gone5=which(df5["TotalCal"]==0)
df5=df5[-gone5,]

#Getting the Coeffecients of Study 1 with Interaction Varaible Added
bo5=c(rep(1,276))
Y5=df5$TotalCal
model5 = list(bo5,df5$CalInfo,df5$CalRef,df5$HealthyMenu,df5$UnhealthyMenu,df5$Female,df5$Age,df5$AfrAmer,df5$Interaction)
x5=matrix(unlist(model5),byrow=FALSE,ncol=9)
b5=solve(t(x5)%*%x5)%*%(t(x5)%*%Y5)

# #Getting the F stat of Study 1 with Interaction Variable Added
# SST1=sum((Y5-mean(Y5))^2)
# yhat5=x5%*%b5
# e5=Y5-yhat5
# SSR1=sum((e5)^2)
# SSM1=SST1-SSR1
# k=8
# n=nrow(df5)
# degfreeresidual=n-k-1
# degfreemodel=k
# degfreetot=degfreeresidual+degfreemodel
# MSmodel1=SSM1/degfreemodel
# MSresidual1=SSR1/degfreeresidual
# MStotal1=MSmodel1+MSresidual1
# Fstat1=MSmodel1/MSresidual1
# fprob1=qf(.99,degfreemodel,degfreeresidual)
# isTRUE(Fstat1>fprob1)

test=lm(df5$TotalCal~df5$CalInfo+df5$CalRef+df5$HealthyMenu+df5$UnhealthyMenu+df5$UnhealthyMenu+df5$Female+df5$Age+df5$AfrAmer+df5$Interaction)
#Creating the Data Frame for Study 2 with the Interaction Term of CalInfo & CalRef
df6=data.frame(TotalCal=numeric(590),CalInfo=numeric(590),CalRef=numeric(590),HealthyMenu=numeric(590),UnhealthyMenu=numeric(590),Female=numeric(590),Age=numeric(590),AfrAmer=numeric(590),Interaction=numeric(590))
for (i in 1:length(df[[1]])){
  if (df["Study2"][i,]==1){
    df6[i,1]=df["TotalCal"][i,]
    df6[i,2]=df["CalInfo"][i,]
    df6[i,3]=df["CalRef"][i,]
    df6[i,4]=df["HealthyMenu"][i,]
    df6[i,5]=df["UnhealthyMenu"][i,]
    df6[i,6]=df["Female"][i,]
    df6[i,7]=df["Age"][i,]
    df6[i,8]=df["AfrAmer"][i,]
    df6[i,9]=(df["CalInfo"][i,])*(df["CalRef"][i,])
  }
}
gone6=which(df6["TotalCal"]==0)
df6=df6[-gone6,]

#Getting the Coeffecients of Study 2 with Interaction Varaible Added
bo6=c(rep(1,314))
Y6=df6$TotalCal
model6 = list(bo6,df6$CalInfo,df6$CalRef,df6$HealthyMenu,df6$UnhealthyMenu,df6$Female,df6$Age,df6$AfrAmer,df6$Interaction)
x6=matrix(unlist(model6),byrow=FALSE,ncol=9)
b6=solve(t(x6)%*%x6)%*%(t(x6)%*%Y6)

# #Getting the F stat of Study 1 with Interaction Variable Added
# SST1=sum((Y6-mean(Y6))^2)
# yhat6=x6%*%b6
# e6=Y6-yhat6
# SSR1=sum((e6)^2)
# SSM1=SST1-SSR1
# k=8
# n=nrow(df5)
# degfreeresidual=n-k-1
# degfreemodel=k
# degfreetot=degfreeresidual+degfreemodel
# MSmodel2=SSM1/degfreemodel
# MSresidual2=SSR1/degfreeresidual
# MStotal1=MSmodel1+MSresidual1
# Fstat2=MSmodel2/MSresidual2
# fprob2=qf(.985,degfreemodel,degfreeresidual)
# isTRUE(Fstat2>fprob2)

test=lm(df6$TotalCal~df6$CalInfo+df6$CalRef+df6$HealthyMenu+df6$UnhealthyMenu+df6$UnhealthyMenu+df6$Female+df6$Age+df6$AfrAmer+df6$Interaction)
#Final b vector table with Interaction Variable Added
rowtitles=c("Constant","Calorie Info","Daily Calorie Info","Healthy Featured Menu","Unhealthy Featured Menu","Female","Age","African American","Interaction")
bitable=data.frame("Study_1"=b5,"Study_2"=b6,row.names=rowtitles)

