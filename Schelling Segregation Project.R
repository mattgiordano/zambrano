#Matt Giordano
#Project 4
#Shelling Model

#given index function
calcindex <- function(x1,y1,x2,y2){
  
  n1 = length(x1)
  n2 = length(x2)
  
  q1e=sum(as.integer((x1<0.5)&(y1<0.5)))
  q2e=sum(as.integer((x1>=0.5)&(y1<0.5)))
  q3e=sum(as.integer((x1<0.5)&(y1>=0.5)))
  q4e=sum(as.integer((x1>=0.5)&(y1>=0.5)))
  
  q1o=sum(as.integer((x2<0.5)&(y2<0.5)))
  q2o=sum(as.integer((x2>=0.5)&(y2<0.5)))
  q3o=sum(as.integer((x2<0.5)&(y2>=0.5)))
  q4o=sum(as.integer((x2>=0.5)&(y2>=0.5)))
  
  r = 0.5*(abs(q1e/n1-q1o/n2)+abs(q2e/n1-q2o/n2)+abs(q3e/n1-q3o/n2)+abs(q4e/n1-q4o/n2))
  
  return(r)
  
}

#Inputs into function  
N=50
t=.4
f=0.5
s=5

#Creating the function
finalfunction=function(N,f,t,s){


#Starting the iteration loop
smatrix=matrix(0,ncol=s,nrow=2)
for (z in 1:s){

#Creating Random Coordinates
x=runif(N)
y=runif(N)

#Random Coordinates of the People
coor=data.frame(x=numeric(N),y=numeric(N),race=numeric(N))
for (i in 1:N){
  coor[i,1]=x[i]
  coor[i,2]=y[i]
}


#Creating race of people
for (i in 1:length(coor$race)){
  if (i<=f*N){
    coor[[3]][i]=1
  }else{
    coor[[3]][i]=0
  }
}

#Starting the while loop
didanyonemove=TRUE
count=0
while(didanyonemove){
  count=count+1
 
#Getting the initial index of the people
  if (count==1){
      x1=c(coor[1:(f*N),1])
      y1=c(coor[1:(f*N),2])
      x2=c(coor[(f*N+1):N,1])
      y2=c(coor[(f*N+1):N,2])
    }
  begindex=calcindex(x1,y1,x2,y2)
   
#Creating a Distance Matrix
distance=matrix(0,nrow=N,ncol=N) 
for (j in 1:N){
  for (i in 1:length(coor$x)){
    distance[i,j]=((((coor$x[j]-coor$x[i])^2)+((coor$y[j]-coor$y[i])^2))^.5)
  }
}

#Sorting all Neighbors
allneighbors=matrix(0,nrow=N,ncol=N)
for (j in 1:N){
    allneighbors[,j]=sort(distance[,j],decreasing=FALSE)
}

#Getting the Ten Closest Neighbors
closeneighbors=matrix(0,nrow=11,ncol=N)
for (j in 1:N){
    closeneighbors[,j]=allneighbors[1:11,j]
}

#Relate the 10 closest neighbors to their race
#In the distance matrix, how close is everyone 
#Row 2, Column 1, that person is the xth closest person from the Column(th) person
persondistcheck=matrix(0,nrow=N,ncol=N)
for (i in 1:N){
  for (j in 1:N){
    persondistcheck[i,j]=which(allneighbors[,j]==distance[i,j])
  }
}

#In persondistcheck, take the closest people and see where they were assigned in the coordinates
#Row 2, Column 1, that person is the x person on the coordinates I assigned
closedistcheck=matrix(0,nrow=11,ncol=N)
for (i in 1:11){
  for (j in 1:N){
    closedistcheck[i,j]=which(persondistcheck[,j]==i)
  }
}

#Putting the race of the 10 closest people in order
#top row is the race of the person in question
racematrix=matrix(0,nrow=11,ncol=N)
for (i in 1:11){
  for (j in 1:N){
    racematrix[i,j]=coor$race[closedistcheck[i,j]]
  }
}

#Seeing percentage of race of 10 closest neighbors
racecount=matrix(0,nrow=2,ncol=N)
racecount[1,]=racematrix[1,]
for (i in 1:N){
  if (racecount[1,i]==1){
    racecount[2,i]=(sum(racematrix[2:11,i])/10)
  }else{
    racecount[2,i]=((10-sum(racematrix[2:11,i]))/10)
  }  
}

didanyonemove=FALSE
#Moving if not happy with the amount of race around them
for (i in 1:N){
  if (racecount[2,i]>=t){
    coor$x[i]=coor$x[i]
    coor$y[i]=coor$y[i]
  }else{
    didanyonemove=TRUE
    coor$x[i]=runif(1)
    coor$y[i]=runif(1)
  }
}
} #closing the while loop

#getting the ending racial index
x3=c(coor[1:(f*N),1])
y3=c(coor[1:(f*N),2])
x4=c(coor[(f*N+1):N,1])
y4=c(coor[(f*N+1):N,2])

endindex=calcindex(x3,y3,x4,y4)
  smatrix[1,z]=begindex
  smatrix[2,z]=endindex
  beginningindex=mean(smatrix[1,])
  endingindex=mean(smatrix[2,])
} #closing the iteration loop
return(c(beginningindex,endingindex))
} #closing the function

