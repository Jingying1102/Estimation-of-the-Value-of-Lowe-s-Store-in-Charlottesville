library(readxl)
set.seed(34)

setwd("D:/STAT5180/proj4")

data<-read.csv("lowes_data.csv")

data_ori<- read_excel("lowes.xlsx")


cluster_first<-unique(data$FirstClass)
index<-sample(1:26,20,replace=FALSE)
index
first_sample<-cluster_first[index]

our_list<-list(0)
for (i in 1:20) {
  cluster_now<-data[data$FirstClass==first_sample[i],]
  len<-length(cluster_now$SecondClass)
  if (len<=5) our_list[[i]]<-cluster_now
  if (len>5) {
    index_second<-sample(len,5,replace=FALSE)
    our_list[[i]]<-cluster_now[index_second,]
  }
}

n_h<-as.vector(table(data$FirstClass))

value_list<-list(0)

value16<-matrix(nrow=5,ncol=5)
value16[1,]<-c(109.98,12,24.98,8,0)
value16[2,]<-c(22.98,6,17.98,50,0)
value16[3,]<-c(6.48,26,9.98,9,0)
value16[4,]<-c(2.98,35,3.27,10,0)
value16[5,]<-c(64.98,1,7.99,6,0)
value_list[[16]]<-value16

value17<-matrix(nrow=5,ncol=5)
value17[1,]<-c(6.98,94,4.49,14,0)
value17[2,]<-c(3.48,2,4.99,11,0)
value17[3,]<-c(11.98,35,6.98,15,0)
value_list[[17]]<-value17

value18<-matrix(nrow=5,ncol=5)
value18[1,]<-c(25.98,176,14.98,18,0)
value18[2,]<-c(65.98,16,12.98,25,0)
value18[3,]<-c(3.28,883,8.98,39,0)
value18[4,]<-c(5.98,24,4.98,18,0)
value18[5,]<-c(29.99,67,19.98,29,0)
value_list[[18]]<-value18

value19<-matrix(nrow=5,ncol=5)
value19[1,]<-c(89,4,168.67,5,0)
value19[2,]<-c(199,16,79.99,6,0)
value19[3,]<-c(1699,1,1199,1,0)
value19[4,]<-c(499.99,2,6.98,5,0)
value19[5,]<-c(386,4,599,1,0)
value_list[[19]]<-value19

value20<-matrix(nrow=5,ncol=5)
value20[1,]<-c(539,1,29.98,2,0)
value20[2,]<-c(8.28,55,5.38,11,0)#Patching & Repair
value20[3,]<-c(10.98,7,21.58,3,0)
value20[4,]<-c(9.28,130,9.92,7,0)
value20[5,]<-c(1.48,152,6.98,14,0)
value_list[[20]]<-value20

for(j in 16:20){
  if(n_h[index[j]]<5){
    weight<-1/(20/26*1*2/our_list[[j]]$Number)
  }else{
    weight<-1/(20/26*5/n_h[index[j]]*2/our_list[[j]]$Number)
  }
  value_list[[j]][,5]<-weight
}

























