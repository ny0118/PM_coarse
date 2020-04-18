#�����վ�MEE��PM2.5��PM10��PMcoarse
#����Դ��http://beijingair.sinaapp.com/  �й���������
#2020-04-17
#Author�� Dr. NAN Yang


#(1)read Raw data and calcalate daily mean--------------------
getwd()
dirname='E:/��ѧ����/2019Spring_PMcoarse/data/վ��_20150101-20151231'
setwd(dirname)

filename<-list.files(dirname,full.names=T,all.files=T,recursive=T)

#print(filename)

length(filename)



for (f in 1:length(filename)) { 
#f=1
MyData <- read.csv(file=filename[f], header=TRUE, sep=",")

#������������ЧСʱ��
hour<-dim(MyData)[1]/15 

#������������Ч��վ����
dim(MyData)[2]

type<-data.frame(type=MyData$type)

#������ͬtype����ֱ����ƽ�������վ�ֵ��na.rm=T
library(dplyr)

daily_mean<-MyData %>% 
  group_by(type) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))

daily_mean$hour=hour


write.csv(daily_mean, file =dirname[f], row.names = F, quote = F)
}




#(2)each site annual mean------------------
gc() #�ͷ��ڴ�

memory.size(T)#�鿴�ѷ����ڴ�
memory.size(F)#�鿴��ʹ���ڴ�
memory.limit()#�鿴�ڴ�����
memory.size()#�鿴���ڵ�work space���ڴ�ʹ��
memory.limit(102400)#���ó�100G

getwd()
dirname='E:/��ѧ����/2019Spring_PMcoarse/data/վ��_20150101-20151231'
setwd(dirname)
filename<-list.files(dirname,full.names=T,all.files=T,recursive=T)
#length(filename)

#��ȡվ����Ϣ
list_data<-read.csv("E:/��ѧ����/2019Spring_PMcoarse/data/site_list_2020.04.05_vacnt filled.csv")
list_data_PM25<- list_data
list_data_PM10<- list_data
list_data_PMcoarse<- list_data

#��filenameֻ��ȡYYYYMMDD��������Ҫ��,����������
#nchar(filename[1])
day_name<-substring(filename[],67,74)
#print(day_name)

a<-c(rep(NA,length(list_data[,1])))

#��ȡ�����ļ�����ÿ���ļ�����Ӧsite��PM2.5�վ�ֵ��Ϊһ�У�����list_data_PM25����
#����һ�е�����Ϊ��YYYYMMDD��
#f=1
for (f in 1:length(filename)) {
#for (f in 1:2) {

#��ȡÿ�տ���������Ϣ
print(paste("processing file: ", filename[f]))
MyData1 <- read.csv(file=filename[f], header=TRUE, sep=",")
dim(MyData1)[2]

#��list��һ�У��Ұ��¼ӵ������ĳ�����
list_data_PM25<- cbind(list_data_PM25,a)
colnames(list_data_PM25)[6+f]=day_name[f]

list_data_PM10<- cbind(list_data_PM10,a)
colnames(list_data_PM10)[6+f]=day_name[f]

list_data_PMcoarse<- cbind(list_data_PMcoarse,a)
colnames(list_data_PMcoarse)[6+f]=day_name[f]


#�վ�ֵ��Xȥ��,���2015
site_name_daily=unlist(dimnames(MyData1)[2])
site_name_daily[4]
site_name_daily<-site_name_daily[-c(1,2,3)]
site_name_daily<-substring(site_name_daily[],2)

#MyData1[12,3+m]

length(site_name_daily)

#�ȶ�Ȼ�����ݺϲ���
for (n in 1:1657) {
  for (m in 1:(dim(MyData1)[2]-3)) {
    if(list_data$�������[n]==site_name_daily[m]){
      #PM2.5
      list_data_PM25[n,6+f]<-MyData1[12,3+m]
      
      #PM10
      list_data_PM10[n,6+f]<-MyData1[10,3+m]
      
      #PMcoarse
      list_data_PMcoarse[n,6+f]<-MyData1[10,3+m]-MyData1[12,3+m]
    }
  }
  print(paste(day_name[f],"sites:",n))
}

}

#(3)���վ����Ͻ���洢------------------------
#csv
getwd()
dirname='E:/��ѧ����/2019Spring_PMcoarse/0417-0418 �����վ�PMcoarse/'
setwd(dirname)
write.csv(list_data_PM25, file ="2015_daily_PM25.csv", row.names = F, quote = F)
write.csv(list_data_PM10, file ="2015_daily_PM10.csv", row.names = F, quote = F)
write.csv(list_data_PMcoarse, file ="2015_daily_PMcoarse.csv", row.names = F, quote = F)

list_data_PM25[is.na(list_data_PM25)]=-9999
list_data_PMcoarse[is.na(list_data_PMcoarse)]=-9999

#txt
write.table(list_data_PM25, file ="2015_daily_PM25.txt",row.names = F, quote = F,sep = " ")
write.table(list_data_PMcoarse, file ="2015_daily_PMcoarse.txt",row.names = F, quote = F,sep = " ")

     