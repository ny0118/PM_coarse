#计算日均MEE的PM2.5、PM10、PMcoarse
#数据源：http://beijingair.sinaapp.com/  中国空气质量
#2020-04-17
#Author： Dr. NAN Yang


#(1)read Raw data and calcalate daily mean--------------------
getwd()
dirname='E:/XXX/站点_20150101-20151231'
setwd(dirname)

filename<-list.files(dirname,full.names=T,all.files=T,recursive=T)

#print(filename)

length(filename)



for (f in 1:length(filename)) { 
#f=1
MyData <- read.csv(file=filename[f], header=TRUE, sep=",")

#当天数据中有效小时数
hour<-dim(MyData)[1]/15 

#当天数据中有效总站点数
dim(MyData)[2]

type<-data.frame(type=MyData$type)

#根据相同type类型直接求平均，算日均值，na.rm=T
library(dplyr)

daily_mean<-MyData %>% 
  group_by(type) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))

daily_mean$hour=hour


write.csv(daily_mean, file =dirname[f], row.names = F, quote = F)
}




#(2)each site annual mean------------------
gc() #释放内存

memory.size(T)#查看已分配内存
memory.size(F)#查看已使用内存
memory.limit()#查看内存上限
memory.size()#查看现在的work space的内存使用
memory.limit(102400)#设置成100G

getwd()
dirname='E:/XXX/站点_20150101-20151231'
setwd(dirname)
filename<-list.files(dirname,full.names=T,all.files=T,recursive=T)
#length(filename)

#读取站点信息
list_data<-read.csv("E:/XXX/site_list_2020.04.05_vacnt filled.csv")
list_data_PM25<- list_data
list_data_PM10<- list_data
list_data_PMcoarse<- list_data

#对filename只截取YYYYMMDD，其他不要了,给列做列名
#nchar(filename[1])
day_name<-substring(filename[],67,74)
#print(day_name)

a<-c(rep(NA,length(list_data[,1])))

#读取所有文件，把每个文件，对应site的PM2.5日均值加为一列，贴到list_data_PM25后面
#且这一列的列名为“YYYYMMDD”
#f=1
for (f in 1:length(filename)) {
#for (f in 1:2) {

#读取每日空气质量信息
print(paste("processing file: ", filename[f]))
MyData1 <- read.csv(file=filename[f], header=TRUE, sep=",")
dim(MyData1)[2]

#给list加一列，且把新加的列名改成日期
list_data_PM25<- cbind(list_data_PM25,a)
colnames(list_data_PM25)[6+f]=day_name[f]

list_data_PM10<- cbind(list_data_PM10,a)
colnames(list_data_PM10)[6+f]=day_name[f]

list_data_PMcoarse<- cbind(list_data_PMcoarse,a)
colnames(list_data_PMcoarse)[6+f]=day_name[f]


#日均值把X去掉,针对2015
site_name_daily=unlist(dimnames(MyData1)[2])
site_name_daily[4]
site_name_daily<-site_name_daily[-c(1,2,3)]
site_name_daily<-substring(site_name_daily[],2)

#MyData1[12,3+m]

length(site_name_daily)

#比对然后数据合并。
for (n in 1:1657) {
  for (m in 1:(dim(MyData1)[2]-3)) {
    if(list_data$监测点编码[n]==site_name_daily[m]){
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

#(3)把日均贴合结果存储------------------------
#csv
getwd()
dirname='E:/XXX/0417-0418 计算日均PMcoarse/'
setwd(dirname)
write.csv(list_data_PM25, file ="2015_daily_PM25.csv", row.names = F, quote = F)
write.csv(list_data_PM10, file ="2015_daily_PM10.csv", row.names = F, quote = F)
write.csv(list_data_PMcoarse, file ="2015_daily_PMcoarse.csv", row.names = F, quote = F)

list_data_PM25[is.na(list_data_PM25)]=-9999
list_data_PMcoarse[is.na(list_data_PMcoarse)]=-9999

#txt
write.table(list_data_PM25, file ="2015_daily_PM25.txt",row.names = F, quote = F,sep = " ")
write.table(list_data_PMcoarse, file ="2015_daily_PMcoarse.txt",row.names = F, quote = F,sep = " ")

     
