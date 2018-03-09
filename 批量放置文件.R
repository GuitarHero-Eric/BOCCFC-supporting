c<-data.frame(dir("test\\测试样本"))
c<-cbind(c,substr(c$dir..test..测试样本..,1,13))
colnames(c)<-c("编号加姓名","消费金融账号")
a<-read.csv("test\\样本.csv")
a<-merge(c,a,by ="消费金融账号",all.x = TRUE )
a$客户姓名<-gsub(" ","",a$客户姓名)
for (i in 1:length(a$客户姓名))
  {
  file.copy(c(paste("test\\",a[i,4],".pdf",sep = ""),paste("test\\",a[i,4],".pdf",sep = "")),paste("test\\测试样本\\",a[i,1],a[i,3],sep = ""))
  }
