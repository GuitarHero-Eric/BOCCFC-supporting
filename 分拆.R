a<-read.csv("kaishu.csv")
q<-as.data.frame(table(a$区域中心))
# a<-subset(a,管理团队=="首逾团队")
dir.create(paste("kaishu",sep = ""), showWarnings = FALSE)
for (x in 1:nrow(q))
{
  x1<-subset(a,区域中心==q[x,1])
  wd<-as.character(q[x,1])
  wd<-gsub(" ","",wd)
  write.csv(x1,paste("E:/R路径/kaishu/",wd,".csv",sep=""))
}
