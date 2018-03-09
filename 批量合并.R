a <- list.files("E:\\费用报告/1105/csv")
n<-length(a)
merge.data<-NULL
for (i in 11:n) {
  new.data <- read.csv(file = a[i], stringsAsFactors = F)
  merge.data <- rbind(merge.data,new.data)
}
merge.data <- subset(merge.data,交易类型=="退还金额还款")
write.csv(merge.data, file="E:\\费用报告/1105/csv/1105merge.csv",row.names=F)
