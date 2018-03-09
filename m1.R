m1<-function(dates,datee)
  
{timeipx<-seq(as.Date(dates),as.Date(datee),by="day")

for (qqq in 1:length(timeipx))
  
{
  aa<-read.csv("M1回正率.csv",stringsAsFactors = F)
  aa<-aa[,-1]
  
  ##判断当前日处于M3的什么阶段
  a<-read.csv("账龄变动日x.csv",stringsAsFactors = F)
  
  timeip<-as.Date(timeipx[qqq])
  
  M0取数日<-as.Date(a$M0取数日)
  
  M0取数日<-max(subset(M0取数日,M0取数日<=timeip))
  
  
  M1逾期天数<-as.numeric((timeip - as.Date(M0取数日))+1)
  
  M0日期<-as.Date(M0取数日)
  M1日期<-as.Date(timeip)
  
  
  if (M0日期==M1日期)
  {M0日期<-as.Date(M0日期)-months(1)
  M1逾期天数<-as.numeric((timeip - as.Date(M0取数日))+1+30)}
  M0日期<-gsub("-","",M0日期)
  M1日期<-gsub("-","",timeip)
  
  
  #取M2的数
  c<-colnamecorr(paste("lab/现金贷款产品信息",M0日期,".csv",sep=""))
  
  c<-subset(c,逾期天数=="1" & 还款日设定区分!="首次消费时确定还款日" )
  c$额度代码<-gsub("=","",c$额度代码)
  c$额度代码<-gsub("\"","",c$额度代码)
  c<-subset(c,额度代码=="042221" | 额度代码=="041221" | 额度代码=="053221" | 额度代码=="051221" | 额度代码=="0D1221"| 额度代码=="091213"| 额度代码=="092213"| 额度代码=="043221"| 额度代码=="061221"| 额度代码=="052221")
  
  cfin<-ddply(c,.(受理城市),summarize,M1初始=length(申请编号),M1初额=sum(本金余额,na.rm=T))
  
  
  da<-colnamecorr(paste("lab/现金贷款产品信息",M1日期,".csv",sep=""))
  
  d<-subset(da,逾期天数==M1逾期天数 & 还款日设定区分!="首次消费时确定还款日" )
  d$额度代码<-gsub("=","",d$额度代码)
  d$额度代码<-gsub("\"","",d$额度代码)
  d<-subset(d,额度代码=="042221" | 额度代码=="041221" | 额度代码=="053221" | 额度代码=="051221" | 额度代码=="0D1221"| 额度代码=="091213"| 额度代码=="092213"| 额度代码=="043221"| 额度代码=="061221"| 额度代码=="052221")
  
  
  
  if (length(d[,1])==0)
  {d<-subset(da,逾期天数=="31" & 还款日设定区分!="首次消费时确定还款日" )
  d$额度代码<-gsub("=","",d$额度代码)
  d$额度代码<-gsub("\"","",d$额度代码)
  d<-subset(c,额度代码=="042221" | 额度代码=="041221" | 额度代码=="053221" | 额度代码=="051221" | 额度代码=="0D1221"| 额度代码=="091213"| 额度代码=="092213"| 额度代码=="043221"| 额度代码=="061221"| 额度代码=="052221")
  }
  
  dfin<-ddply(d,.(受理城市),summarize,M1至今=length(申请编号),M1今额=sum(本金余额,na.rm=T))
  
  #合并
  e<-merge(cfin,dfin,by="受理城市",all=T)
  e[is.na(e)]<-0
  
  回正率<-(e$M1初始-e$M1至今)/e$M1初始
  回收率<-(e$M1初额-e$M1今额)/e$M1初额
  e<-cbind(e,回正率,回收率,逾期天数=M1逾期天数,实际日期=as.character(as.Date(timeip)-1))
  e<-e[c("受理城市","M1初始","M1至今","回正率","M1初额","M1今额","回收率","逾期天数","实际日期")]
  
  ee<-rbind(aa,e)
  write.csv(ee,"M1回正率.csv")}
}