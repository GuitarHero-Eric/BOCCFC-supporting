loadW<- function(date="today")
{
  if (date=="today")
  {date <- as.character(format(Sys.Date(),"%Y%m%d"))}else{
    date<-as.Date(date)
    date <- as.character(format(date,"%Y%m%d"))}
  
  customerfile <- paste("�ͻ���Ʒ����弶�����ձ�_",date,".zip",sep="")
  #����XYD���ļ���
  download.file(paste("ftp://fengxian:fengxian@172.16.6.238/",customerfile,sep=""),customerfile)
  #���ص���XYD�ļ�
  unzip(customerfile,exdir = "daily_report")
  #��ѹXYD�ļ�
  file.remove(customerfile)
  #ɾ��XYDѹ���ļ�
  a <- c(0,1)
  for (i in a) {
    customerfilecsv <- paste("�ͻ���Ʒ����弶�����ձ�",date,"_",i,".csv",sep="")
    customerfilecsv <- paste("daily_report/",customerfilecsv,sep="")
    xt1 <- read.csv(customerfilecsv)
    if (i==0)
    {xtt <- xt1}
    else
    {xtt <- rbind(xtt,xt1)}
  }
  
  product_clsssify <- paste("ģ��/","�ָ��Ʒ����",".CSV",sep="")
  product_clsssify <- read.csv(product_clsssify)
  
  city_clsssify <- paste("ģ��/","�ָ��������",".CSV",sep="")
  city_clsssify <- read.csv(city_clsssify)
  xtt <- merge(xtt,product_clsssify,by="��Ȳ�Ʒ",all.x=T)
  xtt <- merge(xtt,city_clsssify,by="��������",all.x=T)
}