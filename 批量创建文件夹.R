dir.create(paste(as.character(Sys.Date()),sep = ""), showWarnings = FALSE)
for (i in 1:5000) {
  dir.create(paste(as.character(Sys.Date()),"/",i,sep = ""), showWarnings = FALSE)
  }
