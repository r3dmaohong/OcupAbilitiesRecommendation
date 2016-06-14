rm(list = ls()) #去除工作空間中所有物件
gc() #記憶體釋放
path<-"D:\\abc\\wjhong\\projects\\ocup_recommend"
setwd(path)

options(stringsAsFactors = FALSE)


ocup = read.csv(file.choose())
#ocup = ocup[which(ocup$職務X職能指標關聯性=='高'),]

job = unique(ocup$職務小類名稱)

ocup_r = data.frame('職務名稱'=character(),'對應職務名稱'=character(),'相似度'=numeric())
total_ocup_r = data.frame('職務名稱'=character(),'對應職務名稱'=character(),'相似度'=numeric())
##以main為主體
##x為nrow
##去修改為跑全部
len_job = 1:length(job)
for(n_job in 1:length(job)){
  
  ocup_r[(len_job-1),1] = job[n_job]
  ocup_r[(len_job-1),2] = job[len_job[! len_job %in% n_job]] #a [! a %in% remove]
  
  main = ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[n_job])]
  
  for(i in 1:nrow(ocup_r)){
    sub = ocup$職能指標小類名稱[which(ocup$職務小類名稱==ocup_r[i,2])]
    tmp = 0
    for(j in 1:length(main)){
      if(main[j] %in% sub){
        tmp = tmp + 1
      }
    }
    if(tmp==0){
      ocup_r[i,3] = 0
    }else{
      ocup_r[i,3] = (tmp*2)/(length(main)+length(sub))
    }
  }
  
  ocup_r = ocup_r[order(-ocup_r[,3]),]
  ocup_r$相似度 = ocup_r$相似度[1]
  total_ocup_r = rbind(total_ocup_r,ocup_r)
}

##
if(F){
  ocup_r[1:length(job)-1,1] = job[1]
  ocup_r[1:length(job)-1,2] = job[2:length(job)] #a [! a %in% remove]
  
  main = ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[1])]
  
  for(i in 1:nrow(ocup_r)){
    sub = ocup$職能指標小類名稱[which(ocup$職務小類名稱==ocup_r[i,2])]
    tmp = 0
    for(j in 1:length(main)){
      if(main[j] %in% sub){
        tmp = tmp + 1
      }
    }
    if(tmp==0){
      ocup_r[i,3] = 0
    }else{
      ocup_r[i,3] = (tmp*2)/(length(main)+length(sub))
    }
  }
  
  ocup_r = ocup_r[order(-ocup_r[,3]),]
}
total_ocup_r = total_ocup_r[which(total_ocup_r[,3]!=0),]

##
write.csv(total_ocup_r,'職能相似度換算.csv',row.names=F)
