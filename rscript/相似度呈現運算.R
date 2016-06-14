rm(list = ls()) #去除工作空間中所有物件
gc() #記憶體釋放
path<-"D:\\abc\\wjhong\\projects\\ocup_recommend"
setwd(path)

options(stringsAsFactors = FALSE)


ocup = read.csv('職務與職能指標關聯總表0-20160518-1.csv',stringsAsFactors=F)
#ocup = ocup[which(ocup$職務X職能指標關聯性=='高'),]

job = unique(ocup$職務小類名稱)

salary <- read.csv('2015職務小類年資薪資-20160614-1.csv',stringsAsFactors=F)

total_ocup_r = data.frame('職務A'=character(),'A相關職能'=character(),'職務B'=character(),'B相關職能'=character(),'相同職能'=character(),'缺乏職能'=character(),'相似度'=character(),'職務暫存'=character())

##以main為主體
##x為nrow
##去修改為跑全部
len_job = 1:length(job)
for(n_job in 1:length(job)){
  for(x_job in len_job[! len_job %in% n_job]){
    ocup_r = data.frame('職務A'=character(),'A相關職能'=character(),'職務B'=character(),'B相關職能'=character(),'相同職能'=character(),'缺乏職能'=character(),'相似度'=character())
    ocup_r[1:6,1] = unlist(c(job[n_job],salary[which(salary[,2]==job[n_job]),3:7]))
    ocup_r[1:length(ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[n_job])]),2] = ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[n_job])]
    ocup_r[1:6,3] = unlist(c(job[x_job],salary[which(salary[,2]==job[x_job]),3:7]))
    ocup_r[1:length(ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[x_job])]),4] = ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[x_job])]
    
    tmp_same_ocup = c()
    a_ocup = ocup_r[which(!is.na(ocup_r[,2])),2]
    
    for(i_same_ocup in 1:length(a_ocup)){
      if(toString(which(grepl(a_ocup[i_same_ocup],ocup_r[,4])))!=''){
        tmp_same_ocup = c(tmp_same_ocup, a_ocup[i_same_ocup])
      }
    }
    if(length(tmp_same_ocup)>0){
      ocup_r[1:length(tmp_same_ocup),5] = tmp_same_ocup
  
    }
    if(length(ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[x_job])][!(ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[x_job])] %in% tmp_same_ocup)])>0){
      ocup_r[1:length(ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[x_job])][!(ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[x_job])] %in% tmp_same_ocup)]),6] = ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[x_job])][!(ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[x_job])] %in% tmp_same_ocup)]      
    }
    
    
    main = ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[n_job])]
    sub = ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[x_job])]
    tmp = 0
    for(j in 1:length(main)){
      if(main[j] %in% sub){
        tmp = tmp + 1
      }
    }
    if(tmp==0){
      ocup_r[1,7] = 0
    }else{
      ocup_r[1,7] = (tmp*2)/(length(main)+length(sub))
    }
    ocup_r$相似度 = ocup_r$相似度[1]
    total_ocup_r = rbind(total_ocup_r,ocup_r)
    total_ocup_r[is.na(total_ocup_r)] <- ""
    
  }
  write.csv(total_ocup_r, paste0('output\\',n_job,'.',gsub('/','／',job[n_job]),'相似度觀看格式.csv'),row.names=F)
  total_ocup_r = total_ocup_r[0,]
  cat(paste0('\r',n_job,'.',job[n_job],' ',format(round(n_job/length(job)*100,2),nsmall=2),'%'))
}

#write.csv(total_ocup_r, '相似度觀看格式.csv',row.names=F)
##篩選掉的
if(T){
  match_table = read.csv('1111職務類別表-20160613-1.csv',stringsAsFactors=F)
  ##以main為主體
  ##x為nrow
  ##去修改為跑全部
  len_job = 1:length(job)
  for(n_job in 1:length(job)){
    for(x_job in len_job[! len_job %in% n_job]){
      ocup_r = data.frame('職務A'=character(),'A相關職能'=character(),'職務B'=character(),'B相關職能'=character(),'相同職能'=character(),'缺乏職能'=character(),'相似度'=character())
      ocup_r[1:7,1] = unlist(c(job[n_job],match_table[which(match_table[,6]==job[n_job]),2],salary[which(salary[,2]==job[n_job]),3:7]))
      ocup_r[1:length(ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[n_job])]),2] = ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[n_job])]
      ocup_r[1:7,3] = unlist(c(job[x_job],match_table[which(match_table[,6]==job[x_job]),2],salary[which(salary[,2]==job[x_job]),3:7]))
      ocup_r[1:length(ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[x_job])]),4] = ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[x_job])]
      
      tmp_same_ocup = c()
      a_ocup = ocup_r[which(!is.na(ocup_r[,2])),2]
      
      for(i_same_ocup in 1:length(a_ocup)){
        if(toString(which(grepl(a_ocup[i_same_ocup],ocup_r[,4])))!=''){
          tmp_same_ocup = c(tmp_same_ocup, a_ocup[i_same_ocup])
        }
      }
      if(length(tmp_same_ocup)>0){
        ocup_r[1:length(tmp_same_ocup),5] = tmp_same_ocup
        
      }
      if(length(ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[x_job])][!(ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[x_job])] %in% tmp_same_ocup)])>0){
        ocup_r[1:length(ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[x_job])][!(ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[x_job])] %in% tmp_same_ocup)]),6] = ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[x_job])][!(ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[x_job])] %in% tmp_same_ocup)]      
      }
      
      
      main = ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[n_job])]
      sub = ocup$職能指標小類名稱[which(ocup$職務小類名稱==job[x_job])]
      tmp = 0
      for(j in 1:length(main)){
        if(main[j] %in% sub){
          tmp = tmp + 1
        }
      }
      if(tmp==0){
        ocup_r[1,7] = 0
      }else{
        ocup_r[1,7] = (tmp*2)/(length(main)+length(sub))
      }
      ocup_r$相似度 = ocup_r$相似度[1]
      ocup_r$職務暫存 = ocup_r$職務B[1]
      total_ocup_r = rbind(total_ocup_r,ocup_r)
      total_ocup_r[is.na(total_ocup_r)] <- ""
      
    }
    total_ocup_r = total_ocup_r[which(!(total_ocup_r$職務暫存 %in% match_table[which(match_table[,2]==match_table[which(match_table[,6]==job[n_job]),2]),6])),]
    total_ocup_r = total_ocup_r[which(total_ocup_r$相似度>0.5),]
    total_ocup_r$職務暫存 = NULL
    if(nrow(total_ocup_r)>0){
      write.csv(total_ocup_r, paste0('output\\篩選後\\',n_job,'.',gsub('/','／',job[n_job]),'相似度觀看格式.csv'),row.names=F)
    }
    total_ocup_r = data.frame('職務A'=character(),'A相關職能'=character(),'職務B'=character(),'B相關職能'=character(),'相同職能'=character(),'缺乏職能'=character(),'相似度'=character(),'職務暫存'=character())
    
    
    cat(paste0('\r',n_job,'.',job[n_job],' ',format(round(n_job/length(job)*100,2),nsmall=2),'%'))
  }
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
