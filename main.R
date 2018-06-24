file="I:/HOM_MouseHumanSequence.rpt"
data=read.csv(file = file,stringsAsFactors = FALSE,sep = "\t")
library(tidyr)# 使用的gather & spread
library(reshape2) # 使用的函数 melt & dcast 
data=unique(data)
data_b=spread(data = data,key ="HomoloGene.ID",value = "Common.Organism.Name")
datamapping=data.frame()
mouse_homologenes=unique(data$HomoloGene.ID[which(data$NCBI.Taxon.ID=="10090")])
for(onehomologene in mouse_homologenes){
  AA=data[intersect(which(data$HomoloGene.ID==onehomologene),which(data$NCBI.Taxon.ID=="10090")),c("NCBI.Taxon.ID","EntrezGene.ID")]
  AA[,"EntrezGene.ID.HUMAN"]=paste(data[intersect(which(data$HomoloGene.ID==onehomologene),which(data$NCBI.Taxon.ID==9606)),"EntrezGene.ID"],collapse = "\t")
  datamapping=rbind(datamapping,AA)
}
write.csv(file = "mapping_mouse_genid_human_geneid.csv",x=datamapping)

##mapping mouse
BB=read.csv(file = "I:/GSEA/mart_export.txt/mouse_ensemble_geneid.txt")
BB=BB[which(!is.na(BB$NCBI.gene.ID)),]
length(unique(BB$Gene.stable.ID));length(unique(BB$NCBI.gene.ID))
CC=merge(x=datamapping,y=BB,by.x = "EntrezGene.ID",by.y = "NCBI.gene.ID")
###靠CC转换GMT文件
gmt_in="I:/GSEA/download_file/msigdb_v6.1_files_to_download_locally/msigdb_v6.1_GMTs/"
gmt_out="I:/GSEA/download_file/msigdb_v6.1_files_to_download_locally/msigdb_v6.1_GMTs_ensembled/"
(files_gmt=list.files(path = gmt_in,pattern = ".*gmt",full.names = FALSE))
for(onegmt in  files_gmt){
 setwd(gmt_in)
 library(LaF)
 nrows=LaF::determine_nlines(filename = onegmt)
 for(rowindex in c(1:nrows)){
  setwd(gmt_in)
  oneline=get_lines(onegmt,rowindex)  
  data_gmt_one=stringr::str_replace_all(string = oneline,pattern = "(?<=\\t)(\\d+)(?=\\t)",replacement =function(x){
    #data_gmt_one=stringr::str_replace_all(string = oneline,pattern = "\\t\\d+\\t",replacement =function(x){
    if(nchar(paste(CC$Gene.stable.ID[which(CC$EntrezGene.ID.HUMAN==x)],collapse = "\\t"))>0){
      return(paste(CC$Gene.stable.ID[which(CC$EntrezGene.ID.HUMAN==x)],collapse = "\t"))
    }else{
      return(x) 
    }
  })
  #print(data_gmt_one)
  setwd(gmt_out)
  write.table(x=data_gmt_one,file = paste("ensembeld",onegmt,sep = "_"),col.names = FALSE,sep = "",append = TRUE,quote = FALSE,qmethod = "double")
 }
  
  
}





