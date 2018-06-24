##mapping mouse
BB=read.csv(file = "I:/mart_export.txt/mart_export_human_ncbiid.txt",stringsAsFactors = FALSE)
CC=BB[,c("Gene.stable.ID","NCBI.gene.ID")]
CC=unique(CC)

###¿¿CC×ª»»GMTÎÄ¼þ
gmt_in="I:/GSEA/download_file/msigdb_v6.1_files_to_download_locally/msigdb_v6.1_GMTs/msigdb.v6.1.entrez.gmt"
gmt_out="I:/GSEA/download_file/msigdb_v6.1_files_to_download_locally/msigdb_v6.1_GMTs/msigdb.v6.1.ensemble.gmt"
library(parallel);
cls <- parallel::makePSOCKcluster(3);
  library(LaF)
  nrows=LaF::determine_nlines(filename = gmt_in)
  pass=c()
  #for(rowindex in ){
    #sapply(X = c(1:nrows),FUN = function(rowindex){
  parSapplyLB(cl=cls,X = c(1:nrows),FUN = function(rowindex){
    library(LaF)
    gmt_in="I:/GSEA/download_file/msigdb_v6.1_files_to_download_locally/msigdb_v6.1_GMTs/msigdb.v6.1.entrez.gmt"
    gmt_out="I:/GSEA/download_file/msigdb_v6.1_files_to_download_locally/msigdb_v6.1_GMTs/msigdb.v6.1.ensemble.gmt"
    oneline=get_lines(gmt_in,rowindex) 
    data_gmt_one=stringr::str_replace_all(string = oneline,pattern = "(?<=\\t)(\\d+)(?=\\t)",replacement =function(x){
      #data_gmt_one=stringr::str_replace_all(string = oneline,pattern = "\\t\\d+\\t",replacement =function(x){
      if(nchar(paste(unique(CC$Gene.stable.ID[which(CC$NCBI.gene.ID==x)]),collapse = "\\t"))>0){
        return(paste(unique(CC$Gene.stable.ID[which(CC$NCBI.gene.ID==x)]),collapse = "\t"))
      }else{
        return(x) 
      }
    })
    write.table(x=data_gmt_one,file = gmt_out,col.names = FALSE,sep = "",append = TRUE,quote = FALSE,qmethod = "double")
    gc()
    pass<<-c(pass,rowindex)
  })
stopCluster(cls)
