setwd("F:/min-labs_paper/work/WHX_DATA/")
A=list.files(path = c("./0","./1","./5","./10","./15","./20","./30","./50"),full.names = T,recursive = T,all.files = T,pattern = "enplot.*png$")


B_T1C1=unique(na.omit(stringr::str_extract(string = A,pattern = "(?<=T1_C1.{10,20})enplot.*?(?=_\\d+.{0,10}png)")))
B_T2C1=unique(na.omit(stringr::str_extract(string = A,pattern = "(?<=T2_C1.{10,20})enplot.*png")))
B_T3C1=na.omit(stringr::str_extract(string = A,pattern = "(?<=T3_C1.{10,20})enplot.*png"))




######ÐÞ¸ÄÑÕÉ«
red=c(255,0,0)
green=c(0,255,0)
blue=c(0,0,255)

merge=function(file_in_png,file_out_png,RGB){
library(png)
aPic=readPNG(source = file_in_png)
dim(aPic)
aPic_=aPic*255;max(aPic_);min(aPic)
C=(aPic_[,,1]<250*0.9)&(aPic_[,,2]>=250*0.6)&(aPic_[,,3]<=250*0.6)
for (rowIndex in c(1:nrow(C))) {
  for (colIndex in c(1:ncol(C))) {
    if(C[rowIndex,colIndex]==TRUE){
      # aPic_[rowIndex,colIndex,1]=R
      # aPic_[rowIndex,colIndex,2]=G
      # aPic_[rowIndex,colIndex,3]=B
      aPic_[rowIndex,colIndex,c(1,2,3)]=RGB
    }
  }
}
# which(aPic_[,,1]==R)
# which(aPic_[,,2]==G)
# which(aPic_[,,3]==B)
aPic__=aPic_/255
aPic___=aPic__
# setwd("F:/min-labs_paper/work/WHX_DATA/")
writePNG(aPic___,target = file_out_png)
}





