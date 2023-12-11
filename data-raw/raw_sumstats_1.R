# Read in raw sumstats
library(data.table)
tmp<-fread(paste0('~/oliverpainfel/Software/MyGit/GenoDisc/pipeline/test_data/reference/gwas_sumstats/COAD01.gz'))

tmp3<-NULL
for(i in 1:22){
  tmp2<-tmp[tmp$CHR == i,]
  tmp2<-tmp2[1:100,]
  tmp3<-rbind(tmp3, tmp2)
}

raw_sumstats_1 <- tmp3

usethis::use_data(raw_sumstats_1, overwrite = TRUE)
