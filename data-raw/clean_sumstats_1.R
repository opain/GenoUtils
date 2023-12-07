# Read in cleaned sumstats
library(data.table)
tmp<-fread(paste0('~/oliverpainfel/Software/MyGit/GenoDisc/pipeline/test_data/output/test1/data/gwas_sumstat/COAD01/COAD01.cleaned.gz'))

tmp3<-NULL
for(i in 1:22){
  ref<-readRDS(paste0('inst/extdata/ref.chr',i,'.rds'))
  tmp2 <- tmp[tmp$SNP %in% ref$SNP,]
  tmp3<-rbind(tmp3, tmp2)
}

clean_sumstats_1 <- tmp3

usethis::use_data(clean_sumstats_1, overwrite = TRUE)
