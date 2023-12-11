# Read in raw sumstats
library(data.table)
tmp<-fread(paste0('~/oliverpainfel/Software/MyGit/GenoDisc/pipeline/test_data/reference/gwas_sumstats/BODY04.gz'))

tmp3<-NULL
for(i in 1:22){
  ref<-readRDS(paste0('inst/extdata/ref.chr',i,'.rds'))
  tmp2 <- tmp[tmp$SNP %in% ref$SNP,]
  tmp3<-rbind(tmp3, tmp2)
}

raw_sumstats_2 <- tmp3

usethis::use_data(raw_sumstats_2, overwrite = TRUE)

