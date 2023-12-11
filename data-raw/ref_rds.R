# Read in real rds files generated using 1KG phase 3 and subset 100 rows from each chromosome that are present in the test raw GWAS
library(data.table)

for(i in 1:22){
  tmp<-readRDS(paste0('~/oliverpainfel/Software/MyGit/GenoDisc/pipeline/resources/data/1kg/1KG.Phase3.MAF_001.chr',i,'.rds'))
  tmp <- tmp[tmp$SNP %in% raw_sumstats_1$SNP,]
  tmp<-tmp[1:100,]
  saveRDS(tmp, paste0('inst/extdata/ref.chr',i,'.rds'))
}
