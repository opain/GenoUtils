# Read in real rds files generated using 1KG phase 3 and subset 100 rows from each chromosome that are present in the test GWAS
library(data.table)
sumstats<-fread(paste0('~/oliverpainfel/Software/MyGit/GenoDisc/pipeline/test_data/output/test1/data/gwas_sumstat/COAD01/COAD01.cleaned.gz'))

for(i in 1:22){
  tmp<-readRDS(paste0('~/oliverpainfel/Software/MyGit/GenoDisc/pipeline/resources/data/1kg/1KG.Phase3.EUR.MAF_001.chr',i,'.rds'))
  tmp <- tmp[tmp$SNP %in% sumstats$SNP,]
  tmp<-tmp[1:100,]
  saveRDS(tmp, paste0('inst/extdata/ref.chr',i,'.rds'))
}
