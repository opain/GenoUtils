# Read in cleaned sumstats
library(data.table)

# Write test GWAS sumstats as temporary file on disk
tmp_dir<-tempdir()
fwrite(raw_sumstats_1, paste0(tmp_dir,'/raw.txt'), sep=' ', na='NA', row.names=F)

# Run sumstat_cleaner.R with test data
system(paste0("Rscript inst/scripts/sumstat_cleaner.R --sumstats ", tmp_dir,"/raw.txt --ref_chr inst/extdata/ref.chr --population EUR --output ", tmp_dir,"/clean"))

# Read in cleaned sumstats
log<-readLines(paste0(tmp_dir,"/clean.log"))
cleaned<-fread(paste0(tmp_dir,"/clean.gz"))

sumstat_cleaner_output <- list(log = log, sumstats = cleaned)

saveRDS(sumstat_cleaner_output, 'inst/extdata/sumstat_cleaner_output.rds')

