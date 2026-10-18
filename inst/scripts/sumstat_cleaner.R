#!/usr/bin/Rscript
# This script was written by Oliver Pain whilst at King's College London University.
start.time <- Sys.time()
library("optparse")

option_list = list(
  make_option("--sumstats", action="store", default=NA, type='character',
              help="Path to summary statistics file [required]"),
  make_option("--ref_chr", action="store", default=NA, type='character',
              help="Path to per chromosome reference .rds files [required]"),
  make_option("--population", action="store", default=NA, type='character',
              help="Reference population code [required]"),
  make_option("--sampling", action="store", default=NA, type='numeric',
              help="Sampling fraction of cases vs. controls [optional]"),
  make_option("--n", action="store", default=NA, type='numeric',
              help="Total sample size [optional]"),
  make_option("--info", action="store", default=0.9, type='numeric',
              help="INFO threshold [optional]"),
  make_option("--maf", action="store", default=0.01, type='numeric',
              help="MAF threshold [optional]"),
  make_option("--maf_diff", action="store", default=0.2, type='numeric',
              help="Difference between reference and reported MAF threshold [optional]"),
  make_option("--output", action="store", default='./Output', type='character',
              help="Path for output files [optional]"),
  make_option("--munged", action="store", default=FALSE, type='logical',
              help="Also write a munged (Z-score) copy of the sumstats [optional]"),
  make_option("--merge_alleles", action="store", default=NA, type='character',
              help="Path to an LDSC --merge-alleles SNP list (columns SNP, A1, A2, e.g. w_hm3.snplist). When set (with --munged T), an additional .munged.mergealleles.sumstats.gz copy is written, restricted to these SNPs with compatible alleles; the full .munged.sumstats.gz is left intact [optional]"),
  make_option("--test", action="store", default=NA, type='character',
              help="Specify number of SNPs to include [optional]"),
  make_option("--removed_p_thresh", action="store", default=1e-5, type='numeric',
              help="Only variants removed during QC with P below this threshold are written to <output>.removed.gz, to keep the file small [optional]"),
  make_option("--manhattan_raw_subsample", action="store", default=20000, type='numeric',
              help="Number of non-significant (P >= 1e-3) raw-input variants to retain (random subsample) in the raw Manhattan data, in addition to all P < 1e-3 variants [optional]")
)

opt = parse_args(OptionParser(option_list=option_list))

# Load dependencies
library(GenoUtils)
library(data.table)

# Initiate log file
log_file <- paste0(opt$output,'.log')
log_header(log_file = log_file, opt = opt, script = 'sumstat_cleaner.R', start.time = start.time)

# If testing, change CHROMS to chr value
if(!is.na(opt$test) && opt$test == 'NA'){
  opt$test<-NA
}

if(!is.na(opt$test)){
  CHROMS <- as.numeric(gsub('chr','',opt$test))
}

#####
# Read in sumstats
#####

log_add(log_file = log_file, message = 'Reading in sumstats.')

GWAS <- fread(opt$sumstats)

log_add(log_file = log_file, message = paste0('GWAS contains ',nrow(GWAS),' variants.'))

#####
# Interpret and update header
#####

# Update header and drop columns that aren't recognised
GWAS <- format_header(sumstats = GWAS, log_file = log_file)

# Insert N using opt$N if N data isn't already present
if(!(all(c('N_CAS','N_CON') %in% names(GWAS)) | 'N' %in% names(GWAS))){
  if(is.na(opt$n)){
    stop("N (sample size) information must be provided.\n")
  } else {
    GWAS$N <- opt$n
  }
}

if(any(names(GWAS) == 'CHR')){
  GWAS <- GWAS[GWAS$CHR %in% CHROMS,]
}

#####
# Format FREQ and N columns
#####

if(!('FREQ' %in% names(GWAS)) & all(c('FRQ_A','FRQ_U') %in% names(GWAS))){
  # Calculate average allele frequency across cases and controls
  GWAS$FREQ <- calc_mean_freq(sumstats = GWAS, sampling = opt$sampling, log_file = log_file)
}

# If N_CAS and N_CON are provided, but N is not, calculate total N
if(all(c('N_CAS','N_CON') %in% names(GWAS)) & !('N' %in% names(GWAS))){
  GWAS$N <- GWAS$N_CAS + GWAS$N_CON
}

# Retain only expected columns
GWAS <- GWAS[, names(GWAS) %in% c('CHR','BP','SNP','A1','A2','BETA','SE','OR','Z','FREQ','N','INFO','P'), with=F]

# Assign correct class to columns
chr_cols<-names(GWAS)[names(GWAS) %in% c('SNP','A1','A2')]
num_cols<-names(GWAS)[!(names(GWAS) %in% c('SNP','A1','A2'))]
GWAS[, (chr_cols) := lapply(.SD, as.character), .SDcols = chr_cols]
GWAS[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

#####
# Insert IUPAC codes into target
#####

# Convert A1 and A2 columns to uppercase if not already
GWAS$A1 <- toupper(GWAS$A1)
GWAS$A2 <- toupper(GWAS$A2)

# Insert IUPAC codes into target
GWAS$IUPAC<-snp_iupac(GWAS$A1, GWAS$A2)

# Retains only SNPs
log_add(log_file = log_file, message = paste0('After removal of variants that are not SNPs, ',nrow(GWAS),' variants remain.'))
log_add(log_file = log_file, message = paste0('GWAS contains ', sum(GWAS$IUPAC %in% c('S','W')), ' ambiguous variants.'))

#####
# GenoDisc: set up per-variant attrition capture
#####
# Give every input variant a stable id so that, after reference harmonisation has rewritten
# SNP/BP to reference coordinates, removed variants can still be traced back to the original
# (user-build) SNP/CHR/BP. GD_ROWID is carried through every QC step (ref_harmonise preserves
# it) and dropped again before the cleaned sumstats are written, so cleaned output is unchanged.
GWAS$GD_ROWID <- seq_len(nrow(GWAS))
gd_has_chrbp <- all(c('CHR','BP') %in% names(GWAS))

# Original P (used only for the removed-variant file and raw Manhattan data). Derived the same
# way the cleaner derives P downstream when it is absent, so significance can still be judged.
gd_P <- if('P' %in% names(GWAS)){
  GWAS$P
} else if('Z' %in% names(GWAS)){
  2 * pnorm(-abs(GWAS$Z))
} else if(all(c('BETA','SE') %in% names(GWAS))){
  2 * pnorm(-abs(GWAS$BETA / GWAS$SE))
} else if(all(c('OR','SE') %in% names(GWAS))){
  2 * pnorm(-abs(log(GWAS$OR) / GWAS$SE))
} else {
  rep(NA_real_, nrow(GWAS))
}

gd_orig <- data.table(GD_ROWID = GWAS$GD_ROWID, IUPAC = GWAS$IUPAC, P = gd_P)
for(cc in c('SNP','CHR','BP')){
  if(cc %in% names(GWAS)) gd_orig[[cc]] <- GWAS[[cc]]
}

# Accumulator of removed variants (one row per variant, tagged with the step that dropped it).
# Each variant is dropped at most once, at its first failing step.
gd_removed <- list()
gd_record <- function(ids_before, ids_after, reason){
  dropped <- ids_before[!(ids_before %in% ids_after)]
  if(length(dropped) > 0){
    gd_removed[[length(gd_removed) + 1]] <<- data.table(GD_ROWID = dropped, reason = reason)
  }
}

#####
# Harmonise per chromosome with reference
#####
# We identify variants present in GWAS and reference, insert missing data (SNP, CHR, BP), and REF.FREQ

gd_ids0 <- GWAS$GD_ROWID
GWAS <- ref_harmonise(targ = GWAS, ref_rds = opt$ref_chr, population = opt$population, log_file = log_file, chr = CHROMS)
# Variants dropped here are either not biallelic SNPs (IUPAC undefined in the input) or
# absent from the reference panel (incl. unmatched ambiguous variants); sub-classify by IUPAC.
gd_dropped <- gd_ids0[!(gd_ids0 %in% GWAS$GD_ROWID)]
if(length(gd_dropped) > 0){
  gd_iu <- gd_orig$IUPAC[match(gd_dropped, gd_orig$GD_ROWID)]
  gd_removed[[length(gd_removed) + 1]] <- data.table(
    GD_ROWID = gd_dropped,
    reason = ifelse(is.na(gd_iu), 'not_biallelic_snp', 'not_in_reference'))
}

#####
# Remove SNPs with INFO < opt$info
#####

gd_ids0 <- GWAS$GD_ROWID
GWAS <- filter_info(targ = GWAS, thresh = opt$info, log_file = log_file)
gd_record(gd_ids0, GWAS$GD_ROWID, 'info')

#####
# Remove SNPs with reported MAF < opt$maf
#####

gd_ids0 <- GWAS$GD_ROWID
GWAS <- filter_maf(targ = GWAS, thresh = opt$maf, ref = F, log_file = log_file)
gd_record(gd_ids0, GWAS$GD_ROWID, 'reported_maf')

#####
# Remove SNPs with reference MAF < opt$maf
#####

gd_ids0 <- GWAS$GD_ROWID
GWAS <- filter_maf(targ = GWAS, thresh = opt$maf, ref = T, log_file = log_file)
gd_record(gd_ids0, GWAS$GD_ROWID, 'reference_maf')

#####
# Remove SNPs with discordant MAF
#####

gd_ids0 <- GWAS$GD_ROWID
GWAS <- discord_maf(targ = GWAS, thresh = opt$maf_diff, log_file = log_file, plot_file = paste0(opt$output,'.MAF_plot.png'))
gd_record(gd_ids0, GWAS$GD_ROWID, 'maf_discordance')

#####
# Remove SNPs with out-of-bounds p-values
#####

if(!any(names(GWAS) == 'P')){
  if(any(names(GWAS) == 'Z')){
    GWAS$P <- 2 * pnorm(-abs(GWAS$Z))
    log_add(log_file = log_file, message = 'P inserted based on Z.')
  } else {
    if(!any(names(GWAS) == 'BETA')){
      GWAS$BETA <- log(GWAS$OR)
      log_add(log_file = log_file, message = "BETA inserted based on log(OR).")
    }
    GWAS$Z <- GWAS$BETA/GWAS$SE
    log_add(log_file = log_file, message = "Z inserted based on BETA and SE.")
    GWAS$P <- 2 * pnorm(-abs(GWAS$Z))
    log_add(log_file = log_file, message = 'P inserted based on Z.')
  }
}

gd_ids0 <- GWAS$GD_ROWID
GWAS<-GWAS[GWAS$P <= 1 & GWAS$P > 0,]
gd_record(gd_ids0, GWAS$GD_ROWID, 'bad_p')
log_add(log_file = log_file, message = paste0('After removal of SNPs with out-of-bound P values, ',nrow(GWAS),' variants remain.'))

#####
# Remove SNPs with duplicated rs numbers
#####

gd_ids0 <- GWAS$GD_ROWID
dups<-GWAS$SNP[duplicated(GWAS$SNP)]
GWAS<-GWAS[!(GWAS$SNP %in% dups),]
gd_record(gd_ids0, GWAS$GD_ROWID, 'duplicate_id')
log_add(log_file = log_file, message = paste0('After removal of SNPs with duplicate IDs, ',nrow(GWAS),' variants remain.'))

#####
# Remove SNPs with N < 3SD from median N
#####

gd_ids0 <- GWAS$GD_ROWID
GWAS <- filter_n(targ = GWAS, log_file = log_file)
gd_record(gd_ids0, GWAS$GD_ROWID, 'n_outlier')

#####
# Create BETA column if not present
#####

GWAS <- insert_beta(targ = GWAS, log_file = log_file)

# Remove OR and Z columns if present (GD_ROWID retained here, dropped just before writing)
GWAS<-GWAS[, names(GWAS) %in% c('CHR','BP','SNP','A1','A2','BETA','SE','FREQ','REF.FREQ','N','INFO','P','GD_ROWID'), with=F]

#####
# Check for genomic control
#####

GWAS<-avoid_gc(targ = GWAS, log_file = log_file)

#####
# Insert SE column
#####

GWAS <- insert_se(targ = GWAS, log_file = log_file)

#####
# Remove SNPs with SE == 0 or Inf
#####

gd_ids0 <- GWAS$GD_ROWID
GWAS<-GWAS[GWAS$SE != 0 & !(is.infinite(GWAS$SE)),]
gd_record(gd_ids0, GWAS$GD_ROWID, 'se_zero')
log_add(log_file = log_file, message = paste0('After removal of SNPs with SE == 0, ',nrow(GWAS),' variants remain.'))

#####
# Log summary statistics describing the cleaned sumstats
#####

chi2 <- qchisq(GWAS$P, df = 1, lower.tail = FALSE)
log_add(log_file = log_file, message = paste0('Lambda GC = ', round(median(chi2) / qchisq(0.5, 1), 3)))
log_add(log_file = log_file, message = paste0('Max chi-square = ', round(max(chi2), 3)))
log_add(log_file = log_file, message = paste0(sum(GWAS$P < 5e-8), ' variants with P < 5e-8.'))

#####
# GenoDisc: write removed-variant file and raw Manhattan data (both additive)
#####
# Assemble the removed variants captured above and attach their original (user-build)
# coordinates and P. A variant is recorded at most once (its first failing step).
gd_removed_dt <- if(length(gd_removed) > 0) data.table::rbindlist(gd_removed) else data.table(GD_ROWID = integer(0), reason = character(0))
gd_removed_full <- merge(gd_removed_dt, gd_orig, by = 'GD_ROWID', all.x = TRUE)

# (B1) Per-variant removed file in the user's build, limited to P < removed_p_thresh to stay small.
# When the input had no CHR/BP, only SNP, P and reason are written. Variants whose P could not be
# determined are omitted (they cannot be ranked by significance).
gd_cols <- intersect(c('SNP','CHR','BP','P'), names(gd_removed_full))
gd_out <- gd_removed_full[!is.na(P) & P < opt$removed_p_thresh, c(gd_cols, 'reason'), with = FALSE]
if(nrow(gd_out) > 0) gd_out <- gd_out[order(P)]
if(file.exists(paste0(opt$output,'.removed.gz'))){
  system(paste0('rm ',opt$output,'.removed.gz'))
}
fwrite(gd_out, paste0(opt$output,'.removed.gz'), sep='\t', na='NA', quote=F)
log_add(log_file = log_file, message = paste0(nrow(gd_out), ' removed variants with P < ', opt$removed_p_thresh, ' written to ', opt$output, '.removed.gz.'))

# (B3) Raw-input thinned Manhattan data (from the RAW input, not restricted to the reference),
# stored as data so the web app can plot it. Keep all P < 1e-3 plus a random subsample of the
# rest; flag removed variants and carry their reason. Only when CHR/BP are available.
if(gd_has_chrbp){
  gd_surv <- GWAS$GD_ROWID
  gd_man_cols <- c('GD_ROWID','CHR','BP','P', intersect('SNP', names(gd_orig)))
  gd_man <- gd_orig[!is.na(P), gd_man_cols, with = FALSE]
  gd_sig <- gd_man[P < 1e-3]
  gd_rest <- gd_man[P >= 1e-3]
  gd_nsub <- min(nrow(gd_rest), opt$manhattan_raw_subsample)
  if(nrow(gd_rest) > gd_nsub){
    set.seed(1)
    gd_rest <- gd_rest[sample(seq_len(nrow(gd_rest)), gd_nsub)]
  }
  gd_man <- rbind(gd_sig, gd_rest)
  gd_man$removed <- !(gd_man$GD_ROWID %in% gd_surv)
  gd_man$reason <- gd_removed_dt$reason[match(gd_man$GD_ROWID, gd_removed_dt$GD_ROWID)]
  gd_man[, GD_ROWID := NULL]
  saveRDS(gd_man, paste0(opt$output,'.manhattan_raw.rds'))
  log_add(log_file = log_file, message = paste0('Raw Manhattan data written for ', nrow(gd_man), ' variants (', sum(gd_man$removed), ' removed) to ', opt$output, '.manhattan_raw.rds.'))
} else {
  saveRDS(NULL, paste0(opt$output,'.manhattan_raw.rds'))
  log_add(log_file = log_file, message = 'Raw Manhattan data not written (CHR/BP unavailable in input).')
}

# Drop the attrition id before writing cleaned sumstats, so cleaned output is unchanged.
if('GD_ROWID' %in% names(GWAS)) GWAS[, GD_ROWID := NULL]

#####
# Write out results
#####

if(file.exists(paste0(opt$output,'.gz'))){
  system(paste0(paste0('rm ',opt$output,'.gz')))
}

fwrite(GWAS, paste0(opt$output,'.gz'), sep='\t', na='NA', quote=F)

#####
# Write out munged results
#####
# Z-score format required by LDSC and FUSION

if(isTRUE(opt$munged)){
  munged <- GWAS[, list(CHR, SNP, BP, A1, A2, Z = BETA/SE, N)]

  if(file.exists(paste0(opt$output,'.munged.sumstats.gz'))){
    system(paste0('rm ',opt$output,'.munged.sumstats.gz'))
  }

  fwrite(munged, paste0(opt$output,'.munged.sumstats.gz'), sep='\t', na='NA', quote=F)
  log_add(log_file = log_file, message = paste0('Munged sumstats written for ',nrow(munged),' variants.'))

  # When a --merge_alleles list is supplied, ALSO write a separate copy restricted
  # to that SNP list with a common allele coding (same, ref-allele flip or strand
  # flip; strand-ambiguous dropped), mirroring munge_sumstats.py --merge-alleles.
  # This copy is for LDSC (h2 / genetic correlation), which needs a shared allele
  # coding to avoid 'Incompatible alleles in .sumstats files'. The full .munged
  # file above is left intact for analyses (e.g. FUSION TWAS) that use SNPs beyond
  # the merge-alleles list.
  if(!is.na(opt$merge_alleles) && opt$merge_alleles != 'NA'){
    ma <- fread(opt$merge_alleles)
    if(!all(c('SNP','A1','A2') %in% names(ma))){
      stop('--merge_alleles file must have columns SNP, A1, A2.')
    }
    ma <- ma[, list(SNP, MA.IUPAC = snp_iupac(A1, A2))]
    merged <- merge(munged, ma, by = 'SNP')
    merged$IUPAC <- snp_iupac(merged$A1, merged$A2)
    keep <- (merged$IUPAC == merged$MA.IUPAC |
             detect_strand_flip(merged$IUPAC, merged$MA.IUPAC)) &
            !(merged$IUPAC %in% c('S','W'))
    keep[is.na(keep)] <- FALSE
    merged <- merged[keep, list(CHR, SNP, BP, A1, A2, Z, N)]

    if(file.exists(paste0(opt$output,'.munged.mergealleles.sumstats.gz'))){
      system(paste0('rm ',opt$output,'.munged.mergealleles.sumstats.gz'))
    }

    fwrite(merged, paste0(opt$output,'.munged.mergealleles.sumstats.gz'), sep='\t', na='NA', quote=F)
    log_add(log_file = log_file, message = paste0('Merge-alleles copy written for ', nrow(merged), ' of ', nrow(munged), ' variants present in ', opt$merge_alleles, ' with compatible alleles.'))
  }
}

end.time <- Sys.time()
time.taken <- end.time - start.time
sink(file = log_file, append = T)
cat0('Analysis finished at ',as.character(end.time),'\n')
cat0('Analysis duration was ',as.character(round(time.taken,2)),attr(time.taken, 'units'),'\n')
sink()
