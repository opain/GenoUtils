\name{raw_sumstats_1}
\alias{raw_sumstats_1}
\title{Raw GWAS Summary Statistics for COAD01}
\description{
  A dataset containing raw Genome-Wide Association Study (GWAS) summary statistics for the COAD01 study.
  The dataset is a subset of the original GWAS summary statistics, including the first 100 rows for each
  chromosome (1 to 22). This subset was selected to provide a representative sample of the larger dataset.
}
\usage{
data(raw_sumstats_1)
}
\format{
  A data frame with a variable number of rows (up to 2200) and columns representing different GWAS summary statistics.
  The exact structure depends on the original COAD01 dataset, but typically includes columns like SNP identifiers,
  chromosome numbers, base pair positions, allele information, association statistics, and p-values.
}
\details{
  This dataset is particularly useful for testing and demonstrating analysis methods, as it provides a manageable
  subset of a typical GWAS dataset while still covering all chromosomes. Users should note that this is a processed
  subset of the original data and should not be used for definitive scientific conclusions without considering its
  limited scope.
}
\source{
  GWAS summary statistics from the COAD01 study, subsetted and processed for demonstration purposes.
}
\examples{
data(raw_sumstats_1)
head(raw_sumstats_1)
}
