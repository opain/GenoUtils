\name{raw_sumstats_2}
\alias{raw_sumstats_2}
\title{Raw GWAS Summary Statistics for BODY04}
\description{
  A dataset containing raw Genome-Wide Association Study (GWAS) summary statistics for the BODY04 study.
  The dataset is a subset of the original GWAS summary statistics, including rows matching SNP IDs in the example reference rds files. This subset was selected to provide a representative sample of the larger dataset.
}
\usage{
data(raw_sumstats_2)
}
\format{
  A data frame with a variable number of rows (up to 2200) and columns representing different GWAS summary statistics.
  The exact structure depends on the original BODY04 dataset, but typically includes columns like SNP identifiers,
  chromosome numbers, base pair positions, allele information, association statistics, and p-values.
}
\details{
  This dataset is particularly useful for testing and demonstrating analysis methods, as it provides a manageable
  subset of a typical GWAS dataset while still covering all chromosomes. Users should note that this is a processed
  subset of the original data and should not be used for definitive scientific conclusions without considering its
  limited scope.
}
\source{
  GWAS summary statistics from the BODY04 study, subsetted and processed for demonstration purposes.
}
\examples{
data(raw_sumstats_2)
head(raw_sumstats_2)
}
