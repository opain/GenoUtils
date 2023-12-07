\name{clean_sumstats_1}
\alias{clean_sumstats_1}
\title{Cleaned GWAS Summary Statistics for COAD01}
\description{
  A dataset containing cleaned Genome-Wide Association Study (GWAS) summary statistics for the COAD01 study.
  The data is filtered to include only the first 100 rows for each chromosome from 1 to 22.
}
\usage{
data(clean_sumstats_1)
}
\format{
  A data frame with 2200 observations on the following variables:
  \describe{
    \item{\code{SNP}}{SNP ID.}
    \item{\code{CHR}}{Chromosome number.}
    \item{\code{FREQ}}{Allele frequency reported in GWAS.}
    \item{\code{P}}{Statistical significance of association.}
    \item{\code{BETA}}{BETA effect size.}
    \item{\code{SE}}{Standard error of BETA.}
    \item{\code{N}}{Total sample size.}
    \item{\code{REF.FREQ}}{Allele frequency in European 1KG Phase 3.}
    \item{\code{A1}}{Allele 1, Effect allele.}
    \item{\code{A2}}{Allele 2.}
    \item{\code{BP}}{Basepair position.}
  }
}
\details{
  This dataset was created by reading in GWAS summary statistics, filtering by chromosome,
  and then selecting the first 100 rows for each chromosome.
  It includes data for chromosomes 1 to 22.
}
\source{
  GWAS summary statistics from the COAD01 study.
}
\examples{
data(clean_sumstats_1)
head(clean_sumstats_1)
}
