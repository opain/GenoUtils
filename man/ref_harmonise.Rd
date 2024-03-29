% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sumstat_cleaner.R
\name{ref_harmonise}
\alias{ref_harmonise}
\title{Harmonize Target Data with Reference Data}
\usage{
ref_harmonise(targ, ref_rds, population, log_file = NULL, chr = 1:22)
}
\arguments{
\item{targ}{A data frame of target SNP data to be harmonized.}

\item{ref_rds}{The path to reference data files in RDS format.}

\item{population}{The reference population matching the GWAS sample. This option is used to determine ancestry-matched reference allele frequencies.}

\item{log_file}{An optional path to a log file where messages will be recorded.}

\item{chr}{An optional numeric vector indicating chromosomes to be processed.}
}
\value{
A harmonized data frame of target SNP data.
}
\description{
This function harmonizes target SNP data with reference SNP data by alleles, and either chromosome and base pair, or SNP ID. It accounts for strand flips between the reference and target.
}
\examples{
# Get path and prefix to example ref_rds data
reference_data_path <- gsub( '22.rds','',
  system.file("extdata", "ref.chr22.rds", package = "GenoUtils"))
harmonised_data <- ref_harmonise(clean_sumstats_1, reference_data_path, 'EUR')
print(head(harmonised_data))
}
