% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sumstat_cleaner.R
\name{head_interp}
\alias{head_interp}
\title{Interpret the header of GWAS summary statistics}
\usage{
head_interp(sub_ss)
}
\arguments{
\item{sub_ss}{A data frame or matrix containing GWAS summary statistics, where the column names represent different data fields.}
}
\value{
A data frame with columns 'Original' (original column names from `sub_ss`), 'Interpreted' (standardized column names based on a predefined dictionary), 'Keep' (logical indicating whether the column is kept), 'Reason' (reason for ignoring a column, if applicable), and 'Description' (description of the interpreted column).
}
\description{
This function interprets the header of GWAS (Genome-Wide Association Studies) summary statistics. It reads the header, matches column names to a predefined dictionary, and provides a data frame indicating the original and interpreted column names, whether each column is kept or ignored, the reason for ignoring any columns, and a description of each interpreted column.
}
\examples{
header_info <- head_interp(raw_sumstats_1)
print(header_info)
}
