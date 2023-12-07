\name{iupac_codes}
\alias{iupac_codes}
\title{Simulated IUPAC Codes for SNPs with Strand Flips}
\description{
  A dataset containing two sets of simulated IUPAC codes representing SNPs.
  The 'target' vector contains the original SNPs, and the 'reference' vector contains
  SNPs after simulating strand flips for approximately 20% of the entries.
}
\usage{
  data(iupac_codes)
}
\format{
  A data frame with 100 rows and 2 variables:
    \describe{
      \item{\code{target}}{Character vector of original IUPAC codes (R, Y, K, M) for SNPs.}
      \item{\code{reference}}{Character vector of IUPAC codes after simulating strand flips for some SNPs.}
    }
}
\details{
  Strand flips were simulated by replacing some of the IUPAC codes in the 'target' vector with their
  complementary pairs (R ↔ Y, K ↔ M) to create the 'reference' vector. Approximately 20% of the SNPs
  were subject to strand flips.
}
\source{
  Simulated data.
}
\examples{
  data(iupac_codes)
  head(iupac_codes)
}
