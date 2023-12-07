set.seed(1) # For reproducibility

# Define the unambiguous IUPAC codes
iupac_codes <- c("R", "Y", "K", "M")

# Generate a random vector of 100 IUPAC codes
snps <- sample(iupac_codes, 100, replace = TRUE)

# Define the strand flips (excluding S and W)
strand_flips <- list(R = "Y", Y = "R", K = "M", M = "K")

# Apply strand flips to approximately 20% of the SNPs
flipped_snps <- sapply(snps, function(code) {
  if (runif(1) < 0.2) { # 20% chance of a flip
    return(strand_flips[[code]])
  } else {
    return(code)
  }
})

iupac_codes<-data.frame(target=snps, reference=flipped_snps)

usethis::use_data(iupac_codes, overwrite = TRUE)
