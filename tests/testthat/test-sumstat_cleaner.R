##########
# head_interp
##########

test_that("head_interp interprets standard headers correctly", {
  # Create a mock data frame with standard column names
  mock_data <- data.table(
    SNP = c("rs1", "rs2"),
    A1 = c("A", "G"),
    A2 = c("T", "C"),
    BETA = c(0.5, -0.3),
    OR = c(1.2, 0.8),
    SE = c(0.1, 0.2),
    Z = c(3.2, -1.5),
    INFO = c(0.9, 0.8),
    P = c(0.001, 0.05),
    N = c(1000, 2000),
    CHR = c(1, 1),
    BP = c(12345, 12346)
  )

  # Run the function
  header_info <- head_interp(mock_data)

  # Test if the function correctly interpreted the header
  expected_interpreted <- c("A1","A2","BETA","BP","CHR","INFO","N","OR","P","SE","SNP","Z")
  expect_equal(header_info$Interpreted, expected_interpreted)
})

test_that("head_interp handles alternative headers correctly", {
  # Create a mock data frame with alternative column names
  mock_data <- data.table(
    RSID = c("rs1", "rs2"),
    ALLELE1 = c("A", "G"),
    ALLELE2 = c("T", "C"),
    EFFECT = c(0.5, -0.3),
    ODDS_RATIO = c(1.2, 0.8),
    STDERR = c(0.1, 0.2),
    ZSCORE = c(3.2, -1.5),
    IMPINFO = c(0.9, 0.8),
    PVAL = c(0.001, 0.05),
    WEIGHT = c(1000, 2000),
    CHROM = c(1, 1),
    POS = c(12345, 12346)
  )

  # Run the function
  header_info <- head_interp(mock_data)

  # Test if the function correctly interpreted the alternative headers
  expected_interpreted <- c("A1","A2","BETA","BP","CHR","INFO","N","OR","P","SE","SNP","Z")
  expect_equal(header_info$Interpreted, expected_interpreted)
})

test_that("head_interp identifies and excludes unrecognized headers", {
  # Create a mock data frame with some unrecognized column names
  mock_data <- data.table(
    RSID = c("rs1", "rs2"),
    ALLELE1 = c("A", "G"),
    UNKNOWN_COL = c("X", "Y"),
    ODDS_RATIO = c(1.2, 0.8),
    PVAL = c(0.001, 0.05)
  )

  # Run the function
  header_info <- head_interp(mock_data)

  # Test if the function correctly identifies and excludes unrecognized headers
  expect_false("UNKNOWN_COL" %in% header_info$Interpreted)
  expect_true(any(header_info$Original == "UNKNOWN_COL" & header_info$Reason == "Not recognised"))
})

############
# format_header
############

test_that("format_header updates header and validates required columns", {
  # Create a mock data frame with standard column names
  mock_data <- data.table(
    SNP = c("rs1", "rs2"),
    A1 = c("A", "G"),
    A2 = c("T", "C"),
    BETA = c(0.5, -0.3),
    P = c(0.001, 0.05),
    N = c(1000, 2000)
  )

  # Run the function
  formatted_data <- format_header(mock_data)

  # Test if the function correctly updated the header
  expected_colnames <- c("SNP", "A1", "A2", "BETA", "P", "N")
  expect_equal(names(formatted_data), expected_colnames)
})

test_that("format_header handles missing required columns", {
  # Create a mock data frame missing required columns
  mock_data <- data.table(
    SNP = c("rs1", "rs2"),
    A1 = c("A", "G")
    # Missing A2, BETA, P, and N columns
  )

  # Expect an error when required columns are missing
  expect_error(format_header(mock_data))
})

test_that("format_header logs messages to console or file", {
  # Create a mock data frame that will trigger a warning message
  mock_data <- data.table(
    SNP = c("rs1", "rs2"),
    A1 = c("A", "G"),
    A2 = c("T", "C"),
    BETA = c(0.5, -0.3),
    P = c(0.001, 0.05),
    N = c(1000, 2000),
    FRQ_A = c(0.1,0.2)
  )

  # Run the function with logging to console
  expect_output(print(format_header(mock_data)), "Warning:")

  # Run the function with logging to a file
  log_file <- tempfile()
  expect_silent(format_header(mock_data, log_file = log_file))
  log_content <- readLines(log_file)
  expect_true(any(grepl("Warning:", log_content)))
})

test_that("format_header handles alternative headers correctly", {
  # Create mock data with alternative column names that are valid in the dictionary
  mock_data <- data.table(
    RSID = c("rs1", "rs2"),
    ALLELE1 = c("A", "G"),
    ALLELE2 = c("T", "C"),
    EFFECT = c(0.5, -0.3),
    PVAL = c(0.001, 0.05),
    N = c(1000, 2000)
  )

  # Run the function
  formatted_data <- format_header(mock_data)

  # Test if the function correctly interpreted and updated the alternative headers
  expected_colnames <- c("SNP", "A1", "A2", "BETA", "P", "N")
  expect_equal(names(formatted_data), expected_colnames)
})

test_that("format_header validates related optional columns correctly", {
  # Create mock data where related optional columns are present/absent
  # Case 1: Both N_CAS and N_CON are present
  mock_data_1 <- data.table(
    SNP = c("rs1", "rs2"),
    A1 = c("A", "G"),
    A2 = c("T", "C"),
    BETA = c(0.5, -0.3),
    P = c(0.001, 0.05),
    N = c(1000, 2000),
    N_CAS = c(500, 1000),
    N_CON = c(500, 1000)
  )

  # Case 2: Only one of N_CAS or N_CON is present
  mock_data_2 <- data.table(
    SNP = c("rs1", "rs2"),
    A1 = c("A", "G"),
    A2 = c("T", "C"),
    BETA = c(0.5, -0.3),
    P = c(0.001, 0.05),
    N_CAS = c(500, 1000)
    # Missing N_CON
  )

  # Run the function and test
  formatted_data_1 <- format_header(mock_data_1)
  expect_true(all(c("N_CAS", "N_CON") %in% names(formatted_data_1)))

  expect_error(format_header(mock_data_2))
})

test_that("format_header functions correctly with no errors or warnings", {
  # Create mock data with no errors or warnings expected
  mock_data <- data.table(
    SNP = c("rs1", "rs2"),
    A1 = c("A", "G"),
    A2 = c("T", "C"),
    BETA = c(0.5, -0.3),
    P = c(0.001, 0.05),
    N = c(1000, 2000),
    CHR = c(1, 1),
    BP = c(12345, 12346)
  )

  # Run the function
  formatted_data <- format_header(mock_data)

  # Test if the function runs without errors or warnings and correctly updates the header
  expected_colnames <- c("SNP", "A1", "A2", "BETA", "P", "N", "CHR", "BP")
  expect_equal(names(formatted_data), expected_colnames)
})

#########
# sumstat_cleaner.R
#########

test_that("run sumstat_cleaner.R script with raw_sumstats_1", {
  # Specify location of relevent files
  rscript_path <- file.path(Sys.getenv("R_HOME"), "bin", "Rscript")
  script_path <- system.file("scripts", "sumstat_cleaner.R", package = "GenoUtils")
  ref_path <- gsub( '22.rds','', system.file("extdata", "ref.chr22.rds", package = "GenoUtils"))
  tmp_dir<-tempdir()

  # Write test GWAS sumstats as temporary file on disk
  fwrite(raw_sumstats_1, paste0(tmp_dir, '/raw.txt'), sep=' ', na='NA', row.names=F)

  # Run sumstat_cleaner.R with test data
  system(paste0(rscript_path, " ", script_path ," --sumstats ",tmp_dir, "/raw.txt --ref_chr ",ref_path," --population EUR --output ", tmp_dir,"/clean"))

  # Read in cleaned sumstats
  cleaned<-fread(paste0(tmp_dir,"/clean.gz"))

  # Read in example output
  sumstat_cleaner_output_1<-readRDS(system.file("extdata", "sumstat_cleaner_output_1.rds", package = "GenoUtils"))

  # Order columns
  cleaned<-cleaned[, names(sumstat_cleaner_output_1$sumstats), with=F]

  # Test if the function runs without errors or warnings and correctly updates the header
  expect_equal(sumstat_cleaner_output_1$sumstats, cleaned)
})

test_that("run sumstat_cleaner.R script with raw_sumstats_2", {
  # Specify location of relevant files
  rscript_path <- file.path(Sys.getenv("R_HOME"), "bin", "Rscript")
  script_path <- system.file("scripts", "sumstat_cleaner.R", package = "GenoUtils")
  ref_path <- gsub( '22.rds','', system.file("extdata", "ref.chr22.rds", package = "GenoUtils"))
  tmp_dir<-tempdir()

  # Write test GWAS sumstats as temporary file on disk
  fwrite(raw_sumstats_2, paste0(tmp_dir, '/raw.txt'), sep=' ', na='NA', row.names=F)

  # Run sumstat_cleaner.R with test data
  system(paste0(rscript_path, " ", script_path ," --sumstats ",tmp_dir, "/raw.txt --ref_chr ",ref_path," --population EUR --output ", tmp_dir,"/clean"))

  # Read in cleaned sumstats
  cleaned<-fread(paste0(tmp_dir,"/clean.gz"))

  # Read in example output
  sumstat_cleaner_output_2<-readRDS(system.file("extdata", "sumstat_cleaner_output_2.rds", package = "GenoUtils"))

  # Test if the function runs without errors or warnings and correctly updates the header
  expect_equal(sumstat_cleaner_output_2$sumstats, cleaned)
})

test_that("run sumstat_cleaner.R script with raw_sumstats_2 with allele codes in lower case", {
  # Specify location of relevant files
  rscript_path <- file.path(Sys.getenv("R_HOME"), "bin", "Rscript")
  script_path <- system.file("scripts", "sumstat_cleaner.R", package = "GenoUtils")
  ref_path <- gsub( '22.rds','', system.file("extdata", "ref.chr22.rds", package = "GenoUtils"))
  tmp_dir<-tempdir()

  # Convert A1 and A2 to lower case
  raw_sumstats_2$A1<-tolower(raw_sumstats_2$A1)
  raw_sumstats_2$A2<-tolower(raw_sumstats_2$A2)

  # Write test GWAS sumstats as temporary file on disk
  fwrite(raw_sumstats_2, paste0(tmp_dir, '/raw.txt'), sep=' ', na='NA', row.names=F)

  # Run sumstat_cleaner.R with test data
  system(paste0(rscript_path, " ", script_path ," --sumstats ",tmp_dir, "/raw.txt --ref_chr ",ref_path," --population EUR --output ", tmp_dir,"/clean"))

  # Read in cleaned sumstats
  cleaned<-fread(paste0(tmp_dir,"/clean.gz"))

  # Read in example output
  sumstat_cleaner_output_2<-readRDS(system.file("extdata", "sumstat_cleaner_output_2.rds", package = "GenoUtils"))

  # Test if the function runs without errors or warnings and correctly updates the header
  expect_equal(sumstat_cleaner_output_2$sumstats, cleaned)
})

test_that("run sumstat_cleaner.R script with raw_sumstats_2 when N is not specified", {
  # Specify location of relevant files
  rscript_path <- file.path(Sys.getenv("R_HOME"), "bin", "Rscript")
  script_path <- system.file("scripts", "sumstat_cleaner.R", package = "GenoUtils")
  ref_path <- gsub( '22.rds','', system.file("extdata", "ref.chr22.rds", package = "GenoUtils"))
  tmp_dir<-tempdir()

  # Remove N column
  raw_sumstats_2$N<-NULL

  # Write test GWAS sumstats as temporary file on disk
  fwrite(raw_sumstats_2, paste0(tmp_dir, '/raw.txt'), sep=' ', na='NA', row.names=F)

  # Run sumstat_cleaner.R with test data
  status <- system(paste0(rscript_path, " ", script_path ," --sumstats ",tmp_dir, "/raw.txt --ref_chr ",ref_path," --population EUR --output ", tmp_dir,"/clean"), ignore.stdout = TRUE, ignore.stderr = TRUE)

  # Expect that the status is not 0, indicating an error
  expect_true(status != 0, info = "The command should fail and return a non-zero exit status.")
})

test_that("run sumstat_cleaner.R script with raw_sumstats_2 when N is provided as parameter", {
  # Specify location of relevant files
  rscript_path <- file.path(Sys.getenv("R_HOME"), "bin", "Rscript")
  script_path <- system.file("scripts", "sumstat_cleaner.R", package = "GenoUtils")
  ref_path <- gsub( '22.rds','', system.file("extdata", "ref.chr22.rds", package = "GenoUtils"))
  tmp_dir<-tempdir()

  # Remove N column
  raw_sumstats_2$N<-NULL

  # Write test GWAS sumstats as temporary file on disk
  fwrite(raw_sumstats_2, paste0(tmp_dir, '/raw.txt'), sep=' ', na='NA', row.names=F)

  # Run sumstat_cleaner.R with test data
  system(paste0(rscript_path, " ", script_path ," --sumstats ",tmp_dir, "/raw.txt --ref_chr ",ref_path," --population EUR --n 10000 --output ", tmp_dir,"/clean"))

  # Read in cleaned sumstats
  cleaned<-fread(paste0(tmp_dir,"/clean.gz"))

  # Read in example output
  sumstat_cleaner_output_2<-readRDS(system.file("extdata", "sumstat_cleaner_output_2.rds", package = "GenoUtils"))
  sumstat_cleaner_output_2$sumstats$N<-10000

  # Test if the function runs without errors or warnings and correctly updates the header
  expect_equal(sumstat_cleaner_output_2$sumstats, cleaned)
})

test_that("run sumstat_cleaner.R script with raw_sumstats_2 and test restricted to chr22", {
  # Specify location of relevant files
  rscript_path <- file.path(Sys.getenv("R_HOME"), "bin", "Rscript")
  script_path <- system.file("scripts", "sumstat_cleaner.R", package = "GenoUtils")
  ref_path <- gsub( '22.rds','', system.file("extdata", "ref.chr22.rds", package = "GenoUtils"))
  tmp_dir<-tempdir()

  # Write test GWAS sumstats as temporary file on disk
  fwrite(raw_sumstats_2, paste0(tmp_dir, '/raw.txt'), sep=' ', na='NA', row.names=F)

  # Run sumstat_cleaner.R with test data
  system(paste0(rscript_path, " ", script_path ," --sumstats ",tmp_dir, "/raw.txt --ref_chr ",ref_path," --population EUR --output ", tmp_dir,"/clean --test chr22"))

  # Read in cleaned sumstats
  cleaned<-fread(paste0(tmp_dir,"/clean.gz"))

  # Read in example output
  sumstat_cleaner_output_3<-readRDS(system.file("extdata", "sumstat_cleaner_output_3.rds", package = "GenoUtils"))

  # Test if the function runs without errors or warnings and correctly updates the header
  expect_equal(sumstat_cleaner_output_3, cleaned)
})

test_that("run sumstat_cleaner.R script with raw_sumstats_1 and but with '#' in column names", {
  # Specify location of relevant files
  rscript_path <- file.path(Sys.getenv("R_HOME"), "bin", "Rscript")
  script_path <- system.file("scripts", "sumstat_cleaner.R", package = "GenoUtils")
  ref_path <- gsub( '22.rds','', system.file("extdata", "ref.chr22.rds", package = "GenoUtils"))
  tmp_dir<-tempdir()

  tmp <- raw_sumstats_1
  tmp <- tmp[, c("CHR", "SNP", "ORIGBP", "A1", "A2", "FREQ", "P", "BETA", "SE", "N"), with = F]
  names(tmp)[1] <- "#CHR"

  # Write test GWAS sumstats as temporary file on disk
  fwrite(tmp, paste0(tmp_dir, '/raw.txt'), sep=' ', na='NA', row.names=F)

  # Run sumstat_cleaner.R with test data
  system(paste0(rscript_path, " ", script_path ," --sumstats ",tmp_dir, "/raw.txt --ref_chr ",ref_path," --population EUR --output ", tmp_dir,"/clean"))

  # Read in cleaned sumstats
  cleaned<-fread(paste0(tmp_dir,"/clean.gz"))

  # Read in example output
  sumstat_cleaner_output_1<-readRDS(system.file("extdata", "sumstat_cleaner_output_1.rds", package = "GenoUtils"))

  # Order columns
  cleaned<-cleaned[, names(sumstat_cleaner_output_1$sumstats), with=F]

  # Test if the function runs without errors or warnings and correctly updates the header
  expect_equal(sumstat_cleaner_output_1$sumstats, cleaned)
})

test_that("run sumstat_cleaner.R script with raw_sumstats_1 and but with CHR:BP:A1:A2 information in SNP column", {
  # Specify location of relevant files
  rscript_path <- file.path(Sys.getenv("R_HOME"), "bin", "Rscript")
  script_path <- system.file("scripts", "sumstat_cleaner.R", package = "GenoUtils")
  ref_path <- gsub( '22.rds','', system.file("extdata", "ref.chr22.rds", package = "GenoUtils"))
  tmp_dir<-tempdir()

  tmp <- raw_sumstats_1
  tmp$SNP <- paste(tmp$`CHR`, tmp$ORIGBP, tmp$A1, tmp$A2, sep = ':')
  tmp$CHR <- NULL
  tmp$ORIGBP <- NULL

  # Write test GWAS sumstats as temporary file on disk
  fwrite(tmp, paste0(tmp_dir, '/raw.txt'), sep=' ', na='NA', row.names=F)

  # Run sumstat_cleaner.R with test data
  system(paste0(rscript_path, " ", script_path ," --sumstats ",tmp_dir, "/raw.txt --ref_chr ",ref_path," --population EUR --output ", tmp_dir,"/clean"))

  # Read in cleaned sumstats
  cleaned<-fread(paste0(tmp_dir,"/clean.gz"))

  # Read in example output
  sumstat_cleaner_output_1<-readRDS(system.file("extdata", "sumstat_cleaner_output_1.rds", package = "GenoUtils"))

  # Order columns
  cleaned<-cleaned[, names(sumstat_cleaner_output_1$sumstats), with=F]

  # Test if the function runs without errors or warnings and correctly updates the header
  expect_equal(sumstat_cleaner_output_1$sumstats, cleaned)
})
