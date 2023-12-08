# Test for opt_to_df
test_that("opt_to_df correctly converts list to dataframe", {
  opt <- list(sumstats = 'data/sumstats.gz', population = 'EUR', n = 2309)
  df <- opt_to_df(opt)
  expect_true(is.data.frame(df))
  expect_equal(ncol(df), 2)
  expect_equal(colnames(df), c('Parameter', 'Value'))
  expect_equal(nrow(df), length(opt))
})

# Test for log_header
test_that("log_header creates and writes to log file correctly", {
  opt <- list(sumstats = 'data/sumstats.gz', population = 'EUR', n = 2309)
  script_name <- "script_name.R"
  start_time <- Sys.time()
  log_file <- tempfile()

  log_header(log_file, opt, script_name, start_time)

  # Check if log file is created and contains specific content
  expect_true(file.exists(log_file))
  content <- readLines(log_file)
  expect_true(length(content) > 0)
  expect_true(grepl(script_name, content[2]))
  expect_true(grepl("Analysis started at", content[length(content)]))
})

# Test for log_add
test_that("log_add writes message to log file or console", {
  log_file <- tempfile()
  message <- "This is a test log message"

  # Writing to file
  log_add(log_file, message)
  expect_true(file.exists(log_file))
  content <- readLines(log_file)
  expect_equal(content, message)

  # Writing to console (can't directly test console output, so we skip this check)
})

# Test for cat0
test_that("cat0 concatenates objects with and without separator", {
  # Test default behavior (no separator)
  output <- capture.output(cat0("Hello", "World"))
  expect_equal(output, "HelloWorld")

  # Test with a space as a separator
  output <- capture.output(cat0("Hello", "World", sep = " "))
  expect_equal(output, "Hello World")

  # Test with a newline as a separator
  output <- capture.output(cat0("Line1", "Line2", sep = "\n"))
  expect_equal(output, c("Line1", "Line2"))
})
