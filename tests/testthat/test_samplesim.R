source("global_data.R")


clean_dir <-function(dirname) {
  if (dir.exists(dirname)) {
    unlink(dirname, recursive = TRUE)
    return(TRUE)
  }
}

check_results_exist <- function(dest_dir) {
  expect_equal(file.exists(file.path(dest_dir, "medians.rds")), TRUE)
  expect_equal(file.exists(file.path(dest_dir, "widths.rds")), TRUE)
  expect_equal(file.exists(file.path(dest_dir, "intervals.rds")), TRUE)
}

run_samplesim <- function(pkg, type) {
  dest_dir_name <- paste("test", pkg, gsub(" ", "_", type), sep = "_")
  clean_dir(dest_dir_name)
  samplesim::samplesim(
    package     = pkg,
    mix         = consumer,
    source      = sources,
    discr       = discr,
    type        = type,
    nsamples    = c(10),
    modify      = "Enteromorpha",
    nrep        = 1,
    interval    = 90,
    name        = dest_dir_name,
    resid_err   = TRUE,
    process_err = FALSE,
    run         = "test",
    alpha.prior = 1,
    path        = ".",
    overwrite=T
  )
  return(dest_dir_name)
    
}

test_that("Simmr one source", {
  dest_dir <- run_samplesim("simmr", "one source")
  check_results_exist(dest_dir)
  clean_dir(dest_dir)
})


test_that("Simmr all sources", {
  dest_dir <- run_samplesim("simmr", "all sources")
  check_results_exist(dest_dir)
  clean_dir(dest_dir)
})

test_that("Simmr consumer", {
  dest_dir <- run_samplesim("simmr", "consumer")
  check_results_exist(dest_dir)
  clean_dir(dest_dir)
})


test_that("Siar one source", {
  dest_dir <- run_samplesim("Siar", "one source")
  check_results_exist(dest_dir)
  clean_dir(dest_dir)
})


test_that("Siar all sources", {
  dest_dir <- run_samplesim("Siar", "all sources")
  check_results_exist(dest_dir)
  clean_dir(dest_dir)
})

test_that("Siar consumer", {
  dest_dir <- run_samplesim("Siar", "consumer")
  check_results_exist(dest_dir)
  clean_dir(dest_dir)
})

test_that("MixSiar one source", {
  dest_dir <- run_samplesim("MixSiar", "one source")
  check_results_exist(dest_dir)
  clean_dir(dest_dir)
})


test_that("MixSiar all sources", {
  dest_dir <- run_samplesim("MixSiar", "all sources")
  check_results_exist(dest_dir)
  clean_dir(dest_dir)
})

test_that("MixSiar consumer", {
  dest_dir <- run_samplesim("MixSiar", "consumer")
  check_results_exist(dest_dir)
  clean_dir(dest_dir)
})


test_that("Package error", {
 expect_error(run_samplesim("MixSiarr", "consumer"))
})

test_that("Type error", {
  expect_error(run_samplesim("simmr", "consummer"))
})

test_that("One source no modify", {
  expect_error(samplesim::samplesim(
    package     = "simmr",
    mix         = consumer,
    source      = sources,
    discr       = discr,
    type        = "one source",
    nsamples    = c(10),
    modify      = NULL,
    nrep        = 1,
    interval    = 90,
    name        = dest_dir_name,
    resid_err   = TRUE,
    process_err = FALSE,
    run         = "test",
    alpha.prior = 1,
    path        = ".",
    overwrite=T
  ))
})

test_that("One source, modify does not exists", {
  expect_error(samplesim::samplesim(
    package     = "simmr",
    mix         = consumer,
    source      = sources,
    discr       = discr,
    type        = "one source",
    nsamples    = c(10),
    modify      = "Test source",
    nrep        = 1,
    interval    = 90,
    name        = dest_dir_name,
    resid_err   = TRUE,
    process_err = FALSE,
    run         = "test",
    alpha.prior = 1,
    path        = ".",
    overwrite=T
  ))
})



