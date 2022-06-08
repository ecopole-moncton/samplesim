## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse   = TRUE,
  comment    = "#>",
  fig.width  = 7,
  fig.height = 7,
  out.width  = "100%",
  dpi        = 300
)

## ----'setup', echo = FALSE----------------------------------------------------
library("samplesim")
library("ggplot2")

## ---- eval = TRUE, echo = TRUE------------------------------------------------
## Get the location of the dataset ----
file_path <- system.file("extdata", "foxes_consumer.csv", package = "samplesim")

## Import isotopic plasma values of the consumers ----
consumer <- MixSIAR::load_mix_data(filename     = file_path,
                                   iso_names    = c("d13C", "d15N"),
                                   factors      = NULL,
                                   fac_random   = NULL,
                                   fac_nested   = NULL,
                                   cont_effects = NULL)

## ---- eval = TRUE, echo = TRUE------------------------------------------------
## Get the class of the object ----
class(consumer)

## Name of the list elements ----
names(consumer)

## Print the ten first rows of the isotopic values ----
head(consumer$"data_iso", 10)

## Get the name of the isotopes ----
consumer$"iso_names"

## How many consumers are there in the dataset? ----
consumer$"N"

## ---- eval = TRUE, echo = TRUE------------------------------------------------
## Get the location of the dataset ----
file_path <- system.file("extdata", "foxes_sources.csv", package = "samplesim")

## Import mean isotopic plasma values of the sources (Format #1) ----
sources <- MixSIAR::load_source_data(filename       = file_path,
                                     source_factors = NULL,
                                     conc_dep       = FALSE,
                                     data_type      = "means",
                                     mix            = consumer)

## ---- eval = TRUE, echo = TRUE------------------------------------------------
## Get the class of the object ----
class(sources)

## Name of the list elements ----
names(sources)

## Print the mean values of isotopic plasma values ----
sources$"S_MU"

## Print the SD values of isotopic plasma values ----
sources$"S_SIG"

## Get the name of the sources ----
sources$"source_names"

## How many sources are there in the dataset? ----
sources$"n_array"

## ---- eval = TRUE, echo = TRUE------------------------------------------------
## Get the location of the dataset ----
file_path <- system.file("extdata", "foxes_discrimination.csv", package = "samplesim")

## Import TDF values ----
discr <- MixSIAR::load_discr_data(filename = file_path, mix = consumer)

## ---- eval = TRUE, echo = TRUE------------------------------------------------
## Get the class of the object ----
class(discr)

## Name of the list elements ----
names(discr)

## Print the mean values of TDF ----
discr$"mu"

## Print the SD values of TDF ----
discr$"sig2"

## ---- echo = TRUE, eval = TRUE, fig.width = 5, fig.height = 5-----------------
samplesim::plot_isospace(mix = consumer, source = sources, discr = discr)

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  samplesim::samplesim(package     = "siar",
#                       mix         = consumer,
#                       source      = sources,
#                       discr       = discr,
#                       type        = NULL,
#                       nsamples    = NULL,
#                       modify      = NULL,
#                       nrep        = 100,
#                       interval    = 90,
#                       name        = NULL,
#                       resid_err   = TRUE,
#                       process_err = FALSE,
#                       run         = "test",
#                       alpha.prior = 1,
#                       path        = ".")

## ---- eval = FALSE, echo = TRUE, message = FALSE------------------------------
#  ## samplesim run for one source ----
#  samplesim::samplesim(package     = "siar",
#                       mix         = consumer,
#                       source      = sources,
#                       discr       = discr,
#                       type        = "one source",
#                       nsamples    = c(2:10, 15, 25, 50, 75, 100, 150, 250, 500),
#                       modify      = "Voles",
#                       nrep        = 999,
#                       interval    = 90,
#                       name        = "test_siar",
#                       resid_err   = TRUE,
#                       process_err = FALSE,
#                       run         = "test",
#                       alpha.prior = 1,
#                       path        = ".")

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  ## Objects created by samplesim ----
#  list.files("./test_siar")

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
list.files("docs/test_siar")

## ---- echo = FALSE, eval = TRUE-----------------------------------------------
cat(paste0(readLines("docs/test_siar/logfile.txt"), collapse = "\n"))

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  ## Import medians of credible intervals ----
#  medians <- readRDS("./test_siar/medians.rds")

## ---- echo = FALSE, eval = TRUE-----------------------------------------------
# Import medians of credible intervals
medians <- readRDS("docs/test_siar/medians.rds")

## ---- echo = TRUE, eval = TRUE------------------------------------------------
## Structure of the object ----
class(medians)

## Names of the dimensions ----
names(dimnames(medians))

## Names of the content of the second dimension ----
dimnames(medians)[[2]]

## Names of the content of the third dimension ----
dimnames(medians)[[3]]

## ---- echo = TRUE, eval = TRUE------------------------------------------------
## Extract results of the first replicate ----
medians[1, , ]

## Compute mean over replicates ----
apply(medians, 3:2, mean)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  ## Import medians and widths of credible intervals ----
#  datas <- samplesim::get_output("test_siar")

## ---- echo = FALSE, eval = TRUE-----------------------------------------------
datas <- samplesim::get_output("test_siar", path = "docs")

## ---- echo = TRUE, eval = TRUE------------------------------------------------
## Structure of the data frame ----
str(datas)

## Print the first ten and last ten rows ----
rbind(head(datas, 10), tail(datas, 10))

## ---- echo = TRUE, eval = TRUE------------------------------------------------
## Extract widths of credible intervals ----
widths <- datas[datas$type == "Width of credible intervals", ]

## Check ----
rbind(head(widths, 10), tail(widths, 10))

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  ## Import medians and widths of credible intervals expressed as percentage of change ----
#  datas <- samplesim::get_output(name = "test_siar", change = TRUE, reference = 2)

## ---- echo = FALSE, eval = TRUE-----------------------------------------------
datas <- samplesim::get_output(name = "test_siar", change = TRUE, reference = 2, path = "docs")

## ---- echo = TRUE, eval = TRUE------------------------------------------------
## Structure of the data frame ----
str(datas)

## Print the first ten and last ten rows ----
rbind(head(datas, 10), tail(datas, 10))

## ---- echo = TRUE, eval = FALSE, fig.width = 8.5, fig.height = 5.5------------
#  ## Visualize results ----
#  samplesim::plot_samplesim(name = "test_siar")

## ---- echo = FALSE, eval = TRUE, fig.width = 8.5, fig.height = 5.5------------
# Visualize results
samplesim::plot_samplesim(name = "test_siar", path = "docs")

## ---- echo = TRUE, eval = FALSE, fig.width = 8.5, fig.height = 5.5------------
#  # Visualize results expressed as percentages of change
#  samplesim::plot_samplesim(name = "test_siar", change = TRUE, reference = 2)

## ---- echo = FALSE, eval = TRUE, fig.width = 8.5, fig.height = 5.5------------
samplesim::plot_samplesim(name = "test_siar", change = TRUE, reference = 2, path = "docs")

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  ## Import medians and widths of credible intervals ----
#  datas <- get_output("test_siar")

## ---- echo = FALSE, eval = TRUE-----------------------------------------------
datas <- get_output("test_siar", path = "docs")

## ---- echo = TRUE, eval = TRUE, fig.width = 8.5, fig.height = 5.5-------------
ggplot(aes(x = size, y = value), data = datas) +
  geom_boxplot(aes(fill = source), width = 0.8, outlier.shape = NA) +
  labs(x = "Sample size", y = "Values", fill = "Sources") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  facet_grid(. ~ type)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  ## Import medians and widths of credible intervals ----
#  datas <- samplesim::get_output(name = "test_siar", change = TRUE)

## ---- echo = FALSE, eval = TRUE-----------------------------------------------
datas <- samplesim::get_output(name = "test_siar", change = TRUE, path = "docs")

## ---- echo = TRUE, eval = TRUE, fig.width = 8.5, fig.height = 5.5-------------
## Select only medians of credible intervals ----
medians <- datas[datas$"type" == "Median of posterior distribution", ]

ggplot(aes(x = size, y = value, group = source), data = medians) +
  geom_point(aes(color = source)) +
  geom_line(aes(color = source)) +
  labs(x = "Sample size", y = "Change in medians (%)", color = "Sources") +
  theme_light() +
  theme(legend.position = "bottom", legend.title = element_blank())

