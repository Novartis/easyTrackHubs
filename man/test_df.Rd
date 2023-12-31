% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_example_data.R
\docType{data}
\name{test_df}
\alias{test_df}
\title{Example data for input to easyTrackHubs}
\format{
A data frame with 21 rows (samples) and 6 variables:
sample_name: Sample identifier from ENCODE.
file_format: Specification of the file format (bigWig).
cell_type: Cell type used in the assay.
reference_genome: Genome assembly in UCSC format.
data_type: Name of the assay (ChIP-seq, CUT&RUN, etc).
}
\usage{
data(test_df)
}
\description{
An example dataset used as input to easyTrackHubs.
Example data, also contained int he package, was downloaded
from ENCODE and subset in order to keep the files small.
}
\keyword{datasets}
