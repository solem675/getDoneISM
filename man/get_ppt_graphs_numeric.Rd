% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_ppt_graphs_numeric.R
\name{get_ppt_graphs_numeric}
\alias{get_ppt_graphs_numeric}
\title{Print pseudo-boxplots for numeric variables to a Power Point document}
\usage{
get_ppt_graphs_numeric(
  num_tabs,
  ppt_file,
  ppt_layout,
  ppt_theme,
  indicator_col = "indicator",
  question_label = "indicator",
  stat = "stat",
  unit_col = "unit",
  value_col = "value",
  exclude_stats = c("observations count", "mean")
)
}
\arguments{
\item{num_tabs}{dataframe; set of summary statistics for numeric variables in a long format, with variable labels added (can be generated with \code{get_numerics_summaries()}, "wide" parameter is set to FALSE and followed by \code{get_numerics_formatted()})}

\item{ppt_file}{character; name of the target ppt file (with an extension)}

\item{ppt_layout}{character; name of a layout in a ppt file (suggested option - title and content)}

\item{ppt_theme}{character; name of a theme in a ppt file}

\item{indicator_col}{character; name of a variable, containing indicators}

\item{question_label}{character; name of a variable, containing questions' labels}

\item{stat}{character; name of a variable, containing an indication of statistics}

\item{unit_col}{character; name of a variable, containing a unit of disaggregation (independent variable)}

\item{value_col}{character; name of a variable, containing numeric values of proportions}

\item{exclude_stats}{character; vector of statistics' names that should be excluded from plotting}
}
\value{
prints to the ppt file as a side effect
}
\description{
Print pseudo-boxplots for numeric variables to a Power Point document
}
