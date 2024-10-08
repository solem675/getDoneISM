% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_logic_errors_categorical.R
\name{get_logic_errors_categorical}
\alias{get_logic_errors_categorical}
\title{Find categorical to categorical errors in a survey dataframe}
\usage{
get_logic_errors_categorical(
  ds,
  logic_frame,
  idvar = "_uuid",
  enum_col = "complementary_col",
  date_col = "complementary_col",
  enum_com_col = "complementary_col",
  index_col = "complementary_col"
)
}
\arguments{
\item{ds}{dataframe;}

\item{logic_frame}{dataframe; list of survey logic checks. The template for it is produced with \code{get_logic_check_template()}}

\item{idvar}{character; name of unique observations IDs variable}

\item{enum_col}{character; (optional) name of a variable with enumerator's/interviewer's ID}

\item{date_col}{character; (optional) name of a variable, containing survey date}

\item{enum_com_col}{character; (optional) name of a variable containing enumerator's (interviewer's comment)}

\item{index_col}{character; (optional) name of column containing index of observation (for loops in xls forms)}
}
\value{
dataframe
}
\description{
Find categorical to categorical errors in a survey dataframe
}
