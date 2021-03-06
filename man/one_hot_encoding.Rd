% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/one_hot_encoding.R
\name{one_hot_encoding}
\alias{one_hot_encoding}
\title{Encode categorical values in data.table.}
\usage{
one_hot_encoding(dt, var_list, drop = FALSE, keep = FALSE)
}
\arguments{
\item{dt}{data.table containining the data [data.table].}

\item{var_list}{Names of variables which are to be encoded [character].}

\item{drop}{Whether to omit one category per variable (to avoid multicolinearity) [logical].}
}
\value{
Modified data.table.
}
\description{
Expand each categorical value into a full set of dummies.
}
