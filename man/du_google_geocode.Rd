% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/google_api.R
\name{du_google_geocode}
\alias{du_google_geocode}
\title{get geocode with error handling built in,reasons are given for failed items}
\usage{
du_google_geocode(
  address,
  apikey,
  result_type = c("list", "df", "raw"),
  multi_coord_handling = c("none", "first", "average"),
  progress = TRUE,
  show_summary = TRUE
)
}
\arguments{
\item{address}{a character vector that contains address(es)}

\item{apikey}{google's apikey for using geocode service}

\item{result_type}{how should the results be returned, see more in details}

\item{multi_coord_handling}{how to handle results with multiple matching coordinates}

\item{progress}{whether to show a progress bar}

\item{show_summary}{whether to show a summary on job status}
}
\value{
a list of two elements \code{result} and \code{error}, return details
are dependent on \code{result_type} argument.
}
\description{
getting geocode can be tedious sometimes since sometimes we get
errors, sometimes we multiple matched results. And doing the error/exception
handling has always been a headache. This functions tries do the error handling
for your api call won't be interrupted by any errors and you receive clean
results.

Note that since this function won't be interrupted, please always test in smaller
scale to make sure everything is working as intended.
}
\details{
\strong{result_type:}

This function returns a list of two parts, \code{result} and \code{error}, and
result_type determines how \code{result} is structured. If a item fails or has
been processed by this function internally, a comment will show. Otherwise,
comment will be NA.

\code{result}

\describe{
    \item{list}{returns a list of 3 elements, lat,lng and comment}
    \item{df}{returns a data frame with 3 columns, lat, lng and comment}
    \item{raw}{returns the raw result from google's api as a list. Don't show
    any comments and this function don't process any data
    (\code{multi_coord_handling} won't be applied),you receive everything
    provided by geocode api}
}

\code{error}

a list that shows the error or special treatment for items in \code{result}

\strong{multiple_coord_handling}

Sometimes an address can be confusing to google's geocode API and multiple
matches are returned. For data analysis, It is undesirable in most cases. So
this functions tries handle this situation automatically. Regardless how this
functions handles this scenario, one entry is generated in \code{error}.

\describe{
    \item{none}{no hanlding, the coordinates will be NA}
    \item{first}{use the first coordinates provided}
    \item{average}{average all coordinates and use it as result}
}

\strong{show_summary}

summary will be printed to console, it shows how many addresses are tried in
total, how many succeeded or failed

\strong{comment}

Below are explanations for comments:

\itemize{
    \item API Error: something's wrong during API call
    \item Multiple Matches Error: Multiple paries of coordinates found for the
    same address.
    \item Multiple Matches Found, Using First Result: use the first result returned
    by geocode API
    \item Multiple Matches Found, Using Average Coordinates: use the average of
    coordinates returned by geocode API
}
}
\examples{
\dontrun{
apikey = your_apikey
du_google_geocode(
    c(
        "new york, usa",
        "#####", # contains error
        "canada, usa" # multiple matching results
    )
)
}
}
