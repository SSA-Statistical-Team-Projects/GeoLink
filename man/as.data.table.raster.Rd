% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rastertile_processor.R
\name{as.data.table.raster}
\alias{as.data.table.raster}
\title{Convert a raster to data.table object}
\usage{
\method{as.data.table}{raster}(
  x,
  row.names = NULL,
  optional = FALSE,
  xy = FALSE,
  inmem = raster::canProcessInMemory(x, 2),
  ...
)
}
\arguments{
\item{x}{an object of class \code{raster}.}

\item{row.names}{(optional) Names for the rows.}

\item{optional}{(optional) Logical. If \code{TRUE}, setting row names in the output is optional.}

\item{xy}{(optional) Logical. If \code{TRUE}, include coordinates of raster cells in the output.}

\item{inmem}{(optional) Logical. If \code{TRUE}, process the raster in memory. Default is determined by \code{raster::canProcessInMemory()}.}

\item{...}{Additional arguments (none)}
}
\description{
Convert a raster to data.table object
}
