#' Client-side utilities for the dsPUcopula DataSHIELD package
#'
#' These helper functions provide client-facing wrappers around the
#' server-side procedures delivered by the dsPUcopula package. They simplify
#' the workflow for fitting the partition-of-unity copula model, estimating
#' marginal distributions, simulating copula samples, and generating synthetic
#' data inside a DataSHIELD session.
#'
#' @keywords internal
#' @importFrom DSI datashield.aggregate datashield.assign.expr
#'   datashield.connections_find
#' @name dsPUcopulaClient-package
NULL


#' Validate a DataSHIELD connection list
#'
#' @param datasources A list of DataSHIELD connection objects as returned by
#'   [DSI::datashield.connections_find()].
#' @return The validated `datasources` list.
#' @noRd
.validate_datasources <- function(datasources) {
  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }

  if (!is.list(datasources) || length(datasources) == 0) {
    stop(
      "No active DataSHIELD connections were supplied. Use ",
      "DSI::datashield.login() to create a connection first.",
      call. = FALSE
    )
  }

  datasources
}


#' Fit a Partition-of-Unity Copula Model on the server
#'
#' @description
#' Creates or replaces the `PU_copula_model` object in the connected
#' DataSHIELD sessions by delegating to the server-side `fitPUcopulaDS`
#' function. The resulting object stores the fitted copula model that can be
#' reused by subsequent client calls.
#'
#' @param data `character(1)`. The name of the server-side data frame that
#'   contains the samples.
#' @param driver_strength_factor `numeric(1)`. Tuning parameter for the driver
#'   strength estimation. Values must be strictly between 0 and 1. Defaults to
#'   `0.5`.
#' @param bin_size `integer(1)`. Bin size used during the density estimation
#'   steps of the algorithm. Defaults to `3`.
#' @param datasources Optional list of DataSHIELD connections. When omitted the
#'   currently active connections obtained via
#'   [DSI::datashield.connections_find()] are used.
#' @param ... Additional arguments passed through to the server function. At
#'   the moment these are ignored but reserved for future compatibility.
#'
#' @return Invisibly returns `TRUE` after the remote object has been created.
#' @export
#'
#' @examples
#' \dontrun{
#' ds.fitPUcopula(
#'   data = "opals.cancer", driver_strength_factor = 0.6, bin_size = 4
#' )
#' }
ds.fitPUcopula <- function(
    data,
    driver_strength_factor = 0.5,
    bin_size = 3,
    datasources = NULL,
    ...
) {
  if (!is.character(data) || length(data) != 1L || !nzchar(data)) {
    stop("`data` must be a non-empty character string.", call. = FALSE)
  }

  if (!is.numeric(driver_strength_factor) || length(driver_strength_factor) != 1L ||
      !is.finite(driver_strength_factor) || driver_strength_factor <= 0) {
    stop(
      "`driver_strength_factor` must be a numeric value larger than 0.",
      call. = FALSE
    )
  }

  if (!is.numeric(bin_size) || length(bin_size) != 1L || is.na(bin_size) ||
      bin_size <= 0) {
    stop("`bin_size` must be a positive numeric value.", call. = FALSE)
  }

  datasources <- .validate_datasources(datasources)

  calltext <- call("fitPUcopulaDS", data, driver_strength_factor, bin_size, ...)
  datashield.assign.expr(datasources, "PU_copula_model", calltext)

  invisible(TRUE)
}


#' Estimate marginal distributions inside DataSHIELD
#'
#' @description
#' Computes marginal distribution estimates for the variables identified by the
#' server-side symbol `x` through the `estimateMarginalsDS` helper. The result
#' is stored remotely as `marginals_list` for later reuse.
#'
#' @param x `character(1)`. The name of the server-side object that holds the
#'   variables for which the marginals should be estimated.
#' @param datasources Optional list of DataSHIELD connections as returned by
#'   [DSI::datashield.connections_find()].
#' @param method `character(1)`. The marginal estimation approach to use. The
#'   default is `"ecdf"`, but the value must match a method supported on the
#'   server side.
#'
#' @return Invisibly returns `TRUE` after the remote object has been created.
#' @export
#'
#' @examples
#' \dontrun{
#' ds.estimateMarginals("PU_copula_model$data")
#' }
ds.estimateMarginals <- function(x, datasources = NULL, method = "ecdf") {
  if (!is.character(x) || length(x) != 1L || !nzchar(x)) {
    stop("`x` must be a non-empty character string.", call. = FALSE)
  }

  if (!is.character(method) || length(method) != 1L || !nzchar(method)) {
    stop("`method` must be a non-empty character string.", call. = FALSE)
  }

  datasources <- .validate_datasources(datasources)

  calltext <- call("estimateMarginalsDS", x, method)
  datashield.assign.expr(datasources, "marginals_list", calltext)

  invisible(TRUE)
}


#' Simulate from the fitted copula model
#'
#' @description
#' Generates a simulated sample of size `n` from the copula model previously
#' stored in `PU_copula_model`. The simulated uniform scores are kept on the
#' server-side as `PU_copula_model_u_sims_clientcode`.
#'
#' @param n `integer(1)`. Number of draws to request from the server.
#' @param datasources Optional list of DataSHIELD connections to use.
#'
#' @return Invisibly returns `TRUE` after the simulation object has been
#'   stored on the server.
#' @export
#'
#' @examples
#' \dontrun{
#' ds.simulateCopula(1000)
#' }
ds.simulateCopula <- function(n, datasources = NULL) {
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n <= 0) {
    stop("`n` must be a positive numeric value.", call. = FALSE)
  }

  datasources <- .validate_datasources(datasources)

  calltext <- call("simulateCopulaDS", n)
  datashield.assign.expr(
    datasources,
    "PU_copula_model_u_sims_clientcode",
    calltext
  )

  invisible(TRUE)
}


#' Retrieve synthetic data from the server
#'
#' @description
#' Aggregates the synthetic observations generated by the server-side
#' `generateSyntheticDS` function and returns them to the client.
#'
#' @param n `integer(1)`. Number of synthetic observations to retrieve.
#' @param datasources Optional list of DataSHIELD connections to use.
#'
#' @return A data frame with `n` rows containing the synthesised observations.
#' @export
#'
#' @examples
#' \dontrun{
#' synth <- ds.generateSynthetic(500)
#' }
ds.generateSynthetic <- function(n, datasources = NULL) {
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n <= 0) {
    stop("`n` must be a positive numeric value.", call. = FALSE)
  }

  datasources <- .validate_datasources(datasources)

  calltext <- call("generateSyntheticDS", n)
  datashield.aggregate(datasources, calltext)
}
