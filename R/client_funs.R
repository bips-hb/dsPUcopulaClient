### Client-side functions (dsPUcopula package)

# ds.fitPUcopula.R
ds.fitPUcopula <- function(data,
                           driver_strength_factor =0.5,
                           bin_size=3,
                           datasources = DSI::datashield.connections_find(), ...) {
  #args <- list(x = x, y = y)
  #args <- c(args, list(...))
  calltext <- call("fitPUcopulaDS", data, driver_strength_factor, bin_size)#, list(...))
  DSI::datashield.assign.expr(datasources, "PU_copula_model", calltext)
  return(invisible(TRUE))
}

# ds.estimateMarginals.R
ds.estimateMarginals <- function(x, datasources = NULL, method = "ecdf") {
  if (is.null(datasources)) datasources <- DSI::datashield.connections_find()
  calltext <- call("estimateMarginalsDS", x, method)
  DSI::datashield.assign.expr(datasources, "marginals_list", calltext)
  return(invisible(TRUE))
}

# ds.simulateCopula.R
ds.simulateCopula <- function(n,
                              datasources = DSI::datashield.connections_find()) {
  calltext <- call("simulateCopulaDS", n)

  DSI::datashield.assign.expr(datasources, "PU_copula_model_u_sims_clientcode", calltext)
  return(invisible(TRUE))
  #DSI::datashield.aggregate(datasources, calltext)
}

# ds.generateSynthetic.R
ds.generateSynthetic <- function(n, datasources = NULL) {
  if (is.null(datasources)) datasources <- DSI::datashield.connections_find()
  calltext <- call("generateSyntheticDS", n)
  DSI::datashield.aggregate(datasources, calltext)
}
