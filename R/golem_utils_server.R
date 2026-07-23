#' Generate optim_sample() function call as text
#'
#' @param input Shiny input object
#' @return Character string containing the function call
#' @noRd
generate_optim_sample_call <- function(input) {

  par_names <- c("alpha", "beta", "phi", "lambda")
  thresh_inputs <- vapply(par_names, function(p) {
    val <- input[[paste0("thresh_", p)]]
    if (is.null(val) || is.na(val)) NA_real_ else as.numeric(val)
  }, numeric(1))

  active <- !is.na(thresh_inputs)
  thresh  <- thresh_inputs[active]
  out.par <- par_names[active]

  if (length(out.par) == 1) {
    thresh_str  <- thresh
    out.par_str <- paste0("'", out.par, "'")
  } else {
    thresh_str  <- paste0("c(", paste(thresh, collapse = ", "), ")")
    out.par_str <- paste0("c('", paste(out.par, collapse = "', '"), "')")
  }

  mat <- input$corr_sd_item
  corr_mat <- matrix(as.numeric(mat[1:4, 1:4]), nrow = 4, ncol = 4)
  sd_item <- as.numeric(mat[5, ])

  format_matrix <- function(m, indent = "                         ") {
    rows <- apply(m, 1, function(r) paste(format(r, nsmall = 1), collapse = ", "))
    paste0("matrix(c(", paste(rows, collapse = ",\n                  "), "),\n",
           indent, "nrow = 4, byrow = TRUE)")
  }

  call_str <- paste0(
    "optim_sample(\n",
    "  thresh         = ", thresh_str, ",\n",
    "  out.par        = ", out.par_str, ",\n",
    "  range          = c(", input$lb, ", ", input$ub, "),\n",
    "  iter           = ", input$iter, ",\n",
    "  K              = ", as.integer(input$K), ",\n",
    "  mu.person      = c(", input$mu_theta, ", ", input$mu_zeta, "),\n",
    "  mu.item        = c(", input$mu_alpha, ", ", input$mu_beta, ", ",
    input$mu_phi, ", ", input$mu_lambda, "),\n",
    "  meanlog.sigma2 = ", input$meanlog_sigma2, ",\n",
    "  cov.m.person   = matrix(c(1, ", input$corr_person, ",\n",
    "                            ", input$corr_person, ", 1), nrow = 2, byrow = TRUE),\n",
    "  cov.m.item     = ", format_matrix(corr_mat), ",\n",
    "  sd.item        = c(", paste(sd_item, collapse = ", "), "),\n",
    "  cor2cov.item   = TRUE,\n",
    "  sdlog.sigma2   = ", input$sdlog_sigma2, ",\n",
    "  XG             = ", input$XG, ",\n",
    "  burnin         = ", input$burnin, ",\n",
    "  seed           = ", input$seed, ",\n",
    "  keep.err.dat   = FALSE,\n",
    "  keep.rhat.dat  = TRUE\n",
    ")"
  )

  return(call_str)
}

#' Build design list from inputs
#'
#' @param input Shiny input object
#' @return List containing the design parameters
#' @noRd
build_design_from_inputs <- function(input) {

  par_names <- c("alpha", "beta", "phi", "lambda")
  thresh_inputs <- vapply(par_names, function(p) {
    val <- input[[paste0("thresh_", p)]]
    if (is.null(val) || is.na(val)) NA_real_ else as.numeric(val)
  }, numeric(1))

  active <- !is.na(thresh_inputs)

  mat <- input$corr_sd_item
  corr_mat <- matrix(as.numeric(mat[1:4, 1:4]), nrow = 4, ncol = 4)
  sd_item <- as.numeric(mat[5, ])

  list(
    thresh         = thresh_inputs[active],
    out.par        = par_names[active],
    range          = c(as.integer(input$lb), as.integer(input$ub)),
    iter           = as.integer(input$iter),
    K              = as.integer(input$K),
    mu.person      = c(as.numeric(input$mu_theta), as.numeric(input$mu_zeta)),
    mu.item        = c(as.numeric(input$mu_alpha), as.numeric(input$mu_beta),
                       as.numeric(input$mu_phi), as.numeric(input$mu_lambda)),
    meanlog.sigma2 = as.numeric(input$meanlog_sigma2),
    cov.m.person   = matrix(c(1, as.numeric(input$corr_person),
                              as.numeric(input$corr_person), 1),
                            nrow = 2, byrow = TRUE),
    cov.m.item     = corr_mat,
    sd.item        = sd_item,
    cor2cov.item   = TRUE,
    sdlog.sigma2   = as.numeric(input$sdlog_sigma2),
    XG             = as.integer(input$XG),
    burnin         = as.integer(input$burnin),
    seed           = as.integer(input$seed)
  )
}

#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %|NA|% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- function(...) shiny::reactiveValues(...)
rvtl <- function(...) shiny::reactiveValuesToList(...)
