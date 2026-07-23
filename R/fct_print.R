#' Print Method for sspLNIRT Summary Objects
#'
#' @description
#' Formats and prints a `"summary.sspLNIRT"` object to the console. The
#' output adapts to the source: [optim_sample()] results show the minimum
#' sample size, optimization diagnostics, and parameter accuracy tables;
#' [comp_rmse()] results show item and person parameter accuracy tables.
#'
#' @param x An object of class `"summary.sspLNIRT"`, as returned by
#'   [summary.sspLNIRT()].
#' @param digits Integer. Number of decimal places shown in tables. Default 4.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [summary.sspLNIRT()].
#'
#' @method print summary.sspLNIRT
#' @exportS3Method print summary.sspLNIRT
print.summary.sspLNIRT <- function(x, digits = 4, ...) {

  fmt <- paste0("%.", digits, "f")

  cat("==================================================\n\n")

  if (!is.null(x$N.min)) {

    cat("Call: optim_sample()\n\n")

    if (length(x$res.best) > 0 && !is.null(names(x$res.best))) {
      rb_str <- paste(
        names(x$res.best),
        sprintf(fmt, as.numeric(x$res.best)),
        sep = " = ",
        collapse = ", "
      )
    } else {
      rb_str <- paste(sprintf(fmt, as.numeric(x$res.best)), collapse = ", ")
    }

    cat("Sample Size Optimization\n")
    cat("--------------------------------------------------\n")
    cat("  Min Sample Size:      ", x$N.min, "\n")
    cat("  Critical Parameter:     ", paste(x$out.par, collapse = ", "), "\n")
    cat("  RMSE at Min N:        ", rb_str, "\n")
    cat("  Optimizer Steps:      ", x$trace$steps, "\n")
    cat("  Time Taken:           ", format(x$trace$time.taken), "\n\n")

    detailed <- !is.null(x$comp.rmse) &&
      !is.null(x$comp.rmse$item$rmse) &&
      length(x$comp.rmse$item$rmse) > 0

    if (detailed) {
      cat("Item Parameter RMSEs:\n")
      cat("--------------------------------------------------\n")
      print_rmse_block(x$comp.rmse$item, fmt = fmt)

      cat("\nPerson Parameter RMSEs:\n")
      cat("--------------------------------------------------\n")
      print_rmse_block(x$comp.rmse$person, fmt = fmt)
    } else {
      cat("Detailed RMSE breakdown not available at this boundary.\n")
    }

  } else {

    cat("Call: comp_rmse()\n\n")

    cat("Item Parameter RMSEs:\n")
    cat("--------------------------------------------------\n")
    print_rmse_block(x$item, fmt = fmt)

    cat("\nPerson Parameter RMSEs:\n")
    cat("--------------------------------------------------\n")
    print_rmse_block(x$person, fmt = fmt)
  }

  cat("---\n")
  invisible(x)
}

#' Helper: RMSE / MC SD / Bias block
#' @keywords internal
#' @noRd
print_rmse_block <- function(block, fmt = "%.4f") {

  rmse_v  <- unlist(block$rmse)
  mcsd_v  <- unlist(block$mc.sd.rmse)
  bias_v  <- unlist(block$bias)

  if (length(rmse_v) == 0) {
    cat("(no values)\n")
    return(invisible(NULL))
  }

  ## MC SD / Bias with NAs if missing
  pad <- function(v, n) {
    if (length(v) == n) v
    else stats::setNames(rep(NA_real_, n), names(rmse_v))
  }
  mcsd_v <- pad(mcsd_v, length(rmse_v))
  bias_v <- pad(bias_v, length(rmse_v))

  df <- data.frame(
    RMSE   = sprintf(fmt, rmse_v),
    `MC SD` = sprintf(fmt, mcsd_v),
    Bias   = sprintf(fmt, bias_v),
    check.names = FALSE
  )
  rownames(df) <- names(rmse_v)
  print(t(df), right = TRUE, quote = FALSE)
}
