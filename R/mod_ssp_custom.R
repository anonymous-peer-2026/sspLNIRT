#' ssp_custom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ssp_custom_ui <- function(id) {

  ns <- shiny::NS(id)

  tagList(
    bslib::page_fluid(
      bslib::layout_columns(
        col_widths = c(4, 8),

        # LEFT SIDE: INPUTS
        bslib::accordion(
          id   = ns("acc"),
          open = FALSE,

          # Person Parameters
          bslib::accordion_panel(
            title = "Person Parameters",
            icon  = bsicons::bs_icon("people"),

            shiny::helpText("Person parameters are drawn from a bivariate normal distribution with unit variance."),

            bslib::layout_columns(
              col_widths = c(4, 4, 4),

              shiny::numericInput(
                ns("mu_theta"),
                label = name_with_info("\u03B8 (theta)", "Latent ability parameter. Higher theta = higher probability of correct response."),
                value = 0, step = 0.01
              ),

              shiny::numericInput(
                ns("mu_zeta"),
                label = name_with_info("\u03B6 (zeta)", "Latent speed parameter. Higher zeta = faster responses."),
                value = 0, step = 0.01
              ),

              shiny::numericInput(
                ns("corr_person"),
                label = name_with_info("\u03C1 (rho)", "Correlation between ability (\u03B8) and speed (\u03B6). Positive = faster respondents tend to be more accurate."),
                value = 0.2, min = -1, max = 1, step = 0.1
              )
            )
          ),

          # Item Parameters
          bslib::accordion_panel(
            title = "Item Parameters",
            icon  = bsicons::bs_icon("list-check"),

            shiny::helpText("Item parameters are drawn from a truncated multivariate normal distribution."),

              shiny::numericInput(
                inputId = ns("K"),
                label   = name_with_info("K (Test Length)", "Number of items in the test."),
                value   = 30, min = 1, step = 1
              ),

            shiny::tags$hr(),

            shiny::tags$h6("Item Parameter Means", class = "text-muted"),
            bslib::layout_columns(
              col_widths = c(3, 3, 3, 3),

              shiny::numericInput(
                inputId = ns("mu_alpha"),
                label   = name_with_info("\u03B1 (alpha)", "Item discrimination. Higher \u03B1 = item better differentiates between high/low ability."),
                value   = 1, min = 0, step = 0.01
              ),

              shiny::numericInput(
                ns("mu_beta"),
                label = name_with_info("\u03B2 (beta)", "Item difficulty. Higher \u03B2 = more difficult item."),
                value = 0, step = 0.01
              ),

              shiny::numericInput(
                ns("mu_phi"),
                label = name_with_info("\u03C6 (phi)", "Time discrimination. Higher \u03C6 = RT more sensitive to speed differences."),
                value = 0.5, min = 0, step = 0.01
              ),

              shiny::numericInput(
                ns("mu_lambda"),
                label = name_with_info("\u03BB (lambda)", "Time intensity. Higher \u03BB = item takes longer on average."),
                value = 1, step = 0.01
              )
            ),

            shiny::tags$hr(),

            shinyMatrix::matrixInput(
              inputId = ns("corr_sd_item"),
              label   = name_with_info("Correlations & SDs", "Upper 4\u00D74: correlation matrix. Bottom row: standard deviations of item parameters."),
              value   = matrix(
                c(
                  1, 0, 0, 0,
                  0, 1, 0, 0.4,
                  0, 0, 1, 0,
                  0, 0.4, 0, 1,
                  0.2, 1.0, 0.2, 0.5
                ),
                nrow = 5,
                byrow = TRUE,
                dimnames = list(
                  c("\u03B1", "\u03B2", "\u03C6", "\u03BB", "SD"),
                  c("\u03B1", "\u03B2", "\u03C6", "\u03BB")
                )
              ),
              rows = list(names = TRUE),
              cols = list(names = TRUE)
            ),

            shiny::tags$hr(),

            shiny::tags$h6("Residual Variance (\u03C3\u00B2)", class = "text-muted"),
            shiny::helpText("Log-normal distribution for RT model residual variance."),

            bslib::layout_columns(
              col_widths = c(6, 6),

              shiny::numericInput(
                inputId = ns("meanlog_sigma2"),
                label   = name_with_info("Mean (log scale)", "Expected value of log(\u03C3\u00B2). Lower = less residual noise in RT."),
                value   = round(log(0.6),2), step = 0.1
              ),

              shiny::numericInput(
                ns("sdlog_sigma2"),
                label = name_with_info("SD (log scale)", "Variability of log(\u03C3\u00B2) across items."),
                value = 0, min = 0, step = 0.01
              )
            )
          ),

          # Targets
          bslib::accordion_panel(
            title = "Targets",
            icon  = bsicons::bs_icon("bullseye"),

            shiny::helpText(
              "Set an RMSE threshold for each item parameter you want to control.",
              "The minimum N is determined by the most demanding parameter.",
              "Leave blank to exclude a parameter."
            ),

            bslib::layout_columns(
              col_widths = c(3, 3, 3, 3),

              shiny::numericInput(
                inputId = ns("thresh_alpha"),
                label   = name_with_info("\u03B1 (alpha)", "RMSE threshold for item discrimination."),
                value   = 0.1, min = 0.001, step = 0.01
              ),

              shiny::numericInput(
                inputId = ns("thresh_beta"),
                label   = name_with_info("\u03B2 (beta)", "RMSE threshold for item difficulty."),
                value   = NA, min = 0.001, step = 0.01
              ),

              shiny::numericInput(
                inputId = ns("thresh_phi"),
                label   = name_with_info("\u03C6 (phi)", "RMSE threshold for time discrimination."),
                value   = NA, min = 0.001, step = 0.01
              ),

              shiny::numericInput(
                inputId = ns("thresh_lambda"),
                label   = name_with_info("\u03BB (lambda)", "RMSE threshold for time intensity."),
                value   = NA, min = 0.001, step = 0.01
              )
            )
          ),

          # Optimizer Settings
          bslib::accordion_panel(
            title = "Optimizer Settings",
            icon  = bsicons::bs_icon("gear"),

            shiny::helpText("Settings for model estimation and the bisection search algorithm."),

            bslib::layout_columns(
              col_widths = c(6, 6),
              shiny::numericInput(
                ns("iter"),
                label = name_with_info("Iterations", "Monte Carlo replications per N evaluation."),
                value = 200, min = 1, step = 10
              ),
              shiny::numericInput(
                ns("XG"),
                label = name_with_info("Gibbs Samples", "MCMC iterations for parameter estimation."),
                value = 5000, min = 100, step = 100
              )
            ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              shiny::numericInput(
                ns("lb"),
                label = name_with_info("N min", "Lower bound of sample size search range."),
                value = 50, min = 2, step = 10
              ),
              shiny::numericInput(
                ns("ub"),
                label = name_with_info("N max", "Upper bound of sample size search range."),
                value = 2000, min = 10, step = 100
              )
            ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              shiny::numericInput(
                ns("burnin"),
                label = name_with_info("Burn-in %", "Percentage of Gibbs samples to discard."),
                value = 20, min = 0, max = 50, step = 5
              ),
              shiny::numericInput(
                ns("seed"),
                label = name_with_info("Seed", "Random seed for reproducibility."),
                value = 123456, step = 1
              )
            )
          )
        ),

        # RIGHT SIDE: OUTPUT WINDOW
        bslib::accordion(
          id   = ns("acc_output"),
          open = FALSE,

          # Function Call Panel
          bslib::accordion_panel(
            title = "Function Call",
            icon  = bsicons::bs_icon("code-slash"),

            shiny::div(
              style = "height: 40vh; overflow-y: auto;",
              shiny::verbatimTextOutput(ns("out_text"))
            ),

            # Downloads
            shiny::tags$details(
              class = "mt-3",
              shiny::tags$summary(
                style = "cursor: pointer; font-size: 0.85em; color: #6c757d;",
                bsicons::bs_icon("download", class = "me-1"),
                "Downloads"
              ),
              shiny::div(
                class = "pt-2",
                bslib::layout_columns(
                  col_widths = c(6, 6),
                  shiny::downloadButton(
                    ns("download_script"),
                    label = "Script (.txt)",
                    class = "btn-outline-secondary btn-sm w-100"
                  ),
                  shiny::downloadButton(
                    ns("download_design"),
                    label = "Design (.rds)",
                    class = "btn-outline-secondary btn-sm w-100"
                  )
                )
              )
            )
          ),

          # Diagnostics Panel
          bslib::accordion_panel(
            title = "Diagnostics",
            icon  = bsicons::bs_icon("graph-up"),

            shiny::div(
              style = "height: 62vh; overflow-y: auto;",
              bslib::navset_pill(

                # Response Time
                bslib::nav_panel(
                  title = "Response Time",
                  icon  = bsicons::bs_icon("clock"),

                  bslib::card(
                    bslib::card_header("Response Time Distribution", class = "bg-light"),
                    bslib::card_body(
                      bslib::layout_columns(
                        col_widths = c(4, 4, 4),
                        shiny::selectInput(
                          inputId = ns("rt_level"),
                          label = name_with_info("Level", "Person: average RT per person. Item: RT distribution per item."),
                          choices = c("Person" = "person", "Item" = "item"),
                          selected = "person"
                        ),
                        shiny::selectInput(
                          inputId = ns("rt_logRT"),
                          label = name_with_info("Scale", "Seconds or log-seconds. Log scale often more symmetric."),
                          choices = c("Seconds" = "FALSE", "Log" = "TRUE"),
                          selected = "FALSE"
                        ),
                        shiny::div(
                          style = "padding-top: 1.9em;",
                          shiny::actionButton(
                            ns("draw_rt"),
                            label = "Draw Sample",
                            icon  = shiny::icon("play"),
                            class = "btn-primary w-100"
                          )
                        )
                      ),
                      shiny::plotOutput(ns("plot1"), height = "40vh")
                    )
                  )
                ),

                # Response Accuracy
                bslib::nav_panel(
                  title = "Response Accuracy",
                  icon  = bsicons::bs_icon("check-circle"),

                  bslib::card(
                    bslib::card_header("Response Accuracy Distribution", class = "bg-light"),
                    bslib::card_body(
                      bslib::layout_columns(
                        col_widths = c(6,6),
                        shiny::selectInput(
                          inputId = ns("ra_level"),
                          label = name_with_info("Level", "Person: total score. Item: item characteristic curves."),
                          choices = c("Person" = "person", "Item" = "item"),
                          selected = "person"
                        ),
                        shiny::div(
                          style = "padding-top: 1.9em;",
                          shiny::actionButton(
                            ns("draw_ra"),
                            label = "Draw Sample",
                            icon  = shiny::icon("play"),
                            class = "btn-primary w-100"
                          )
                        )
                      ),
                      shiny::plotOutput(ns("plot2"), height = "40vh")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}


#' ssp_custom Server Functions
#'
#' @noRd
mod_ssp_custom_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    # Render: Function call
    output$out_text <- shiny::renderPrint({
      call_str <- generate_optim_sample_call(input)
      cat("# sspLNIRT Sample Size Planning\n")
      cat("# Copy and run this code in R\n\n")
      cat("library(sspLNIRT)\n")
      cat("library(future)\n\n")
      cat("# Parallel backend\n")
      cat("plan(multisession, workers = future::availableCores() - 1)\n\n")
      cat("result <- ", call_str, "\n\n", sep = "")
      cat("summary(result)\n\n")
      cat("# Reset parallel backend\n")
      cat("plan(sequential)\n")
    })

    # Download handlers
    output$download_script <- shiny::downloadHandler(
      filename = function() {
        paste0("ssp_lnirt_script_", format(Sys.Date(), "%Y%m%d"), ".txt")
      },
      content = function(file) {
        call_str <- generate_optim_sample_call(input)

        script_content <- paste0(
          "# ===================================================\n",
          "# sspLNIRT Sample Size Planning Script\n",
          "# Generated: ", Sys.time(), "\n",
          "# ===================================================\n\n",
          "library(sspLNIRT)\n",
          "library(future)\n\n",
          "# Parallel backend\n",
          "n.cores <- future::availableCores() - 1\n",
          "plan(multisession, workers = n.cores)\n",
          'cat("Running with", n.cores, "cores.\\n")\n\n',
          "# Run sample size optimization\n",
          "result <- ", call_str, "\n\n",
          "# Save results\n",
          'saveRDS(result, file = paste0("ssp_lnirt_result_", Sys.Date(), ".rds"))\n\n',
          "# View results\n",
          "summary(result)\n\n",
          "# Reset parallel backend\n",
          "plan(sequential)\n"
        )

        writeLines(script_content, file)
      }
    )

    output$download_design <- shiny::downloadHandler(
      filename = function() {
        paste0("ssp_lnirt_design_", format(Sys.Date(), "%Y%m%d"), ".rds")
      },
      content = function(file) {
        design <- build_design_from_inputs(input)
        class(design) <- "sspLNIRT.design"
        saveRDS(design, file)
      }
    )

    # Plot 1: Response Time
    rt_plot_trigger <- shiny::reactiveVal(0)

    shiny::observeEvent(input$draw_rt, {
      rt_plot_trigger(rt_plot_trigger() + 1)
    })

    output$plot1 <- shiny::renderPlot({
      trigger <- rt_plot_trigger()

      if (trigger == 0) {
        plot.new()
        text(0.5, 0.5, "Click 'Draw Sample' to generate plot", cex = 1.2, col = "grey50")
        return()
      }

      design <- build_design_from_inputs(input)

      plot_RT(
        level          = input$rt_level,
        logRT          = as.logical(input$rt_logRT),
        overlay        = TRUE,
        N              = 1e4,
        K              = design$K,
        mu.person      = design$mu.person,
        mu.item        = design$mu.item,
        meanlog.sigma2 = design$meanlog.sigma2,
        cov.m.person   = design$cov.m.person,
        cov.m.item     = design$cov.m.item,
        sd.item        = design$sd.item,
        sdlog.sigma2   = design$sdlog.sigma2,
        cor2cov.item   = design$cor2cov.item
      )
    }, res = 120)

    # Plot 2: Response Accuracy
    ra_plot_trigger <- shiny::reactiveVal(0)

    shiny::observeEvent(input$draw_ra, {
      ra_plot_trigger(ra_plot_trigger() + 1)
    })

    output$plot2 <- shiny::renderPlot({
      trigger <- ra_plot_trigger()

      if (trigger == 0) {
        plot.new()
        text(0.5, 0.5, "Click 'Draw Sample' to generate plot", cex = 1.2, col = "grey50")
        return()
      }

      design <- build_design_from_inputs(input)

      plot_RA(
        level          = input$ra_level,
        by.theta       = TRUE,
        overlay        = TRUE,
        N              = 1e4,
        K              = design$K,
        mu.person      = design$mu.person,
        mu.item        = design$mu.item,
        meanlog.sigma2 = design$meanlog.sigma2,
        cov.m.person   = design$cov.m.person,
        cov.m.item     = design$cov.m.item,
        sd.item        = design$sd.item,
        sdlog.sigma2   = design$sdlog.sigma2,
        cor2cov.item   = design$cor2cov.item
      )
    }, res = 120)

  })
}
