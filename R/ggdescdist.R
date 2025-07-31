#' Cullen and Frey Plot for Empirical Distribution Diagnostics
#'
#' @description
#' This function prints summary statistics and produces a Cullen and Frey skewness-kurtosis plot to assess the
#' shape of an empirical distribution via skewness and kurtosis. The code is based on `descdist` from the `fitdistrplus`
#' package (Delignette-Muller and Dutang, 2015), but the output is a `ggplot2` object.
#'
#' @param data A numeric vector.
#' @param discrete If `TRUE`, the distribution is considered as discrete.
#' @param boot If not `NULL`, boot values of skewness and kurtosis are plotted from bootstrap samples of data.
#' `boot` must be fixed in this case to an integer above 10.
#' @param method `"unbiased"` for unbiased estimated values of statistics or `"sample"` for sample values.
#' @param plot If `FALSE`, the skewness-kurtosis plot is not plotted.
#' @param print If `FALSE`, the descriptive parameters computed are not printed.
#' @param color Color used for the observed point on the skewness-kurtosis plot.
#' @param shape Plotting character used for the observed point on the skewness-kurtosis plot.
#' @param color_boot Color used for bootstrap sample of points on the skewness-kurtosis plot.
#'
#' @importFrom rlang .data
#' @export
#'
#' @details
#' This function was based on code from `fitdistrplus` v1.2-1 (as of 2024-08-29). Please cite `fitdistrplus` if
#' using this function; see `citation("fitdistrplus")` (`fitdistrplus` must be installed). The original code was
#' optimized via ChatGPT by OpenAI and then manually configured to use `ggplot2` for the skewness-kurtosis plot.
#'
#' Summary statistics include the minimum, maximum, median, mean, sample standard
#' deviation, and either sample-based or (by default) unbiased estimates of
#' skewness and Pearson’s kurtosis (Sokal and Rohlf, 1995).
#'
#' The Cullen and Frey skewness-kurtosis plot (Cullen and Frey, 1999) displays the empirical skewness
#' and kurtosis, along with reference values for common theoretical distributions,
#' helping guide the choice of candidate distributions for fitting.
#'
#' - **Point representations**: Distributions with fixed skewness and kurtosis values
#'   (e.g., normal [0, 3], uniform, logistic, exponential) appear as single points.
#' - **Line representations**: Distributions with shape-dependent values (e.g., gamma,
#'   lognormal) are shown as curves.
#' - **Area representations**: Flexible distributions (e.g., beta) appear as regions.
#' - **Note on Weibull**: Not plotted directly but noted in the legend as overlapping
#'   with gamma and lognormal shapes.
#'
#' To show uncertainty in the skewness and kurtosis estimates, set `boot` to an
#' integer greater than 10 to enable bootstrap resampling. Bootstrap estimates will
#' appear on the plot and added to the legend.
#'
#' If `discrete = TRUE`, the plot includes Poisson, negative binomial, and normal
#' (as a limiting distribution). If `discrete = FALSE`, it includes uniform, normal,
#' logistic, lognormal, beta, and gamma distributions.
#'
#' @returns For `print = TRUE`, a list with 7 components (min, max, median, mean, sd, skewness, kurtosis, method), and/or
#'  for `plot = TRUE`, a skewness-kurtosis plot as `ggplot2` object.
#'
#' @references
#' Cullen AC and Frey HC (1999), Probabilistic techniques in exposure assessment. Plenum Press, United States, pp. 81-159.
#'
#' Delignette-Muller ML and Dutang C (2015), fitdistrplus: An R Package for Fitting Distributions. Journal of Statistical Software, 64(4), 1-34, \href{https://www.doi.org/10.18637/jss.v064.i04}{doi: 10.18637/jss.v064.i04}.
#'
#' Evans M, Hastings N and Peacock B (2000), Statistical distributions. John Wiley and Sons Inc, \href{https://www.doi.org/10.1002/9780470627242}{doi: 10.1002/9780470627242}.
#'
#' Sokal RR and Rohlf FJ (1995), Biometry. W.H. Freeman and Company, United States, pp. 111-115.
#'
#' @author The original authors of `descdist` and the `fitdistrplus` package are Marie-Laure Delignette-Muller and Christophe Dutang.
#'
#' @seealso \code{\link[fitdistrplus:descdist]{descdist}}
#'
#' @examples
#' set.seed(42)
#' x <- rnorm(1000, mean = 27, sd = 3)
#'
#' # Without bootstrapping
#' ggdescdist(x)
#'
#' # With bootstrapping
#' ggdescdist(x, boot = 20)
#'
#' # Discrete distribution
#' set.seed(42)
#' y <- rpois(1000, lambda = 2)
#' ggdescdist(y, discrete = TRUE)

ggdescdist <- function(data,
                       discrete = FALSE,
                       boot = NULL,
                       method = "unbiased",
                       plot = TRUE,
                       print = TRUE,
                       color = "#e15759",
                       shape = 16,
                       color_boot = "#4e79a7") {

  if (missing(data) || !is.vector(data, mode = "numeric"))
    stop("data must be a numeric vector")
  if (length(data) < 4)
    stop("data must be a numeric vector containing at least four values")

  moment <- function(data, k) {
    m1 <- mean(data)
    return(sum((data - m1) ^ k) / length(data))
  }

  if (method == "unbiased") {
    skewness <- function(data) {
      sd <- sqrt(moment(data, 2))
      n <- length(data)
      gamma1 <- moment(data, 3) / sd ^ 3
      unbiased.skewness <- sqrt(n * (n - 1)) * gamma1 / (n - 2)
      return(unbiased.skewness)
    }
    kurtosis <- function(data) {
      n <- length(data)
      var <- moment(data, 2)
      gamma2 <- moment(data, 4) / var ^ 2
      unbiased.kurtosis <- (n - 1) / ((n - 2) * (n - 3)) *
        ((n + 1) * gamma2 - 3 * (n - 1)) + 3
      return(unbiased.kurtosis)
    }
    standdev <- function(data) {
      stats::sd(data)
    }
  } else if (method == "sample") {
    skewness <- function(data) {
      sd <- sqrt(moment(data, 2))
      return(moment(data, 3) / sd ^ 3)
    }
    kurtosis <- function(data) {
      var <- moment(data, 2)
      return(moment(data, 4) / var ^ 2)
    }
    standdev <- function(data) {
      sqrt(moment(data, 2))
    }
  } else
    stop("The only possible value for the argument method are 'unbiased' or 'sample'")

  res <- list(
    min = min(data),
    max = max(data),
    median = stats::median(data),
    mean = mean(data),
    sd = standdev(data),
    skewness = skewness(data),
    kurtosis = kurtosis(data),
    method = method
  )

  skewdata <- res$skewness
  kurtdata <- res$kurtosis

  if (plot) {
    if (!is.null(boot)) {
      if (!is.numeric(boot) || boot < 10) {
        stop("boot must be NULL or an integer above 10")
      }
      n <- length(data)
      databoot <- matrix(sample(data, size = n * boot, replace = TRUE),
                         nrow = n,
                         ncol = boot)
      s2boot <- sapply(1:boot, function(iter)
        skewness(databoot[, iter]) ^ 2)
      kurtboot <- sapply(1:boot, function(iter)
        kurtosis(databoot[, iter]))
      kurtmax <- max(10, ceiling(max(kurtboot)))
      xmax <- max(4, ceiling(max(s2boot)))
      boot_data <- data.frame(x = s2boot, y = kurtmax - kurtboot)
    } else {
      kurtmax <- max(10, ceiling(kurtdata))
      xmax <- max(4, ceiling(skewdata ^ 2))
    }

    ymax <- kurtmax - 1

    # Prepare data for ggplot2
    plot_data <- data.frame(x = skewdata ^ 2, y = kurtmax - kurtdata)

    # Start ggplot2 plotting
    plot <- ggplot2::ggplot() +
      ggplot2::coord_cartesian(xlim = c(0, xmax), ylim = c(0, ymax)) +
      ggplot2::labs(title = "Cullen and Frey plot") +
      ggplot2::scale_x_continuous(
        name = "Square of skewness",
        guide = ggplot2::guide_axis(minor.ticks = TRUE)
      ) +
      ggplot2::scale_y_continuous(
        name = "Kurtosis",
        breaks = seq(from = 0, to = ymax, by = 2),
        minor_breaks = seq(from = 0, to = ymax, by = 1),
        labels = as.character(kurtmax - seq(from = 0, to = ymax, by = 2)),
        guide = ggplot2::guide_axis(minor.ticks = TRUE)
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 8),
        axis.title = ggplot2::element_text(face = "bold", size = 11),
        legend.background = ggplot2::element_blank(),
        legend.box.background = ggplot2::element_rect(fill = "white", color = NA),
        legend.direction = "vertical",
        legend.key = ggplot2::element_rect(fill = NA, color = NA),
        legend.position = "right",
        legend.title = ggplot2::element_text(face = "bold", size = 11),
        legend.text = ggplot2::element_text(
          size = 10,
          hjust = 0,
          margin = ggplot2::margin(
            l = 3,
            r = 10,
            b = 3,
            t = 3,
            unit = "pt"
          )
        ),
        panel.grid = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(face = "bold", size = 11),
        plot.background = ggplot2::element_blank(),
        plot.caption = ggplot2::element_text(
          hjust = 1,
          size = 8,
          face = "plain"
        ),
        plot.caption.position = "plot",
        strip.text = ggplot2::element_text(face = "bold", size = 11)
      )

    if (!discrete) {
      p <- exp(-100)
      q <- exp(seq(-100, 100, 0.1))
      s2a <- (4 * (q - p) ^ 2 * (p + q + 1)) / ((p + q + 2) ^ 2 *
                                                  p * q)
      ya <- kurtmax - (3 * (p + q + 1) * (p * q * (p +
                                                     q - 6) + 2 * (p + q) ^
                                            2) / (p * q * (p + q + 2) *
                                                    (p + q + 3)))
      p <- exp(100)
      q <- exp(seq(-100, 100, 0.1))
      s2b <- (4 * (q - p) ^ 2 * (p + q + 1)) / ((p + q + 2) ^ 2 *
                                                  p * q)
      yb <- kurtmax - (3 * (p + q + 1) * (p * q * (p +
                                                     q - 6) + 2 * (p + q) ^
                                            2) / (p * q * (p + q + 2) *
                                                    (p + q + 3)))

      # Gamma
      df_gamma <- data.frame(shape = exp(seq(-100, 100, 0.1)))
      df_gamma$s2 <- 4 / df_gamma$shape
      df_gamma$y <- kurtmax - (3 + 6 / df_gamma$shape)
      df_gamma <- df_gamma[df_gamma$s2 <= xmax, ]

      # Lognormal
      df_lognormal <- data.frame(shape = exp(seq(-100, 100, 0.1)))
      df_lognormal$es2 <- exp(df_lognormal$shape^2)
      df_lognormal$s2 <- (df_lognormal$es2 + 2)^2 * (df_lognormal$es2 - 1)
      df_lognormal$y <- kurtmax - (df_lognormal$es2^4 + 2 * df_lognormal$es2^3 + 3 * df_lognormal$es2^2 - 3)
      df_lognormal <- df_lognormal[df_lognormal$s2 <= xmax, ]

      plot <- plot +
        ggplot2::geom_polygon(
          data = data.frame(
            id = 1,
            s2 = c(s2a, s2b),
            y = c(ya, yb)
          ),
          inherit.aes = FALSE,
          ggplot2::aes(
            x = .data$s2,
            y = .data$y,
            group = .data$id,
            fill = "beta"
          ),
          color = "lightgrey"
        ) +
        ggplot2::geom_line(
          data = df_gamma,
          inherit.aes = FALSE,
          ggplot2::aes(
            x = .data$s2,
            y = .data$y,
            linetype = "gamma"
          ),
          color = "#333333"
        ) +
        ggplot2::geom_line(
          data = df_lognormal,
          inherit.aes = FALSE,
          ggplot2::aes(
            x = .data$s2,
            y = .data$y,
            linetype = "lognormal"
          ),
          color = "#333333"
        ) +
        ggplot2::labs(caption = "*Weibull is near gamma and lognormal")

    } else{
      p <- exp(-10)
      r <- exp(seq(-100, 100, 0.1))
      s2a <- (2 - p) ^ 2 / (r * (1 - p))
      ya <- kurtmax - (3 + 6 / r + p ^ 2 / (r * (1 - p)))

      p <- 1 - exp(-10)
      r <- exp(seq(100, -100, -0.1))
      s2b <- (2 - p) ^ 2 / (r * (1 - p))
      yb <- kurtmax - (3 + 6 / r + p ^ 2 / (r * (1 - p)))

      # Poisson
      df_poisson <- data.frame(lambda <- exp(seq(-100, 100, 0.1)))
      df_poisson$s2 <- 1 / df_poisson$lambda
      df_poisson$y <- kurtmax - (3 + 1 / df_poisson$lambda)
      df_poisson <- df_poisson[df_poisson$s2 <= xmax, ]

      plot <- plot +
        ggplot2::geom_polygon(
          data = data.frame(
            id = 1,
            s2 = c(s2a, s2b),
            y = c(ya, yb)
          ),
          inherit.aes = FALSE,
          ggplot2::aes(
            x = .data$s2,
            y = .data$y,
            group = .data$id,
            fill = "negative binomial"
          ),
          color = "lightgrey"
        ) +
        ggplot2::geom_line(
          data = df_poisson,
          inherit.aes = FALSE,
          ggplot2::aes(
            x = .data$s2,
            y = .data$y,
            linetype = "poisson"
          ),
          color = "#333333"
        )
    }

    if (!is.null(boot)) {
      plot <- plot + ggplot2::geom_point(data = boot_data,
                                         ggplot2::aes(x = .data$x,
                                                      y = .data$y,
                                                      shape = "bootstrap",
                                                      color = "bootstrap",
                                                      size = "bootstrap"))
    }

    plot <- plot +
      ggplot2::geom_point(data = plot_data,
                          ggplot2::aes(
                            x = .data$x,
                            y = .data$y,
                            shape = "empirical",
                            color = "empirical",
                            size = "empirical"
                          )) +
      ggplot2::geom_point(ggplot2::aes(
        x = 0,
        y = kurtmax - 3,
        shape = "normal",
        size = "normal",
        color = "normal"
      ))

    if (!discrete) {
      plot <- plot +
        ggplot2::geom_point(ggplot2::aes(
          x = 0,
          y = kurtmax - 9 / 5,
          shape = "uniform",
          size = "uniform",
          color = "uniform"
        )) +
        ggplot2::geom_point(
          ggplot2::aes(
            x = 2^2,
            y = kurtmax - 9,
            shape = "exponential",
            size = "exponential",
            color = "exponential"
          )
        ) +
        ggplot2::geom_point(ggplot2::aes(
          x = 0,
          y = kurtmax - 4.2,
          shape = "logistic",
          size = "logistic",
          color = "logistic"
        ))
    }

    plot <- plot +
      ggplot2::scale_linetype_manual(
        breaks = c(
          "lognormal",
          "gamma",
          "poisson"
        ),
        values = c(
          "lognormal" = "dotted",
          "gamma" = "dashed",
          "poisson" = "dashed"
        ),
        labels = c(
          "lognormal" = "Lognormal*",
          "gamma" = "Gamma*",
          "poisson" = "Poisson"
        )
      ) +
      ggplot2::scale_shape_manual(
        name = "Distribution",
        breaks = c(
          "empirical",
          "bootstrap",
          "normal",
          "uniform",
          "exponential",
          "logistic",
          "beta"
        ),
        values = c(
          "empirical" = shape,
          "bootstrap" = 1,
          "normal" = 8,
          "uniform" = 2,
          "exponential" = 7,
          "logistic" = 3,
          "beta" = 15
        ),
        labels = c(
          "empirical" = "Empirical",
          "bootstrap" = "Bootstrap",
          "normal" = "Normal",
          "uniform" = "Uniform",
          "exponential" = "Exponential",
          "logistic" = "Logistic",
          "beta" = "Beta"
        )
      ) +
      ggplot2::scale_size_manual(
        name = "Distribution",
        breaks = c(
          "empirical",
          "bootstrap",
          "normal",
          "uniform",
          "exponential",
          "logistic",
          "beta"
        ),
        values = c(
          "empirical" = 4,
          "bootstrap" = 2,
          "normal" = 3,
          "uniform" = 3,
          "exponential" = 3,
          "logistic" = 3,
          "beta" = 3
        ),
        labels = c(
          "empirical" = "Empirical",
          "bootstrap" = "Bootstrap",
          "normal" = "Normal",
          "uniform" = "Uniform",
          "exponential" = "Exponential",
          "logistic" = "Logistic",
          "beta" = "Beta"
        )
      ) +
      ggplot2::scale_color_manual(
        name = "Distribution",
        breaks = c(
          "empirical",
          "bootstrap",
          "normal",
          "uniform",
          "exponential",
          "logistic",
          "beta"
        ),
        values = c(
          "empirical" = color,
          "bootstrap" = color_boot,
          "normal" = "#333333",
          "uniform" = "#333333",
          "exponential" = "#333333",
          "logistic" = "#333333",
          "beta" = "lightgrey"
        ),
        labels = c(
          "empirical" = "Empirical",
          "bootstrap" = "Bootstrap",
          "normal" = "Normal",
          "uniform" = "Uniform",
          "exponential" = "Exponential",
          "logistic" = "Logistic",
          "beta" = "Beta"
        )
      ) +
      ggplot2::scale_fill_manual(
        breaks = c("beta",
                   "negative binomial"),
        values = c(
          "beta" = "lightgrey",
          "negative binomial" = "lightgrey"
        ),
        labels = c("beta" = "Beta",
                   "negative binomial" = "Negative binomial")
      ) +
      ggplot2::guides(
        shape = ggplot2::guide_legend(order = 1),
        size = ggplot2::guide_legend(order = 1),
        color = ggplot2::guide_legend(order = 1),
        fill = ggplot2::guide_legend(order = 2, title = NULL),
        linetype = ggplot2::guide_legend(order = 3, title = NULL)
      ) +
      ggplot2::theme(
        legend.spacing.y = ggplot2::unit(0, "pt"),
        legend.margin = ggplot2::margin(
          t = 0,
          r = 5,
          b = 0,
          l = 5,
          unit = "pt"
        )
      )

    print(plot)
  }

  if (!print)
    invisible(structure(res, class = "descdist"))
  else
    structure(res, class = "descdist")
}
