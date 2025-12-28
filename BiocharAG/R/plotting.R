#' Plot RPV vs Carbon Price
#'
#' Generates a plot of Net Present Value (or RPV) for BES, BECCS, and BEBCS
#' across a range of carbon prices, similar to Figure 1 in the article.
#'
#' @param params A list of default parameters.
#' @param c_price_range A numeric vector of carbon prices to simulate (e.g., seq(0, 150, 10)).
#' @param metric Character string, either "RPV" (Relative Present Value) or "NPV" (Net Present Value). Default is "RPV".
#' @return A ggplot object.
#' @import ggplot2
#' @importFrom dplyr bind_rows
#' @export
plot_rpv_vs_c_price <- function(params, c_price_range = seq(0, 150, 10), metric = "RPV") {
    results_df <- data.frame()

    for (cp in c_price_range) {
        # Update carbon price in parameters
        p <- params
        p$c_price <- cp

        # Run models
        bes <- calculate_bes(p)
        beccs <- calculate_beccs(p)
        bebcs <- calculate_bebcs(p)

        # Calculate RPV
        rpv_res <- calculate_rpv(list(bes, beccs, bebcs))
        rpv_res$Carbon_Price <- cp

        results_df <- rbind(results_df, rpv_res)
    }

    # Prepare for plotting
    if (metric == "RPV") {
        y_var <- "RPV"
        y_label <- "Relative Present Value ($/Mg Biomass)"
    } else {
        y_var <- "NPV"
        y_label <- "Net Present Value ($/Mg Biomass)"
    }

    p <- ggplot(results_df, aes(x = Carbon_Price, y = .data[[y_var]], color = Technology)) +
        geom_line(size = 1.2) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        labs(
            title = paste(y_label, "vs Carbon Price"),
            x = "Carbon Price ($/tCO2e)",
            y = y_label
        ) +
        theme_minimal() +
        theme(
            legend.position = "bottom",
            text = element_text(size = 14)
        )

    return(p)
}
