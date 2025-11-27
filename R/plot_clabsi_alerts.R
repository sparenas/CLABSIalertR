#' Plot u-chart with dual y-axes (CLABSI counts & sigma values)
#'
#' @param df Data frame from alert_poisson_clabsi()
#' @return ggplot object
#' @import ggplot2
#' @export
plot_clabsi_alerts <- function(df) {
  

  # --- Compute raw expected CLABSI for counts ---
  df$expected_clabsi <- df$expected
  
  # --- Compute sigma values (z-scores) ---
  df$z <- (df$clabsi - df$expected_clabsi) / sqrt(df$expected_clabsi)
  
  # --- Add SPC rule flags (Western Electric style) ---
  df <- add_spc_rules(df, z_col = "z", y_col = "clabsi")
  
  
  # --- Create readable and ordered Month-Year label ---
  if ("year" %in% names(df)) {
    df$month_year <- factor(
      paste(df$year, df$month, sep = "-"),
      levels = unique(paste(df$year, df$month, sep = "-"))
    )
  } else {
    df$month_year <- factor(df$month, levels = df$month)
  }
  
  
  # Ensure proper ordering
  df$month_year <- paste0(month.name[df$month], "-", df$year)
  
  df$month_year <- factor(df$month_year, levels = unique(df$month_year))
  
  #defining baseline
  centerline <- mean(df$expected_clabsi, na.rm = TRUE)
  df$centerline<-centerline

  
  #---dyna,ic label---------
  centerline_label <- paste0("Centerline = ", round(centerline, 2))
  
  
  # --- Compute sigma values (z-scores) ---
  df$z <- (df$clabsi - df$expected_clabsi) / sqrt(df$expected_clabsi)

  
  # Extract UCL/LCL centerline in raw units
  UCL_line <- mean(df$UCL)
  LCL_line <- mean(df$LCL)
 

  
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = month_year)) +
    
    # Raw CLABSI line
    ggplot2::geom_line(ggplot2::aes(y = clabsi), color = "black", linewidth = 0.8) +
    ggplot2::geom_point(
      ggplot2::aes(y = clabsi,  color = spc_flag,
                   shape = spc_flag),
      size = 2
    )+
    
    # ------------------ CONTROL LINES ------------------
  
  # Centerline
  ggplot2::geom_hline(
    ggplot2::aes(yintercept = centerline, linetype = "centerline"),
    color = "blue", linewidth = 1
  ) +
    
    # UCL (step line)
    ggplot2::geom_step(
      ggplot2::aes(y = UCL, linetype = "UCL (+3 SD)", group = 1),
      color = "red", linewidth = 0.9
    ) +
    
    # LCL (step line)
    ggplot2::geom_step(
      ggplot2::aes(y = LCL, linetype = "LCL (-3 SD)", group = 1),
      color = "orange", linewidth = 0.9
    ) +
    
    # Y-axis: CLABSI counts
    ggplot2::scale_y_continuous(
      name = "Observed CLABSI (raw count)"
    ) +
    
    # Color legend
   
    ggplot2::scale_color_manual(
       name = "SPC Rules",
       breaks = c(
         "No Rules",
         "Rule 1: Point beyond 3 SD",
         "Rule 2: 8 points on one side",
         "Rule 3: 6-point trend"
       ),
       labels = c(
         "No Rules",
         "Rule 1: Point beyond 3 SD",
         "Rule 2: 8 points on one side",
         "Rule 3: 6-point trend"
       ),
       values = c(
         "No Rules"                     = "black",
         "Rule 1: Point beyond 3 SD"      = "red",
         "Rule 2: 8 points on one side" = "forestgreen",
         "Rule 3: 6-point trend"        = "darkorchid2"
       )
     )+
  ggplot2::scale_shape_manual(
    name = "SPC Rules",
    breaks = c(
      "No Rules",
      "Rule 1: Point beyond 3 SD",
      "Rule 2: 8 points on one side",
      "Rule 3: 6-point trend"
    ),
    labels = c(
      "No Rules",
      "Rule 1: Point beyond 3 SD",
      "Rule 2: 8 points on one side",
      "Rule 3: 6-point trend"
    ),
    values = c(
      "No Rules"                     = 21,
      "Rule 1: Point beyond 3 SD"      = 16,  # circle
      "Rule 2: 8 points on one side" = 17,  # triangle
      "Rule 3: 6-point trend"        = 20   # diamond
    )
  )+

    
    # Linetype legend
    ggplot2::scale_linetype_manual(
      name = "Control Lines",
      values = c(
        "centerline" = "dotted",
        "UCL (+3 SD)"  = "dashed",
        "LCL (-3 SD)"  = "dotdash"
      )
    ) +
    
    ggplot2::labs(
      title = "u-Chart: CLABSI Control Chart with Poisson Limits",
      x = "Month-Year"
    ) +
    
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
      legend.position = "right",
      panel.grid.minor = ggplot2::element_blank()
    )

  return (p)
}
  