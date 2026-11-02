library(ggplot2)
library(broom)

annotate_lm <- function(p, x = NULL, y = NULL, slope = F, R2 = T, adjR2 = F, 
                        pv = T, intercept = F, bias = F, xpos = NULL, 
                        ypos = NULL, round_n = 3, hjust = 0, vjust = 1, 
                        size = 3, colour = 'black') {
  # Extract mapping from ggplot object if x/y not supplied
  mapping <- p$mapping
  
  if (is.null(x)) x <- rlang::as_name(mapping$x)
  if (is.null(y)) y <- rlang::as_name(mapping$y)
  
  # Extract data
  data <- p$data
  
  # Fit linear model
  fit <- lm(as.formula(paste(y, "~", x)), data = data)
  fit_summary <- summary(fit)
  
  slopeval <- coef(fit)[2]
  intval <- coef(fit)[1]
  r2val <- fit_summary$r.squared
  adjR2val <- fit_summary$adj.r.squared
  pval <- coef(fit_summary)[2, 4]
  bias_val <- Metrics::bias(data[[x]], data[[y]])
  
  # Format string
  label <- ''
  if (slope == T) label <- paste0(label, 'Slope = ', round(slopeval, round_n), '\n')
  if (intercept == T) label <- paste0(label, 'Intercept = ', round(intval, round_n), '\n')
  if (R2 == T) label <- paste0(label, 'R² = ', round(r2val, round_n), '\n')
  if (adjR2 == T) label <- paste0(label, 'R²adj = ', round(adjR2val, round_n), '\n')
  if (pv == T) label <- paste0(label, 'p = ', round(pval, round_n), '\n')
  if (bias == T) label <- paste0(label, 'Bias = ', round(bias_val, round_n), '\n')
  
  # Default position if not supplied
  if (is.null(xpos)) xpos <- min(data[[x]], na.rm = TRUE)
  if (is.null(ypos)) ypos <- max(data[[y]], na.rm = TRUE)
  
  # Stated x and y positions
  if (xpos == 'min') xpos <- min(data[[x]], na.rm = TRUE)
  if (xpos == 'max') xpos <- max(data[[x]], na.rm = TRUE)
  
  if (ypos == 'min') ypos <- min(data[[y]], na.rm = TRUE)
  if (ypos == 'max') ypos <- max(data[[y]], na.rm = TRUE)
  
  
  # Add annotation
  p + annotate("text", x = xpos, y = ypos, label = label, hjust = hjust, vjust = vjust, size = size,
               col = colour)
}


lm_to_df <- function(x, y){
  fit <- lm(y ~ x)
  fit_summary <- summary(fit)
  
  slopeval <- coef(fit)[2]
  intval <- coef(fit)[1]
  r2val <- fit_summary$r.squared
  adjR2val <- fit_summary$adj.r.squared
  pval <- coef(fit_summary)[2, 4]
  
  df <- tibble(slope = slopeval, intercept = intval, R2 = r2val, 
               adj_R2 = adjR2val, p = pval)
  return(df)
}
