library(multcomp)
library(dplyr)
library(ggplot2)

# Function to categorize associations
# Helper function to classify each coefficient
classify_coefficient_glht <- function(estimate, p_value, ci_range, estimated_strength) {
  ci_category <- ifelse(ci_range > 1, "wide", "narrow")
  
  if (p_value < 0.05) {
    if (estimated_strength == "weak") return("(1) Weak estimated association and in favor of association")
    if (estimated_strength == "moderate") return("(2) Moderate estimated association and in favor of association")
    if (estimated_strength == "strong") return("(3) Strong estimated association and in favor of association")
  } else if (p_value >= 0.05 && p_value < 0.2) {
    if (estimated_strength == "weak") return("(4) Weak estimated association and possible association")
    if (estimated_strength == "moderate") return("(5) Moderate estimated association and possible association")
    if (estimated_strength == "strong") return("(6) Strong estimated association and possible association")
  } else if (p_value >= 0.2) {
    if (estimated_strength == "weak") {
      if (ci_category == "wide") return("(7) Wide CI: inconclusive")
      if (ci_category == "narrow") return("(8) Narrow CI: In favor of no association")
    }
    if (estimated_strength == "moderate") return("(9) Inconclusive")
    if (estimated_strength == "strong") return("(10) Inconclusive")
  }
  return("Invalid input")
}

# Function to extract confidence intervals and categorize based on overlap
extract_glht_ci <- function(glht_obj, level = 0.95) {
  # Get the confidence intervals
  ci <- confint(glht_obj, level = level)$confint
  
  # Create a data frame for the CI
  ci_df <- data.frame(
    term = rownames(ci),
    estimate = ci[, 1],
    lwr = ci[, 2],
    upr = ci[, 3],
    p_value = summary(glht_obj)$test$pvalues
  )
  
  # Determine the range category
  ci_df$range <- with(ci_df, upr - lwr)
  
  ci_df$estimated_strength <- with(ci_df, ifelse(abs(estimate) < 0.5, "weak",
                                                 ifelse(abs(estimate) < 1, "moderate", "strong")))
  
  # Assign association category
  ci_df$category <- mapply(classify_coefficient_glht, ci_df$estimate, ci_df$p_value, ci_df$range, ci_df$estimated_strength)
  
  return(ci_df)
}

# Plotting function
plot_glht_ci <- function(glht_obj, level = 0.95) {
  ci_df <- extract_glht_ci(glht_obj, level)
  
  ggplot(ci_df, aes(x = reorder(term, estimate), y = estimate, ymin = lwr, ymax = upr, color = as.factor(category))) +
    geom_pointrange() +
    coord_flip() +
    labs(
      x = "Terms",
      y = "Estimate",
      color = "Category",
      title = "Confidence Intervals from glht"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed")+
    theme_bw()
}
