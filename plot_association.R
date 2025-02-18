# Function to plot coefficients with association categories
plot_association <- function(lm_model) {
  library(ggplot2)
  
  categorized_data <- categorize_association(lm_model)
  
  ggplot(categorized_data, aes(x = Estimate, y = Term)) +
    geom_point(aes(color = Classification), size = 3) +
    geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper, height = 0.2)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    # scale_color_manual(values = c("weak" = "green", "moderate" = "orange", "strong" = "red")) +
    theme_minimal() +
    labs(title = "Coefficient Estimates and Association Categories",
         x = "Estimate",
         y = "Term",
         color = "Categories")
}
