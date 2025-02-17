# Function to categorize association from lm summary with extended output
categorize_association <- function(lm_model) {
  # Extract coefficients, p-values, and confidence intervals
  coef_summary <- summary(lm_model)$coefficients
  ci <- confint(lm_model)
  
  # Helper function to classify each coefficient
  classify_coefficient <- function(estimate, p_value, ci_range, estimated_strength) {
    ci_category <- ifelse(ci_range > 1, "wide", "narrow")
    
    if (p_value < 0.01) {
      if (estimated_strength == "weak") return("(1) Weak estimated association and in favor of association")
      if (estimated_strength == "moderate") return("(2) Moderate estimated association and in favor of association")
      if (estimated_strength == "strong") return("(3) Strong estimated association and in favor of association")
    } else if (p_value >= 0.01 && p_value < 0.2) {
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
  
  # Loop through each coefficient and classify
  result <- data.frame(Term = rownames(coef_summary),
                       Estimate = coef_summary[, 1],
                       Std_Error = coef_summary[, 2],
                       t_value = coef_summary[, 3],
                       p_value = coef_summary[, 4],
                       CI_Lower = ci[, 1],
                       CI_Upper = ci[, 2],
                       CI_Range = abs(ci[, 2] - ci[, 1]),
                       Classification = NA)
  
  # Define estimated strength (you may adjust based on context)
  result$Estimated_Strength <- ifelse(abs(result$Estimate) < 1, "weak", 
                                      ifelse(abs(result$Estimate) < 5, "moderate", "strong"))
  
  result$Classification <- mapply(classify_coefficient, result$Estimate, result$p_value, result$CI_Range, result$Estimated_Strength)
  
  return(result)
}

# Example usage
lm_model <- lm(mpg ~ wt, data = mtcars)
View(categorize_association(lm_model))
# Output: Detailed summary with classifications for each coefficient
