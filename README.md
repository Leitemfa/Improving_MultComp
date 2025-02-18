# Improving_MultComp

This repository aims to improve the output generated from multiple comparisons

It is based on the paper: I know what I shouldn’t say, but what should I say? An approach to present results without statistical significance I really liked the table presented in this paper. It might be something interesting to consider in future analysis <https://doi.org/10.3389/fams.2024.1487750>

Because of that I want to create a new funcion to decribe the results in line with their suggestions

# Tutorial to run from githu

First download the folder with the scripts and then use the `source` function to extract the code

``` {r}
system("git clone https://github.com/Leitemfa/Improving_MultComp.git")
```

I want to make it in a way that you can extract their output from the simple lm model and also for multiple comparisons. For that last one, I made a code from the `multcomp` package.

# Loading libraries

For running this code you will need the following packages.

```{r}
library(tidyverse)
library(multcomp)
```

# Running the simple lm model

For this example I will use the classic data from the `cars` library

```{r}
#loading the source code
source("Improving_MultComp/Categorize_lm.R")

# Example usage
lm_model <- lm(mpg ~ wt+hp+qsec+vs+am, data = mtcars)
summary(lm_model)
View(categorize_association(lm_model))

lm_model <- lm(mpg ~ -1 +wt+hp+qsec+vs+am, data = mtcars)
summary(lm_model)
View(categorize_association(lm_model))

# Output: Detailed summary with classifications for each coefficient

```

As you can see the output now extracted the p-value and the confidence interval for the estimates of every regression coefficient. Then, we can look at its range and create a classification for each estimate from the linear model.

We can also make a plot out of this output

```{r}
source("Improving_MultComp/plot_association.R")
plot_association(lm_model)

```

Next step: made the code to also include mixed models

# Multiple comparisons

As I said I also want to make it so we can extract the same type of classification from multiple comparisons. For that I have choosen to start with the `multcomp` package

```{r}
source("Improving_MultComp/Categories_glht.R")

### multiple linear model, swiss data
lmod <- lm(Fertility ~ ., data = swiss)

### test of H_0: all regression coefficients are zero 
### (ignore intercept)

### define coefficients of linear function directly
K <- diag(length(coef(lmod)))[-1,]
rownames(K) <- names(coef(lmod))[-1]
K

### set up general linear hypothesis
lmod.glht <- glht(lmod, linfct = K)
str(lmod.glht)
summary(lmod.glht)
View(extract_glht_ci(lmod.glht))
plot_glht_ci(lmod.glht)
### alternatively, use a symbolic description 
### instead of a matrix 
lmod.glht2 <- glht(lmod, linfct = c("Agriculture = 0",
                      "Examination = 0",
                      "Education = 0",
                      "Catholic = 0",
                      "Infant.Mortality = 0"))
extract_glht_ci(lmod.glht2)
plot_glht_ci(lmod.glht2)

### multiple comparison procedures
### set up a one-way ANOVA
amod <- aov(breaks ~ tension, data = warpbreaks)

### set up all-pair comparisons for factor `tension'
### using a symbolic description (`type' argument 
### to `contrMat()')
lmod.glht3 <- glht(amod, linfct = mcp(tension = "Tukey"))
extract_glht_ci(lmod.glht3)
plot_glht_ci(lmod.glht3)

### alternatively, describe differences symbolically
lmod.glht4 <- glht(amod, linfct = mcp(tension = c("M - L = 0", 
                                    "H - L = 0",
                                    "H - M = 0")))
extract_glht_ci(lmod.glht3)
plot_glht_ci(lmod.glht3)

### mix of one- and two-sided alternatives
warpbreaks.aov <- aov(breaks ~ wool + tension,
                      data = warpbreaks)

### contrasts for `tension'
K <- rbind("L - M" = c( 1, -1,  0),     
           "M - L" = c(-1,  1,  0),       
           "L - H" = c( 1,  0, -1),     
           "M - H" = c( 0,  1, -1))

warpbreaks.mc <- glht(warpbreaks.aov, 
                      linfct = mcp(tension = K),
                      alternative = "less")
confint(warpbreaks.mc, level = 0.95)$confint
extract_glht_ci(warpbreaks.mc)
plot_glht_ci(warpbreaks.mc)

```
