R version 4.4.1 (2024-06-14 ucrt) -- "Race for Your Life"
Copyright (C) 2024 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64




> setwd("E:/Asadithesis")
> # Load packages
> library(lme4)
> library(lmerTest)
> library(ggplot2)
Use suppressPackageStartupMessages() to eliminate package startup messages
> library(reshape2)
> library(viridis)
Loading required package: viridisLite
> library(corrplot)
corrplot 0.95 loaded
> library(emmeans)
> library(simr)

Attaching package: ‘simr’

The following object is masked from ‘package:lme4’:

    getData

> library(readxl)
> library(BlandAltmanLeh)
> 
> # Define the path to your Excel data file
> data_file_path <- "E:/Asadithesis/FINAL_RESULTS_with_Brain_PAD.xlsx"
> 
> # Read the data from the Excel file
> # If your file has multiple sheets, specify the sheet name or index
> data <- read_excel(data_file_path, sheet = 1)  # Adjust 'sheet' parameter if needed
> 
> # Ensure correct data types
> data$ID <- factor(data$ID)
> data$Scanner <- factor(trimws(data$Scanner))
> data$Run <- factor(trimws(data$Run))
> data$Gender <- factor(data$Gender)
> data$Scanner_Run <- paste(data$Scanner, data$Run, sep = "_")
> 
> # View the structure of the data
> str(data)
tibble [162 × 13] (S3: tbl_df/tbl/data.frame)
 $ ID                       : Factor w/ 27 levels "sub-h01","sub-h02",..: 1 1 1 1 1 1 2 2 2 2 ...
 $ Scanner                  : Factor w/ 3 levels "GE","PhilipsDV",..: 1 1 2 2 3 3 1 1 2 2 ...
 $ Run                      : Factor w/ 2 levels "run-1","run-2": 1 2 1 2 1 2 1 2 1 2 ...
 $ Gender                   : Factor w/ 2 levels "F","M": 2 2 2 2 2 2 2 2 2 2 ...
 $ Gender_Code              : num [1:162] 1 1 1 1 1 1 1 1 1 1 ...
 $ Birth_Date               : POSIXct[1:162], format: "1983-10-13" "1983-10-13" "1983-10-13" ...
 $ MS_Type                  : chr [1:162] "HC" "HC" "HC" "HC" ...
 $ Estimated_Age_Y          : num [1:162] 29 29.1 27.3 27.3 31.1 ...
 $ Scan_Date                : POSIXct[1:162], format: "2016-11-07" "2016-11-07" "2016-11-07" ...
 $ Chronical_Age_At_Scan_Y_D: chr [1:162] "33 Y 34 D" "33 Y 34 D" "33 Y 34 D" "33 Y 34 D" ...
 $ Chronical_Age_Years_2dp  : num [1:162] 33.1 33.1 33.1 33.1 33.1 ...
 $ Brain_PAD                : num [1:162] -4.06 -4.01 -5.77 -5.78 -1.97 -3.28 -5.94 -6.21 -7.05 -5.72 ...
 $ Scanner_Run              : chr [1:162] "GE_run-1" "GE_run-2" "PhilipsDV_run-1" "PhilipsDV_run-2" ...
> 
> 3.1 Fit Linear Mixed-Effects Models for Each Scanner
Error: unexpected symbol in "3.1 Fit"
> # List of scanners
> scanners <- unique(data$Scanner)
> 
> # Initialize a list to store models and results
> within_scanner_results <- list()
> 
> for (scanner in scanners) {
+     # Subset data for the scanner
+     scanner_data <- subset(data, Scanner == scanner)
+     
+     # Fit the linear mixed-effects model
+     model <- lmer(Brain_PAD ~ Run + (1 | ID), data = scanner_data)
+     
+     # Extract variance components
+     var_components <- as.data.frame(VarCorr(model))
+     sigma_ID <- var_components[1, 'vcov']
+     sigma_Residual <- var_components[2, 'vcov']
+     
+     # Calculate ICC and SEM
+     ICC_within <- sigma_ID / (sigma_ID + sigma_Residual)
+     SEM_within <- sqrt(sigma_Residual)
+     
+     # Store results
+     within_scanner_results[[as.character(scanner)]] <- list(
+         model = model,
+         ICC = ICC_within,
+         SEM = SEM_within
+     )
+     
+     # Print results
+     cat("Scanner:", scanner, "\n")
+     cat("ICC:", round(ICC_within, 4), "\n")
+     cat("SEM:", round(SEM_within, 4), "\n\n")
+ }
Scanner: GE 
ICC: 0.9918 
SEM: 0.9779 

Scanner: PhilipsDV 
ICC: 0.9906 
SEM: 1.028 

Scanner: Toshiba 
ICC: 0.9985 
SEM: 0.4125 

> 
> # Use data from Run 1 to avoid duplication
> between_scanner_data <- subset(data, Run == "run-1")
> 
> # Fit the linear mixed-effects model
> model_between <- lmer(Brain_PAD ~ Scanner + (1 | ID), data = between_scanner_data)
> 
> # Display the summary of the model
> summary(model_between)
Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
Formula: Brain_PAD ~ Scanner + (1 | ID)
   Data: between_scanner_data

REML criterion at convergence: 458.5

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-3.3880 -0.3885 -0.0193  0.4559  1.8596 

Random effects:
 Groups   Name        Variance Std.Dev.
 ID       (Intercept) 109.003  10.440  
 Residual               4.345   2.085  
Number of obs: 81, groups:  ID, 27

Fixed effects:
                 Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)        7.7315     2.0489 27.3723   3.773  0.00079 ***
ScannerPhilipsDV  -2.7878     0.5673 52.0000  -4.914 9.31e-06 ***
ScannerToshiba     2.8489     0.5673 52.0000   5.021 6.39e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
            (Intr) ScnPDV
ScnnrPhlpDV -0.138       
ScannerTshb -0.138  0.500
> 
> # Extract variance components
> var_components_between <- as.data.frame(VarCorr(model_between))
> sigma_ID_between <- var_components_between[1, 'vcov']
> sigma_Residual_between <- var_components_between[2, 'vcov']
> 
> # Since 'Scanner' is included as a fixed effect, we cannot extract its variance component directly
> # Alternative approach: Fit a model with 'Scanner' as a random effect to estimate its variance
> 
> # Fit the model with 'Scanner' as a random effect
> model_between_random <- lmer(Brain_PAD ~ 1 + (1 | ID) + (1 | Scanner), data = between_scanner_data)
> 
> # Extract variance components
> var_components_random <- as.data.frame(VarCorr(model_between_random))
> sigma_ID_random <- var_components_random[var_components_random$grp == "ID", 'vcov']
> sigma_Scanner_random <- var_components_random[var_components_random$grp == "Scanner", 'vcov']
> sigma_Residual_random <- var_components_random[var_components_random$grp == "Residual", 'vcov']
> 
> # Calculate ICC and SEM
> ICC_between <- sigma_Scanner_random / (sigma_ID_random + sigma_Scanner_random + sigma_Residual_random)
> SEM_between <- sqrt(sigma_Scanner_random + sigma_Residual_random)
> 
> # Print results
> cat("Between-Scanner ICC:", round(ICC_between, 4), "\n")
Between-Scanner ICC: 0.0642 
> cat("Between-Scanner SEM:", round(SEM_between, 4), "\n\n")
Between-Scanner SEM: 3.4825 

> 
> # Perform pairwise comparisons using emmeans
> emm <- emmeans(model_between, ~ Scanner)
> pairs <- contrast(emm, method = "pairwise", adjust = "tukey")
> 
> # Display the results
> print(pairs)
 contrast            estimate    SE df t.ratio p.value
 GE - PhilipsDV          2.79 0.567 52   4.914  <.0001
 GE - Toshiba           -2.85 0.567 52  -5.021  <.0001
 PhilipsDV - Toshiba    -5.64 0.567 52  -9.935  <.0001

Degrees-of-freedom method: kenward-roger 
P value adjustment: tukey method for comparing a family of 3 estimates 
> 
> # Function to create Bland-Altman plots
> library(BlandAltmanLeh)
> 
> create_bland_altman <- function(scanner1, scanner2, data) {
+     # Subset data for the two scanners
+     data_scanner1 <- subset(data, Scanner == scanner1 & Run == "run-1")
+     data_scanner2 <- subset(data, Scanner == scanner2 & Run == "run-1")
+     
+     # Merge data based on 'ID'
+     merged_data <- merge(data_scanner1[, c("ID", "Brain_PAD")],
+                          data_scanner2[, c("ID", "Brain_PAD")],
+                          by = "ID",
+                          suffixes = c(paste0("_", scanner1), paste0("_", scanner2)))
+     
+     # Calculate mean and differences
+     merged_data$Mean <- rowMeans(merged_data[, c(paste0("Brain_PAD_", scanner1), paste0("Brain_PAD_", scanner2))])
+     merged_data$Difference <- merged_data[, paste0("Brain_PAD_", scanner1)] - merged_data[, paste0("Brain_PAD_", scanner2)]
+     
+     # Create Bland-Altman plot
+     ba_plot <- bland.altman.plot(merged_data[, paste0("Brain_PAD_", scanner1)],
+                                  merged_data[, paste0("Brain_PAD_", scanner2)],
+                                  main = paste("Bland-Altman Plot:", toupper(scanner1), "vs.", toupper(scanner2)),
+                                  xlab = "Mean Brain-PAD",
+                                  ylab = "Difference in Brain-PAD",
+                                  pch = 19, col.points = "blue",
+                                  graph.sys = "ggplot2")
+     
+     # Save the plot
+     ggsave(filename = paste0("Bland_Altman_", scanner1, "_vs_", scanner2, ".png"), plot = ba_plot, width = 6, height = 4, dpi = 300)
+     
+     # Calculate mean difference and limits of agreement
+     mean_diff <- mean(merged_data$Difference)
+     sd_diff <- sd(merged_data$Difference)
+     loa_upper <- mean_diff + 1.96 * sd_diff
+     loa_lower <- mean_diff - 1.96 * sd_diff
+     
+     # Print statistics
+     cat("Comparison:", toupper(scanner1), "vs.", toupper(scanner2), "\n")
+     cat("Mean difference:", round(mean_diff, 2), "years\n")
+     cat("SD of differences:", round(sd_diff, 2), "years\n")
+     cat("Limits of agreement:", round(loa_lower, 2), "to", round(loa_upper, 2), "years\n\n")
+ }
> 
> # Generate Bland-Altman plots for each pair of scanners
> scanner_pairs <- combn(scanners, 2)
> 
> for (i in 1:ncol(scanner_pairs)) {
+     create_bland_altman(scanner_pairs[1, i], scanner_pairs[2, i], data)
+ }
Comparison: GE vs. PHILIPSDV 
Mean difference: 2.79 years
SD of differences: 2.14 years
Limits of agreement: -1.41 to 6.98 years

Comparison: GE vs. TOSHIBA 
Mean difference: -2.85 years
SD of differences: 3.14 years
Limits of agreement: -8.99 to 3.3 years

Comparison: PHILIPSDV vs. TOSHIBA 
Mean difference: -5.64 years
SD of differences: 3.41 years
Limits of agreement: -12.33 to 1.06 years

> 
> for (scanner in scanners) {
+     # Subset data for the scanner
+     scanner_data <- subset(data, Scanner == scanner)
+     
+     # Fit the within-scanner model
+     model_within <- lmer(Brain_PAD ~ Run + (1 | ID), data = scanner_data)
+     
+     # Residuals vs. Fitted Values Plot
+     resid_fitted_plot <- ggplot(data = data.frame(Fitted = fitted(model_within), Residuals = resid(model_within)), 
+                                 aes(x = Fitted, y = Residuals)) +
+         geom_point(alpha = 0.6) +
+         geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
+         labs(title = paste("Residuals vs. Fitted Values -", toupper(scanner), "Scanner"),
+              x = "Fitted Values",
+              y = "Residuals") +
+         theme_minimal()
+     
+     # Save the plot
+     ggsave(filename = paste0("Diagnostic_Residuals_vs_Fitted_", scanner, ".png"), plot = resid_fitted_plot, width = 6, height = 4, dpi = 300)
+     
+     # Q-Q Plot of Residuals
+     qq_plot <- ggplot(data = data.frame(Residuals = resid(model_within)), aes(sample = Residuals)) +
+         stat_qq(alpha = 0.6) +
+         stat_qq_line(color = "red") +
+         labs(title = paste("Q-Q Plot of Residuals -", toupper(scanner), "Scanner"),
+              x = "Theoretical Quantiles",
+              y = "Sample Quantiles") +
+         theme_minimal()
+     
+     # Save the plot
+     ggsave(filename = paste0("Diagnostic_QQ_Plot_", scanner, ".png"), plot = qq_plot, width = 6, height = 4, dpi = 300)
+ }
> 
> # Use data from Run 1
> between_scanner_data <- subset(data, Run == "run-1")
> 
> # Fit the between-scanner model
> model_between <- lmer(Brain_PAD ~ Scanner + (1 | ID), data = between_scanner_data)
> 
> # Residuals vs. Fitted Values Plot
> resid_fitted_between <- ggplot(data = data.frame(Fitted = fitted(model_between), Residuals = resid(model_between)), 
+                                aes(x = Fitted, y = Residuals)) +
+     geom_point(alpha = 0.6) +
+     geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
+     labs(title = "Residuals vs. Fitted Values - Between-Scanner Model",
+          x = "Fitted Values",
+          y = "Residuals") +
+     theme_minimal()
> 
> # Save the plot
> ggsave(filename = "Diagnostic_Residuals_vs_Fitted_Between_Scanner.png", plot = resid_fitted_between, width = 6, height = 4, dpi = 300)
> 
> # Q-Q Plot of Residuals
> qq_plot_between <- ggplot(data = data.frame(Residuals = resid(model_between)), aes(sample = Residuals)) +
+     stat_qq(alpha = 0.6) +
+     stat_qq_line(color = "red") +
+     labs(title = "Q-Q Plot of Residuals - Between-Scanner Model",
+          x = "Theoretical Quantiles",
+          y = "Sample Quantiles") +
+     theme_minimal()
> 
> # Save the plot
> ggsave(filename = "Diagnostic_QQ_Plot_Between_Scanner.png", plot = qq_plot_between, width = 6, height = 4, dpi = 300)
> 
> # Define the effect size (difference between scanners)
> effect_size <- 5  # Clinically significant difference in Brain-PAD (years)
> 
> # Use the between-scanner model as the base
> power_model <- extend(model_between, along = "ID", n = 50)  # Extend to 50 subjects
> 
> # Specify the effect to test (difference between scanners)
> fixed_effects <- fixef(power_model)
> fixed_effects["Scannerphilipsdv"] <- fixed_effects["Scannerphilipsdv"] + effect_size
> 
> # Update the model with the new fixed effect
> fixef(power_model) <- fixed_effects
Error in `fixef<-`(`*tmp*`, value = c(`(Intercept)` = 7.73148148148141,  : 
  Scannerphilipsdv is not the name of a fixed effect.
> # Define the effect size (difference between scanners)
> effect_size <- 5  # Clinically significant difference in Brain-PAD (years)
> 
> # Use the between-scanner model as the base
> power_model <- extend(model_between, along = "ID", n = 50)  # Extend to 50 subjects
> 
> # Specify the effect to test (difference between scanners)
> fixed_effects <- fixef(power_model)
> fixed_effects["Scannerphilipsdv"] <- fixed_effects["Scannerphilipsdv"] + effect_size
> 
> # Update the model with the new fixed effect
> fixef(power_model) <- fixed_effects
Error in `fixef<-`(`*tmp*`, value = c(`(Intercept)` = 7.73148148148141,  : 
  Scannerphilipsdv is not the name of a fixed effect.
> # Ensure 'Scanner', 'Run', and 'ID' are factors
> data$Scanner <- factor(data$Scanner)
> data$Run <- factor(data$Run)
> data$ID <- factor(data$ID)
> 
> # Create a combined 'Scanner_Run' variable
> data$Scanner_Run <- paste(data$Scanner, data$Run, sep = "_")
> 
> # Reshape data to wide format
> wide_data <- dcast(data, ID ~ Scanner_Run, value.var = "Brain_PAD")
> 
> # Convert data back to long format for plotting
> long_data <- melt(wide_data, id.vars = "ID", variable.name = "Scanner_Run", value.name = "Brain_PAD")
> 
> # Create the heat map
> heatmap_plot <- ggplot(long_data, aes(x = Scanner_Run, y = ID)) +
+     geom_tile(aes(fill = Brain_PAD), color = "white") +
+     scale_fill_viridis_c(option = "viridis", na.value = "grey90") +
+     labs(title = "Heat Map of Brain-PAD Measurements Across Scanners and Runs",
+          x = "Scanner and Run",
+          y = "Participant ID",
+          fill = "Brain-PAD") +
+     theme_minimal() +
+     theme(axis.text.x = element_text(angle = 45, hjust = 1))
> 
> # Save the heat map
> ggsave("HeatMap_Brain_PAD.png", plot = heatmap_plot, width = 8, height = 6, dpi = 300)
> 
> # Prepare data for correlation matrix
> wide_data_corr <- dcast(data, ID ~ Scanner_Run, value.var = "Brain_PAD")
> 
> # Remove rows with missing values
> wide_data_corr <- na.omit(wide_data_corr)
> 
> # Ensure there are at least two variables
> if (ncol(wide_data_corr) >= 3) {
+     # Calculate Spearman correlation matrix
+     cor_matrix <- cor(wide_data_corr[,-1], method = "spearman")
+     
+     # Generate heat map of the correlation matrix
+     corrplot(cor_matrix, method = "color", type = "upper",
+              tl.col = "black", tl.cex = 0.8,
+              title = "Spearman Correlation Heat Map of Brain-PAD Measurements",
+              mar = c(0,0,1,0))
+     
+     # Save the plot
+     ggsave("HeatMap_Spearman_Correlation.png", width = 6, height = 6, dpi = 300)
+ } else {
+     print("Not enough variables to compute a correlation matrix.")
+ }
> 
> # Calculate the Spearman correlation matrix
> cor_matrix <- cor(wide_data_corr[,-1], method = "spearman")
> 
> # Save the correlation matrix to a CSV file
> write.csv(cor_matrix, "Supplementary_Table_Correlation_Matrix.csv", row.names = TRUE)
> 
> 
