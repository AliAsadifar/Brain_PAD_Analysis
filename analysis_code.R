> # List of necessary packages
  > required_packages <- c("tidyverse", "readxl", "irr", "BlandAltmanLeh", "pwr", "ggplot2", "reshape2", "lme4", "lmerTest", "emmeans")
  > 
    > # Install packages if not already installed
    > installed_packages <- rownames(installed.packages())
    > for(pkg in required_packages){
      +     if(!(pkg %in% installed_packages)){
        +         install.packages(pkg, dependencies = TRUE)
        +     }
      + }
    > 
      > # Load the libraries
      > library(tidyverse)
    ── Attaching core tidyverse packages ─────────────────────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ✔ purrr     1.0.2     
    ── Conflicts ───────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package to force all conflicts to become errors
    > library(readxl)
    > library(irr)
    Loading required package: lpSolve
    > library(BlandAltmanLeh)
    > library(pwr)
    > library(ggplot2)
    > library(reshape2)
    
    Attaching package: ‘reshape2’
    
    The following object is masked from ‘package:tidyr’:
      
      smiths
    
    > library(lme4)
    Loading required package: Matrix
    
    Attaching package: ‘Matrix’
    
    The following objects are masked from ‘package:tidyr’:
      
      expand, pack, unpack
    
    > library(lmerTest)
    
    Attaching package: ‘lmerTest’
    
    The following object is masked from ‘package:lme4’:
      
      lmer
    
    The following object is masked from ‘package:stats’:
      
      step
    
    > library(emmeans)
    Welcome to emmeans.
    Caution: You lose important information if you filter this package's results.
See '? untidy'
> 
> # Read data from Excel file
> data <- read_excel("E:/Asadithesis/FINAL_RESULTS_with_Brain_PAD.xlsx")
> 
> # View the first few rows to confirm data loading
> head(data)
# A tibble: 6 × 12
  ID      Scanner   Run   Gender Gender_Code Birth_Date          MS_Type Estimated_Age_Y
  <chr>   <chr>     <chr> <chr>        <dbl> <dttm>              <chr>             <dbl>
1 sub-h01 GE        run-1 M                1 1983-10-13 00:00:00 HC                 29.0
2 sub-h01 GE        run-2 M                1 1983-10-13 00:00:00 HC                 29.1
3 sub-h01 PhilipsDV run-1 M                1 1983-10-13 00:00:00 HC                 27.3
4 sub-h01 PhilipsDV run-2 M                1 1983-10-13 00:00:00 HC                 27.3
5 sub-h01 Toshiba   run-1 M                1 1983-10-13 00:00:00 HC                 31.1
6 sub-h01 Toshiba   run-2 M                1 1983-10-13 00:00:00 HC                 29.8
# ℹ 4 more variables: Scan_Date <dttm>, Chronical_Age_At_Scan_Y_D <chr>,
#   Chronical_Age_Years_2dp <dbl>, Brain_PAD <dbl>
> 
> # Convert categorical variables to factors
> data_patients <- data %>%
+     mutate(
+         Scanner = factor(Scanner, levels = c("GE", "PhilipsDV", "Toshiba")),
+         Run = factor(Run, levels = c("run-1", "run-2")),
+         Gender = factor(Gender, levels = c("M", "F")),
+         MS_Type = factor(MS_Type)
+     )
> 
> # Check for missing values
> total_na <- sum(is.na(data_patients))
> print(paste("Total missing values in the dataset:", total_na))
[1] "Total missing values in the dataset: 0"
> 
> # Aggregate the two runs per scanner per patient by calculating the mean Brain_PAD
> data_aggregated <- data_patients %>%
+     group_by(ID, Scanner) %>%
+     summarize(
+         Brain_PAD_Mean = mean(Brain_PAD, na.rm = TRUE),
+         Chronical_Age_Years_2dp = unique(Chronical_Age_Years_2dp),
+         Gender = unique(Gender)
+     ) %>%
+     ungroup()
`summarise()` has grouped output by 'ID'. You can override using the `.groups` argument.
> 
> # Ensure that each patient has Brain_PAD_Mean for each scanner
> table(data_aggregated$ID, data_aggregated$Scanner)
         
          GE PhilipsDV Toshiba
  sub-h01  1         1       1
  sub-h02  1         1       1
  sub-h04  1         1       1
  sub-h09  1         1       1
  sub-h11  1         1       1
  sub-h22  1         1       1
  sub-p01  1         1       1
  sub-p09  1         1       1
  sub-p10  1         1       1
  sub-p13  1         1       1
  sub-p18  1         1       1
  sub-p22  1         1       1
  sub-p23  1         1       1
  sub-p28  1         1       1
  sub-p29  1         1       1
  sub-p38  1         1       1
  sub-p39  1         1       1
  sub-p43  1         1       1
  sub-p50  1         1       1
  sub-p55  1         1       1
  sub-p67  1         1       1
  sub-p76  1         1       1
  sub-p83  1         1       1
  sub-p84  1         1       1
  sub-p85  1         1       1
  sub-p86  1         1       1
  sub-p91  1         1       1
> 
> # Prepare data for ICC calculation
> icc_results_patients <- data_patients %>%
+     group_by(Scanner, ID) %>%
+     summarize(
+         run1 = Brain_PAD[Run == "run-1"],
+         run2 = Brain_PAD[Run == "run-2"]
+     ) %>%
+     ungroup()
`summarise()` has grouped output by 'Scanner'. You can override using the `.groups` argument.
> 
> # Compute ICC(3,1) for each scanner
> icc_list_patients <- list()
> for(s in unique(icc_results_patients$Scanner)){
+     subset_data <- icc_results_patients %>% filter(Scanner == s) %>% select(run1, run2)
+     icc_value <- icc(subset_data, model = "twoway", type = "agreement", unit = "single")
+     icc_list_patients[[s]] <- icc_value$value
+ }
> 
> # Display ICC results
> print(icc_list_patients)
$GE
[1] 0.991974

$PhilipsDV
[1] 0.9904882

$Toshiba
[1] 0.9985189

> 
> # Fit the Linear Mixed-Effects Model
> model_lmer <- lmer(Brain_PAD_Mean ~ Scanner + (1 | ID), data = data_aggregated)
> 
> # View the summary of the model
> summary(model_lmer)
Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
    Formula: Brain_PAD_Mean ~ Scanner + (1 | ID)
    Data: data_aggregated
    
    REML criterion at convergence: 451.7
    
    Scaled residuals: 
      Min      1Q  Median      3Q     Max 
    -3.3621 -0.3239 -0.0147  0.4152  1.6772 
    
    Random effects:
      Groups   Name        Variance Std.Dev.
    ID       (Intercept) 109.812  10.48   
    Residual               3.804   1.95   
    Number of obs: 81, groups:  ID, 27
    
    Fixed effects:
      Estimate Std. Error      df t value Pr(>|t|)    
    (Intercept)        7.8256     2.0513 27.1937   3.815 0.000714 ***
      ScannerPhilipsDV  -2.7241     0.5308 52.0000  -5.132 4.34e-06 ***
      ScannerToshiba     2.7944     0.5308 52.0000   5.264 2.71e-06 ***
      ---
      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    Correlation of Fixed Effects:
      (Intr) ScnPDV
    ScnnrPhlpDV -0.129       
    ScannerTshb -0.129  0.500
    > 
      > # Obtain Estimated Marginal Means for Scanner
      > emm_scanner <- emmeans(model_lmer, "Scanner")
    > 
      > # Perform Pairwise Comparisons with Tukey Adjustment
      > pairwise_comparisons <- pairs(emm_scanner, adjust = "tukey")
    > 
      > # View the Pairwise Comparisons
      > print(pairwise_comparisons)
    contrast            estimate    SE df t.ratio p.value
    GE - PhilipsDV          2.72 0.531 52   5.132  <.0001
    GE - Toshiba           -2.79 0.531 52  -5.264  <.0001
    PhilipsDV - Toshiba    -5.52 0.531 52 -10.396  <.0001
    
    Degrees-of-freedom method: kenward-roger 
    P value adjustment: tukey method for comparing a family of 3 estimates 
    > 
      > # Save the results to a CSV file
      > write.csv(as.data.frame(pairwise_comparisons), "TukeyHSD_Patient_Scanners.csv")
    > 
      > # Diagnostic Plots for the Linear Mixed-Effects Model
      > 
      > # Residuals vs Fitted
      > png("Diagnostic_Plots_LMM_Residuals_vs_Fitted.png")
    > plot(model_lmer, which = 1)
    > dev.off()
    null device 
    1 
    > 
      > # Normal Q-Q Plot
      > png("Diagnostic_Plots_LMM_Normal_QQ.png")
    > qqnorm(resid(model_lmer))
    > qqline(resid(model_lmer))
    > dev.off()
    null device 
    1 
    > 
      > # Scale-Location Plot
      > png("Diagnostic_Plots_LMM_Scale_Location.png")
    > plot(model_lmer, which = 3)
    > dev.off()
    null device 
    1 
    > 
      > # Residuals vs Leverage
      > png("Diagnostic_Plots_LMM_Residuals_vs_Leverage.png")
    > plot(model_lmer, which = 5)
    > dev.off()
    null device 
    1 
    > 