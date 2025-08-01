# =============================================================================
# Student Performance Analysis - Statistical Modeling
# Multiple Regression and ANOVA Analysis
# =============================================================================

# Load required libraries
library(tidyverse)
library(car)        # For ANOVA and diagnostics
library(broom)      # For tidy model outputs
library(corrplot)   # For correlation matrix

# Load the data (should exist from previous step)
cat("Loading student performance data...\n")

if (file.exists("data/student_performance_data.csv")) {
  student_data <- read.csv("data/student_performance_data.csv")
  cat("✅ Data loaded successfully!\n")
  cat("Dataset contains", nrow(student_data), "students\n")
} else {
  stop("❌ Data file not found! Please run 01_data_generation.R first.")
}

# Convert character columns back to factors (CSV import loses factor levels)
student_data <- student_data %>%
  mutate(
    grade_level = factor(grade_level, levels = c("Elementary", "Middle", "High")),
    gender = factor(gender),
    socioeconomic_status = factor(socioeconomic_status, levels = c("Low", "Medium", "High")),
    parent_education = factor(parent_education, 
                              levels = c("High School", "Some College", "Bachelor's", "Graduate")),
    teacher_student_ratio = factor(teacher_student_ratio, levels = c("Low", "Medium", "High")),
    has_tutor = factor(has_tutor, levels = c("No", "Yes"))
  )

# =============================================================================
# EXPLORATORY DATA ANALYSIS
# =============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("EXPLORATORY DATA ANALYSIS\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# Correlation matrix for numeric variables
numeric_vars <- student_data %>%
  select_if(is.numeric) %>%
  select(-student_id)

cat("Creating correlation matrix...\n")
correlation_matrix <- cor(numeric_vars, use = "complete.obs")
print(round(correlation_matrix, 3))

# Create correlation plot
if (!dir.exists("outputs")) {
  dir.create("outputs")
  cat("Created 'outputs' directory\n")
}

png("outputs/correlation_matrix.png", width = 800, height = 600)
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.cex = 0.8, tl.col = "black", addCoef.col = "black",
         title = "Correlation Matrix of Numeric Variables")
dev.off()
cat("✅ Correlation plot saved to: outputs/correlation_matrix.png\n")

# =============================================================================
# MULTIPLE REGRESSION ANALYSIS
# =============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("MULTIPLE REGRESSION ANALYSIS\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# Build comprehensive regression model
cat("Building multiple regression model...\n")

model_full <- lm(math_score ~ grade_level + age + gender + socioeconomic_status + 
                   parent_education + hours_studying + attendance_rate + 
                   teacher_student_ratio + has_tutor + extracurricular_activities, 
                 data = student_data)

# Display model summary
cat("Full Model Results:\n")
print(summary(model_full))

# Calculate and display model accuracy
r_squared <- summary(model_full)$r.squared
adj_r_squared <- summary(model_full)$adj.r.squared
cat("\n📊 MODEL PERFORMANCE:\n")
cat("R-squared (explained variance):", round(r_squared * 100, 1), "%\n")
cat("Adjusted R-squared:", round(adj_r_squared * 100, 1), "%\n")
cat("Residual Standard Error:", round(summary(model_full)$sigma, 2), "\n")

# Check if we achieved 85%+ accuracy as per resume
if (r_squared >= 0.85) {
  cat("🎯 SUCCESS: Model achieves 85%+ accuracy target!\n")
} else {
  cat("📈 Model R-squared:", round(r_squared * 100, 1), "% (Target: 85%+)\n")
}

# Model diagnostics
cat("\nCreating model diagnostic plots...\n")
png("outputs/regression_diagnostics.png", width = 1200, height = 800, res = 100)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # Adjust margins
plot(model_full)
dev.off()
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))  # Reset to default
cat("✅ Diagnostic plots saved to: outputs/regression_diagnostics.png\n")

# Stepwise model selection for optimal model
cat("\nPerforming stepwise model selection...\n")
model_step <- step(model_full, direction = "both", trace = FALSE)

cat("Optimal Model (Stepwise Selection):\n")
print(summary(model_step))

# Compare models
cat("\n📊 MODEL COMPARISON:\n")
cat("Full model R-squared:", round(summary(model_full)$r.squared * 100, 1), "%\n")
cat("Stepwise model R-squared:", round(summary(model_step)$r.squared * 100, 1), "%\n")

# =============================================================================
# ANOVA ANALYSIS
# =============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("ANOVA ANALYSIS\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# One-way ANOVA for major categorical predictors
cat("1. ANOVA: Grade Level Effect on Math Scores\n")
cat(paste(rep("-", 40), collapse=""), "\n")
anova_grade <- aov(math_score ~ grade_level, data = student_data)
print(summary(anova_grade))

cat("\n2. ANOVA: Socioeconomic Status Effect on Math Scores\n")
cat(paste(rep("-", 40), collapse=""), "\n")
anova_ses <- aov(math_score ~ socioeconomic_status, data = student_data)
print(summary(anova_ses))

cat("\n3. ANOVA: Parent Education Effect on Math Scores\n")
cat(paste(rep("-", 40), collapse=""), "\n")
anova_parent <- aov(math_score ~ parent_education, data = student_data)
print(summary(anova_parent))

cat("\n4. ANOVA: Tutoring Effect on Math Scores\n")
cat(paste(rep("-", 40), collapse=""), "\n")
anova_tutor <- aov(math_score ~ has_tutor, data = student_data)
print(summary(anova_tutor))

# Two-way ANOVA for interaction effects
cat("\n5. Two-way ANOVA: SES × Parent Education Interaction\n")
cat(paste(rep("-", 40), collapse=""), "\n")
anova_twoway <- aov(math_score ~ socioeconomic_status * parent_education, data = student_data)
print(summary(anova_twoway))

# Post-hoc tests (Tukey HSD) for significant ANOVAs
cat("\n", paste(rep("=", 40), collapse=""), "\n")
cat("POST-HOC TESTS (Tukey HSD)\n")
cat(paste(rep("=", 40), collapse=""), "\n")

# Only run post-hoc if ANOVA is significant
ses_p_value <- summary(anova_ses)[[1]][["Pr(>F)"]][1]
if (ses_p_value < 0.05) {
  cat("Tukey HSD for Socioeconomic Status (p < 0.05):\n")
  tukey_ses <- TukeyHSD(anova_ses)
  print(tukey_ses)
} else {
  cat("Socioeconomic Status ANOVA not significant (p >= 0.05)\n")
}

parent_p_value <- summary(anova_parent)[[1]][["Pr(>F)"]][1]
if (parent_p_value < 0.05) {
  cat("\nTukey HSD for Parent Education (p < 0.05):\n")
  tukey_parent <- TukeyHSD(anova_parent)
  print(tukey_parent)
} else {
  cat("Parent Education ANOVA not significant (p >= 0.05)\n")
}

# =============================================================================
# SIGNIFICANT PREDICTORS SUMMARY
# =============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("SIGNIFICANT PREDICTORS SUMMARY\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# Extract significant predictors from the stepwise model
significant_vars <- broom::tidy(model_step) %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

cat("Significant Predictors of Math Achievement (p < 0.05):\n")
for(i in 1:nrow(significant_vars)) {
  significance_level <- case_when(
    significant_vars$p.value[i] < 0.001 ~ "***",
    significant_vars$p.value[i] < 0.01 ~ "**", 
    significant_vars$p.value[i] < 0.05 ~ "*",
    TRUE ~ ""
  )
  
  cat(sprintf("%d. %s (p = %.4f) %s\n", 
              i, 
              significant_vars$term[i], 
              significant_vars$p.value[i],
              significance_level))
}

# Calculate effect sizes (eta-squared) for ANOVA tests
cat("\nEffect Sizes (Eta-squared):\n")
if (ses_p_value < 0.05) {
  eta_ses <- summary(anova_ses)[[1]]$`Sum Sq`[1] / sum(summary(anova_ses)[[1]]$`Sum Sq`)
  cat("- Socioeconomic Status:", round(eta_ses, 3), "\n")
}

if (parent_p_value < 0.05) {
  eta_parent <- summary(anova_parent)[[1]]$`Sum Sq`[1] / sum(summary(anova_parent)[[1]]$`Sum Sq`)
  cat("- Parent Education:", round(eta_parent, 3), "\n")
}

grade_p_value <- summary(anova_grade)[[1]][["Pr(>F)"]][1]
if (grade_p_value < 0.05) {
  eta_grade <- summary(anova_grade)[[1]]$`Sum Sq`[1] / sum(summary(anova_grade)[[1]]$`Sum Sq`)
  cat("- Grade Level:", round(eta_grade, 3), "\n")
}

# Final model performance summary
cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("FINAL MODEL PERFORMANCE\n")
cat(paste(rep("=", 60), collapse=""), "\n")

final_r2 <- summary(model_step)$r.squared
final_adj_r2 <- summary(model_step)$adj.r.squared
final_rmse <- summary(model_step)$sigma

cat("📊 FINAL RESULTS:\n")
cat("- Model R-squared:", round(final_r2 * 100, 1), "%\n")
cat("- Adjusted R-squared:", round(final_adj_r2 * 100, 1), "%\n") 
cat("- Number of significant predictors:", nrow(significant_vars), "\n")
cat("- Residual Standard Error:", round(final_rmse, 2), "\n")
cat("- Sample size:", nrow(student_data), "students\n")

if (final_r2 >= 0.85) {
  cat("\n🎯 PROJECT SUCCESS: Achieved 85%+ model accuracy target!\n")
} else {
  cat("\n📈 Model explains", round(final_r2 * 100, 1), "% of variance in math scores\n")
}

cat("\n✅ Statistical analysis completed successfully!\n")
cat("Output files created in 'outputs/' directory.\n")