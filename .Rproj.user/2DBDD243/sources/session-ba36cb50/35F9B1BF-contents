# =============================================================================
# Fall 2024 Student Performance Analysis - MASTER SCRIPT
# 
# Complete statistical analysis of factors affecting K-12 math achievement
# Author: [Your Name]
# Date: Fall 2024
# 
# This script performs:
# 1. Data generation (250+ synthetic student records)
# 2. Multiple regression analysis (targeting 85%+ accuracy)
# 3. ANOVA testing with post-hoc comparisons
# 4. Interactive visualizations for stakeholder presentations
# =============================================================================

# Clear environment and set working directory
rm(list = ls())
cat("Starting Student Performance Analysis...\n")
cat("=====================================\n\n")

# Record start time
start_time <- Sys.time()

# =============================================================================
# STEP 1: INSTALL AND LOAD REQUIRED PACKAGES
# =============================================================================

cat("STEP 1: Installing and loading required packages...\n")
cat("--------------------------------------------------\n")

# List of required packages
required_packages <- c(
  "tidyverse",    # Data manipulation and ggplot2
  "corrplot",     # Correlation plots  
  "plotly",       # Interactive plots
  "car",          # ANOVA and regression diagnostics
  "broom",        # Tidy statistical outputs
  "DT"            # Interactive data tables
)

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("Installing package:", pkg, "\n")
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    } else {
      cat("✓ Package", pkg, "loaded\n")
    }
  }
}

# Install missing packages
install_if_missing(required_packages)
cat("✅ All packages loaded successfully!\n\n")

# =============================================================================
# STEP 2: DATA GENERATION
# =============================================================================

cat("STEP 2: Generating synthetic student performance dataset...\n")
cat("--------------------------------------------------------\n")

# Set seed for reproducibility
set.seed(42)

# Number of students (250+ as per resume requirement)
n_students <- 250

# Create realistic student data
student_data <- data.frame(
  student_id = 1:n_students,
  grade_level = sample(c("Elementary", "Middle", "High"), n_students, 
                       replace = TRUE, prob = c(0.4, 0.3, 0.3)),
  gender = sample(c("Male", "Female"), n_students, replace = TRUE),
  socioeconomic_status = sample(c("Low", "Medium", "High"), n_students, 
                                replace = TRUE, prob = c(0.3, 0.5, 0.2)),
  parent_education = sample(c("High School", "Some College", "Bachelor's", "Graduate"), 
                            n_students, replace = TRUE, prob = c(0.25, 0.35, 0.25, 0.15)),
  hours_studying = pmax(0, rnorm(n_students, mean = 2.5, sd = 1.2)),
  attendance_rate = pmax(0.5, pmin(1.0, rnorm(n_students, mean = 0.85, sd = 0.1))),
  has_tutor = sample(c(0, 1), n_students, replace = TRUE, prob = c(0.7, 0.3)),
  extracurricular_activities = sample(0:4, n_students, replace = TRUE, 
                                      prob = c(0.1, 0.3, 0.3, 0.2, 0.1)),
  teacher_student_ratio = sample(c("Low", "Medium", "High"), n_students, 
                                 replace = TRUE, prob = c(0.2, 0.6, 0.2))
)

# Add age based on grade level
student_data$age <- case_when(
  student_data$grade_level == "Elementary" ~ sample(6:10, n_students, replace = TRUE),
  student_data$grade_level == "Middle" ~ sample(11:13, n_students, replace = TRUE),
  student_data$grade_level == "High" ~ sample(14:18, n_students, replace = TRUE)
)

# Generate math scores with realistic relationships
student_data <- student_data %>%
  mutate(
    base_score = 50 + 
      case_when(socioeconomic_status == "Low" ~ -8,
                socioeconomic_status == "Medium" ~ 2,
                socioeconomic_status == "High" ~ 12) +
      case_when(parent_education == "High School" ~ -5,
                parent_education == "Some College" ~ 0,
                parent_education == "Bachelor's" ~ 8,
                parent_education == "Graduate" ~ 15) +
      hours_studying * 5 + (attendance_rate - 0.5) * 40 + has_tutor * 8 +
      extracurricular_activities * 2 +
      case_when(teacher_student_ratio == "Low" ~ 10,
                teacher_student_ratio == "Medium" ~ 5,
                teacher_student_ratio == "High" ~ -3) +
      rnorm(n_students, 0, 10),
    math_score = pmax(0, pmin(100, base_score))
  ) %>%
  select(-base_score)

# Convert to factors
student_data <- student_data %>%
  mutate(
    grade_level = factor(grade_level, levels = c("Elementary", "Middle", "High")),
    gender = factor(gender),
    socioeconomic_status = factor(socioeconomic_status, levels = c("Low", "Medium", "High")),
    parent_education = factor(parent_education, 
                              levels = c("High School", "Some College", "Bachelor's", "Graduate")),
    teacher_student_ratio = factor(teacher_student_ratio, levels = c("Low", "Medium", "High")),
    has_tutor = factor(has_tutor, labels = c("No", "Yes"))
  )

# Create directories
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("outputs")) dir.create("outputs")

# Save dataset
write.csv(student_data, "data/student_performance_data.csv", row.names = FALSE)

cat("✅ Generated dataset with", nrow(student_data), "students\n")
cat("✅ Math scores range:", round(min(student_data$math_score), 1), "to", round(max(student_data$math_score), 1), "\n")
cat("✅ Dataset saved to: data/student_performance_data.csv\n\n")

# =============================================================================
# STEP 3: STATISTICAL ANALYSIS
# =============================================================================

cat("STEP 3: Performing statistical analysis...\n")
cat("------------------------------------------\n")

# Multiple regression analysis
cat("Building multiple regression model...\n")
model_full <- lm(math_score ~ grade_level + age + gender + socioeconomic_status + 
                   parent_education + hours_studying + attendance_rate + 
                   teacher_student_ratio + has_tutor + extracurricular_activities, 
                 data = student_data)

# Stepwise model selection
model_step <- step(model_full, direction = "both", trace = FALSE)

# Extract model performance
r_squared <- summary(model_step)$r.squared
adj_r_squared <- summary(model_step)$adj.r.squared

cat("✅ Model R-squared:", round(r_squared * 100, 1), "%\n")
cat("✅ Adjusted R-squared:", round(adj_r_squared * 100, 1), "%\n")

if (r_squared >= 0.85) {
  cat("🎯 SUCCESS: Achieved 85%+ accuracy target!\n")
}

# ANOVA analysis
cat("\nPerforming ANOVA tests...\n")
anova_ses <- aov(math_score ~ socioeconomic_status, data = student_data)
anova_parent <- aov(math_score ~ parent_education, data = student_data)
anova_grade <- aov(math_score ~ grade_level, data = student_data)

# Extract significant predictors
significant_vars <- broom::tidy(model_step) %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

cat("✅ Found", nrow(significant_vars), "significant predictors (p < 0.05)\n")

# Create correlation matrix plot
numeric_vars <- student_data %>% select_if(is.numeric) %>% select(-student_id)
correlation_matrix <- cor(numeric_vars, use = "complete.obs")

png("outputs/correlation_matrix.png", width = 800, height = 600)
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.cex = 0.8, tl.col = "black", addCoef.col = "black")
title("Correlation Matrix of Numeric Variables")
dev.off()

# Create diagnostic plots
png("outputs/regression_diagnostics.png", width = 1200, height = 800, res = 100)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(model_step)
dev.off()
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

cat("✅ Statistical analysis completed\n\n")

# =============================================================================
# STEP 4: INTERACTIVE VISUALIZATIONS
# =============================================================================

cat("STEP 4: Creating interactive visualizations...\n")
cat("----------------------------------------------\n")

# Visualization 1: Study Hours vs Math Scores
p1 <- ggplot(student_data, aes(x = hours_studying, y = math_score, 
                               color = socioeconomic_status,
                               text = paste("Student ID:", student_id,
                                            "<br>Grade:", grade_level,
                                            "<br>Study Hours:", round(hours_studying, 1),
                                            "<br>Math Score:", round(math_score, 1),
                                            "<br>SES:", socioeconomic_status))) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Study Hours vs Math Achievement by Socioeconomic Status",
       x = "Hours Studying per Day", y = "Math Score",
       color = "Socioeconomic Status") +
  theme_minimal() +
  scale_color_manual(values = c("Low" = "#d62728", "Medium" = "#ff7f0e", "High" = "#2ca02c"))

interactive_p1 <- ggplotly(p1, tooltip = "text")
htmlwidgets::saveWidget(interactive_p1, "outputs/study_hours_vs_scores.html", selfcontained = TRUE)

# Visualization 2: Box Plot by Grade Level and Gender
p2 <- ggplot(student_data, aes(x = grade_level, y = math_score, fill = gender)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Math Score Distribution by Grade Level and Gender",
       x = "Grade Level", y = "Math Score", fill = "Gender") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "#1f77b4", "Female" = "#ff7f0e"))

interactive_p2 <- ggplotly(p2)
htmlwidgets::saveWidget(interactive_p2, "outputs/scores_by_grade_gender.html", selfcontained = TRUE)

# Visualization 3: Average Scores by Parent Education
avg_scores <- student_data %>%
  group_by(parent_education, socioeconomic_status) %>%
  summarise(avg_score = mean(math_score), count = n(), .groups = "drop") %>%
  mutate(hover_text = paste("Parent Education:", parent_education,
                            "<br>SES:", socioeconomic_status,
                            "<br>Average Score:", round(avg_score, 1),
                            "<br>Count:", count))

p3 <- ggplot(avg_scores, aes(x = parent_education, y = avg_score, 
                             fill = socioeconomic_status, text = hover_text)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(title = "Average Math Scores by Parent Education and SES",
       x = "Parent Education Level", y = "Average Math Score",
       fill = "Socioeconomic Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Low" = "#d62728", "Medium" = "#ff7f0e", "High" = "#2ca02c"))

interactive_p3 <- ggplotly(p3, tooltip = "text")
htmlwidgets::saveWidget(interactive_p3, "outputs/scores_by_parent_education.html", selfcontained = TRUE)

# Create interactive data table
summary_table <- student_data %>%
  select(student_id, grade_level, gender, socioeconomic_status, parent_education,
         hours_studying, attendance_rate, has_tutor, math_score) %>%
  mutate(hours_studying = round(hours_studying, 1),
         attendance_rate = round(attendance_rate * 100, 1),
         math_score = round(math_score, 1))

interactive_table <- datatable(summary_table,
                               caption = "Student Performance Dataset - Interactive Exploration",
                               filter = "top", options = list(pageLength = 25, scrollX = TRUE))

htmlwidgets::saveWidget(interactive_table, "outputs/interactive_data_table.html", selfcontained = TRUE)

cat("✅ Created 4 interactive visualizations\n")
cat("✅ All HTML files saved to outputs/ directory\n\n")

# =============================================================================
# FINAL SUMMARY
# =============================================================================

end_time <- Sys.time()
analysis_time <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 1)

cat(paste(rep("=", 70), collapse=""), "\n")
cat("STUDENT PERFORMANCE ANALYSIS COMPLETED\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("📊 ANALYSIS RESULTS:\n")
cat("- Dataset size:", nrow(student_data), "students\n")
cat("- Model R-squared:", round(r_squared * 100, 1), "%\n")
cat("- Significant predictors:", nrow(significant_vars), "out of", length(coef(model_full))-1, "tested\n")
cat("- Analysis runtime:", analysis_time, "seconds\n")

cat("\n📁 FILES CREATED:\n")
cat("- data/student_performance_data.csv\n")
cat("- outputs/correlation_matrix.png\n")
cat("- outputs/regression_diagnostics.png\n")
cat("- outputs/study_hours_vs_scores.html\n")
cat("- outputs/scores_by_grade_gender.html\n")
cat("- outputs/scores_by_parent_education.html\n")
cat("- outputs/interactive_data_table.html\n")

cat("\n🎯 KEY FINDINGS:\n")
top_predictors <- head(significant_vars$term, 3)
for(i in 1:length(top_predictors)) {
  cat(paste0(i, ". ", top_predictors[i], "\n"))
}

if (r_squared >= 0.85) {
  cat("\n🏆 PROJECT SUCCESS: Achieved 85%+ model accuracy target!\n")
} else {
  cat("\n📈 Model explains", round(r_squared * 100, 1), "% of variance in math scores\n")
}

cat("\n✅ Analysis completed successfully! Ready for GitHub and portfolio.\n")
cat(paste(rep("=", 70), collapse=""), "\n")
