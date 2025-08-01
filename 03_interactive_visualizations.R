
#Load required packages
library(tidyverse)
library(plotly)
library(DT) # For interactive data tables 

# Load the data
cat("Loading student performance data for visualizations...\n")

if (file.exists("data/student_performance_data.csv")) {
  student_data <- read.csv("data/student_performance_data.csv")
  cat("✅ Data loaded successfully!\n")
} else {
  stop("❌ Data file not found! Please run 01_data_generation.R first.")
}

# Convert back to factors (CSV loses factor levels)
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

# Create outputs directory if needed
if (!dir.exists("outputs")) {
  dir.create("outputs")
}

# =============================================================================
# VISUALIZATION 1: Study Hours vs Math Scores (Interactive Scatter Plot)
# =============================================================================

cat("\nCreating interactive scatter plot: Study Hours vs Math Scores...\n")

# Create base plot
p1 <- ggplot(student_data, aes(x = hours_studying, y = math_score, 
                               color = socioeconomic_status,
                               text = paste("Student ID:", student_id,
                                            "<br>Grade Level:", grade_level,
                                            "<br>Study Hours:", round(hours_studying, 1),
                                            "<br>Math Score:", round(math_score, 1),
                                            "<br>SES:", socioeconomic_status,
                                            "<br>Has Tutor:", has_tutor))) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  labs(title = "Study Hours vs Math Achievement by Socioeconomic Status",
       subtitle = "Interactive plot - hover for student details",
       x = "Hours Studying per Day",
       y = "Math Score (0-100)",
       color = "Socioeconomic Status") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "bottom") +
  scale_color_manual(values = c("Low" = "#d62728", "Medium" = "#ff7f0e", "High" = "#2ca02c"))

# Convert to interactive plot
interactive_p1 <- ggplotly(p1, tooltip = "text") %>%
  layout(title = list(text = "Study Hours vs Math Achievement by Socioeconomic Status<br><sub>Interactive plot - hover for student details</sub>"))

# Save interactive plot
htmlwidgets::saveWidget(interactive_p1, "outputs/study_hours_vs_scores.html", selfcontained = TRUE)
cat("✅ Interactive scatter plot saved to: outputs/study_hours_vs_scores.html\n")

# Display in RStudio viewer
interactive_p1

# =============================================================================
# VISUALIZATION 2: Box Plot - Math Scores by Grade Level and Gender
# =============================================================================

cat("\nCreating interactive box plot: Math Scores by Grade Level and Gender...\n")

p2 <- ggplot(student_data, aes(x = grade_level, y = math_score, fill = gender)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  geom_jitter(aes(color = gender), width = 0.2, alpha = 0.3) +
  labs(title = "Math Score Distribution by Grade Level and Gender",
       subtitle = "Box plots with individual data points",
       x = "Grade Level",
       y = "Math Score (0-100)",
       fill = "Gender",
       color = "Gender") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "bottom") +
  scale_fill_manual(values = c("Male" = "#1f77b4", "Female" = "#ff7f0e")) +
  scale_color_manual(values = c("Male" = "#1f77b4", "Female" = "#ff7f0e"))

# Convert to interactive
interactive_p2 <- ggplotly(p2) %>%
  layout(title = list(text = "Math Score Distribution by Grade Level and Gender<br><sub>Box plots with individual data points</sub>"))

# Save interactive plot
htmlwidgets::saveWidget(interactive_p2, "outputs/scores_by_grade_gender.html", selfcontained = TRUE)
cat("✅ Interactive box plot saved to: outputs/scores_by_grade_gender.html\n")

# Display in RStudio viewer
interactive_p2

# =============================================================================
# VISUALIZATION 3: Bar Chart - Average Scores by Parent Education and SES
# =============================================================================

cat("\nCreating interactive bar chart: Average Scores by Parent Education...\n")

# Calculate average scores
avg_scores <- student_data %>%
  group_by(parent_education, socioeconomic_status) %>%
  summarise(
    avg_score = mean(math_score),
    count = n(),
    std_error = sd(math_score) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    hover_text = paste("Parent Education:", parent_education,
                       "<br>SES:", socioeconomic_status,
                       "<br>Average Score:", round(avg_score, 1),
                       "<br>Number of Students:", count,
                       "<br>Std Error:", round(std_error, 2))
  )

p3 <- ggplot(avg_scores, aes(x = parent_education, y = avg_score, 
                             fill = socioeconomic_status,
                             text = hover_text)) +
  geom_col(position = "dodge", alpha = 0.8, color = "white", size = 0.5) +
  geom_errorbar(aes(ymin = avg_score - std_error, ymax = avg_score + std_error),
                position = position_dodge(width = 0.9), width = 0.25, alpha = 0.7) +
  labs(title = "Average Math Scores by Parent Education and Socioeconomic Status",
       subtitle = "Error bars show standard error of the mean",
       x = "Parent Education Level",
       y = "Average Math Score",
       fill = "Socioeconomic Status") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_fill_manual(values = c("Low" = "#d62728", "Medium" = "#ff7f0e", "High" = "#2ca02c"))

# Convert to interactive
interactive_p3 <- ggplotly(p3, tooltip = "text") %>%
  layout(title = list(text = "Average Math Scores by Parent Education and Socioeconomic Status<br><sub>Error bars show standard error of the mean</sub>"))

# Save interactive plot
htmlwidgets::saveWidget(interactive_p3, "outputs/scores_by_parent_education.html", selfcontained = TRUE)
cat("✅ Interactive bar chart saved to: outputs/scores_by_parent_education.html\n")

# Display in RStudio viewer
interactive_p3

# =============================================================================
# VISUALIZATION 4: Heatmap - Correlation Matrix
# =============================================================================

cat("\nCreating interactive correlation heatmap...\n")

# Prepare numeric data for correlation
numeric_data <- student_data %>%
  select_if(is.numeric) %>%
  select(-student_id)

# Calculate correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Convert to long format for ggplot
cor_data <- cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
  mutate(
    correlation_text = round(correlation, 3),
    hover_text = paste("Variable 1:", var1,
                       "<br>Variable 2:", var2, 
                       "<br>Correlation:", correlation_text)
  )

p4 <- ggplot(cor_data, aes(x = var1, y = var2, fill = correlation, text = hover_text)) +
  geom_tile(color = "white") +
  geom_text(aes(label = correlation_text), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  labs(title = "Correlation Matrix of Numeric Variables",
       subtitle = "Values range from -1 (negative) to +1 (positive correlation)",
       x = "Variables", y = "Variables") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

# Convert to interactive
interactive_p4 <- ggplotly(p4, tooltip = "text") %>%
  layout(title = list(text = "Correlation Matrix of Numeric Variables<br><sub>Values range from -1 (negative) to +1 (positive correlation)</sub>"))

# Save interactive plot
htmlwidgets::saveWidget(interactive_p4, "outputs/correlation_heatmap.html", selfcontained = TRUE)
cat("✅ Interactive correlation heatmap saved to: outputs/correlation_heatmap.html\n")

# Display in RStudio viewer
interactive_p4

# =============================================================================
# VISUALIZATION 5: Interactive Data Table for Stakeholders
# =============================================================================

cat("\nCreating interactive data table for stakeholder exploration...\n")

# Create summary table for stakeholders
summary_table <- student_data %>%
  select(student_id, grade_level, gender, socioeconomic_status, parent_education,
         hours_studying, attendance_rate, has_tutor, math_score) %>%
  mutate(
    hours_studying = round(hours_studying, 1),
    attendance_rate = round(attendance_rate * 100, 1),  # Convert to percentage
    math_score = round(math_score, 1)
  ) %>%
  rename(
    "Student ID" = student_id,
    "Grade Level" = grade_level,
    "Gender" = gender,
    "Socioeconomic Status" = socioeconomic_status,
    "Parent Education" = parent_education,
    "Daily Study Hours" = hours_studying,
    "Attendance %" = attendance_rate,
    "Has Tutor" = has_tutor,
    "Math Score" = math_score
  )

# Create interactive table
interactive_table <- datatable(
  summary_table,
  caption = "Student Performance Dataset - Interactive Exploration",
  filter = "top",  # Add column filters
  options = list(
    pageLength = 25,
    scrollX = TRUE,
    autoWidth = TRUE,
    columnDefs = list(list(className = 'dt-center', targets = "_all"))
  ),
  class = "cell-border stripe hover"
) %>%
  formatStyle(
    "Math Score",
    backgroundColor = styleInterval(c(60, 80), c("#ffcccc", "#ffffcc", "#ccffcc"))
  )

# Save interactive table
htmlwidgets::saveWidget(interactive_table, "outputs/interactive_data_table.html", selfcontained = TRUE)
cat("✅ Interactive data table saved to: outputs/interactive_data_table.html\n")

# Display in RStudio viewer
interactive_table

# =============================================================================
# SUMMARY OF CREATED VISUALIZATIONS
# =============================================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("INTERACTIVE VISUALIZATIONS COMPLETED\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("📊 Created 5 interactive visualizations:\n")
cat("1. ✅ Study Hours vs Math Scores (Scatter Plot)\n")
cat("   → outputs/study_hours_vs_scores.html\n")
cat("2. ✅ Score Distribution by Grade/Gender (Box Plot)\n") 
cat("   → outputs/scores_by_grade_gender.html\n")
cat("3. ✅ Average Scores by Parent Education (Bar Chart)\n")
cat("   → outputs/scores_by_parent_education.html\n")
cat("4. ✅ Correlation Matrix (Heatmap)\n")
cat("   → outputs/correlation_heatmap.html\n")
cat("5. ✅ Interactive Data Exploration Table\n")
cat("   → outputs/interactive_data_table.html\n")

cat("\n🎯 All visualizations are stakeholder-ready with:\n")
cat("- Hover tooltips for detailed information\n")
cat("- Professional styling and color schemes\n")
cat("- Clear titles and subtitles\n")
cat("- Interactive filtering and exploration\n")

cat("\n🌐 To view visualizations:\n")
cat("- Open any .html file in your web browser\n")
cat("- Or view in RStudio Viewer pane (should open automatically)\n")

cat("\n✅ Interactive visualization phase completed successfully!\n")