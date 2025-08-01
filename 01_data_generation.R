# =============================================================================
# Student Performance Analysis - Data Generation
# Test this part first before building the full analysis
# =============================================================================

# Load required libraries (run requirements.R first!)
library(tidyverse)

# Set seed for reproducibility
set.seed(42)

# Generate synthetic student performance data
cat("Generating student performance dataset...\n")

# Number of students (250+ as per resume)
n_students <- 250

# Create realistic student data
cat("Creating student demographics and factors...\n")

student_data <- data.frame(
  student_id = 1:n_students,
  
  # Demographics
  grade_level = sample(c("Elementary", "Middle", "High"), n_students, 
                       replace = TRUE, prob = c(0.4, 0.3, 0.3)),
  gender = sample(c("Male", "Female"), n_students, replace = TRUE),
  
  # Socioeconomic factors
  socioeconomic_status = sample(c("Low", "Medium", "High"), n_students, 
                                replace = TRUE, prob = c(0.3, 0.5, 0.2)),
  parent_education = sample(c("High School", "Some College", "Bachelor's", "Graduate"), 
                            n_students, replace = TRUE, prob = c(0.25, 0.35, 0.25, 0.15)),
  
  # Behavioral variables
  hours_studying = pmax(0, rnorm(n_students, mean = 2.5, sd = 1.2)),
  attendance_rate = pmax(0.5, pmin(1.0, rnorm(n_students, mean = 0.85, sd = 0.1))),
  has_tutor = sample(c(0, 1), n_students, replace = TRUE, prob = c(0.7, 0.3)),
  extracurricular_activities = sample(0:4, n_students, replace = TRUE, 
                                      prob = c(0.1, 0.3, 0.3, 0.2, 0.1)),
  
  # School factors
  teacher_student_ratio = sample(c("Low", "Medium", "High"), n_students, 
                                 replace = TRUE, prob = c(0.2, 0.6, 0.2))
)

# Add age based on grade level
student_data$age <- case_when(
  student_data$grade_level == "Elementary" ~ sample(6:10, n_students, replace = TRUE),
  student_data$grade_level == "Middle" ~ sample(11:13, n_students, replace = TRUE),
  student_data$grade_level == "High" ~ sample(14:18, n_students, replace = TRUE)
)

cat("Generating math scores based on realistic relationships...\n")

# Generate math scores with realistic relationships
student_data <- student_data %>%
  mutate(
    # Base score influenced by various factors
    base_score = 50 + 
      # Socioeconomic impact
      case_when(socioeconomic_status == "Low" ~ -8,
                socioeconomic_status == "Medium" ~ 2,
                socioeconomic_status == "High" ~ 12) +
      # Parent education impact
      case_when(parent_education == "High School" ~ -5,
                parent_education == "Some College" ~ 0,
                parent_education == "Bachelor's" ~ 8,
                parent_education == "Graduate" ~ 15) +
      # Study habits impact
      hours_studying * 5 +
      # Attendance impact
      (attendance_rate - 0.5) * 40 +
      # Tutoring impact
      has_tutor * 8 +
      # Activities impact (moderate)
      extracurricular_activities * 2 +
      # Teacher ratio impact
      case_when(teacher_student_ratio == "Low" ~ 10,
                teacher_student_ratio == "Medium" ~ 5,
                teacher_student_ratio == "High" ~ -3) +
      # Random variation
      rnorm(n_students, 0, 10),
    
    # Ensure realistic score range (0-100)
    math_score = pmax(0, pmin(100, base_score))
  ) %>%
  select(-base_score)  # Remove intermediate variable

# Convert categorical variables to factors with proper levels
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

# Display dataset summary
cat("\n" %+% "="*50 %+% "\n")
cat("DATASET SUMMARY\n")
paste(rep("=", 50), collapse="")
cat("Number of students:", nrow(student_data), "\n")
cat("Number of variables:", ncol(student_data), "\n")

cat("\nMath Score Summary:\n")
print(summary(student_data$math_score))

cat("\nGrade Level Distribution:\n")
print(table(student_data$grade_level))

cat("\nSocioeconomic Status Distribution:\n")
print(table(student_data$socioeconomic_status))

cat("\nFirst 6 rows of data:\n")
print(head(student_data))

# Save the dataset
write.csv(student_data, "data/student_performance_data.csv", row.names = FALSE)
cat("\n✅ Dataset saved to: data/student_performance_data.csv\n")

# Quick visualization test
cat("\nCreating a quick test plot...\n")
library(ggplot2)

test_plot <- ggplot(student_data, aes(x = hours_studying, y = math_score, color = grade_level)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Study Hours vs Math Scores by Grade Level",
       x = "Hours Studying per Day",
       y = "Math Score") +
  theme_minimal()

print(test_plot)

cat("\n🎉 Data generation completed successfully!\n")
cat("You can now proceed to the statistical analysis phase.\n")