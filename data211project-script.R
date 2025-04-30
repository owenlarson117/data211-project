library(ggplot2)
library(dplyr)

# Convert time to seconds
log_data <- read.csv("log-02-23-Copy.csv")

log_data_renamed <- log_data %>%
  rename(
    Intake_Temp = `Intake.Air.Temperature..C.`,
    Engine_Load = `Engine.Load....`,
    Throttle    = `Electronic.throttle.control.actual..deg.`,
    RPM         = `Engine.speed..RPM.`
  ) %>%
  mutate(Time_s = Time / 1000)

# Plot intake temp over time, color = RPM
ggplot(log_data_renamed, aes(x = Time_s, y = Intake_Temp, color = RPM)) +
  geom_line(linewidth = 1.2) + 
  geom_vline(xintercept = 45, linetype = "dashed", color = "black") + 
  annotate("text", x = 50, y = max(log_data_renamed$Intake_Temp), label = "Heat Soak Begins ?", hjust = 0) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(
    title = "Heat Soak Over Time: Intake Temperature vs RPM",
    x = "Time (s)",
    y = "Intake Air Temperature (°C)",
    color = "Engine RPM"
  ) +
  
  theme_minimal(base_size = 16)

# Plot intake temp vs rpm, color = Throttle
ggplot(log_data_renamed, aes(x = RPM, y = Intake_Temp, color = Throttle)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(
    title = "High RPM + High Throttle = Falling Intake Temps",
    x = "Engine RPM",
    y = "Intake Air Temperature (°C)",
    color = "Throttle Position (°)"
  ) +
  theme_minimal(base_size = 16)

cai_data <- log_data_renamed %>%
  filter(Throttle > 80, RPM > 3000) %>%
  mutate(Time_s = Time / 1000)

# Plot: Intake Temp over Time, colored by RPM or Speed
ggplot(cai_data, aes(x = Time_s, y = Intake_Temp, color = RPM)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.4) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(
    title = "Cold Air Intake Effect During Full Throttle Pulls",
    x = "Time (s)",
    y = "Intake Air Temperature (°C)",
    color = "Engine RPM"
  ) +
  theme_minimal(base_size = 16)
