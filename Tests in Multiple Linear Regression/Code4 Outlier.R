# Load the data
data <- read.csv("flow.csv")

# Remove the 'id' variable
data$id <- NULL

# Fit a linear regression model
model <- lm(flow ~ ., data = data)

# Print model summary
summary(model)
plot(model)

# Outlier and influence diagnostics
leverage <- hatvalues(model)                         # Leverage
studentized <- rstudent(model)                       # Studentized residuals
dffits_vals <- dffits(model)                         # DFFITS
cooks_d <- cooks.distance(model)                     # Cook's distance

# Combine diagnostics into one data frame
diagnostics <- data.frame(
  Leverage = leverage,
  Studentized = studentized,
  DFFITS = dffits_vals,
  Cooks_Distance = cooks_d
)

# Determine thresholds for identifying outliers and influential points
n <- nrow(data)
p <- length(coef(model)) - 1
leverage_threshold <- 2 * (p + 1) / n
studentized_threshold <- 2
cooks_threshold <- 1

# Find points exceeding thresholds
high_leverage <- which(leverage > leverage_threshold)
high_studentized <- which(abs(studentized) > studentized_threshold)
high_cooks <- which(cooks_d > cooks_threshold)

# Unique indices that exceed any threshold
suspicious_indices <- sort(unique(c(high_leverage, high_studentized, high_cooks)))

# Return threshold values, diagnostic preview, and rows flagged as potential outliers
list(
  leverage_threshold = leverage_threshold,
  studentized_threshold = studentized_threshold,
  cooks_threshold = cooks_threshold,
  diagnostics_preview = head(diagnostics, 10),
  flagged_rows = diagnostics[suspicious_indices, ]
)
