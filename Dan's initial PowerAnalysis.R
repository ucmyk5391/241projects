set.seed(123)

# Function to generate random user IDs
generate_user_id <- function(n) {
  replicate(n, paste0(sample(c(0:9, letters, LETTERS), 12, replace = TRUE), collapse = ""))
}

n <- 10000

# Define baseline conversion rates and lifts
baseline_rates <- c(0.01, 0.03, 0.10)
lifts <- c(0.03, 0.09, 0.15)

# Nested loops for different combinations of baseline conversion rates and lifts
for (baseline in baseline_rates) {
  for (lift in lifts) {
    
    # Create the dataframe for each combination
    df <- data.frame(
      user_id = generate_user_id(n), 
      test_group = sample(c("treatment", "control"), n, replace = TRUE),
      applied = NA
    )
    
    # Apply baseline conversion for the control group
    df$applied[df$test_group == "control"] <- rbinom(sum(df$test_group == "control"), 1, baseline)
    
    # Apply lift for the treatment group (baseline + lift)
    df$applied[df$test_group == "treatment"] <- rbinom(sum(df$test_group == "treatment"), 1, baseline * (lift + 1))
    
    # Initialize vector to store power by sample size
    power_by_size <- numeric(length = length(seq(0.01, 1, by = 0.01)))
    percentages_to_sample <- seq(0.01, 1, by = 0.01)
    
    # Loop through different sample sizes
    for (i in seq_along(percentages_to_sample)) {
      sample_size <- floor(percentages_to_sample[i] * nrow(df)) # Calculate actual sample size (percentage * 10000)
      
      if (sample_size == 0) next # Skip iteration if sample size is zero
      
      t_test_p_values <- rep(NA, 100)
      
      for (j in 1:100) {
        # Determine sample sizes for treatment and control groups
        treatment_sample_size <- floor(sample_size / 2)
        control_sample_size <- sample_size - treatment_sample_size
        
        # Sample treatment and control data
        treatment_sample <- df[df$test_group == "treatment", ]
        control_sample <- df[df$test_group == "control", ]
        
        treatment_sample <- treatment_sample[sample(1:nrow(treatment_sample), 
                                                    size = min(treatment_sample_size, nrow(treatment_sample)), 
                                                    replace = TRUE), ]
        control_sample <- control_sample[sample(1:nrow(control_sample), 
                                                size = min(control_sample_size, nrow(control_sample)), 
                                                replace = TRUE), ]
        
        # Combine treatment and control samples
        sampled_data <- rbind(treatment_sample, control_sample)
        
        # Perform t-test on the sampled data
        t_test <- t.test(applied ~ test_group, data = sampled_data)
        t_test_p_values[j] <- t_test$p.value
      }
      
      # Calculate proportion of rejections (power) for this sample size
      t_test_rejects <- t_test_p_values < 0.05
      power_by_size[i] <- mean(t_test_rejects)
    }
    
    # Plot power by actual sample size
    plot(percentages_to_sample * n, power_by_size, type = 'b', pch = 19, 
         xlab = "Sample Size", ylab = "Power", 
         main = paste0("Baseline: ", baseline*100, "%, Lift: ", lift*100, "%"),
         ylim = c(0,1))
    
  }
}
