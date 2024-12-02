
#===============================================================================
# Rasmus Duret
# This is a short helper function which tracks amount of time spent per iteration in a loop, with an additional option for dynamically plotting the iteration time.
# This function can be imported from a helper file, or simply copied into your file.
# Then, it only needs 2 extra lines of code: initialise the function at the beginning of the loop, and to update it each iteration.
# An example is provided at the end.
#===============================================================================


#===============================================================================
# 1 FUNCTION
#===============================================================================

# Function to initialize timing stats with optional plotting parameter
# Args: total_iterations (length of the loop), message (text to display), plot_on_update (whether to plot in the window), moving_avg_window (how many iters to average time over).
initialise_progress <- function(total_iterations, message = "Processing...", plot_on_update = FALSE, moving_avg_window = 5) {
  start_time <- Sys.time()
  cat(sprintf("%s | Start time: %s\n", message, start_time))  # Print custom message and start time
  
  # Initialize timing statistics as a data.frame
  timing_stats <- data.frame(
    Iteration = integer(0),
    Time = numeric(0)  # Store individual iteration times
  )
  
  # List to store the last n iteration times for moving average
  moving_avg_times <- numeric(0)
  
  list(
    start_time = start_time,
    total_iterations = total_iterations,
    timing_stats = timing_stats,
    moving_avg_times = moving_avg_times,
    plot_on_update = plot_on_update,
    moving_avg_window = moving_avg_window
  )
}

# Function to update and print progress with optional plot update
update_progress <- function(progress_info, current_iteration) {
  # Record the time for this iteration
  current_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(current_time, progress_info$start_time, units = "secs"))
  iteration_time <- if (current_iteration > 1) {
    elapsed_time - sum(progress_info$timing_stats$Time)
  } else {
    elapsed_time
  }
  
  # Update timing stats
  progress_info$timing_stats <- rbind(
    progress_info$timing_stats,
    data.frame(Iteration = current_iteration, Time = iteration_time)
  )
  

  # Calculate moving average (over all available iterations, or last 'moving_avg_window' iterations)
  if (length(progress_info$moving_avg_times) < progress_info$moving_avg_window) {
    avg_time <- mean(progress_info$timing_stats$Time)
  } else {
    avg_time <- mean(tail(progress_info$timing_stats$Time, progress_info$moving_avg_window))
  }
  
  # Store the last n iteration times for moving average
  progress_info$moving_avg_times <- c(progress_info$moving_avg_times, avg_time)


  # Compute max and min times
  max_time <- max(progress_info$timing_stats$Time)
  min_time <- min(progress_info$timing_stats$Time)
  
  # Calculate remaining time
  remaining_time <- avg_time * (progress_info$total_iterations - current_iteration)
  percentage <- (current_iteration / progress_info$total_iterations) * 100
  
  # Print progress and stats on a single line
  # Function to format time dynamically
  format_time <- function(seconds) {
    if (seconds >= 3600) {
      sprintf("%.2f hr", seconds / 3600)
    } else if (seconds >= 60) {
      sprintf("%.2f min", seconds / 60)
    } else {
      sprintf("%.2f sec", seconds)
    }
  }
  
  cat(sprintf("\rProgress: %d/%d (%.2f%%) | Avg (last %s): %s | Max: %s | Min: %s | Rem: %s", 
              current_iteration, 
              progress_info$total_iterations, 
              percentage, 
              progress_info$moving_avg_window,
              format_time(avg_time), 
              format_time(max_time), 
              format_time(min_time), 
              format_time(remaining_time)))
  
  # If selected, redraw the plot
  if (progress_info$plot_on_update) {
    plot_iteration_times(progress_info)
  }
  
  # Print completion message if finished
  if (current_iteration == progress_info$total_iterations) {
    end_time <- Sys.time()
    total_time <- as.numeric(difftime(end_time, progress_info$start_time, units = "secs"))
    cat(sprintf("\nEnd time: %s | Total elapsed time: %s\n", end_time, format_time(total_time)))
    cat("Done!\n")
  }
  
  return(progress_info)
}

# Function to plot iteration times (with moving average red line)
plot_iteration_times <- function(progress_info) {
  timing_stats <- progress_info$timing_stats
  
  # Plot the times for individual iterations (blue dots)
  plot(timing_stats$Iteration, timing_stats$Time, type = "b", pch = 19, col = "blue",
       xlab = "Iteration", ylab = "Time (seconds)", main = "Iteration Times",
       ylim = c(0, max(timing_stats$Time, na.rm = TRUE)))
  
  # Extract the moving average times directly from the stored values in progress_info
  moving_avg_times <- progress_info$moving_avg_times
  moving_avg = moving_avg_times

  
  # Add a red line for the moving average
  lines(1:length(moving_avg), moving_avg, col = "red", lwd = 2, lty = 2)  # Red dashed line for moving average
  
}


#===============================================================================
# 2 Example
#===============================================================================

# This does iteration time for a process with random but positive-trending iteration time

# Total iterations
n <- 100

# Initialize progress tracker with plot updates on every iteration
progress_info <- initialise_progress(n, message = "Tracking iteration times", plot_on_update = TRUE, moving_avg_window = 10)

# Example loop
for (i in 1:n) {
  Sys.sleep(log(i) * runif(1, 0.01, 0.5))  # Simulate work with random time
  
  # Update progress and timing
  progress_info <- update_progress(progress_info, i)
}
