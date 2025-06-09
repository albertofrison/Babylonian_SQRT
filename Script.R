################################################################################
# Babylonian SQRT program - inspired by Keith McNulty in Linkedin
# Original Post: https://www.linkedin.com/posts/keith-mcnulty_analytics-python-datascience-activity-7337877746175410178-jkka?utm_source=share&utm_medium=member_desktop&rcm=ACoAAAEYNKMBrgLmnVcg_I3b00D0nnPi9iNBTJg
# This function calculates the square root of a number using the Babylonian
# method. It is an efficient, iterative algorithm that starts with an initial
# guess and repeatedly refines it to converge on the true square root.
#
# THE ALGORITHM:
# The core of the method is the following iterative formula, where we
# continuously update our guess to get a more accurate one:
#
# x_next = 0.5 * (x_current + S / x_current)
#
# This process is repeated until the guess is "good enough".
#
# PARAMETERS:
# S:            The non-negative number for which to find the square root.
#
# tolerance:    The desired precision. The algorithm stops when the change
#               between successive guesses is less than this value.
#
# max_iterations: A safety limit on the number of iterations to prevent
#                 the function from running indefinitely.
#
#-------------------------------------------------------------------------------

################################################################################


babylonian_sqrt <- function(S, tolerance = 1e-10, max_iterations = 100) {
  
  # --- 1. Input Validation ---
  if (S < 0) {
    warning("NaN produced: Input number cannot be negative.")
    return(NaN)
  }
  if (S == 0) {
    return(0)
  }
  
  # --- 2. Initial Guess ---
  # We'll use S itself as the initial guess. A closer guess like S/2
  # could also be used, but this is simple and robust.
  x <- S
  
  # --- 3. Iteration Loop ---
  for (i in 1:max_iterations) {
    # Apply the Babylonian formula
    x_next <- 0.5 * (x + S / x)
    
    # Check for convergence
    if (abs(x - x_next) < tolerance) {
      # If the result is precise enough, return it
      return(x_next)
    }
    
    # Update the guess for the next iteration
    x <- x_next
  }
  
  # --- 4. Failure to Converge ---
  # If the loop finishes without meeting the tolerance, issue a warning
  # and return the last calculated value.
  warning(paste("Failed to converge within", max_iterations, "iterations."))
  return(x)
}

################################################################################
# USAGE

# --- Example 1: Calculate the square root of 10 ---
result <- babylonian_sqrt(10)
print(result)

# Compare with the built-in R function
print(sqrt(10))


# --- Example 2: A larger number ---
result_large <- babylonian_sqrt(1337)
print(result_large)
print(sqrt(1337))

# --- Example 3: Your Number ---
your_number <- .Machine$double.xmax

result_large <- babylonian_sqrt(your_number)
print(result_large)
print(sqrt(your_number))




