# ***************************************3a*****************************************

# Sum values in a column of a data frame.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d, provided as a string
#
# RETURN VALUE:
# if the specified column exists and contains numbers, returns the sum of
# all values in the column; otherwise, returns NULL
sum_column <- function(d, var) {
  # Set a default value to return
  result <- NULL
  x <- d[[var]] # Remember, as we showed in class: there are two ways
  # to access a column, and this is one; you should try
  # and figure out why the other way won't work here,
  # but that's not part of the homework
  if (!is.null(x)) { # This tests to see whether the column exists in
    # d; again, you should try and find a way to test
    # this out for yourself to verify that it's true,
    # but that's not part of the homework
    # YOUR CODE HERE: if x contains numbers, set the variable
    # result to be the sum of the values in x
    if (is.numeric(x)){result <- sum(x)
    }
    # You will need to do a bit of research in order to figure out how
    # to test whether a vector contains numbers.
  }
  # YOUR CODE HERE: return the result
}


# ***************************************3b*****************************************


# Sum values in a vector.
#
# ARGUMENTS:
# x: a vector
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of
# all values; otherwise, returns NULL
#
# Here we first define the function and we give it a variable name "my_sum".
# We assign to it one argument "x".
my_sum <- function(x) {
  # The variable "result" is initialised and its unique numeric value is 0 for now.
  result <- 0
  # We open our if/else condition and we first test whether our argument
  # that our function is taking is a numeric vector with the function is.numeric()
  if (is.numeric(x)){
    # We open a loop "for" where for each element of our vector x, provided it is
    # a numeric one, we will do something
    for (i in x){
      # We reassign the variable "result" to a new value, which is now the value of
      # result plus the value of the element currently inside this loop
      # result is thus a sum of the previous value plus the value of the current i
      result <- result + i
    }
    # The function will, once the loop has been gone through (so it has applied all
    # it needed to to every element), return the final result, which is the sum of
    # all the values of the inputted numeric vector.
    return(result)
  }
  # We have just exited our "for" loop and our "if" statement, now we will decide 
  # what happens if the answer to the if statement if FALSE, namely if the vector 
  # is not a numeric one. We simply return 'NULL' instead.
  else {return(NULL)}
}



# ***************************************3c*****************************************

# Sum of the elements of x divided by the number k.
#
# ARGUMENTS:
# x: a vector
# k: a number
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of
# all values of x, divided by the number k; otherwise returns NULL
#
# Here we first define the function and we give it a variable name "sum_divided_by".
# We assign to it two arguments "x" and "k".
sum_divided_by <- function(x,k) {
  # We check whether "x" is a numeric vector. At the same time we check if
  # "k" is also a number so we do not get errors if we try to divide "x"
  # with a non-numeric "k".
  # If both conditions are respected, we proceed with the "if" statement.
  if (is.numeric(x) & is.numeric(k)){
    # We assign a variable "result" to the value of the output of the function
    # my_sum, divided by the number k.
    result <- my_sum(x) / k
    # The function will finish here as it will return/output the result.
    return(result)
  }
  # If the "if" statement was false (at least one of the two conditions was false)
  # we proceed to "else" statement. The output of the function will be "NULL".
  else {return(NULL)}
}



# ***************************************3d*****************************************

# Mean of a vector (the sum divided by the number of elements).
#
# ARGUMENTS:
# x: a vector
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of
# all values of x, divided by the number of its elements; otherwise returns NULL
#
# Here we first define the function and we give it a variable name "my_mean".
# We assign to it one arguments "x".
my_mean <- function(x) {
  # We open the "if" statement and check whether the argument "x" is numeric 
  if (is.numeric(x)){
    # If the condition is true, we proceed and the function immediately returns
    # the output of the function "sum_divided_by", whose arguments are now
    # the numeric vector "x" and its length - the number that equals the number
    # of elements in this vector.
    return(sum_divided_by(x, length(x)))
  }
  # If the condition is false, we return NULL instead.
  else {return(NULL)}
}


# ***************************************4a*****************************************


# Return a violin plot.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a
#     string
# grouping_var: the name of a column of d containing a grouping variable,
#               provided as a string
#
# RETURN VALUE:
# A ggplot plot object containing a violin plot, grouped by the values
# of the grouping variable.
#
grouped_violin_plot <- function(d, var, grouping_var) {
  # Create the base ggplot object
  p <- ggplot2::ggplot(d, ggplot2::aes_string(y=var,
                                              x=grouping_var,
                                              fill=grouping_var))
  
  p <- p + ggplot2::geom_violin() 
  return(p) }



# ***************************************5a*****************************************


# Difference in the medians between two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
# group1: the value of grouping_var that corresponds to the first group
# group2: the value of grouping_var that corresponds to the second group
#
# RETURN VALUE:
# The median value of var for the first group, minus the median value of var for the second
# group.
#
difference_in_medians <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  # YOUR CODE HERE: assign the difference in the medians to to the variable 'result'
  result <- median(d_1[[var]]) - median(d_2[[var]])
  return(result)
}


# ***************************************5b*****************************************


# Randomize the order of a column.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the variable to randomize,
#      provided as a string
#
# RETURN VALUE:
# A data frame or tibble exactly the same as d, except with the order of
# var permuted randomly.
#
randomize <- function(d, var) {
  d[[var]] <- sample(d[[var]], nrow(d))
    return(d) }



# ***************************************5c*****************************************


# Perform a permutation test for two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of the column in d on which the test statistic will be calculated,
#      provided as a string
# grouping_var: the name of the column in d which gives the grouping
# group1: the value of grouping_var corresponding to the first group
# group2: the value of grouping_var corresponding to the second group
# statistic: a function yielding a test statistic, which takes as input
#            a data frame, the name of a variable on which to calculate the
#            test statistic, the name of a grouping variable, the value of
#            the grouping variable corresponding to the first group, and
#            the value of the grouping variable corresponding to the second
#            group
# n_samples: the number of permutation samples to draw (default: 9999)
#
# RETURN VALUE:
#
# A list containing two elements:
#
#  - observed: the value of statistic() in d
#  - permuted: a vector containing the values of statistic() under n_samples
#              permutations
#
permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    # YOUR CODE HERE: use randomize(...) to create a permutation and then
    #                 fill in the vector permutation_statistics with the
    #                 value of statistic(...) for this new permutation
    permuted_data <- randomize(d, var)
    permutation_statistics[i] <- statistic(permuted_data, var, grouping_var, group1, group2)
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}

