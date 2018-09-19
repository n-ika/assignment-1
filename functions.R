
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


# **********************************************************************************


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



# **********************************************************************************

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



# **********************************************************************************


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

