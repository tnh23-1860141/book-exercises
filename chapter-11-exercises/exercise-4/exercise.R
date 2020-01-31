# Exercise 4: practicing with dplyr

# Install the `"nycflights13"` package. Load (`library()`) the package.
# You'll also need to load `dplyr`
install.packages("nycflights13")
library("nycflights13")
library("dplyr")

# The data frame `flights` should now be accessible to you.
# Use functions to inspect it: how many rows and columns does it have?
# What are the names of the columns?
# Use `??flights` to search for documentation on the data set (for what the 
# columns represent)
View(flights)


# Use `dplyr` to give the data frame a new column that is the amount of time
# gained or lost while flying (that is: how much of the delay arriving occured
# during flight, as opposed to before departing).
flights <- mutate(flights, delayed_in_air = arr_delay - dep_delay)
View(flights)

# Use `dplyr` to sort your data frame in descending order by the column you just
# created. Remember to save this as a variable (or in the same one!)
flights <- arrange(flights, -delayed_in_air)
View(flights)

# For practice, repeat the last 2 steps in a single statement using the pipe
# operator. You can clear your environmental variables to "reset" the data frame


# Make a histogram of the amount of time gained using the `hist()` function


# On average, did flights gain or lose time?
# Note: use the `na.rm = TRUE` argument to remove NA values from your aggregation
summarize(flights, avg = mean(delayed_in_air, na.rm = TRUE))


# Create a data.frame of flights headed to SeaTac ('SEA'), only including the
# origin, destination, and the "gain_in_air" column you just created
to_seattle <- filter(flights, dest == "SEA")
to_seattle <- select(to_seattle, origin, dest, delayed_in_air)


# On average, did flights to SeaTac gain or loose time?
summarize(to_seattle, avg_delayed = mean(delayed_in_air, na.rm = TRUE))


# Consider flights from JFK to SEA. What was the average, min, and max air time
# of those flights? Bonus: use pipes to answer this question in one statement
# (without showing any other data)!

