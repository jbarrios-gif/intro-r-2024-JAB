#### R Basics ####
# "A foolish consistency is the hobgoblin of 
#   little minds"   -Ralph Waldo Emerson 

# Literals ----
"this is a string literal" # double quotes preferred in R but not required
42
T
F
TRUE
FALSE

# Operators ----
2 + 3 # note the spacing
2 - 3
2 * 3 # multiplication
2 / 3 # division

2 ** 3 # but be careful: this is an exponent
2 ^ 3 # that is better

# Comparison
2 == 2 # tests for equality
"Joe" == "joe" # case-sensitive
"Joe" == "Joe"
2 == 1 + 1 # ok
2 == (1 + 1) # better

2 != 1 # tests inequality

2 < 3
2 > 3
2 <= 2
2 >= 3

TRUE == 1 #
FALSE == 0
isTRUE(TRUE) # function testing if the argument is literally TRUE
isTRUE(1)
?isTRUE # queries built-in help

2 < 3 & 2 > 3 # both have to pass to return TRUE
2 <3 | 2 > 3 # | means OR, either one TRUE, all TRUE
2 < 3 & (2 == 1 | 2 == 2) # grouping statements for ordering
 
# type matters (sometimes)
"joe" # string or character type
typeof("joe")
42 # numeric type (double precision, floating point)
typeof(42)
TRUE
typeof(TRUE) # logical or boolean type

42 == "42" # equality can cross type
identical(42, "42") # type matters for identity


# variables ----
x <- "this is a string" # in R, read as assigning the string to variable x
x
typeof(x)
x <- 10
x
x ^ 2 # always refers to the assigned value

x <- "pizza"
pizza <- "x"
pizza
my var <- 42 # not everything though
my_var <- 42 # that is better
my_var = 42 # works, but not standard in R
my_var
x <- my_var # helps reader follow assignment direction
x


# data structures ----
# vectors have a single dimension, like a column or row of data
a <- c("1", "2", "3") # c() stands for collect (what's inside)
a 
a <- c(1, 2, 3)
a
a + 1

a <- c(1, 2, 3, "4") # R will auto-type to form that "works"
a
typeof(a)
a + 1

a <- c(1, 2, 3)
a < 3

any(a < 3) # tests whether any comparison TRUE
all(a <3) # tests whether all comparisons TRUE

3 %in% a # testing membership in a vector
4 %not in% a # nope
!4 %in% a # yep

# data frames - the key structure for data science, multi-dimensional
#   collections of vectors
df <- data.frame(a = c (1, 2, 3),
                 b = c("joe", "tammy", "matt")) # collection of vectors
df

df$a # references single column
df$b

df
df$mode <- c("bike", "car", "bus") #adding a column
df
summary(df) # summarizes by column


# Special type: factors, and putting it all together ----
# factors are categorical variables with a fixed set of
#   potential values


