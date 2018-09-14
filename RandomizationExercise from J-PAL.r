# RANDOMIZATION IN R
# Originally for Stata by Rohit Naimpally from J-PAL; https://www.povertyactionlab.org/research-resources
# Author for R: Jorge Cimentada; cimentadaj@gmail.com
# Last Version: 4th of March 2016



# Packages needed: 
# install.packages(c("haven","dplyr"))
library(haven)
library(dplyr)

#Let's download the zip file containing the Stata do file and the dataset and unzip it to your working directory.

download.file("https://www.povertyactionlab.org/sites/default/files/resources/Simple%20Guide%20Randomization%20Stata.zip"
         , dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./")

# Let's load the Balsakhi dataset:

Data <- read_dta("RandomizationExercise_balsakhi_data.dta")

View(Data)

# The dataset has five variables. What do these variables mean? Do you need them for the randomization?

# We will discuss two common types of randomization:

# 1. Simple randomization
# 2. Stratified randomization

# -----------------------------------------------------------------------------

# EXAMPLE 1: SIMPLE RANDOMIZATION

# -----------------------------------------------------------------------------

Data <- read_dta("RandomizationExercise_balsakhi_data.dta")

# In this example, we will simply randomize the schools into treatment
# and control groups.

# 1. For the Stata example, you need to set the version of your current Stata. This is not necessary in R
 
# 2. Setting seed

# Apart from the actual randomization, this is the most important part of
# the do-file.  Why? Because setting this number ensures that your
# randomization is replicable i.e. that the same sequence of random
# numbers can be generated, ensuring that you can reproduce your random
# assignment if need be.

set.seed(20110402)

# You could just as easily select another number, like 19830625.  The
# choice of seed doesn't matter for now.  When you write a real-life
# randomization, do not choose dates or other numbers with human meaning,
# but rather use a service like random.org to randomly generate a seed.

# 2. Generating Random Numbers

# We want to create a variable of random numbers.  Before that, we must
# sort by a unique ID.  This is important for the reproducibility of the
# randomization:  if the sort order is not replicable, it will not be
# possible to create the same random number variable.  It is good
# practice to keep your unique ID as a string variable, even if the
# characters are all numeric.  In this dataset, schoolid, the school ID,
# uniquely identifies observations:
    
anyDuplicated(Data$schoolid)

# The command anyDuplicated helps you verify if a variable indeed uniquely
# identifies the observations in a dataset. If it does not, the anyDuplicated
# function returns a positive number (which means that X number of observations are duplicate);
# For instance, try the anyDuplicate command on another variable in the dataset and
# see what happens.

Data <- Data[order(Data$schoolid), ]

Data$random <- runif(nrow(Data))

# 3. Assigning treatment and control

# Now, the next step is to assign treatment and control to schools.
# First, let us start by sorting by random number.  This is done using
# the following command line:
    
Data <- Data[order(Data$random), ]


# In R, we will create a variable with the row number for each observation called index and then assign the treatment groups.
Data$index <- 1:nrow(Data)

Data$treatment <- as.numeric(Data$index <= nrow(Data) / 2)


# The logical expression above can be read as "create a variable named
# treatment that takes on the value 0. Whenever the observation number
# in 'index' is less than or equal to half of the total number of observations
# (nrow(Data)) the treatment variable takes the value 1 otherwise it is zero".

# An alternative way of doing it would be:

Data$treatment <- 0
Data$treatment[Data$index <= nrow(Data) / 2] <- 1

# What is important to note here is that we have assigned the top half of
# schools to the treatment condition and the bottom half of schools to
# the control condition (where treatment is designated by the treatment
# variable taking on the value 1 and control is designated by the
# treatment variable taking on the value 0).

# Note that there are other ways of doing this as well:
    
# For example, we could have set the bottom half of schools to be
# treatment and because it is all random, it would not have mattered! How
# would the generate treatment command have looked if we had decided to
# set the bottom half of schools to be treatment and the top half
# control?

# Lastly, let us sort by school ID again and look at the result of our
# effort:
    
Data <- Data[order(Data$schoolid),]
View(Data)

# +----------------------------------------------------------------------
#    > --------+
#    | Technical Tip!
#    |
#    |----------------------------------------------------------------------
#    > --------|
# In case there are sort-order ties on the random number variable -- if
# the variable contains duplicates, which happens more often than you may
# imagine -- it is actually best practice to generate and sort on two
# random variables.

Data <- Data[order(Data$schoolid), ]
Data$random1 <- runif(nrow(Data))
Data$random2 <- runif(nrow(Data))

Data <- Data[order(Data$random1, Data$random2), ]
Data$treament <- as.numeric(Data$index <= nrow(Data) / 2)

#    > |
#    +----------------------------------------------------------------------
#    > --------+

#-----------------------------------------------------------------------------
    
#    EXAMPLE 2:STRATIFICATION AND RANDOMIZATION

#-----------------------------------------------------------------------------
    
# Reset the dataset.

Data <- read_dta("RandomizationExercise_balsakhi_data.dta")

# In this example, we will randomize schools after stratifying them by
# language and gender. Be sure to check out the technical tip boxes above
# in case you haven't already.

# 1. Usual generation of the random number

# * Set the seed.
set.seed(20110402)
# * Sort by school ID.
Data <- Data[order(Data$schoolid),]
# * Generate the random number variable.
Data$random <- runif(nrow(Data))

# 2. Stratification by language and gender

# What you want to do is the following:

Data <- Data %>%
        group_by(language, gender) %>%
        arrange(language, gender) %>% ## sorted by language and gender
        mutate(strata_size=n()) ## n() fills in the total number of observations

# This tells us how many schools there are in a language-gender group.
# Now let us assign a serial number to each school in a language-gender
# group.

Data <- Data %>%
        group_by(language, gender) %>%
        arrange(language, gender, random) %>% ## sorted by language and gender
        mutate(strata_index=row_number()) ## row_number() counts consecutive number inside each groups


# What we've now done is to create a variable called strata_index that
# takes on the observation number for each observation within a given
# language and gender group.  For instance, the value of strata_index for
# the 16th observation in a particular language and gender combination
# will be 16.

# Moreover, notice the syntax of the arrange command here: We have sorted
# on three variables: language, gender, and random. However, the mutate
# command is only applied to the language and gender groups, i.e., the
# strata_index variable is generated for each language and gender
# combination rather than for each language, gender, and random
# combination.

# 3. Assigning treatment and control within the strata

# We can now use similar syntax to what we used in the simple
# randomization:


Data$treatment_stratified <- as.numeric(Data$strata_index <= Data$strata_size / 2)

# Let us now check the result:
    
View(Data)

# Now go randomize everything. Eggs or pancakes for breakfast? RANDOMIZE!
    
# Actually, the correct answer to the above is: both.

    
