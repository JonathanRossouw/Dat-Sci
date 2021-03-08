R <- c("Very Happy", "Happy", "Not Happy")
# Let's now create responses:
W <- c(15, 5, 3)
M <- c(35, 15, 14)
C <- c(23, 35, 32)
# Now we have many variables assigned names, but we now want
# to concatenate it all...  i.e. let's merge the columns
# together in a single data.frame (as a single unit) To
# change the column names, simply type the name first:
HappySurvey <- data.frame(Responses = R, Women = W, Men = M, 
                          Children = C)
# Now to isolate a column, say Men, and count the responses,
# Use the $ sign:
sum(HappySurvey$Men)
# Note the following syntax, these two are the same:
x <- HappySurvey$Men
x <- HappySurvey[, 3]  # calling all rows of column 3
# Other useful base R commands include:
mean(x)
min(x)
median(x)
summary(x)
# To do, say, a Chisquare test
chisq.test(HappySurvey$Men)

install.packages("xts")

library(xts)  

Ray <- array(c(1:20), dim = c(2, 5))
# Note what happened in your Rstudio console when executing
# this command. Let's give the data row and column names as
# follows:
colnames(Ray) <- c("Men", "boys", "women", "children", "babies")
rownames(Ray) <- c("satisfaction", "Communication")
# ... By the way, I have no idea what the above names
# imply...
  


xx <- seq(from = 1, to = 16, by = 3)
# Note what seq() does...
yy <- seq(from = 1, to = 100, by = 18)
# Now let's merge these into a single matrix:
mat <- matrix(cbind(xx, yy), nrow = 6, ncol = 2)

library(tibble)

x = rnorm(100)
y = rnorm(100)
z = rnorm(200)

df1 <- tibble(var1 = x, var2 = y)
# We could of course add columns that have text, or are
# contingent on preceding column values, e.g.:
df2 <- tibble(var1 = x, var2 = y, message = "Msg", contingent_Column = ifelse(var1 > 
                                                                                0, "Positive", "Negative"))
# Notice that dataframes should be balanced. The following
# will fail (check this yourself and argue why):

# df3 <- tibble(var1 = x, var2 = z)



pacman::p_install_gh("Nicktz/fmxdat", force = T)


pacman::p_load(tidyverse)  # We will see later
pacman::p_load(tibble)  # We will see later

df_ugly <- fmxdat::ugly_df
head(df_ugly)

tibble(df_ugly)

df_ugly %>% tbl_df()

df <- df_ugly %>% as_tibble()

# To isolate the first row, fifth column:
df[1, 5]

# To isolate the entire third row:
df[3, ]

# To remove the second column:
df[, -2]

# select the first four rows of columns 1, 4 and 7
df[1:4, c(1, 4, 7)]


String <- c("SOME STRING", "Another STRING", "Last One", "SOME Other One")

# gsub: replacement tool
gsub(x = String, pattern = " STRING", replacement = "")

# grepl: Identifying patter match Let's trim String to only
# include entries with the word 'SOME'
grepl(pattern = "SOME", String)

String[grepl(pattern = "SOME", String)]

df <- fmxdat::ugly_df %>% as_tibble()

# Let's now subset our dataframe by selecting columns of df
# that contain a 'Z' or a 'Y', as well as the date column
df[, grepl("date|Z|Y", colnames(df))]

Smith <- list(name = "John", title = "President", firm = "Google", 
              salary = 5e+05)

print(Smith)

# To acces buckets in the list, we can use $ or [[]]:

Smith$title
# Which is equivalent to:
Smith[[2]]  # double [[k]] imply ``entry at k''

# Lists can also store characters and values: e.g.
# calculating the salary in 1000s:
Smith$salary/1000

pacman::p_load(tibble)
df_TRI <- fmxdat::DailyTRIs

# Let's quickly remove the ' SJ' from all column names first.
# This requires us using colnames and gsub:

colnames(df_TRI) <- gsub(" SJ", "", colnames(df_TRI))

# Let's get each column's max value (we will explain apply
# next):
Max_vals <- apply(df_TRI[, -1], 2, max, na.rm = T)
Max_vals


# Let's replace the is.infinite values with zero next (max of
# a column of NAs gives -infinity):
Max_vals[is.infinite(Max_vals)] <- 0
# Go through the above line of code to understand what is
# cooking here..

Min_vals <- apply(df_TRI[, -1], 2, min, na.rm = T)
Min_vals[is.infinite(Min_vals)] <- 0

Result <- list()
Result$Name <- "Result of Max Returns"
Result$Max <- Max_vals
Result$Min <- Min_vals
Result$LastDate <- tail(df_TRI, 1)  # Notice my use of tail here... Converse is head.

Result$Max[which(Result$Max == max(Result$Max))]

# Let's create a function for creating normal data: Note ::::
# function(inputs){commands}
NormalDataGen <- function(n, mu, sd) {
  rnorm(n, mean = mu, sd = sd)
}
a <- NormalDataGen(n = 100, mu = 5, sd = 2)
b <- NormalDataGen(n = 100, mu = 3, sd = 2.3)
c <- NormalDataGen(n = 100, mu = 2, sd = 1)
d <- NormalDataGen(n = 100, mu = 4, sd = 4)
e <- NormalDataGen(n = 100, mu = 3, sd = 2)
data <- data.frame(a, b, c, d, e)
class(data)
# Now, using apply note the following: apply(data, 1 is row
# and 2 is columns,function to be done) Using this, to
# calculate the mean of all the columns, simply use:
apply(data, 2, sum)
# Another SUPER USEFUL addition to this is that we can add
# commands that are part of the function. E.g. had we
# calculated column means, we could use 'na.rm = TRUE''
# (remove NAs) and 'trim' to trim for outliers.  Adding these
# commands to mean should not be done in brackets, but after
# commas
apply(data, 2, mean, na.rm = TRUE, trim = 0.35)
# Note that na.rm and trim belond to the mean function, not
# apply...


df_TRI[, which(apply(!is.na(df_TRI), 2, all))]

x <- tibble(RandomData = rnorm(100, 50, 2))
# Let's only keep data that lie between 48 and 50 (for
# whatever reason...)
z <- x$RandomData[which(x$RandomData < 50 & x$RandomData > 48)]
# ==== Note: x$RandomData is the column using square
# brackets, like : x$RandomData[1:5], gives me e.g. the first
# five positions 'which' gives me the positions that satisfy
# it being <50 & > 48... so: x[ which( x$RandomData <50 &
# x$RandomData >48) ] thus does the job!


x <- rnorm(1, mean = 100, sd = 10)
# If statement:
if (x < 100) {
  m <- "Heads!"
} else {
  m <- "Tails!"
}
print(m)

sqr <- c()  # Create an open vector to be filled:
x <- seq(from = 1, to = 100, by = 10)

# Loop to square every entry of x:
for (i in 1:length(x)) {
  sqr[i] <- x[i]^2
}

print(sqr)

x <- tibble(Random = rnorm(100))

while (max(x$Random) < 3) {
  x <- bind_cols(x, Another_Random = rnorm(100, 1, 0.1))
  x$Random <- x$Random * x$Another_Random
  x <- x[, -2]
  print(max(x$Random))
}

t <- "text1"
t2 <- "text2"
paste(t, t2, sep = "/")
paste0(t, t2)  # No need for sep, implied that sep = ''

pacman::p_load(lubridate, glue)


date <- lubridate::today()
randomValue <- rnorm(1)
Mood <- ifelse(randomValue > 0, "happy", "sad")
print(
  
  glue::glue("
\n=======\n
This is an example of pasting strings in text.
On this day, {date}, my mood is {Mood}, as my value is {ifelse(randomValue>0, 'positive', 'negative')}.
That's pretty cool right? ...note here will now follow\n\n a few spaces...
\n=======\n
       ")
  
)

pacman::p_load(tidyverse)
dta <- fmxdat::BRICSTRI
# Create a folder called data in your root.  If you followed
# my gif's steps above - you are currently in a .Rproj
# environment, meaning you can straight use:
dir.create("data/")
# And now let's store our data file in here:
write_rds(x = dta, file = "data/Example_File.rds")

# And now to load it:
df <- read_rds(file = "data/Example_File.rds")


data <- read_csv("/Data/Indexes.csv", col_types = cols(.default = "d", 
                                                       Date = "D"))
# Above, d stands for dbl (used for numerical columns) If you
# want to specify that a column contains characters, use 'c'

data <- fmxdat::Indexes

View(data)
head(data, 10)
tail(data, 10)

colSums(is.na(data))  

# na.rm = TRUE tells R to count all the non-NA items in each
# column:
colSums(data == 0, na.rm = TRUE)
colSums(data >= 500, na.rm = TRUE)

dataNoNa <- na.omit(data)

data[is.na(data)] <- 0

data.frame(date = seq(as.Date("2012-01-01"),
                     as.Date("2015-08-18"), "day"))

df$Date <- as.Date(df$Date, format = format = "%d/%m/%Y")

pacman::p_load(dplyr)
df <- tibble(date = seq(as.Date("2012-01-01"), as.Date("2015-08-18"), 
                        "day"), TRI = rnorm(1326, 0.05, 0.04))
# Let's now create a function that only focuses on weekdays:
dow <- function(x) format(as.Date(x), "%a")
df$day <- dow(df$date)
df[!grepl("Sat|Sun", df$day), ]

# To focus only on particular months, let's say January and
# February:
dom <- function(x) format(as.Date(x), "%b")
df$Month <- dom(df$date)
df[grepl("Jan|Jun", df$Month), ]
# Note: Use the above only after loading package dplyr, and
# setting your data.frame in tbl_df format.


x = rnorm(1000)
# Let's now create a function to replace all positive values
# with the word positive:
Max_Value <- function(vector) {
  
  Maximum <- max(vector)
  
  return(Maximum)
  
}

Max_Value(vector = x)


# Notice that Maximum was not saved anywhere as the
# function's environment was temporary:
exists("Maximum")
