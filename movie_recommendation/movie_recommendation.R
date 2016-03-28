library(plyr)
library(dplyr)

# DATA PREP: movie_ids 
    # read in movie_ids.txt
movie_ids = readLines(con <- file("movie_ids.txt"), encoding="latin1") # Otherwise non-English movie names would be recognized as invalid multibyte string
str(movie_ids)

    # Spliting text lines in movie_ids into three columns: id, name, year
        # separate out movie year
substrRight = function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}
substrLeft = function(x, n) {
  substr(x, 1, nchar(x) - n - 1) # whitespace between movie name and year should also be removed
}

movie_years = substrRight(movie_ids, 6)
movie_years = substr(movie_years, 2, 5) # remove brackets from year data
movie_ids_names = substrLeft(movie_ids, 6) # remvoe year data from movie ids and names
View(movie_ids_names)

        # Split movie ids and names using regular expression
rexp = "^(\\w+)\\s?(.*)$"
movie_data = data.frame(id = sub(rexp, "\\1", movie_ids_names), 
                        name = sub(rexp, "\\2", movie_ids_names),
                        year = movie_years)
        # sanity check: remove duplicated name-year combination or missing name/year
duplicates = which(duplicated(movie_data[,2:3]))
missing = which(movie_data$name == "")
invalid_ids = c(duplicates, missing)
        # Generate a subset of unique year-name combination 
valid_pairs = movie_data[-invalid_ids,]
        # Concatenate movie year and name 
valid_pairs$name_year = paste(valid_pairs$name, valid_pairs$year, sep = "_")

# DATA PREP: given movies 
m = c("Toy Story",
           "Twelve Monkeys",
           "Usual Suspects, The",
           "Outbreak",
           "Shawshank Redemption, The",
           "While You Were Sleeping",
           "Forrest Gump",
           "Silence of the Lambs, The",
           "Alien",
           "Die Hard 2",
           "Sphere")
yr = c(1995, 1995, 1995, 1995, 1994, 1995, 1994, 1991, 1979, 1990, 1998)
r = c(4,3,5,4,5,3,5,2,4,5,5)
id = movie_data$id[which(valid_pairs$name %in% m)]
mr = cbind(m, r, id, yr) # this matches movie names with ids)
mr = as.data.frame(mr)
mr$name_year = paste(mr$m, mr$yr, sep = "_")
mr$id = as.numeric(as.character(mr$id))
mr = mr[order(mr$id),] # order given data by movie id


# DATA PREP: movies_users_rate.csv
movie_users_rate = read.csv("movie_users_rate.csv")
View(head(movie_users_rate))
    # Transpose
movie_users_rate$X = gsub("movie", "", movie_users_rate$X) # removing string "movie" from variable X
movie_users_rate$X = as.numeric(movie_users_rate$X) # convert movie id from string character to numeric variable
movie_users_rate_transpose = as.data.frame(t(movie_users_rate)) # duplicates are alive here so far
    # Rename columns & Remove invalid ids
names(movie_users_rate_transpose) = gsub("V","",names(movie_users_rate_transpose))
movie_users_rate_transpose = select(movie_users_rate_transpose, -invalid_ids)
    # Remove X as column names contains the same information
mur = movie_users_rate_transpose[-1,] 
    # Replace id column names with name-year 
valid_pairs = valid_pairs[order(as.numeric(as.character(valid_pairs$id))),]
names(mur) = valid_pairs$name_year # for the sake of lm regression
View(mur)

# Fitting multiple linear regression 
  # Model 1: 
  # predicator vars (x): the 11 movies we have an imagined user rated, mr$name_year
  # output vars (y): every single movie except the 11 movies
y = as.character(valid_pairs$name_year[!(valid_pairs$name_year %in% mr$name_year)])
x = mr$name_year
# standardize variable names 
xname = paste("x", 1:11, sep = "")
d = list()
t = vector()
# prediction data
newdata = data.frame(x1=4,x2=3,x3=5,x4=4,x5=5,x6=3,x7=5,x8=2,x9=4,x10=5,x11=5)

for (i in 1:length(y)) {
  d[[i]] = mur[c(y[i],x)]
  colnames(d[[i]])[-1] = xname
  colnames(d[[i]])[1] = "output"
  fmla = as.formula(paste("output ~ ", paste(xname, collapse = "+")))
  t[i] = predict(lm(fmla, data = d[[i]]), newdata)
}

rate_predict = data.frame(t, y)
rate_predict$rate = round(rate_predict$t, digits = 0) # predicted rates for every single movie

# movies that get this user high (rated 5 I assume)
  # print out results
print(as.character(rate_predict$y[which(rate_predict$rate == 5)]))

  # Model 2: Multinomial logistic regression
library(nnet)
t2 = list()
for (i in 1:length(y)) {
  d[[i]] = mur[c(y[i],x)]
  colnames(d[[i]])[-1] = xname
  colnames(d[[i]])[1] = "output"
  fmla = as.formula(paste("output ~ ", paste(xname, collapse = "+")))
  t2[[i]] = predict(multinom(fmla, data = d[[i]]), newdata, "probs")
}

# For each predicted matrix, rank the prob for each option. The choice with the max value would be the predicted rating
rating = 
  sapply(t2, function(x) {
  temp = as.numeric(names(sort(x, decreasing = TRUE))) # order the vector of probs in descending order 
  rate = temp[1]
})
# Select movies with a rating of 5
rating = as.data.frame(rating)
movie_rating = cbind(rating, y)
highly_rated = movie_rating[which(movie_rating$rating > 4),]
write.csv(highly_rated,"highly_rated.csv")

# 

# Movies with no user rating (all zero values) are detected 



