###########################################################################
# analysis of the Brochure Survey
# conducted 7/31/14 - ?????
###########################################################################


## clear the console of all variables
rm(list = ls())


## read the Results file
survey_results_full=read.csv("C:\\Users\\Blake.Abbenante\\Google Drive\\Work\\r\\Brochure Survey\\BrochureSurveyResults-Full.csv")
survey_results <- survey_results_full[complete.cases(survey_results_full[,21:39]),]
## dimensions
dim(survey_results_full)
dim(survey_results)
## summary
summary(survey_results)
summary(survey_results_full)

# Choose only the attitudinal questions 21 through 39
# 40-53 - maybe, but on a different scale, and really not much difference in results
variables_i_want_in_cluster = c(21:39)
#variables_i_want_in_cluster = c(21:53)
# Confirm that's right
colnames(survey_results)[variables_i_want_in_cluster]

summary(survey_results[variables_i_want_in_cluster])


dataset <-survey_results[variables_i_want_in_cluster]

## Decide how many clusters to use by trying out 1 to 20 clusters and seeing
## where the 'elbow' is in the plot of sum of squared errors
# No need to normalize because all variables on the same scale
# No need to remove NA's (no NA's for these variables)
#
# But do keep the means and standard deviations aside for later use
column.means = colMeans(dataset)
column.sds = apply(dataset, 2, sd)


sum_squared_errors = NULL  # This is the empty array
k_vals <- seq(20)
for (k in k_vals) {
  cat (" k =", k, ".....")
  clusters <- kmeans(dataset, k, iter.max=20)
  sum_squared_errors <- c(sum_squared_errors, clusters$tot.withinss)
}

plot(k_vals, sum_squared_errors, 
     xlab="clusters", 
     ylab="sum of squared errors within the cluster",
     main="Performance of clustering on survey results",
     cex.main=1.1)

# This part is to plot the red circle
# Just pick 4 clusters. Could pick a bigger number but I don't want to
# go through by hand and name more than 10 clusters
k <- 5
points(k_vals[k], sum_squared_errors[k], col="red", lwd=1.5)
# lines()  to superimpose lines on an existing plot

# This plot is to annotate it that we chose
text(k_vals[k], sum_squared_errors[k], labels = paste("Select k =", k),
     pos=4, # pos = 4 --> "text goes to the right of the coordinate"
     offset=0.5)  # offset x in fractions of character width
# in the same direction as specified by pos


### NOTE :: Clustering is random so each time the values will be different...
###         ... recommend saving the result to analyze in the future.
###
# Rerun here for just 4 clusters
clusters <- kmeans(dataset, k)
# And save it
save(clusters, file="C:\\Users\\Blake.Abbenante\\Google Drive\\Work\\r\\Brochure Survey\\bro_clusters.RData")

# Then don't rerun the clustering ever again, use your saved model
load("C:\\Users\\Blake.Abbenante\\Google Drive\\Work\\r\\Brochure Survey\\bro_clusters.RData")


###----------------------------------------------------------------------------###
### We have clusters. Now we can do a regression to match demographics to
### attitudinal clusters.

# One way to match demographics to attitudes is to do a tree classification
# model predicting the cluster based on the demographics.
#
# For this, you want to bind the clusters and the datasets together.
survey_results$cluster <- clusters$cluster



# Now I can name my clusters by inspecting the centers.
# Rounding helps to make it visually obvious which values are important to which cluster
t(round(clusters$centers))


# --> I would paste this in Excel and paste the questions next to the strings 'q4r4' etc
#     and sort each cluster in descending order and ascending order to see which values
#     are extreme (0 or 6 or something) 
#
# To save to excel, assign this to a variable (and round out to a bigger decimal)
cluster_centers <- t(round(clusters$centers, 3))

## And here, add the means and standard deviations so we can identify
## which questions have the highest variation in response while naming them.
column.means = apply(dataset, 2, mean)
column.sds = apply(dataset, 2, sd)

# Before I add these to the cluster_centers matrix, I have to change
# cluster_centers from a matrix to a data frame
cluster_centers <- as.data.frame(cluster_centers)
# Now I can create the new columns
cluster_centers$variable.means <- column.means
cluster_centers$variable.sds <- column.sds

# Write to a CSV file that can be opened in Excel
write.csv(cluster_centers, file="C:\\Users\\Blake.Abbenante\\Google Drive\\Work\\r\\Brochure Survey\\bro_cluster_centers.csv")


# the command table() will count the occurrences of each cluster
table(clusters$cluster)


############
## Fast foward to after we set cluster names
############
cluster_name_matcher = data.frame(
  cluster=c(1,2,3,4,5),
  name=c("Persuadables","Middle Road","Information Seekers","Digital","Traditional"
))

# Match the cluster name to the cluster ID.
# this is like a SQL join. See help(merge) for options to left join, right join, etc
survey_results <- merge(survey_results, cluster_name_matcher, by="cluster")

# And another CSV for the clients with the extra cluster identifier
# Open this in Paige's Excel spreadsheet and use Vlookup to map the
# clusers / names to the CaseID
write.csv(survey_results[, c("RespondentID", "cluster", "name")],
          file="C:\\Users\\Blake.Abbenante\\Google Drive\\Work\\r\\Brochure Survey\\bro_ids_to_clusters.csv",
          row.names=FALSE)

###-----------------------------------------------------------------------------###
# Next the goal is to predict which cluster an individual would be in given
# their demographic information. We can use a tree to predict this
#

# Note that my columns got shifted over by one because the cluster 
# varable got assigned to column one.

### !! caveat -- I added one to get the right column numbers
#demographic_and_behavioural_columns <- c(7:20, 63:71) +1
#demographic_and_behavioural_columns <- c(7, 63:74) +1
demographic_and_behavioural_columns <- c(63:74) +1

# Confirm the columns are right:
colnames(survey_results)[demographic_and_behavioural_columns]

# OK now it's right
demographic_and_behavioural_dataset <- survey_results[demographic_and_behavioural_columns]



# Let's make all of the responses ordinal
#levels( survey_results$MOST.HELPFUL..Deciding.to.travel )
#is.ordered( survey_results$MOST.HELPFUL..Deciding.to.travel )
#demographic_and_behavioural_dataset$MOST.HELPFUL..Deciding.to.travel <- as.ordered(survey_results$MOST.HELPFUL..Deciding.to.travel)
#levels( survey_results$MOST.HELPFUL..Deciding.to.travel )
#is.ordered( survey_results$MOST.HELPFUL..Deciding.to.travel )
#demographic_and_behavioural_dataset$MOST.HELPFUL..Choosing.a.tour <- as.ordered(survey_results$MOST.HELPFUL..Choosing.a.tour)
#levels( survey_results$MOST.HELPFUL..Choosing.a.tour )
#demographic_and_behavioural_dataset$MOST.HELPFUL..Pricing.tours <- as.ordered(survey_results$MOST.HELPFUL..Pricing.tours)
#levels( survey_results$MOST.HELPFUL..Pricing.tours )
#demographic_and_behavioural_dataset$MOST.HELPFUL..Recruiting.students.travelers <- as.ordered(survey_results$MOST.HELPFUL..Recruiting.students.travelers)
#levels( survey_results$MOST.HELPFUL..Recruiting.students.travelers )
#demographic_and_behavioural_dataset$MOST.HELPFUL..Keeping.track.of.dates.and.forms <- as.ordered(survey_results$MOST.HELPFUL..Keeping.track.of.dates.and.forms)
#levels( survey_results$MOST.HELPFUL..Keeping.track.of.dates.and.forms )
#demographic_and_behavioural_dataset$MOST.HELPFUL..Learning.about.EF.in.general <- as.ordered(survey_results$MOST.HELPFUL..Learning.about.EF.in.general)
#levels( survey_results$MOST.HELPFUL..Learning.about.EF.in.general )
#demographic_and_behavioural_dataset$INFLUENCE..Online.research <- as.ordered(survey_results$INFLUENCE..Online.research)
#levels( survey_results$INFLUENCE..Online.research )
#demographic_and_behavioural_dataset$INFLUENCE..Conversations.with.colleagues <- as.ordered(survey_results$INFLUENCE..Conversations.with.colleagues)
#levels( survey_results$INFLUENCE..Conversations.with.colleagues )
#demographic_and_behavioural_dataset$INFLUENCE..Information.I.read.on.eftours.com <- as.ordered(survey_results$INFLUENCE..Information.I.read.on.eftours.com)
#levels( survey_results$INFLUENCE..Information.I.read.on.eftours.com )
#demographic_and_behavioural_dataset$INFLUENCE..Information.I.read.in.the.brochure <- as.ordered(survey_results$INFLUENCE..Information.I.read.in.the.brochure)
#levels( survey_results$INFLUENCE..Information.I.read.in.the.brochure )
#demographic_and_behavioural_dataset$INFLUENCE..Conversations.with.my.Tour.Consultant <- as.ordered(survey_results$INFLUENCE..Conversations.with.my.Tour.Consultant)
#levels( survey_results$INFLUENCE..Conversations.with.my.Tour.Consultant )
#demographic_and_behavioural_dataset$INFLUENCE..My.own.prior.travel.experiences.with.EF <- as.ordered(survey_results$INFLUENCE..My.own.prior.travel.experiences.with.EF)
#levels( survey_results$INFLUENCE..My.own.prior.travel.experiences.with.EF )
#demographic_and_behavioural_dataset$INFLUENCE..Other <- as.ordered(survey_results$INFLUENCE..Other)
#levels( survey_results$INFLUENCE..Other )
demographic_and_behavioural_dataset$HowManyBrochuresPerYear <- as.ordered(survey_results$HowManyBrochuresPerYear)
levels( survey_results$HowManyBrochuresPerYear )
demographic_and_behavioural_dataset$InternationalToursLed <- as.ordered(survey_results$InternationalToursLed)
levels( survey_results$InternationalToursLed )
demographic_and_behavioural_dataset$CustomerAge <- as.ordered(survey_results$CustomerAge)
levels( survey_results$CustomerAge )
demographic_and_behavioural_dataset$CustomerGender <- as.ordered(survey_results$CustomerGender)
levels( survey_results$CustomerGender )
demographic_and_behavioural_dataset$Metro_Rural_dy <- as.ordered(survey_results$Metro_Rural_dy)
levels( survey_results$Metro_Rural_dy )
demographic_and_behavioural_dataset$Metro_Unknown_dy <- as.ordered(survey_results$Metro_Unknown_dy)
levels( survey_results$Metro_Unknown_dy )
demographic_and_behavioural_dataset$Metro_Town_dy <- as.ordered(survey_results$Metro_Town_dy)
levels( survey_results$Metro_Town_dy )
demographic_and_behavioural_dataset$Metro_Suburban_dy <- as.ordered(survey_results$Metro_Suburban_dy)
levels( survey_results$Metro_Suburban_dy )
demographic_and_behavioural_dataset$CodedEnrollment <- as.ordered(survey_results$CodedEnrollment)
levels( survey_results$CodedEnrollment )
demographic_and_behavioural_dataset$HasTravelledWithStudents <- as.ordered(survey_results$HasTravelledWithStudents)
levels( survey_results$HasTravelledWithStudents )
demographic_and_behavioural_dataset$HasTravelledAbroad <- as.ordered(survey_results$HasTravelledAbroad)
levels( survey_results$HasTravelledAbroad )
demographic_and_behavioural_dataset$HasTravelledAlone <- as.ordered(survey_results$HasTravelledAlone)
levels( survey_results$HasTravelledAlone )
demographic_and_behavioural_dataset$Score <- as.ordered(survey_results$Score)
levels( survey_results$Score )



#rows <- which(is.na(demographic_and_behavioural_dataset))
#demographic_and_behavioural_dataset[rows] <- "No Response"


# Add the cluster names
demographic_and_behavioural_dataset$name <- survey_results$cluster

# Use a tree model to classify individuals into clusters based on their
# demographic information
library(rpart)

# FROM help(rpart)
#
# Usage
#
#rpart(formula, data, weights, subset, na.action = na.rpart, method,
#      model = FALSE, x = FALSE, y = TRUE, parms, control, cost, ...)
#
# formula is the same as for a regression...
#     <Dependent variable>  ~ [ a tilde symbol]
#     <Independent variable 1> +  ... + <Independent variable N>
#     (interactions are v1 * v2;  cannot do interactions for a tree)
#     (the dot with nothing is shorthand for every other column except the
#      independent variable)

######################################################################
### TTS NOTE :: Monday night we had a discussion that training/test splits
### 2014-07-08  are good for very large datasets but not so great for small
###             ones. Deleted the training_rows part
######################################################################

# 'class' to classify in categories
# rpart.control to tune the tree
tree_fit <- rpart(name ~ . ,
                  data=demographic_and_behavioural_dataset,
                  method="class", 
                  control=rpart.control(minbucket=25, cp=0.005))  
# otherwise since the clusters are numbers
# the default method is ANOVA and it's a regression
# tree not a classification tree
print(tree_fit)

plot(tree_fit, margin=0.1)  # pad around the plot to allow text to show
text(tree_fit, use.n = TRUE, cex=0.8)  # cex is a scale for marker or text size
# use.n makes the numbers like 11/73/2/50/10/3/12/0 appear at the bottom (leaves)
# They show the composition of the leaves by cluster

# Without the lots of numbers at the bottom
plot(tree_fit, margin=0.1) # pad around the plot to allow text to show
text(tree_fit, cex=0.8)


###==============================================================================###
## This is how you would use the tree model to make a prediction
###  (we are just using the same dataset here)
new_dataset = demographic_and_behavioural_dataset
probabilities_for_clusters = predict(tree_fit, newdata=new_dataset)

## Output (predicted cluster) is a probability that the observation
##  is in each cluster. We can choose the one with the highest probability
column_choice <- apply(probabilities_for_clusters, 1, which.max)
predicted_cluster <- colnames(probabilities_for_clusters)[column_choice]

## You would assign the result to the dataset by just adding the column
new_dataset$predicted_cluster = predicted_cluster

# Comparison (in a table)
table(new_dataset$name, new_dataset$predicted_cluster)

# Or as a mosaic plot (presentation depends on your preference)
mosaicplot(name ~ predicted_cluster, data=new_dataset,
           main="Model and actual cluster categories",
           xlab="Actual", ylab="Model",
           las=2)

mosaicplot(predicted_cluster ~ name, data=new_dataset,
           main="Model and actual cluster categories",
           xlab="Model", ylab="Actual",
           las=2)  # las sets how the axis labels are oriented


###=====================================================================###
### Using cross-validation to quantify performance of the tree model
library(caret)
N_FOLDS = 10
tc <- trainControl("cv", N_FOLDS)

# set the complexity parameter to the same constant value every time
rpart.grid <- expand.grid(.cp=0.01)
train.rpart <- train(name ~ .,
                     data=demographic_and_behavioural_dataset,
                     method="rpart", trControl=tc, tuneGrid=rpart.grid)

print(train.rpart)
