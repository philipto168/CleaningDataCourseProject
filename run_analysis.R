
# This project cleans up the data from the project at
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones


# read the test data
test_subject <- read.table("./test/subject_test.txt")
test_data <- read.table("./test/X_test.txt")
train_subject <- read.table("./train/subject_train.txt")
train_data <- read.table("./train/X_train.txt")

# read the column names for each measurement
col_name <- read.table("./features.txt")

# replace column labels
names(test_data) <- col_name[,"V2"]
names(train_data) <- col_name[, "V2"]

# map activity to meaningful name
act_name <- read.table("./activity_labels.txt")

test_y_data <- read.table("./test/y_test.txt")
train_y_data <- read.table("./train/y_train.txt")

test_merged_act_name <- merge(act_name, test_y_data)
train_merged_act_name <- merge(act_name, train_y_data)

test_data["Subject"] = test_subject["V1"]
test_data["Activity"] = test_merged_act_name["V1"]
test_data["Actname"] = test_merged_act_name["V2"]

train_data["Subject"] = train_subject["V1"]
train_data["Activity"] = train_merged_act_name["V1"]
train_data["Actname"] = train_merged_act_name["V2"]
# merge the activity name later

# new data set containing only mean and std
t2 <- test_data[,grep("mean|std|Activity|Subject",names(test_data))]
tr2 <- train_data[,grep("mean|std|Activity|Subject",names(train_data))]
# t2 and tr2 are the clean data set for later use.

# t3 and tr3 contains the data for each combination of test subject and activity.
t3 <-split(t2,list(t2$Subject,t2$Activity),drop=TRUE)
tr3<-split(tr2,list(tr2$Subject,tr2$Activity),drop=TRUE)

# compute the mean of all the measurements for each combination of subject and activity.
# merge all the results together to a data frame.
if (length(t3)>0) {
  temp_t <- colMeans(t3[[1]])
  for (i in 2:length(t3)) {
    # append the colMeans data to final_t
    temp_t <- rbind(temp_t, colMeans(t3[[i]]))
  }
}

if (length(tr3)>0) {
  temp_tr <- colMeans(tr3[[1]])
  for (i in 2:length(tr3)) {
    # append the colMeans data to final_t
    temp_tr <- rbind(temp_tr, colMeans(tr3[[i]]))
  }
}

# Now combine the test and train data to form the final tidy data set.
final_data_set <- t2
final_data_set <- rbind(final_data_set, tr2)

# combine the derived data into final data set.
final_avg_data_set <- temp_t
final_avg_data_set <- rbind(final_avg_data_set, temp_tr)
final_avg_data_set <- data.frame(final_avg_data_set)

# final_data_set is the clean version of the data containing only mean and std.
# final_avg_data_set is the average measurements for various subjects and activities.

# Add the descriptive activity name to the data.
names(act_name) = c("Activity","Activity_Name")
final_data_set <- merge(final_data_set,act_name)
final_avg_data_set <- merge(final_avg_data_set,act_name)
names(final_avg_data_set) <- names(final_data_set)

# output to file.
write.csv(final_data_set,"./final_data_set.csv", row.names = FALSE)
write.csv(final_avg_data_set,"./derived_data_set.csv", row.names = FALSE)
write.table(final_avg_data_set,"./derived_data_set.txt", row.names=FALSE)