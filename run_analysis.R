library(dplyr)
library(data.table)
# Load the 'Maps'
feature_names <- read.table("features.txt")
activity_labels <- read.table("activity_labels.txt")

# Load the Training Data (The biggest files)
train_x <- read.table("train/X_train.txt")
train_y <- read.table("train/y_train.txt")
train_subject <- read.table("train/subject_train.txt")

# Load the Test Data


test_x <- read.table("test/X_test.txt")
test_y <- read.table("test/y_test.txt")
test_subject <- read.table("test/subject_test.txt")
# Stack the measurements, activities, and subjects
all_x <- rbind(train_x, test_x)
all_y <- rbind(train_y, test_y)
all_subject <- rbind(train_subject, test_subject)

colnames(all_subject) <- "subject"
colnames(all_y) <- "activity"
# We'll use the feature_names we loaded earlier for all_x later!

# Combine into one master dataset
master_data <- cbind(all_subject, all_y, all_x)

# 1. Find which column names have "mean()" or "std()" in them
# We use grep to search for these specific patterns
required_columns <- grep("mean\\(\\)|std\\(\\)", feature_names$V2)

# 2. Subset the master_data
# We keep Subject (col 1), Activity (col 2), and then our required measurement columns
# We add +2 to the column indices because we added Subject and Activity at the front
extracted_data <- master_data[, c(1, 2, required_columns + 2)]
# Use the activity_labels file to replace numbers with names
extracted_data$activity <- activity_labels[extracted_data$activity, 2]

# Get the current names
current_names <- names(extracted_data)

# Clean them up using 'gsub' (which stands for Global Substitute)
current_names <- gsub("^t", "Time", current_names)
current_names <- gsub("^f", "Frequency", current_names)
current_names <- gsub("Acc", "Accelerometer", current_names)
current_names <- gsub("Gyro", "Gyroscope", current_names)
current_names <- gsub("Mag", "Magnitude", current_names)
current_names <- gsub("-mean\\(\\)", "Mean", current_names)
current_names <- gsub("-std\\(\\)", "STD", current_names)

# Put the clean names back onto the data
names(extracted_data) <- current_names

# First, let's get the names from the 'feature_names' file for our specific columns
selected_feature_names <- feature_names$V2[required_columns]

# Apply them to our extracted_data (skipping the first two: subject and activity)
names(extracted_data)[3:ncol(extracted_data)] <- as.character(selected_feature_names)

# NOW run the cleaning code from before to make them pretty
current_names <- names(extracted_data)
current_names <- gsub("^t", "Time", current_names)
current_names <- gsub("^f", "Frequency", current_names)
current_names <- gsub("Acc", "Accelerometer", current_names)
current_names <- gsub("Gyro", "Gyroscope", current_names)
current_names <- gsub("Mag", "Magnitude", current_names)
current_names <- gsub("-mean\\(\\)", "Mean", current_names)
current_names <- gsub("-std\\(\\)", "STD", current_names)
current_names <- gsub("-", "", current_names) # Remove extra dashes

names(extracted_data) <- current_names

# Create the second, independent tidy data set
tidy_data <- extracted_data %>%
  group_by(subject, activity) %>%
  summarise_all(mean)

# Save the file to your folder
write.table(tidy_data, "tidy_data.txt", row.name=FALSE)
