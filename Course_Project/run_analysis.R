#This program:
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Transforms labels to descriptive variable names, by supressing special characters
#Creates a dataset named "DF" containing the information metioned above
#Creates a second data set named "DF_avg" with the average of each variable for each activity 
#and each subject.
#Finally, both datasets are written to files "DF.txt" and "DF_avg.txt" in the current directory 


# Including libraries
library(readr)
library(dplyr)

# Reading Features
features <- read_table2("features.txt", col_names = c("Feature_ID", "Feature_Description"), col_types = c(col_integer(), col_character()))

# Adjusting variable names to make them more readable
avector<- as.vector(features$Feature_Description)
# Substitute character "-" by "_" in description
avector2<- gsub("-", "_", avector)
# Substitute character "(" by "nothing" in description
avector3<- gsub("\\(", "", avector2)
# Substitute character ")" by "nothing" in description
avector4<- gsub("\\)", "", avector3)
# Substitute character "," by "_" in description
avector5<- gsub("\\,", "_", avector4)
# Substitute the initial 't' by the word 'time_'
avector6<- gsub("^t", "time_", avector5)
# Substitute the initial 'f' by the word 'frequency_'
avector7<- gsub("^f", "frequency_", avector6)
# after these changes, for instance, the variable name "fBodyGyro-bandsEnergy()-1,24" was changed to "frequency_BodyGyro_bandsEnergy_1_24"
var_names_vector <- avector7


# Merging Subjects and adding column names/types
subject_test<- read_table2("test/subject_test.txt", col_names = "Subject_ID", col_types = c(col_integer()))
subject_train<- read_table2("train/subject_train.txt", col_names = "Subject_ID", col_types = c(col_integer()))
subject_total<- rbind(subject_test, subject_train)

# Merging Activities and adding column names/types
activity_test <- read_table2("test/y_test.txt", col_names = "Activity_ID", col_types = c(col_integer()))
activity_train<- read_table2("train/y_train.txt", col_names = "Activity_ID", col_types = c(col_integer()))
activity_total <- rbind(activity_test, activity_train)
# obtaining activity labels
activity_labels<- read_table2("activity_labels.txt", col_names = c("Activity_ID", "Activity_Description"), col_types = c(col_integer(), col_character()))
# adding activity labels to activities
activities<- inner_join(activity_total, activity_labels, by = c("Activity_ID"))


# Merging Measures and adding column names/types
X_test <- suppressWarnings(read_table2("test/X_test.txt", col_names = var_names_vector, col_types = c(col_double())))
X_train<- suppressWarnings(read_table2("train/X_train.txt", col_names = var_names_vector, col_types = c(col_double())))
X_total<- rbind(X_test, X_train)

# Selecting only the "mean" and "std" related variables
selected_variables_mean<- grep("*[Mm]ean*", var_names_vector, value = TRUE)
selected_variables_std <- grep("*[Ss]td*",  var_names_vector,  value = TRUE)
df_mean <- select(X_total, selected_variables_mean)
df_std  <- select(X_total, selected_variables_std)

# Creating the whole dataset showing: activity description, subject ID and all the variables related to mean and std
DF <- cbind(select(activities, "Activity_Description"), subject_total, df_mean, df_std)

# Creating file with output dataset
if (file.exists("DF.txt")) file.remove("DF.txt")	
write.table(DF, "DF.txt", row.names = FALSE)


# Grouping dataset by Activity Description and Subject ID
DF_grouped <- group_by(DF, Activity_Description, Subject_ID)

# Summarising data set
selected_fields <- append(selected_variables_mean, selected_variables_std)
DF_avg <- summarise_at(DF_grouped,selected_fields, mean) 

# Creating summarised file with output dataset
if (file.exists("DF_avg.txt")) file.remove("DF_avg.txt")	
write.table(DF_avg, "DF_avg.txt", row.names = FALSE)


