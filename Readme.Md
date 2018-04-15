# Getting and Cleaning Data 4th Week Assignment Solution

This repo was includes the solution of the assignment of the 4th week assignment solution. It's comprised of 3 major stages
* a) Downloading the dataset from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
     and decompressing it.
* b) Downloading run_analysis.R to your working directory
* c) Running the analysis file from within R.

### Data description
The data is comprised out of signals which were measure with waist-mounted of Samsung's smartphone. The research was conducted over 30 subjects.
The data is distributed within two subfolders: training & test. Within each subfolder there the data of two variables that were measures 
is included: 
* X - The sensor's signals.
* Y - The activity types.

The activity types are listed within the activity_labels.txt file with their coding & labels.
The names of the headers is found in the features.txt file.

### Code explaination
The code does the following:
1) Combines the test & training datasets into one dataset
2) Extracts the mean & standard deviation measurements
3) Sets the latter dataset with the appropriate headers from the features.txt file.
4) Sets the activity names with proper labels from the the activity_labels.txt file.
5) Creates a tidy dataset that summarizes the average of each variable that was extracted at step 2.

### The resulting dataset
The resulting dataset contains the average of each variable for each activity and each subject.