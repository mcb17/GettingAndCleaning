# Download all files we need

train <- read.table("UCIHARDataset/train/X_train.txt")
ytrain <- read.table("UCIHARDataset/train/y_train.txt")
subjecttrain <- read.table("UCIHARDataset/train/subject_train.txt")

test <- read.table("UCIHARDataset/test/X_test.txt")
subjectest <- read.table("UCIHARDataset/test/subject_test.txt")
ytest <- read.table("UCIHARDataset/test/y_test.txt")

# Join every train table to make one unique table

trainVF <- cbind(subjecttrain, ytrain, train)

# Join every test table to make one unique table

testVF <- cbind(subjectest, ytest, test)

# Join test and train table to make one unique table

dataVF <- cbind(trainVF, testVF)

# Download feature table

feature <- read.table("UCIHARDataset/features.txt")

# Determine which number is corresponding to mean or standard deviation metrics.

y <- list()
for (i in 1: nrow(feature)) {
        if ( length(grep("mean|std", feature$V2[i])) > 0 ) {
                y <- rbind(y, i)
        }
}

# We make a vector of these numbers

y <- as.double(y)

# Create a unique dataframe with only means and standard deviation column (thanks to the previous list)

new <- as.data.frame(dataVF[, y[3]])
colnames(new) <- feature$V2[y[3]]

for (i in 2: length(y) ) {
        table <- as.data.frame(dataVF[, y[i+2]])
        colnames(table) <- feature$V2[y[i]]
        new <- cbind(new, table)   
}

# Download activity names file

activity <- read.table("UCIHARDataset/activity_labels.txt")

# rename main dataframe columns

name <- c("VF1")
for (i in 2 : length(dataVF) ) {
        name <- rbind(name, paste0("VF", i))
}

colnames(dataVF) <- name

# Change activity number by activity label (in second column = VF2)

for ( i in 1 : length(dataVF$VF2) ) {
        dataVF$VF2[i] = as.character(activity[dataVF$VF2[i] ,2])
}

# Change columns names

nameColumn <- data.frame()
nameColumn <- rbind("Subject", "Activity")

for (i in 1 : nrow(feature) ) {
        nameColumn <- rbind(nameColumn, paste0(as.character(feature$V2[i]), i))
}

colnames(dataVF2) <- nameColumn

# Create a second dataset and group by subject and activity

data_summary <- dataVF2 %>%
        group_by(Subject,Activity) %>%
        summarise_each(funs(mean))

# View the result

View(data_summary)

# Download a file with this dataFrame

write.table(data_summary, file = "tidyData.txt", row.name = FALSE)
