# exporting libraries
library(data.table)
library(ggplot2)
library(readxl)
# choosing the required input
# steps for looping through all the crime files stored in "data" folder of this package
filenames <- list.files("data")

crime_idx <- grep("crime-statistics", filenames)

crime_files <- filenames[crime_idx]
# for each file, creating data table using Data.Table transformations,
# setting the names of the corresponding columns
# and then applying all the chosen inputs in the function: offence_trends
for (f in crime_files) {
  postcodes_vec <- c("5010","5013")
  offence <- "SERIOUS CRIMINAL TRESPASS"
  crime <- setDT(read_excel(file.path("data", f)))
  setnames(crime, c("date", "suburb", "postcode", "offence_level_1",
                    "offence_level_2", "offence_level_3", "offence_count"))
  offence_trends(crime, offence, postcodes_vec)
}
