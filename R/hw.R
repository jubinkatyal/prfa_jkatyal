## QUESTION 1

#' OFFENCE TRENDS FINDER
#'
#' \code{offence_trends} compares trends of frequency of chosen offence between two chosen passcodes by months
#' @param crime_data A data.table object with the following columns:
#'     "date" (POSIXct), "suburb" (chr), "postcode" (chr), "offence_level_1" (chr),
#'     "offence_level_2" (chr), "offence_level_3" (chr), "offence_count" (num).
#' @param offence_description A character string of <What are your expected inputs?>.
#' @param postcodes A two-element character vector. Each element is an SA postcode.
#' @export png A .png file is created and stored in the "output" forlder in the package
#' @return  A ggplot object showing the correlation in offence count between the two input postcodes.
#' @examples offence_trends(crime_data,"THEFT AND RELATED OFFENCES", c["5041","5010"])
#' offence_trends(crime_data_table_as_above,"any_category_in_offence_level_1", a_vector_with_characters_elements_of_2_passcodes)
offence_trends <- function(crime_data, offence_description, postcodes) {
  require(data.table)
  require(ggplot2)
  require(readxl)
  # Error catching

  if (length(postcodes) != 2) {
    stop("Please enter two postcodes")
  }
  # to compare with incoming column names
  expected_colnames <- c("date", "suburb", "postcode", "offence_level_1",
                         "offence_level_2", "offence_level_3", "offence_count")
  # error catching 2.0
  if (!all.equal(colnames(crime_data),expected_colnames)) {
    stop(paste("Input table columns need to match: ",
               paste(expected_colnames, collapse = ", ")))
  }

  # Check that the input suburbs and offence description exist in crime_data
  if (any(!postcodes %in% crime_data$postcode) |
      !offence_description %in% crime_data$offence_level_1) {
    stop("Either " + offence_description + " or any of the postcodes: " +
           postcodes[1] + ", " + postcodes[2] + " is not present in the data")
  }

  # creating datasets for each of the passcodes for plotting, using filtering, summarizing, and grouping

  plotting_data_1 <- crime_data[offence_level_1 == offence_description & postcode == as.integer(postcodes[1]),
                           list(total_offence_count = sum(as.integer(offence_count))),
                           by = month(date)]
  plotting_data_2 <- crime_data[offence_level_1 == offence_description & postcode == as.integer(postcodes[2]),
                           list(total_offence_count = sum(as.integer(offence_count))),
                           by = month(date)]
  # calculating latest year for the data
  max_year <- year(max(crime_data$date, na.rm = TRUE))
  Postcode <- postcodes[1]
  # programmatically chosing the title for the plot
  plot_title <- paste("Crime statistics for " , (max_year - 1) ,"-", max_year ,sep = " ")
  # programmatically chosing the filename for exporting the plot in the output folder
  chart_title <- paste("output/",plot_title,".png")
  # starting export feature for the .png file of the plot
  png(chart_title)
  # creating a line plot for first postcode
  v <- ggplot(size = 3) + geom_line(data = plotting_data_1,
                                    aes(x = month,
                                        y = total_offence_count,
                                        col = Postcode),
                                    size = 3)
  # within the same plot, adding a line plot for second postcode
  v <- v + geom_line(data = plotting_data_2,
              aes(x = month,
                  y = total_offence_count,
                  col = postcodes[2]),
              size = 3)
  # customizing labels and title
  v<- v + labs(x = "Months (starting from July)",
                               y = paste("Offence count for",
                                         tolower(offence_description),
                                         sep = " "),
                               title = plot_title)
  # customizing the scale of x-axis
  v <- v + scale_x_continuous(breaks = seq(1, 12, 1))
  # adding thematic effects using fonts
  V <- v + theme(axis.title.y = element_text(size=14,
                                             family = "Trebuchet MS",
                                             color = "#666666")) +
    theme(axis.text = element_text(size = 16,
                                   family ="Trebuchet MS")) +
    theme(plot.title = element_text(size = 26,
                                    family = "Trebuchet MS",
                                    face = "bold",
                                    hjust = 0,
                                    color = "#666666"))
# plotting
  plot(v)
# shutting down printing device
  dev.off()
# getting a print inside the console
  plot(v)
}

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
