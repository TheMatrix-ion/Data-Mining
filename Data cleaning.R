#---------------------------Data cleaning Yansong Fan---------------------------------------------------
#Cleaning Cambodia
Cambodia_data <- read.csv("origin data/Cambodia.CSV", stringsAsFactors = FALSE)
# Check for missing data in the dataset
Cambodia_data_NA = colSums(is.na(Cambodia_data))
print(Cambodia_data_NA)
# confirm the "Year_Month" is character
Cambodia_data$Year...Month <- as.character(Cambodia_data$Year...Month)
Cambodia_data$Formatted_Year_Month <- paste0("01-", Cambodia_data$Year...Month)
Cambodia_data$Formatted_Year_Month<- dmy(Cambodia_data$Formatted_Year_Month)
write.csv(Cambodia_data, "origin data/Cambodia_data1.csv", row.names = TRUE)
na_rows <- which(is.na(Cambodia_data$Formatted_Year_Month))
print(na_rows)

#Cleaning Ethiopia_data
Ethiopia_data <- read.csv("origin data/Ethiopia.CSV", stringsAsFactors = FALSE)
#Find the Evacuated column and Directly.affected column have 1 missing value
Ethipopia_data_NA = colSums(is.na(Ethiopia_data))
print(Ethipopia_data_NA)
Ethipopia_missing_rows_directly_affected <- which(is.na(Ethiopia_data$Directly.affected))
cat("Rows with missing 'Directly affected':", Ethipopia_missing_rows_directly_affected, "\n")
#last row so use the total value to replace
Ethiopia_Total_directly_affected <- sum(Ethiopia_data$Directly.affected, na.rm = TRUE)
Ethiopia_data$Directly.affected[Ethipopia_missing_rows_directly_affected] <- Ethiopia_Total_directly_affected
#At Evacuated column
Ethipopia_missing_rows_Evacuated <- which(is.na(Ethiopia_data$Evacuated))
cat("Rows with missing 'Directly affected':", Ethipopia_missing_rows_Evacuated, "\n")
#last row so use the total value to replace
Ethiopia_total_Evacuated <- sum(Ethiopia_data$Directly.affected, na.rm = TRUE)
Ethiopia_data$Evacuated[Ethipopia_missing_rows_Evacuated] <- Ethiopia_total_Evacuated
# confirm the "Year_Month" is character
Ethiopia_data$Year...Month <- as.character(Ethiopia_data$Year...Month)
Ethiopia_data$Formatted_Year_Month <- paste0("01-", Ethiopia_data$Year...Month)
Ethiopia_data$Formatted_Year_Month<- dmy(Ethiopia_data$Formatted_Year_Month)
na_rows <- which(is.na(Ethiopia_data$Formatted_Year_Month))
#Find out the NA values and then change this data manually
print(na_rows)
write.csv(Ethiopia_data, "origin data/Ethiopia_data.csv", row.names = TRUE)
#Some data formats are different and need to be cleaned manually

#Cleaning Ghana data
Ghana_data <- read.csv("origin data/Ghana.CSV", stringsAsFactors = FALSE)
Ghana_data_NA = colSums(is.na(Ghana_data))
print(Ghana_data_NA)
# confirm the "Year_Month" is character
Ghana_data$Year...Month <- as.character(Ghana_data$Year...Month)
Ghana_data$Formatted_Year_Month <- paste0("01-", Ghana_data$Year...Month)
Ghana_data$Formatted_Year_Month<- dmy(Ghana_data$Formatted_Year_Month)
na_rows <- which(is.na(Ghana_data$Formatted_Year_Month))
print(na_rows)
write.csv(Ghana_data, "origin data/Ghana_data.csv", row.names = TRUE)
#Some data formats are different and need to be cleaned manually

#Cleaning I.R.Iran data
I.R.Iran_data <- read.csv("origin data/I.R. Iran.CSV", stringsAsFactors = FALSE)
I.R.Iran_data_NA = colSums(is.na(I.R.Iran_data))
print(I.R.Iran_data_NA)
# confirm the "Year_Month" is character
I.R.Iran_data$Year...Month <- as.character(I.R.Iran_data$Year...Month)
I.R.Iran_data$Formatted_Year_Month <- paste0("01-", I.R.Iran_data$Year...Month)
I.R.Iran_data$Formatted_Year_Month<- dmy(I.R.Iran_data$Formatted_Year_Month)
na_rows <- which(is.na(I.R.Iran_data$Formatted_Year_Month))
print(na_rows)
write.csv(I.R.Iran_data, "origin data/I.R.Iran_data.CSV", row.names = TRUE)
#Some data formats are different and need to be cleaned manually

#Cleaning Indonesia data
Indonesia_data <- read.csv("origin data/Indonesia.CSV", stringsAsFactors = FALSE)
Indonesia_data_NA = colSums(is.na(Indonesia_data))
print(Indonesia_data_NA)
#At Relocated column
Indonesia_missing_rows_Relocated <- which(is.na(Indonesia_data$Relocated))
cat("Rows with missing 'Relocated ':", Indonesia_missing_rows_Relocated, "\n")
#last row so use the total value to replace
Indonesis_total_Relocated <- sum(Indonesia_data$Relocated, na.rm = TRUE)
Indonesia_data$Relocated[Indonesia_missing_rows_Relocated] <- Indonesis_total_Relocated
# confirm the "Year_Month" is character
Indonesia_data$Year...Month <- as.character(Indonesia_data$Year...Month)
Indonesia_data$Formatted_Year_Month <- paste0("01-", Indonesia_data$Year...Month)
Indonesia_data$Formatted_Year_Month<- dmy(Indonesia_data$Formatted_Year_Month)
na_rows <- which(is.na(Indonesia_data$Formatted_Year_Month))
print(na_rows)
write.csv(Indonesia_data, "origin data/Indonesia_data.CSV", row.names = TRUE)
#Some data formats are different and need to be cleaned manually

#Cleaning Jordan data
Jordan_data <- read.csv("origin data/Jordan.CSV", stringsAsFactors = FALSE)
Jordan_data_NA = colSums(is.na(Jordan_data))
print(Jordan_data_NA)
#At Relocated column
Jordan_missing_rows_Relocated <- which(is.na(Jordan_data$Relocated))
cat("Rows with missing 'Relocated ':", Jordan_missing_rows_Relocated, "\n")
#last row so use the total value to replace
Jordan_total_Relocated <- sum(Indonesia_data$Relocated, na.rm = TRUE)
Jordan_data$Relocated[Jordan_missing_rows_Relocated] <- Jordan_total_Relocated
# confirm the "Year_Month" is character
Jordan_data$Year...Month <- as.character(Jordan_data$Year...Month)
Jordan_data$Formatted_Year_Month <- paste0("01-", Jordan_data$Year...Month)
Jordan_data$Formatted_Year_Month<- dmy(Jordan_data$Formatted_Year_Month)
na_rows <- which(is.na(Jordan_data$Formatted_Year_Month))
print(na_rows)
write.csv(Jordan_data, "origin data/Jordan_data.CSV", row.names = TRUE)
#Some data formats are different and need to be cleaned manually

#Cleaning Mali data
Mali_data <- read.csv("origin data/Mali.CSV", stringsAsFactors = FALSE)
Mali_data_NA = colSums(is.na(Mali_data))
print(Mali_data_NA)
# confirm the "Year_Month" is character
Mali_data$Year...Month <- as.character(Mali_data$Year...Month)
Mali_data$Formatted_Year_Month <- paste0("01-", Mali_data$Year...Month)
Mali_data$Formatted_Year_Month<- dmy(Mali_data$Formatted_Year_Month)
na_rows <- which(is.na(Mali_data$Formatted_Year_Month))
print(na_rows)
write.csv(Mali_data, "origin data/Mali_data.CSV", row.names = TRUE)
#Some data formats are different and need to be cleaned manually

#Cleaning Mozambique data
Mozambique_data <- read.csv("origin data/Mozambique_data.CSV", stringsAsFactors = FALSE)
Mozambique_data_NA = colSums(is.na(Mozambique_data))
print(Mozambique_data_NA)
Mozambique_data[is.na(Mozambique_data)] <- 0
# confirm the "Year_Month" is character
Mozambique_data$Year...Month <- as.character(Mozambique_data$Year...Month)
Mozambique_data$Formatted_Year_Month <- paste0("01-", Mozambique_data$Year...Month)
Mozambique_data$Formatted_Year_Month <- dmy(Mozambique_data$Formatted_Year_Month)
na_rows <- which(is.na(Mozambique_data$Formatted_Year_Month))
print(na_rows)
write.csv(Mozambique_data, "origin data/Mozambique_data1.csv", row.names = TRUE)
#Some data formats are different and need to be cleaned manually

#Cleaning Myanmar data
Myanmar_data <- read.csv("origin data/Myanmar.CSV", stringsAsFactors = FALSE)
Myanmar_data_NA = colSums(is.na(Myanmar_data))
print(Myanmar_data_NA)
Myanmar_data$Year...Month <- as.character(Myanmar_data$Year...Month)
Myanmar_data$Formatted_Year_Month <- paste0("01-", Myanmar_data$Year...Month)
Myanmar_data$Formatted_Year_Month<- dmy(Myanmar_data$Formatted_Year_Month)
na_rows <- which(is.na(Myanmar_data$Formatted_Year_Month))
print(na_rows)
write.csv(Myanmar_data, "origin data/Myanmar_data.CSV", row.names = TRUE)
#Some data formats are different and need to be cleaned manually

#Cleaning Nepal data
Nepal_data <- read.csv("origin data/Nepal.CSV", stringsAsFactors = FALSE)
Nepal_data_NA = colSums(is.na(Nepal_data))
print(Myanmar_data_NA)
Nepal_data$Year...Month <- as.character(Nepal_data$Year...Month)
Nepal_data$Formatted_Year_Month <- paste0("01-", Nepal_data$Year...Month)
Nepal_data$Formatted_Year_Month<- dmy(Nepal_data$Formatted_Year_Month)
na_rows <- which(is.na(Nepal_data$Formatted_Year_Month))
print(na_rows)
write.csv(Nepal_data, "origin data/Nepal_data.CSV", row.names = TRUE)
#Some data formats are different and need to be cleaned manually

#Cleaning Niger data
Niger_data <- read.csv("origin data/Niger.CSV", stringsAsFactors = FALSE)
Niger_data_NA = colSums(is.na(Niger_data))
print(Niger_data_NA)
Niger_data$Year...Month <- as.character(Niger_data$Year...Month)
Niger_data$Formatted_Year_Month <- paste0("01-", Niger_data$Year...Month)
Niger_data$Formatted_Year_Month<- dmy(Niger_data$Formatted_Year_Month)
na_rows <- which(is.na(Niger_data$Formatted_Year_Month))
print(na_rows)
write.csv(Niger_data, "origin data/Niger_data.CSV", row.names = TRUE)
#Some data formats are different and need to be cleaned manually

#Cleaning Pakistan data
Pakistan_data <- read.csv("origin data/Pakistan.CSV", stringsAsFactors = FALSE)
Pakistan_data_NA = colSums(is.na(Pakistan_data))
print(Pakistan_data_NA)
Pakistan_data$Year...Month <- as.character(Pakistan_data$Year...Month)
Pakistan_data$Formatted_Year_Month <- paste0("01-", Pakistan_data$Year...Month)
Pakistan_data$Formatted_Year_Month<- dmy(Pakistan_data$Formatted_Year_Month)
na_rows <- which(is.na(Pakistan_data$Formatted_Year_Month))
print(na_rows)
write.csv(Pakistan_data, "origin data/Pakistan_data.CSV", row.names = TRUE)
#change to standard year-month
Pakistan_data <- read.csv("origin data/clean_data/Pakistan.CSV", stringsAsFactors = FALSE)
Pakistan_data$Year_Month <- format(as.Date(Pakistan_data$Year_Month, format = "%Y/%m/%d"), "%Y-%m")
write.csv(Pakistan_data, "origin data/cleandata/Pakistan.CSV", row.names = TRUE)
#Some data formats are different and need to be cleaned manually

#Data Visualisation
#In order to find more relationship between different values use different graph to show different connection.
#graph the missing data heatmap,scatter plot and line plot to show the missing values
#use different countries datas to create different charts.
Mozambique_data <- read.csv("origin data/clean_data/Mozambique.CSV", stringsAsFactors = FALSE)
# scatter plot: Year vs Event
Mozambique_scatterplot_year_Eevent <- ggplot(Mozambique_data, aes(x = Year_Month, y = Event)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  theme_minimal() +
  labs(title = "Year vs Event", x = "Year", y = "Event")
ggsave("Plot/Mozambique_Scatter_year_vs_Eevent.png", plot =Mozambique_scatterplot_year_Eevent, width = 8, height = 6, dpi = 300)

# line plot : year vs event
Mozambique_data <- read.csv("origin data/clean_data/Mozambique.CSV", stringsAsFactors = FALSE)
Pakistan_lineplot_year_Eevent <- ggplot(Mozambique_data, aes(x = Year_Month, y = Injured, group = 1)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(title = "Time Series Plot of Deaths", x = "Year-Month", y = "Deaths")
ggsave("Plot/Mozambique_lineplot_year_Eevent.png", plot =Pakistan_lineplot_year_Eevent, width = 8, height = 6, dpi = 300)

#Heatmap
Indonesia_data <- read.csv("origin data/clean_data/Indonesia.CSV", stringsAsFactors = FALSE)
numeric_cols <- sapply(Indonesia_data, is.numeric)
numeric_data <- Indonesia_data[, numeric_cols]
correlation_matrix <- cor(numeric_data, use = "complete.obs")
correlation_melt <- melt(correlation_matrix)
heatmap_plot <- ggplot(data = correlation_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap",
       x = "Variables",
       y = "Variables")
print(heatmap_plot)
ggsave("Plot/Indonesia_heatmap.png", plot = heatmap_plot, width = 8, height = 6)

# summary Cambodia statistics
data <- read.csv("origin data/clean_data/Cambodia_data.CSV", stringsAsFactors = FALSE)
Cambodia_summary_stats <- summary(data[c("Directly.affected", "Indirectly.Affected",
                                         "Houses.Destroyed", "Houses.Damaged", "Deaths", "Injured")])

# summary Ethiopia statistics
data <- read.csv("origin data/clean_data/Ethiopia_data.CSV", stringsAsFactors = FALSE)
Ethiopia_summary_stats <- summary(data[c("Directly.affected", "Indirectly.Affected",
                                         "Houses.Destroyed", "Houses.Damaged", "Deaths", "Injured")])

# summary Ghana statistics
data <- read.csv("origin data/clean_data/Ghana_data.CSV", stringsAsFactors = FALSE)
Ghana_summary_stats <- summary(data[c("Directly.affected", "Indirectly.Affected",
                                      "Houses.Destroyed", "Houses.Damaged", "Deaths", "Injured")])

#summary I.R.Iran statistics
data <- read.csv("origin data/clean_data/I.R.Iran_data.CSV", stringsAsFactors = FALSE)
I.R.Iran_summary_stats <- summary(data[c("Directly.affected", "Indirectly.Affected",
                                         "Houses.Destroyed", "Houses.Damaged", "Deaths", "Injured")])

#summary Indonesia statistics
data <- read.csv("origin data/clean_data/Indonesia_data.CSV", stringsAsFactors = FALSE)
Indonesia_summary_stats <- summary(data[c("Directly.affected", "Indirectly.Affected",
                                          "Houses.Destroyed", "Houses.Damaged", "Deaths", "Injured")])

#summary Jordan statistics
data <- read.csv("origin data/clean_data/Jordan_data.CSV", stringsAsFactors = FALSE)
Jordan_summary_stats <- summary(data[c("Directly.affected", "Indirectly.Affected",
                                       "Houses.Destroyed", "Houses.Damaged", "Deaths", "Injured")])

#summary Mali statistics
data <- read.csv("origin data/clean_data/Mali_data.CSV", stringsAsFactors = FALSE)
Mali_summary_stats <- summary(data[c("Directly.affected", "Indirectly.Affected", 
                                     "Houses.Destroyed", "Houses.Damaged", "Deaths", "Injured")])

#summary Mozambique statistics
data <- read.csv("origin data/clean_data/Mozambique_data.CSV", stringsAsFactors = FALSE)
Mozambique_summary_stats <- summary(data[c("Directly.affected", "Indirectly.Affected", 
                                           "Houses.Destroyed", "Houses.Damaged", "Deaths", "Injured")])

#summary Myanmar statistics
data <- read.csv("origin data/clean_data/Myanmar_data.CSV", stringsAsFactors = FALSE)
Myanmar_summary_stats <- summary(data[c("Directly.affected", "Indirectly.Affected", 
                                        "Houses.Destroyed", "Houses.Damaged", "Deaths", "Injured")])

#summary Nepal statistics
data <- read.csv("origin data/clean_data/Nepal_data.CSV", stringsAsFactors = FALSE)
Nepal_summary_stats <- summary(data[c("Directly.affected", "Indirectly.Affected", 
                                      "Houses.Destroyed", "Houses.Damaged", "Deaths", "Injured")])

#summary Niger statistics
data <- read.csv("origin data/clean_data/Niger_data.CSV", stringsAsFactors = FALSE)
Niger_summary_stats <- summary(data[c("Directly.affected", "Indirectly.Affected", 
                                      "Houses.Destroyed", "Houses.Damaged", "Deaths", "Injured")])

#summary Pakistan statistics
data <- read.csv("origin data/clean_data/Pakistan_data.CSV", stringsAsFactors = FALSE)
Pakistan_summary_stats <- summary(data[c("Directly.affected", "Indirectly.Affected", 
                                         "Houses.Destroyed", "Houses.Damaged", "Deaths", "Injured")])


all_summary_stats <- list(
  Cambodia = Cambodia_summary_stats,
  Ethiopia = Ethiopia_summary_stats,
  Ghana = Ghana_summary_stats,
  IRIran = I.R.Iran_summary_stats,
  Indonesia = Indonesia_summary_stats,
  Jordan = Jordan_summary_stats,
  Mali = Mali_summary_stats,
  Mozambique = Mozambique_summary_stats,
  Myanmar = Myanmar_summary_stats,
  Nepal = Nepal_summary_stats,
  Niger = Niger_summary_stats,
  Pakistan = Pakistan_summary_stats
)
# convert the summaries into data frame
convert_summary_to_df <- function(summary_stats, country) {
  stats_df <- as.data.frame(as.table(summary_stats))
  stats_df$Country <- country
  colnames(stats_df) <- c("Statistic", "Measure", "Value", "Country")
  return(stats_df)
}

summary_df <- data.frame()

# combine all the summaries
for (country in names(all_summary_stats)) {
  country_summary <- convert_summary_to_df(all_summary_stats[[country]], country)
  summary_df <- rbind(summary_df, country_summary)
}
# print and check the summary
print(summary_df)

