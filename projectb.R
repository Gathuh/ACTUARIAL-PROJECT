setwd("/home/gathu/Documents/Coursework/PROJECT/OUR PROJECT/DATA")

##############
library(dplyr)
###############


csv_files <- list.files(pattern = "*.csv")
data_list <- lapply(csv_files, read.csv)
# Extract company name from file names
company_names <- gsub("^HistoricalPrices \\(5\\)", "", gsub(".csv", "", csv_files))

# Add company column to each dataframe in the list
for (i in seq_along(data_list)) {
  data_list[[i]]$company <- company_names[i]
}
data_list <- lapply(data_list, function(df) {
  df$Date <- as.Date(df$Date,format="%m/%d/%y")
  return(df)
})


data_list_logreturn <- lapply(data_list, function(df) {
  df %>% 
    arrange(Date) %>% 
    mutate(Log_Return = c( diff(log(Close)),NA))%>%.[-nrow(.),]%>%.[, c(1,5,7,8)]%>%
    filter(Date<"2010-12-31")
})



library(dplyr)

# Define a function to find the first occurrence date
library(dplyr)

# Define a function to find the first occurrence date
x_down <- function(Log_Return, Date, threshold) {
  index <- which(Log_Return <= threshold)[1]
  if (!is.na(index)) {
    return(Date[index])
  } else {
    return(NA)
  }
}

x_up <- function(Log_Return, Date, threshold, down_index) {
  if (is.na(down_index)) {
    return(NA)
  } else {
    index <- which(Log_Return >= threshold & seq_along(Log_Return) > down_index)[1]
    if (!is.na(index)) {
      return(Date[index])
    } else {
      return(NA)
    }
  }
}

# Apply the function to each dataframe in data_list_logreturn
data_list_logreturn_x <- lapply(data_list_logreturn, function(df) {
  df %>%
    mutate(Date_down = x_down(Log_Return, Date, -0.05),
           Date_up = x_up(Log_Return, Date, 0.05, which(Log_Return <= -0.05)[1]))%>%
    mutate(Duration=Date_up-Date_down)
})


# Convert the list into a dataframe with only rows where Date == Date_up or the last date of the dataframe
data_df_up <- do.call(rbind, lapply(data_list_logreturn_x, function(df) {
  last_date <- tail(df$Date, 1)
  filtered_df <-if( Date == Date_up)=T{filter(df,Date==Date_up)},else( Date == last_date)
  return(filtered_df)
}))



