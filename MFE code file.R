# Read the data from the CSV file
road_accidents <- read.csv("D:/RProb/Road_Accidents_2017-Annuxure_Tables_3-1.csv")

# Define the colors for each year
colors <- c("darkblue","lightblue","darkgreen","lightgreen")

# Loop through the years and create a bar plot for each
for (i in 2:5) {
  # Create a bar plot of the number of people killed in road accidents
  # in each state for the current year
  barplot(road_accidents[,i], names.arg=road_accidents$`States/UTs`, 
          main=paste0("Number of People Killed in Road Accidents (", i+2012, ")"),
          xlab="States/UTs", ylab="Number of People Killed",
          col=colors[i-1])
}
# Load the necessary libraries
library(dplyr)
library(ggplot2)

# Read the data
weather_df_killed <- read.csv("weather_df_killed.csv")

# Filter the data for the South Zone
sub_df <- weather_df_killed[weather_df_killed$Zones == "South Zone", ]

# Create a pivot table of the data, aggregating by weather conditions and summing the number of people killed
df <- sub_df %>%
  group_by(Weather_Conditions) %>%
  summarise(Total = sum(Number_of_People_Killed))

# Transpose the data frame
df <- df %>%
  t() %>%
  as.data.frame()

# Rename the columns
names(df) <- c("Weather_Conditions", "Total")

# Drop the first row of the data frame
df <- df[-1, ]

# Sort the data frame by the total number of people killed, in descending order
df <- df %>%
  arrange(desc(Total))

# Print the first 10 rows of the data frame
head(df, 10)

# Create a bar plot of the data
ggplot(df, aes(x = Weather_Conditions, y = Total)) +
  geom_bar(stat = "identity") +
  labs(x = "Weather Conditions", y = "# of People Killed") +
  ggtitle("Southern Zone No. of People Killed in Road Accidents Based on Weather") +
  theme(axis.text.x = element_text(angle = 90))

# Filter the data for the South Zone
sub_df <- weather_df_killed[weather_df_killed$Zones == "South Zone", ]

# Create a pivot table of the data, aggregating by states/UTs and summing the number of people killed
df <- sub_df %>%
  group_by(States_UTs) %>%
  summarise(Total = sum(Number_of_People_Killed))

# Transpose the data frame
df <- df %>%
  t() %>%
  as.data.frame()

# Rename the columns
names(df) <- c("States_UTs", "Total")

# Sort the data frame by the total number of people killed, in descending order
df <- df %>%
  arrange(desc(Total))

# Print the first 5 rows of the data frame
head(df, 5)

# Create a line plot of the data
ggplot(df, aes(x = States_UTs, y = Total)) +
  geom_line() +
  labs(x = "States/UTs", y = "# of People Killed in Accidents") +
  ggtitle("Statewise No. of Accidents where People were Killed and Weather Conditions") +
  theme(axis.text.x = element_text(angle = 90))

  # Load the necessary libraries
library(dplyr)
library(seaborn)

# Read the data
weather_df_injured <- read.csv("weather_df_injured.csv")

# Create a subset of the data for the South Zone
sub_df <- weather_df_injured[weather_df_injured$Zones == "South Zone", ]

# Create a pivot table of the subset of data, aggregating by Weather Conditions
df <- sub_df %>%
  pivot_table(index = "Weather Conditions", aggfunc = sum)

# Transpose the data frame
df <- t(df)

# Reset the index
df <- as.data.frame(df)

# Rename the columns
colnames(df) <- c("Weather Conditions", "Total")

# Drop the first row
df <- df[-1, ]

# Sort the data frame by the Total column
df <- df[order(df$Total, decreasing = TRUE), ]

# Select the top 10 rows
df <- df[1:10, ]

# Create a figure and axes
fig, ax <- subplots(1, 1, figsize = (20, 10))

# Plot a bar plot of the data
sns.barplot(x = df$Weather Conditions, y = df$Total, ax = ax)

# Set the y-axis label
ax.set_ylabel("# of People Injured")

# Set the title of the plot
ax.set_title("Southern Zone No. of People Injured in Road Accidents Based on Weather", fontsize = 15)

# Rotate the x-axis labels
ax.set_xticklabels(df$Weather Conditions, rotation = 90)

# Create a new figure and axes
fig, ax <- subplots(1, 1, figsize = (20, 10))

# Create a list of the columns to plot
cols <- list(df$Clear, df$Cloudy, df$Drizzle, df$Fog, df$Heavy Rain, df$Heavy Snow, df$Mist, df$Rain, df$Snow, df$Thunderstorm)

# Loop over the list of columns and plot a line plot for each column
for (i in 1:length(cols)) {
  sns.lineplot(x = df$States/UTs, y = cols[[i]], label = cols[[i]], ax = ax)
}

# Set the y-axis label
ax.set_ylabel("# of People Injured in Accidents")

# Set the title of the plot
ax.set_title("Statewise No. of Accidents where People were Injured and Weather Conditions", fontsize = 15)

# Add a legend
ax.legend(loc = "upper left")

# Show the plots
plt.show()