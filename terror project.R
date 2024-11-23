#terror attack by niv sade and eliyho elimelch 
#packages
install.packages("readxl")
install.packages('tidyverse')
install.packages("ggplot2")
install.packages("highcharter")
library('readxl')
library('tidyverse')
library(ggplot2)
library(dplyr)
library(gridExtra)
library(highcharter)

terror_data <- read.csv("C:/Users/nivsa/OneDrive/שולחן העבודה/terror project/‏‏global terror - the upgarde.csv")

#Q1 the number of the terrorist events during the years.
#dataframe with events, success and percentage success by year
events_and_success <- terror_data %>%
  group_by(Year) %>%
  summarize(Events = n(),
            Success = sum(Success),
            Percentage_Success = (Success / Events) * 100) 

#mean_events_1970_2007
events_and_success %>%
  filter(Year >= 1970 & Year <= 2007) %>%
  summarize(Mean_Events = mean(Events))
#mean_events_2007_2017
events_and_success %>%
  filter(Year >= 2007 & Year <= 2017) %>%
  summarize(Mean_Events = mean(Events))

#graph-number 1 -  events per year+ line with numbers of success + percentage.
ggplot(events_and_success, aes(x = Year, y = Events, fill = Events)) +
  geom_col(position = position_dodge(width = 0.5)) +
  scale_x_continuous(breaks = events_and_success$Year) +
  labs(title = "Number of events per year", y = "Terror attacks") +
  theme(axis.text.x = element_text(angle = 90, size = 5)) +
  scale_fill_gradientn(colors = rev(heat.colors(3))) +
  coord_cartesian(xlim = c(1970, 2017)) +
  theme(panel.background = element_rect(fill = 'white', color = 'black')) +
  geom_text(aes(label = paste0(round(Percentage_Success, 1), "%")), 
            position = position_dodge(width = 0.5), vjust = -0.5, size = 2) +
  geom_line(data = events_and_success, aes(x = Year, y = Success), color = "black")


#Q2 - distinction in terrorist incidents by country or geographic region
# Load the world geographic data needed for plotting the map
data("worldgeojson", package = "highcharter")

# Define the colors to represent different ranges of terror attack counts
colors <- c("#FFFF00", "#FFA500", "#FF0000", "khaki")

# Aggregate the number of terror attacks for each country from the 'terror_data' dataset
country_counts <- aggregate(Longitude ~ Country, data = terror_data, FUN = length)

# Create a highchart object with empty options
world_map <- highchart(hc_opts = list()) %>%
  
  # Add the map series to the highchart with data, matching countries by name
  hc_add_series_map(
    worldgeojson, country_counts, value = "Longitude", joinBy = c("name", "Country"),
    name = "country:number of terror attacks"
  ) %>%  
  
  # Set up the color axis with specific color stops for different data ranges
  hc_colorAxis(
    mincolor = "#FFFF00",
    maxcolor = "#FF0000",
    stops = color_stops(n = length(colors), colors = colors),
    dataClasses = list(
      list(from = 0, to = 500, color = "khaki"),          # Color for 0-500 terror attacks
      list(from = 500, to = 1000, color = "#FFFF00"),     # Color for 500-1000 terror attacks
      list(from = 1000, to = 3000, color = "#FFA500"),    # Color for 1000-3000 terror attacks
      list(from = 3000, color = "#FF0000")                # Color for more than 3000 terror attacks
    )
  ) %>%
  
#title of the chart
  hc_title(text = "World Map according to terror attacks", style = list(color = "black"))

# Display the world_map
world_map

# Calculate the mean number of people killed for each target type in each region
mean_data <- selected_data4 %>%
  group_by(Region, Target_type) %>%
  summarise(Mean_Killed = mean(Number_killed))

# Get the top three target types for each region
top_three_target <- mean_data %>%
  group_by(Region) %>%
  top_n(3, wt = Mean_Killed)

# Calculate the total mean number of people killed for each region
total_mean_data <- mean_data %>%
  group_by(Region) %>%
  summarise(Total_Mean_Killed = sum(Mean_Killed))

# Calculate the percentage contribution for each region
total_mean_data <- total_mean_data %>%
  mutate(Percentage = (Total_Mean_Killed / sum(Total_Mean_Killed)) * 100)

# Merge the top three target types with the total mean data
merged_data <- inner_join(top_three_target, total_mean_data, by = "Region")

# Create the bar plot with percentage labels
ggplot(merged_data, aes(x = Region, y = Mean_Killed, fill = Target_type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Region", y = "Mean Number of People Killed", fill = "Target Type") +
  ggtitle("Mean Number of People Killed by Target Type in Each Region") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#top 3 organizations
top_organizations <- terror_data %>%
  group_by(Region, Name_of_organization) %>%
  summarise(Attacks = n()) %>%
  top_n(3, Attacks) %>%
  arrange(Region, desc(Attacks))

#graph number 2(ג) 2
ggplot(top_organizations, aes(x = reorder(Name_of_organization, -Attacks), y = Attacks, fill = Name_of_organization)) +
  geom_bar(stat = "identity") +
  labs(x = "Organization", y = "Number of Attacks", fill = "Organization") +
  ggtitle("Top 3 Organizations by Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        strip.text = element_text(size = 5)) +
  facet_wrap(~ Region, nrow = 1, scales = "free_x")

#Q3 - the infulance of the orginaztion about the events
# Group the data by  organization
grouped_data <- terror_data %>%
  filter(Name_of_organization != "Unknown") %>%
  group_by(Name_of_organization) %>%
  summarise(Total_Attacks = n()) %>%
  arrange(desc(Total_Attacks))

#the top 10 organizations per year
top_10_organizations <- grouped_data %>%
  top_n(10, Total_Attacks) %>%
  ungroup()

# Group the data by year and organization
grouped_data_year <- terror_data %>%
  filter(Name_of_organization != "Unknown") %>%
  group_by(Year, Name_of_organization) %>%
  summarise(Events = n()) %>%
  arrange(desc(Events))

#the top 10 organizations per year
top_10_organizations_year <- grouped_data_year %>%
  filter(Name_of_organization %in% top_10_organizations$Name_of_organization) %>%
  group_by(Year) %>%
  top_n(10, Events) %>%
  ungroup()


#graph number 3(א) - col graph top 10 organization
ggplot(data = top_10_organizations,
  aes(x = Name_of_organization, y = Total_Attacks, fill = Name_of_organization)) +  
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw(base_size = 8) +
  labs(title = "", x = "Group", y = "Terrorist Attacks") +
  theme(axis.text.y = element_text(size = 10))+
  scale_fill_brewer(palette = "Paired")
  

#graph number 3(ב)- of top 10 organization per year
ggplot(data = top_10_organizations_year, aes(x = Year, y = Events,
  colour = reorder(Name_of_organization, -Events))) +       
  geom_line(size = 0.8) + geom_point()  + theme_bw() +
  scale_color_brewer(palette = "Paired") 

#The correlation coefficient
top_10_separate <- grouped_data_year %>%
  filter(Name_of_organization %in% top_10_organizations$Name_of_organization) %>%
  group_by(Year) %>%
  top_n(10, Events) %>%
  ungroup() %>%
  spread(key = "Name_of_organization", value = "Events") %>%
  replace(is.na(.), 0) %>%
  left_join(events_and_success, by = "Year") %>%
  mutate(Count = rowSums(across(2:11)))
#data from top 10 organiztion
top_10_separate_melted <- top_10_separate %>%
  pivot_longer(cols = -c(Year, Events, Success, Percentage_Success, Count),
               names_to = "Organization", values_to = "Organization_Events")



#graph number 3(ג)- corralation graph total events and organization evnets
ggplot(top_10_separate_melted, aes(x = Count, y = Organization_Events)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ Organization, scales = "free") +
  xlab("Total Events") +
  ylab("Organization Events") +
  ggtitle("Total Events vs Organization Events (Facetted)") +
  theme_bw()

#Q4 - The factors affecting the success rate of terrorist attacks
#BOootstrap of the probability to success
sampales_success <- replicate(15000, sample(terror_data$Success, size = 200, replace = TRUE))
sample_means <- apply(sampales_success, 2, mean)
mean_success <- mean(terror_data$Success)
#confiendce interval
q_5 <- quantile(sample_means, probs = 0.05)
q_95 <- quantile(sample_means, probs = 0.95)
#bootsrap graph
ggplot() + 
  geom_density(aes(x = sample_means, y = after_stat(density),  fill = "Means"), alpha = 0.7) +
  geom_vline(aes(xintercept = mean(sample_means))) +
  geom_vline(xintercept = q_5, linetype = "dotted", color = "blue", size = 0.9) +
  geom_vline(xintercept = q_95, linetype = "dotted", color = "blue", size = 0.9) +
  labs(x = "Sample of success", y = "Density", title = "Bootstrap of success") +
  theme_bw()

# Q4 -table of type  by success
events_and_success_type <- terror_data %>%
  group_by(Attack_type) %>%
  summarize(Events = n(),
            Success = sum(Success),
            Percentage_Success = (Success / Events) * 100) %>%
  filter(Attack_type != "Unknown")

merged_row <- events_and_success_type %>%
  filter(Attack_type %in% c("Hostage Taking (Barricade Incident)", "Hostage Taking (Kidnapping)")) %>%
  summarize(
    Events = sum(Events),
    Success = sum(Success),
    Percentage_Success = (Success / Events) * 100
  ) %>%
  mutate(Attack_type = "Hostage")  # You can assign any label you want for the merged row

# Remove the original rows (row1 and row2) and append the merged row
events_and_success_type <- events_and_success_type %>%
  filter(!(Attack_type %in% c("Hostage Taking (Barricade Incident)", "Hostage Taking (Kidnapping)"))) %>%
  bind_rows(merged_row)
#Q4- table of target bu success 
events_and_success_target <- terror_data %>%
  group_by(Target_type) %>%
  summarize(Events = n(),
            Success = sum(Success),
            Percentage_Success = (Success / Events) * 100) %>%
  filter(!(Target_type %in% c("Other", "Unknown", "Abortion Related","NGO","Maritime",
                              "Violent Political Party", "Government (Diplomatic)", "Food or Water Supply",
                              "Telecommunication", "Tourists")))

#####
#Bootstrap of sample means of means and sd of events over the years 
sampales_events <- replicate(15000, sample(events_and_success$Events, size = 40, replace = TRUE))
sample_means <- apply(sampales_events, 2, mean)
sample_sd <- apply(sampales_events, 2, sd)
ggplot() + 
  geom_density(aes(x = sample_means, y = after_stat(density),  fill = "Means"), alpha = 0.7)+
  geom_density(aes(x = sample_sd, y = after_stat(density), fill = "Sd"), alpha = 0.7)+
  geom_vline(aes(xintercept = mean(sample_means))) +
  labs(x = "Sample Means", y = "density", title = "Bootstrap to events over the years") +
  theme_bw()


#Q5 - connection between the various characterizations of terrorism, the type of attack and the lethality of the attacks
#killed_by_attack taype
killed_attack_type <- terror_data %>%
  filter(!is.na(Number_killed)) %>%
  group_by(Attack_type) %>%
  summarise(Total_Killed = sum(Number_killed),
            Mean_Killed = mean(Number_killed))

# Pie chart for mean number of people killed by attack type
total_pie_chart <- ggplot(killed_attack_type, aes(x = "", y = Total_Killed, fill = Attack_type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "                  Total Number of People Killed",
       fill = "attack type",
       x = NULL,
       y = NULL) +
  theme_void() +
  scale_fill_brewer(palette = "Paired") +
  geom_text(aes(label = Total_Killed), position = position_stack(vjust = 0.5), size = 2)

# Pie chart for total number of people killed by attack type
mean_pie_chart <- ggplot(killed_attack_type, aes(x = "", y = Mean_Killed, fill = Attack_type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "                 mean Number of People Killed",
       fill = "attack type",
       x = NULL,
       y = NULL) +
  theme_void() +
  scale_fill_brewer(palette = "Paired")+
  geom_text(aes(label = round(Mean_Killed, 2)), position = position_stack(vjust = 0.5), size = 2)
combined_pie_charts <- grid.arrange(total_pie_chart, mean_pie_chart, ncol = 2)


