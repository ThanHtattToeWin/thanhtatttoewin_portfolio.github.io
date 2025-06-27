# Load the 'readr' package, which provides functions for reading and writing CSV files
library(readr)

# Import the CSV file from the specified path into a data frame named 'data'
data <- read_csv("F:/Data Analytics/Coursework2/data.csv")

# Load necessary libraries for data visualization and manipulation
# 'ggplot2' is used for creating plots, 'dplyr' for data manipulation, and 'plotly' for creating interactive plots
library(ggplot2)
library(dplyr)
library(plotly)

# Check the first few rows of the data set to ensure data is loaded correctly
head(data)

# Load the visdat package to visualize missing values
library(visdat)
# Visualize missing values in the data set
vis_miss(data)
# The data set does not contain any missing values. 
# This is confirmed by the absence of NA values across all columns.

# Check the structure of the data set to understand the types of variables and their formats
str(data)

# Convert the 'Date' column to the Date format
data$Date <- as.Date(data$Date, format = "%B %d %Y")

# Check if the Date column is now in Date format
str(data$Date)

# Extract the year from the 'Date' column and add it as a new column
data$Year <- as.numeric(format(data$Date, "%Y"))

# Check for unique values in the Year column to ensure it was extracted correctly
unique(data$Year)

# To calculate summary statistics
# Summarize the data by 'County' and 'Year' to calculate total and mean values at the county and year level
summary_data <- data %>%
  group_by(County, Year) %>%
  summarize(
    Total_Vehicles = sum(`Total Vehicles`, na.rm = TRUE),  
    Total_EVs = sum(`Electric Vehicle (EV) Total`, na.rm = TRUE),  
    Mean_Percent_EVs = (Total_EVs / Total_Vehicles) * 100
  )

# Define colorblind-friendly colors
color_palette <- c("2022" = "#0072B2",  # Blue (colorblind-friendly)
                   "2023" = "#E69F00")  # Orange (colorblind-friendly)

# Create the plot
summary_plot <- ggplot(summary_data, aes(
  x = reorder(County, Mean_Percent_EVs),
  y = Mean_Percent_EVs,
  fill = as.factor(Year),
  text = paste0("County: ", County, "\n",
                "Year: ", Year, "\n",
                "Mean Percent EVs: ", round(Mean_Percent_EVs, 2),  "%\n",
                "Total EVs: ", Total_EVs, "\n",
                "Total Vehicles: ", Total_Vehicles
  )
)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.85, width = 0.7) +
  coord_flip() +  # Flip the axes for better readability
  labs(
    title = "Mean Percent of Electric Vehicles by County (2022 vs 2023)",
    subtitle = "Based on aggregated vehicle data",
    x = "County",
    y = "Mean Percent Electric Vehicles (%)",
    fill = "Year"
  ) +
  theme_minimal(base_size = 12) +  # Cleaner theme with readable font size
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.position = "right",  # Place legend beside the graph
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  scale_fill_manual(values = color_palette)  # Use the defined color palette

# To view summary statistics plot of counties values
summary_plot


# Next, to build plot1 map for final visualization
# Load libraries for map visualization
library(maps)      # For creating maps
library(mapproj)   # For map projections

# Load Washington state map data
wa_map <- map_data("county") %>%
  filter(region == "washington")  # Filter only for Washington state
wa_map

# Check if data is loaded correctly
head(data)
str(data)

# Aggregate data by County and calculate the total number of Electric Vehicles (EVs)
data_aggregated <- data %>%
  group_by(County) %>%
  summarize(
    `Electric Vehicle (EV) Total` = sum(`Electric Vehicle (EV) Total`, na.rm = TRUE)  # Sum EVs for each county
  )

# Check unique County names
unique(data_aggregated$County)

# Check unique subregion names in the map data
unique(wa_map$subregion)

# Convert both County and subregion to lowercase
data_aggregated$County <- tolower(data_aggregated$County)
wa_map$subregion <- tolower(wa_map$subregion)

# Now join again with the cleaned data
wa_ev_map <- left_join(wa_map, data_aggregated, by = c("subregion" = "County"))

# Check the joined data
summary(wa_ev_map)
head(wa_ev_map)

# Load libraries for color scales and string manipulation
library(viridis)  # Provides color palettes for data visualization
library(stringr)  # Functions for string manipulation

# Capitalize the first letter of each subregion name
wa_ev_map$subregion <- str_to_title(wa_ev_map$subregion)

# Calculate the total EVs across all counties
total_ev <- sum(data_aggregated$`Electric Vehicle (EV) Total`, na.rm = TRUE)

# Calculate the percentage of electric vehicles (EVs) for each county
data_aggregated <- data_aggregated %>%
  mutate(
    EV_Percentage = (`Electric Vehicle (EV) Total` / total_ev) * 100
  )

library(tools)  # For toTitleCase function

# Join the map data with the aggregated data
wa_ev_map <- left_join(wa_map, data_aggregated, by = c("subregion" = "County")) %>%
            mutate(subregion = toTitleCase(subregion))  # Capitalize the first letter of each word

# Create a choropleth map (plot1) of electric vehicle adoption across Washington counties
plot1 <- ggplot(wa_ev_map) +
  # Plot polygons for each county, color based on total EVs, with hover text
  geom_polygon(aes(
    x = long, y = lat, group = group,
    fill = `Electric Vehicle (EV) Total`, 
    text = paste(
      "County Name: ", subregion, 
      "<br>Registered EVs Vehicles: ", `Electric Vehicle (EV) Total`, 
      "<br>EV Penetration Rate: ", round(EV_Percentage, 2), "%"
    )
  ), colour = "black") +
  scale_fill_viridis(option = "D", trans = "log10", 
                     breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000, 5000000), 
                     labels = c("1", "10", "100", "1k", "10k", "100k", "1M", "5M")) +
  labs(title = "Electric Vehicle Adoption Across Washington Counties, 2022–2023", 
       x = "Longitude", y = "Latitude", 
       fill = "Electric Vehicle (EV) Total\n(Logarithmic Scale)") +
  theme_minimal()  # Minimal theme for a professional look
plot1

# Convert ggplot map (plot1) to an interactive plotly map
interactive_plot1 <- ggplotly(plot1, tooltip = "text")

# Display the interactive plot
interactive_plot1


# Next, to build plot2 graph for final visualization
# Load the 'tidyr' package for data reshaping and manipulation
library(tidyr)

# Reshape the data from wide format to long format for easier plotting and analysis
# The 'pivot_longer' function combines the selected columns ('BEVs', 'PHEVs', 'EV Total') into a new 'Vehicle_Type' column 
# and their corresponding values into the 'Population' column. 
# The 'filter' function removes any rows where 'Population' is NA.
data_long <- data %>%
  pivot_longer(
    cols = c('Battery Electric Vehicles (BEVs)', 'Plug-In Hybrid Electric Vehicles (PHEVs)', 'Electric Vehicle (EV) Total'),
    names_to = 'Vehicle_Type',
    values_to = 'Population'
  ) %>%
  filter(!is.na(Population))  # Remove rows with NA values

# Check the structure of the reshaped data
head(data_long)

# Select specific columns from the reshaped data
selected_data <- data_long %>%
  select(Date, County, `Vehicle Primary Use`, Vehicle_Type, Population)

# Check the selected columns
head(selected_data)

# Ensure the Date column is in date format
selected_data_dateordered <- selected_data %>%
  arrange(Date)  # Sort by Date in ascending order

# View the sorted data
head(selected_data_dateordered)

# Group the data by 'Date' and 'Vehicle_Type' to calculate the total population for each vehicle type on each date
data_summarized <- selected_data_dateordered %>%
  group_by(Date, Vehicle_Type) %>%
  summarise(Total_Population = sum(Population, na.rm = TRUE)) %>%
  ungroup()

# Check the summarized data
head(data_summarized)

# Sort data by Date for "Electric Vehicle (EV) Total"
data_ev_total <- data_summarized %>%
  filter(Vehicle_Type == "Electric Vehicle (EV) Total") %>%
  arrange(Date)

# Calculate BEV and PHEV percentages within each Date (relative to Total EVs: BEVs + PHEVs)
data_summarized <- data_summarized %>%
  group_by(Date) %>%
  mutate(
    Total_EV_Per_Date = sum(Total_Population[Vehicle_Type %in% c("Battery Electric Vehicles (BEVs)", "Plug-In Hybrid Electric Vehicles (PHEVs)")]),
    BEV_Percent = ifelse(Vehicle_Type == "Battery Electric Vehicles (BEVs)", 
                         (Total_Population / Total_EV_Per_Date) * 100, 
                         NA),
    PHEV_Percent = ifelse(Vehicle_Type == "Plug-In Hybrid Electric Vehicles (PHEVs)", 
                          (Total_Population / Total_EV_Per_Date) * 100, 
                          NA)
  ) %>%
  ungroup()

# Create the plot
plot2 <- ggplot(data_summarized, aes(
  x = Date,
  y = Total_Population,
  fill = Vehicle_Type
)) +
  # Plot BEVs and PHEVs as bars with hover text
  geom_bar(data = subset(data_summarized, Vehicle_Type %in% c("Battery Electric Vehicles (BEVs)", "Plug-In Hybrid Electric Vehicles (PHEVs)")),
           stat = "identity", position = "stack", alpha = 0.9, width = 20, 
           aes(text = paste("Date:", format(Date, "%b %Y"), 
                            "<br>Vehicle Type:", Vehicle_Type, 
                            "<br>Total:", Total_Population, 
                            "<br>Percentage:", 
                            ifelse(Vehicle_Type == "Battery Electric Vehicles (BEVs)", round(BEV_Percent, 2), round(PHEV_Percent, 2)), "%")
           )) +  
  # Plot EV Total as a line with hover text
  geom_line(data = data_ev_total,
            aes(group = 1, color = "Electric Vehicle (EV) Total", text = paste("Date:", format(Date, "%b %Y"), 
                                                                               "<br> Electric Vehicle (EV) Total: ", Total_Population)), size = 1.3) +  
  # Add points for EV Total line with hover text
  geom_point(data = data_ev_total,
             aes(color = "Electric Vehicle (EV) Total", text = paste("Date:", format(Date, "%b %Y"), 
                                                                     "<br> Electric Vehicle (EV) Total: ", Total_Population)), size = 3.5) +
  # Add titles and labels
  labs(
    title = "Electric Vehicle Population Chart for Washington, 2022-2023",
    x = "Month Reported by Department of Licensing",
    y = "Currently Registered Electric Vehicles",
    fill = "Vehicle Type"
  ) +
  # Format x-axis to show every month
  scale_x_date(
    breaks = "1 month",
    labels = scales::date_format("%b %Y")
  ) +
  theme_minimal() +  
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.grid.major = element_line(size = 0.5, color = "gray80"),
    panel.grid.minor = element_line(size = 0.25, color = "gray90", linetype = "dotted"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.spacing.y = unit(0.3, "lines")
  ) +
  scale_fill_manual(values = c(
    "Battery Electric Vehicles (BEVs)" = "#440154FF",
    "Plug-In Hybrid Electric Vehicles (PHEVs)" = "#21908CFF"
  )) +
  scale_color_manual(values = c(
    "Electric Vehicle (EV) Total" = "#FDE725FF"
  )) +
  guides(
    color = guide_legend(title = NULL),
    fill = guide_legend(title = NULL)
  )
plot2 

# Convert ggplot to an interactive plotly plot
interactive_plot2 <- ggplotly(plot2, tooltip = "text")

# Display the interactive plot
interactive_plot2


# Combine both plots into a single file (side-by-side layout) with defined height and adjust position
combined_plot <- subplot(
  interactive_plot1, interactive_plot2,
  nrows = 1, 
  widths = c(0.6, 0.4),  # Adjust width proportions
  titleX = TRUE, titleY = TRUE,  # Optional: Show axis titles
  margin = 0.05  # Optional: Adjust spacing between plots
) %>% 
  layout(
    height = 600,  # Set the height in pixels
    margin = list(t = 100)  # Add a top margin of 100 pixels
  )

# Display the combined plot
combined_plot

# Export the combined plot as an HTML file
htmlwidgets::saveWidget(combined_plot, "visualisation_2.html")
# We can open the visualisation_2.html file using any modern web browser (e.g., Chrome, Firefox, Edge)




