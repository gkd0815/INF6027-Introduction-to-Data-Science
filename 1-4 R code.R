# First graph about Employment and Unemployment
# Load necessary libraries
library(ggplot2)
library(plotly)
library(dplyr)
library(patchwork)


# Read actual data file
data <- read.csv("/Users/gkd0815/Documents/Data Visualization/R/data set/employment and unemployment rate.csv")

# Ensure column names match, convert 'Years' to Date type
# assuming each year starts on January 1, to prepare for time series plotting.
data$Years <- as.Date(paste0(data$Years, "-01-01"))

# Plot dual-axis time series with smoothed lines and custom annotations
p <- ggplot(data, aes(x = Years)) +
  geom_line(aes(y = Employment.rate, color = "Employment Rate"), size = 1.2) +
  geom_smooth(aes(y = Employment.rate, color = "Employment Rate"), method = "loess", se = FALSE, linetype = "dashed") +
#Smooths the lines with Loess to emphasize long-term trends.
  geom_line(aes(y = Unemployment.rate, color = "Unemployment Rate"), size = 1.2) +
  geom_smooth(aes(y = Unemployment.rate, color = "Unemployment Rate"), method = "loess", se = FALSE, linetype = "dashed") +
  scale_y_continuous(
    name = "Employment Rate (%)",
    sec.axis = sec_axis(~., name = "Unemployment Rate (%)")
  ) +
  scale_color_manual(values = c("Employment Rate" = "blue", "Unemployment Rate" = "red")) +
  labs(title = "Employment and Unemployment Rates Over Time",
       x = "Year") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    legend.position = "bottom"
  )

# Add dynamic annotations for key years
highlight_years <- data.frame(
  year = c(1980, 1990, 2008, 2020),
  label = c("1980 Recession", "1990 Economic Downturn", "2008 Financial Crisis", "2020 COVID-19 Pandemic")
)

# The for loop iterates through the key years to add corresponding annotations.
for (i in 1:nrow(highlight_years)) {
# Filters data for specific years.
  year_data <- data %>% filter(format(Years, "%Y") == as.character(highlight_years$year[i]))
  if (nrow(year_data) > 0) {
    p1 <- p +
# annotate("text") and annotate("point"): Adds text annotations and points at key years
      annotate("text", x = as.Date(paste0(highlight_years$year[i], "-01-01")), y = max(year_data$Employment_rate), 
               label = highlight_years$label[i], color = "black", size = 4, hjust = -0.1) +
      annotate("point", x = as.Date(paste0(highlight_years$year[i], "-01-01")), y = max(year_data$Employment_rate), 
               color = "black", size = 3)
  }
}

# Convert to interactive plot and enable dynamic tooltips
# ggplotly converts the static plot into an interactive plot, enabling hover tooltips that display x-axis, y-axis, and color values.
interactive_plot <- ggplotly(p, tooltip = c("x", "y", "color"))

# Add slider and zoom functionality
interactive_plot <- interactive_plot %>%
  layout(
# rangeslider: Adds a time slider to allow users to view specific periods.
    xaxis = list(rangeslider = list(type = "date")),
    title = list(text = "<b>Employment and Unemployment Rates Over Time</b>")
  )

interactive_plot


















# Second graph is about the relationship between population and sex's employment rate
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(plotly)

# Load the dataset
data <- read.csv("/Users/gkd0815/Documents/Data Visualization/R/data set/Sex and population and years.csv")

# 'Years' -> Date ,assuming Jan. 1st
data$Years <- as.Date(paste0(data$Years, "-01-01"))

# Scale down Population to one millionth of the original value
population_scale <- 1e-6  # Scaling population to 1/1000000
data$Scaled_Population <- data$Population * population_scale

# Create a stacked area plot with added smooth curves for male and female employment rates and overlay a line plot for scaled population
p <- ggplot(data, aes(x = Years)) +
  # Stacked area plot for male and female employment rates
  geom_area(aes(y = Male, fill = "Male Employment Rate"), alpha = 0.5) +
  geom_area(aes(y = Female, fill = "Female Employment Rate"), alpha = 0.5) +
  
  # Smooth curves for male and female employment rates
  geom_smooth(aes(y = Male, color = "Male Employment Trend"), method = "loess", se = FALSE, linetype = "solid", size = 1.2) +
  geom_smooth(aes(y = Female, color = "Female Employment Trend"), method = "loess", se = FALSE, linetype = "dashed", size = 1.2) +
  
  # Scaled population line plot
  geom_line(aes(y = Scaled_Population, color = "Population (Scaled to 1/1,000,000)"), size = 1.5, linetype = "dotdash") +
  
  # Configure y-axes
  scale_y_continuous(
    name = "Employment Rate (%)",
    sec.axis = sec_axis(~./population_scale, name = "Population")
  ) +
  
  # Color and fill settings
  scale_fill_manual(values = c("Male Employment Rate" = "lightblue", "Female Employment Rate" = "lightcoral")) +
  scale_color_manual(values = c("Male Employment Trend" = "blue", "Female Employment Trend" = "red", "Population (Scaled to 1/1,000,000)" = "green")) +
  
  # Title and labels
  labs(title = "Employment Rates by Gender with Population Trend (Scaled)",
       x = "Year",
       fill = "Employment Rates",
       color = "Legend") +
  
  # Theme customization
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title.y = element_text(color = "blue", size = 12),
    axis.title.y.right = element_text(color = "green", size = 12),
    legend.position = "top"
  )

# Just indicate the main years
notable_years <- c(1980, 1990, 2000, 2010, 2020)
for (year in notable_years) {
  p2 <- p + 
# geom_vline: Adds vertical dotted lines at specified years to highlight key moments
    geom_vline(xintercept = as.Date(paste0(year, "-01-01")), linetype = "dotted", color = "black", size = 0.8)  # Key year markers
}

# Convert to an interactive plot with plotly for enhanced interactivity
interactive_plot <- ggplotly(p, tooltip = c("x", "y", "fill", "color"))

# Adjust layout to reduce legend width and place it at the top without overlapping title
interactive_plot <- interactive_plot %>%
  layout(
    legend = list(
      orientation = "h",        # Place legend horizontally
      x = 0.5,                  # Center the legend
      xanchor = "center",
      y = -0.15,                # Place legend below title to avoid overlap
      traceorder = "normal",
      font = list(size = 10),   # Adjust font size for smaller legend
      itemwidth = 30            # Reduce legend item width
    )
  )

# Display the interactive plot
interactive_plot












# Third graph is about Vacancy and Unemployment
# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(scales)

# Define the file path
excel_path <- "/Users/gkd0815/Documents/Data Visualization/R/data set/Vacancy and Unemployment.xlsx"

# Load and inspect the data
# Loads the Excel file from the specified path and stores it in data_raw
data_raw <- read_excel(excel_path, col_names = TRUE)

# Data transformation
data_long <- data_raw %>%
  # Ensure 'Year' column exists and extract the year part
  # Converting columns to numeric types and handling any non-numeric values.
  mutate(Year = as.integer(gsub("([A-Za-z]+)-[A-Za-z]+ (\\d{4})", "\\2", Year))) %>%
  # Convert columns to numeric and handle any non-numeric entries gracefully
  mutate(
    `All Vacancies1 (thousands)` = as.numeric(`All Vacancies1 (thousands)`),
    `Unemployment2 (thousands)` = as.numeric(`Unemployment2 (thousands)`),
    `Number of unemployed people per vacancy` = as.numeric(`Number of unemployed people per vacancy`)
  ) %>%
  # Reshape data to long format for easier plotting
  # privot_long is more suitable for plotting
  pivot_longer(cols = c(`All Vacancies1 (thousands)`, `Unemployment2 (thousands)`, `Number of unemployed people per vacancy`), 
               names_to = "Metric", values_to = "Value") %>%
  # Scale 'Number of unemployed people per vacancy' for better visibility
  mutate(Value = ifelse(Metric == "Number of unemployed people per vacancy", Value * 1000, Value)) %>%
  # Remove any NA or infinite values
  filter(!is.na(Value) & is.finite(Value))

# Verify transformed data structure/Checking
# The steps is used to reduce the amount of checking in case of errors in the code

print("Transformed data structure:")
print(str(data_long))

# Create the plot
p3 <- ggplot(data_long, aes(x = Year, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("blue", "orange", "green")) +
  labs(title = "Job Vacancies and Unemployment in the UK (2001 - 2024)",
       x = "Year",
       y = "Count (thousands)",
       fill = "Metrics") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  ) +
  # Add smooth trend lines for each metric
  geom_smooth(data = data_long %>% filter(Metric == "All Vacancies1 (thousands)"),
              aes(y = Value), method = "loess", se = FALSE, color = "blue", size = 1.2) +
  geom_smooth(data = data_long %>% filter(Metric == "Unemployment2 (thousands)"),
              aes(y = Value), method = "loess", se = FALSE, color = "green", size = 1.2) +
  geom_smooth(data = data_long %>% filter(Metric == "Number of unemployed people per vacancy"),
              aes(y = Value), method = "loess", se = FALSE, color = "orange", size = 1.2)

# Display the plot
print(p3)















# The forth graph is a heat map
# Load necessary libraries
library(readxl)    
library(dplyr)     # For data manipulation
library(tidyr)     # For data reshaping
library(ggplot2)   
library(sf)        # For handling spatial data
library(patchwork) 
library(geojsonsf) # For reading GeoJSON files
library(purrr)     # For functional programming utilities

# Define file paths for Excel and JSON data
excel_path <- "/Users/gkd0815/Documents/Data Visualization/R/data set/2018 Turism Industry in UK.xls"
json_path <- "/Users/gkd0815/Documents/Data Visualization/R/data set/UK.json"

# Read the Excel file and clean up the data
employment_data <- read_excel(excel_path, col_names = FALSE)  # Read without headers initially
colnames(employment_data) <- employment_data[1, ]  # Set first row as column names
employment_data <- employment_data[-1, ]           # Remove first row (now used as column names)

# Rename and structure data for Main Job and Second Job
employment_data <- employment_data %>%
  rename(JobType = `Erea`) %>%                     # Rename 'Erea' column to 'JobType'
  mutate(JobType = ifelse(row_number() == 1, "Main Job", "Second Job")) # Define job types

# Reshape data to long format for easier plotting
employment_long <- employment_data %>%
  pivot_longer(cols = -JobType, names_to = "Region", values_to = "Employment") %>% # Reshape to long format
  pivot_wider(names_from = JobType, values_from = Employment)                      # Spread to wide format by JobType

# Read GeoJSON file and convert to sf object
# Reads the UK geographic data from a GeoJSON file and converts it to an sf object for mapping
uk_geo <- geojson_sf(json_path)    # Convert GeoJSON to spatial (sf) object for mapping

# Define region mapping list with standardized names
# Manual categorisation grouping
region_mapping <- list(
  "Tyne and Wear" = c("Gateshead", "NewcastleuponTyne", "NorthTyneside", "SouthTyneside", "Sunderland"),
  "Rest of North East" = c("CountyDurham", "Darlington", "Hartlepool", "Middlesbrough", "StocktononTees", "RedcarandCleveland", "Northumberland"),
  "Greater Manchester" = c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan"),
  "Merseyside" = c("Knowsley", "Liverpool", "Sefton", "StHelens", "Wirral"),
  "Rest of North West" = c("BlackburnwithDarwen", "Blackpool", "CheshireEast", "CheshireWestandChester", "Cumbria", "Lancashire", "Warrington"),
  "South Yorkshire" = c("Barnsley", "Doncaster", "Rotherham", "Sheffield"),
  "West Yorkshire" = c("Bradford", "Calderdale", "Kirklees", "Leeds", "Wakefield"),
  "Rest of Yorkshire and Humberside" = c("EastRidingofYorkshire", "KingstonuponHullCityof", "NorthEastLincolnshire", "NorthLincolnshire", "York"),
  "East Midlands" = c("Derby", "Derbyshire", "Leicester", "Leicestershire", "Lincolnshire", "Northamptonshire", "Nottingham", "Nottinghamshire", "Rutland"),
  "West Midlands and Met. County" = c("Birmingham", "Coventry", "Dudley", "Sandwell", "Solihull", "Walsall", "Wolverhampton"),
  "Rest of West Midlands" = c("HerefordshireCountyof", "Shropshire", "Staffordshire", "TelfordandWrekin", "Warwickshire", "Worcestershire"),
  "East of England" = c("Bedford", "Cambridgeshire", "CentralBedfordshire", "Essex", "Hertfordshire", "Luton", "Norfolk", "Peterborough", "SouthendonSea", "Suffolk", "Thurrock"),
  "Central London" = c("Westminster", "KensingtonandChelsea", "CityofLondon"),
  "Inner London" = c("Camden", "Greenwich", "Hackney", "HammersmithandFulham", "Islington", "Lambeth", "Lewisham", "Southwark", "TowerHamlets", "Wandsworth"),
  "Outer London" = c("BarkingandDagenham", "Barnet", "Bexley", "Brent", "Bromley", "Croydon", "Ealing", "Enfield", "Haringey", "Harrow", "Havering", "Hillingdon", "Hounslow", "KingstonuponThames", "Merton", "Newham", "Redbridge", "RichmonduponThames", "Sutton", "WalthamForest"),
  "South East" = c("BracknellForest", "BrightonandHove", "Buckinghamshire", "EastSussex", "Hampshire", "IsleofWight", "Kent", "Medway", "MiltonKeynes", "Oxfordshire", "Portsmouth", "Reading", "Slough", "Surrey", "WestBerkshire", "WestSussex", "WindsorandMaidenhead", "Wokingham"),
  "South West" = c("BathandNorthEastSomerset", "BournemouthChristchurchandPoole", "Bristol", "Cornwall", "Devon", "Dorset", "Gloucestershire", "NorthSomerset", "Plymouth", "Somerset", "SouthGloucestershire", "Swindon", "Torbay", "Wiltshire"),
  "Wales" = c("Caerphilly", "Cardiff", "Carmarthenshire", "Ceredigion", "Conwy", "Flintshire", "IsleofAnglesey", "Monmouthshire", "Newport", "Pembrokeshire", "Powys", "RhonddaCynonTaf"),
  "Strathclyde" = c("GlasgowCity", "Inverclyde", "NorthLanarkshire", "Renfrewshire", "SouthLanarkshire", "WestDunbartonshire"),
  "Rest of Scotland" = c("AberdeenCity", "Aberdeenshire", "Angus", "ArgyllandBute", "CityofEdinburgh", "Clackmannanshire", "DumfriesandGalloway", "DundeeCity", "EastAyrshire", "EastDunbartonshire", "EastLothian", "EastRenfrewshire", "Falkirk", "Fife", "Highland", "Midlothian", "Moray", "NaheileananSiar", "OrkneyIslands", "PerthandKinross", "ScottishBorders", "ShetlandIslands", "SouthAyrshire", "Stirling"),
  "Northern Ireland" = c("AntrimandNewtownabbey", "ArdsandNorthDown", "ArmaghCityBanbridgeandCraigavon", "Belfast", "CausewayCoastandGlens", "DerryCityandStrabane", "FermanaghandOmagh", "LisburnandCastlereagh", "MidandEastAntrim", "MidUlster", "NewryMourneandDown")
)

# Map each area in GeoJSON data to the corresponding region category
# Maps area names in the GeoJSON data to standardized region names defined in region_mapping
uk_geo <- uk_geo %>%
  mutate(Region = purrr::map_chr(NAME_2, function(name) {
    region <- names(region_mapping)[sapply(region_mapping, function(regions) name %in% regions)]
    if (length(region) == 0) NA_character_ else region  # Assign NA if region is not found
  })) %>%
  filter(!is.na(Region))  # Filter out unmatched regions

# Merge employment data with geographic data for display on the map
main_job_map <- uk_geo %>%
  left_join(employment_long, by = c("Region" = "Region")) # Combine spatial and employment data

# Plot Main Job heatmap
main_job_plot <- ggplot(main_job_map) +
  geom_sf(aes(fill = as.numeric(`Main Job`)), color = "white") + # Map main job data
  scale_fill_viridis_c(name = "Main Job", option = "C") +
  labs(title = "UK Employment Heatmap (Main Job)") +
  theme_minimal()

# Plot Second Job heatmap
second_job_plot <- ggplot(main_job_map) +
  geom_sf(aes(fill = as.numeric(`Second Job`)), color = "white") + # Map second job data
  scale_fill_viridis_c(name = "Second Job", option = "C") +
  labs(title = "UK Employment Heatmap (Second Job)") +
  theme_minimal()

# Combine both heatmaps into a single plot
p4 <- main_job_plot + second_job_plot + plot_layout(ncol = 1) # Arrange vertically

# Display the combined plot
print(p4)



# combine all four plots into one layout using patchwork
final_plot <- (p1 | p2) / (p3 | p4)

# Display the combined plot
print(final_plot)
