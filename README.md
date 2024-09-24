# STAT40830 - Advanced Programming in R: Human Development Indicator (HDI) Visualizer Shiny App
**Author**: Lea Sauer

## Overview

This Shiny app provides a user-friendly interface to visualize Human Development Indicators (HDI) for various countries, utilizing data from the [UNDP Human Development Reports](https://data.humdata.org/dataset/?organization=undp-human-development-reports-office&q=Human+Development+Indicators). Users can either load data directly from the web or upload manually downloaded CSV files to explore different development metrics.

## Table of Contents

- [Installation](#installation)
- [Project Structure](#project-structure)
- [Features](#features)
- [Visualizations](#visualizations)
- [User Instructions](#user-instructions)
- [Technical Details](#technical-details)
- [Future Improvements](#future-improvements)
- [Contribution](#contribution)
- [License](#license)
- [Contact Information](#contact-information)
- [Acknowledgements](#acknowledgements)

## Installation

1. Clone the repository:
   ```ruby
   git clone https://github.com/LeaKatharinaSauer/R_shiny_HDI_data
   ```
2. Install necessary R packages:   
   ```ruby
   install.packages(c("shiny", "ggplot2", "rvest", "readr", "stringr", "countrycode", "data.table", "bslib", "shinycssloaders"))
   ```
3. Run the app:
    ```ruby
   shiny::runApp()
   ```

## Project Structure

The repository is organized as follows:

```ruby
app.R                             # Shiny app.
hdro_indicators_khm.csv           # CSV file downloaded from the UNDP Human Development Reports for Cambodia to use as input in app.
hdro_indicators_nga.csv           # CSV file downloaded from the UNDP Human Development Reports for Nigeria to use as input in app.
README.md                         # The file you’re currently reading.
LICENSE                           # Project license.
```

## Features

- **Load HDI Data**: Automatically load data for a selected country from the UNDP Human Development Indicators website or upload a local CSV file downloaded manually.
- **Customizable Table Display**: View the dataset in a table format with the option to adjust the number of rows and columns displayed.
- **Time Series Visualization**: Select an HDI indicator to visualize its trend over the years (e.g., Adolescent Birth Rate, Life Expectancy) for a country.
- **Average Indicator Plot**: Compare the average values of selected indicators over the years using a bar plot.

## Visualizations

1. **Time Series Plot**  
   A dynamic line chart that shows how a selected HDI indicator has evolved over time for a specific country. Users can customize the color of the plot and select a range of years for display.

2. **Average Indicator Plot**  
   A bar plot showing the average value for the selected indicators over time. Multiple indicators can be selected to compare their average values within the specified time range.

## User Instructions

1. **Select Data Source**:  
   - Use the dropdown menu to select the dataset for a country.
   - Alternatively, upload a manually downloaded CSV file containing HDI data.
   
2. **Visualize Data**:  
   - Use the "Time Series" tab to view a time series of a selected indicator for the specified time range.
   - Use the "Average" tab to compare the average values of different indicators.

3. **Customization Options**:  
   - Choose the color of the plots using the color picker.
   - Adjust the range of years to focus on specific time periods

## Technical Details

- **Data Source**: [UNDP Human Development Indicators](https://data.humdata.org/dataset/?organization=undp-human-development-reports-office&q=Human+Development+Indicators)
- **Languages & Tools**:
  
  - R and Shiny
  - ggplot2 for visualization
  - rvest for web scraping
  - readr for file handling
  - stringr for string manipulation
  - countrycode for country code conversion
  - data.table for fast data manipulation
  - bslib for theming and customization
  - shinycssloaders for loading animations

## Future Improvements

- Add more customization options for the plots (e.g., themes, font sizes).
- Implement downloadable reports based on user-selected data and visualizations.

## Contribution

Feel free to open issues or submit pull requests if you'd like to contribute to the project!

## License

This project is licensed under the MIT License. See the LICENSE file for more details.

## Contact Information

For inquiries or collaboration, please reach out to:

- **Lea Sauer**: lea.sauer@ucdconnect.ie


## Acknowledgements

I would like to thank University College Dublin and specifically my professor Dr. Isabella Gollini for her support and guidance throughout this project.
