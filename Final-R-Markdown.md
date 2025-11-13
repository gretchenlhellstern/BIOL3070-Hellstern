COPD Prevalence Map
================
Gretchen Hellstern
2025-11-13

# Background

Air Quality Index (AQI) is a standardized system used to measure the
quality of the air around us. The AQI is calculated using the four major
air pollutants, as determined by the Clean Air Act, which are
ground-level ozone, particle pollution, carbon monoxide, and sulfur
dioxide. AQI is measured on a scale of 0-500, with 0 being the cleanest
air and 500 being the most hazardous. This categorizes 0 as the highest
air quality and 500 as the lowest air quality. AQI is a useful tool that
has become integrated into our weather apps, making it accessible to the
majority of the population.

One of the primary causes of death in urban areas is chronic obstructive
pulmonary disease (COPD). COPD can be caused by many factors including
tobacco use, occupational factors, infection, and air pollution. This
disease progresses slowly and worsens with prolonged exposure. The
damage is usually not reversible and can lead to pulmonary failure.
There are many factors that play into the prevalence of COPD like
region, age, and sex.

# Research Question and Hypothesis

With this information in mind, we have posed the following question: “Do
areas of low air quality have higher rates of Chronic Obstructive
Pulmonary Disease (COPD)?” Although there are many factors that
influence COPD, if there is a negative correlation between air quality
and rates of COPD there are many known preventative measures that can be
taken to prevent COPD and improve lung health. These findings can
influence health practices and help those susceptible to COPD avoid risk
factors, such as low air quality. Although there are many factors that
can play into rates of COPD, we hypothesize that this negative
correlation will exist and that areas with lower air quality will have
higher rates of Chronic Obstructive Pulmonary Disease.

## Setup

``` r
lines <- str_split(data_text, "\n")[[1]]
lines <- trimws(lines)
lines <- lines[lines != ""]
lines <- lines[-1]

# 4. Define the pattern
pattern <- "^([A-Za-z .()0-9]+?)\\s+(\\d+\\.\\d)\\s+([\\d,]+)\\s+(\\d+\\.\\d)\\s+([\\d,]+)\\s+(\\d+\\.\\d)\\s+([\\d,]+)$"
rows <- str_match(lines, pattern)
rows <- rows[!is.na(rows[,1]), ]

# 7. Create the data frame
df <- as.data.frame(rows[,2:8])
colnames(df) <- c("State","Percent_Male","Count_Male","Percent_Female","Count_Female","Percent_Total","Count_Total")
df <- df %>%
  # Filter out rows i don't want to plot
  filter(State != "United States") %>%
  filter(State != "District of Columbia") %>% # <-- NEW FIX: Remove D.C.
  # Convert columns to the right type
  mutate(across(starts_with("Percent"), as.numeric),
         across(starts_with("Count"), ~ as.numeric(str_replace_all(.x, ",", ""))),
         
         State = str_replace(State, "\\s*\\(.*\\)", ""), # Removes (2020)
         State = str_trim(State))                       # Removes whitespace
# data check
print("--- Cleaned Data Frame ---")
```

    ## [1] "--- Cleaned Data Frame ---"

``` r
print(head(df))
```

    ##        State Percent_Male Count_Male Percent_Female Count_Female Percent_Total
    ## 1    Alabama          8.6     161900           10.1       207500           9.4
    ## 2     Alaska          5.3      15300            6.1        16100           5.7
    ## 3    Arizona          5.0     137800            6.4       180700           5.7
    ## 4   Arkansas          8.6      97600           10.5       125600           9.6
    ## 5 California          4.5     680600            4.7       733100           4.6
    ## 6   Colorado          4.3      98900            5.9       135100           5.1
    ##   Count_Total
    ## 1      369400
    ## 2       31400
    ## 3      318500
    ## 4      223200
    ## 5     1413800
    ## 6      233900

``` r
# 1. map data
states_map <- map_data("state")

# 2. heat map data to join
heatmap_data <- df %>%
  select(State, Percent_Female, Percent_Male) %>%
  pivot_longer(cols = starts_with("Percent_"),
               names_to = "Group",
               values_to = "Percent") %>%
  mutate(Group = str_replace(Group, "Percent_", ""),
         region = tolower(State)) 
map_plot_data <- left_join(states_map, heatmap_data, by = "region")
```

    ## Warning in left_join(states_map, heatmap_data, by = "region"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 1 of `x` matches multiple rows in `y`.
    ## ℹ Row 1 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
# NEW stuff this better work:
map_plot_data %>%
  filter(!is.na(Group)) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = Percent)) +
  
  geom_polygon(color = "white", size = 0.1) + # Draw the states
  scale_fill_viridis_c(name = "Percent") +
  facet_wrap(~ Group) + 
  labs(title = "COPD Prevalence by U.S. State and Gender") +
  theme_void() + 
  coord_map() + 
  theme(
    legend.position = "right",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 14, face = "bold")
  
  )
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](Final-R-Markdown_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->


    ``` r
    states_map <- map_data("state") 
    difference_data <- df %>%
      mutate(
        Percent_Difference = Percent_Female - Percent_Male,
        region = tolower(State) # <-- This creates the join key
      ) %>%
      select(region, Percent_Difference)
    map_diff_data <- left_join(states_map, difference_data, by = "region")
    ggplot(map_diff_data, aes(x = long, y = lat, group = group, fill = Percent_Difference)) +
      geom_polygon(color = "white", size = 0.1) + # Draw the states
      # 5. Use the DIVERGING color scale
      scale_fill_gradient2(
        name = "Percent Difference\n(Female - Male)",
        low = "blue",      # States where males are higher (negative)
        mid = "white",     # States where rates are equal (zero)
        high = "red",      # States where females are higher (positive)
        midpoint = 0       # We center the scale at zero
      ) +
      
      labs(title = "Difference in COPD Prevalence (Female vs. Male)") +
      theme_void() + # Use a clean theme
      coord_map() +  # Use correct map projection
      theme(
        legend.position = "right",
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)
      )

![](Final-R-Markdown_files/figure-gfm/unnamed-chunk-4-1.png)<!-- --> \#
air quality index

``` r
# more packages
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(viridis)
library(maps)
library(readr) # Added to read the CSV
```

``` r
aq_df <- read_csv("air-quality-by-state-2025.csv")
```

    ## Rows: 51 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): state
    ## dbl (4): AirQuality_AirQualityIndexViaUSA_num_YearFree, AirQualityRankViaUSN...
    ## lgl (1): stateFlagCode
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
print("--- Air Quality Data ---")
```

    ## [1] "--- Air Quality Data ---"

``` r
print(head(aq_df))
```

    ## # A tibble: 6 × 6
    ##   stateFlagCode state         AirQuality_AirQualityInde…¹ AirQualityRankViaUSN…²
    ##   <lgl>         <chr>                               <dbl>                  <dbl>
    ## 1 NA            Utah                                 51.2                     46
    ## 2 NA            Georgia                              48.2                     26
    ## 3 NA            Ohio                                 48.2                     36
    ## 4 NA            West Virginia                        47.6                      6
    ## 5 NA            Indiana                              47.5                     37
    ## 6 NA            Tennessee                            47.5                     28
    ## # ℹ abbreviated names: ¹​AirQuality_AirQualityIndexViaUSA_num_YearFree,
    ## #   ²​AirQualityRankViaUSNews_2024
    ## # ℹ 2 more variables: DaysWithUnhealthyAirQuality_2024 <dbl>,
    ## #   AirQualityIndustrialToxinConcentration_2024 <dbl>

``` r
states_map <- map_data("state")

aq_data_to_plot <- aq_df %>%
  # Use the correct column names: `state` and `AirQuality_AirQualityIndexViaUSA_num_YearFree`
  # We also rename the long AQI column to `Overall_AQI` to make it easier to use
  select(State = state, Overall_AQI = `AirQuality_AirQualityIndexViaUSA_num_YearFree`) %>% 
  mutate(region = tolower(State)) # <-- This creates the join key

map_plot_data <- left_join(states_map, aq_data_to_plot, by = "region")


map_plot_data <- map_plot_data %>%
  filter(!is.na(Overall_AQI))

ggplot(map_plot_data, aes(x = long, y = lat, group = group, fill = Overall_AQI)) +
  geom_polygon(color = "white", size = 0.1) + # Draw the states
  
  scale_fill_viridis_c(name = "Overall AQI") +
  
  labs(title = "Overall Air Quality Index (AQI) by U.S. State (2025)") +
  theme_void() + # Use a clean theme
  coord_map() +  # Use correct map projection
  theme(
    legend.position = "right",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )
```

![](Final-R-Markdown_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
