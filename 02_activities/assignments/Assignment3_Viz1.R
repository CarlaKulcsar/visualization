library(ggplot2)
library(dplyr)

df <- read.csv('/Users/carla/Downloads/4cc07c1b-62ed-4ece-a2a4-d05d0f45081c.csv')

# Trim whitespace
df <- df %>% mutate(across(c(GEOGRAPHY, IMMIGRANT, TYPE.OF.WORK, EDUCATION, AGE.GROUP), trimws))

# Filter
base <- df %>%
  filter(IMMIGRANT == "Total",
         TYPE.OF.WORK == "Full-time",
         AGE.GROUP == "25 - 64",
         GEOGRAPHY == "Canada")

EDU_LEVELS <- c("0 - 8 years", "High school graduate", "Bachelor's degree", "Above bachelor's degree")
LINE_COLORS <- c("#a8d8a2", "#81c784", "#6f9f6f", "#1e3f1e")
TOTAL_EDU   <- "Total, all education levels"

edu_data <- base %>%
  filter(EDUCATION %in% EDU_LEVELS) %>%
  mutate(EDUCATION = factor(EDUCATION, levels = EDU_LEVELS))

total_data <- base %>%
  filter(EDUCATION == TOTAL_EDU) %>%
  mutate(EDUCATION = TOTAL_EDU)

# X-axis: label only every 5 years
all_years  <- sort(unique(base$YEAR))
year_labels <- ifelse(all_years %% 5 == 0, as.character(all_years), "")

ggplot() +
  # Coloured lines per education level
  geom_line(data = edu_data,
            aes(x = YEAR, y = Both.sexes, colour = EDUCATION), linewidth = 1) +
  geom_point(data = edu_data,
             aes(x = YEAR, y = Both.sexes, colour = EDUCATION), size = 1.5) +
  # Black dashed total line
  geom_line(data = total_data,
            aes(x = YEAR, y = Both.sexes, linetype = EDUCATION),
            colour = "black", linewidth = 1.2) +
  # Colour scale for education lines
  scale_colour_manual(values = setNames(LINE_COLORS, EDU_LEVELS)) +
  scale_linetype_manual(values = setNames("dashed", TOTAL_EDU),
                        name = NULL) +
  # X-axis: tick every year, label every 5
  scale_x_continuous(breaks = all_years, labels = year_labels) +
  labs(
    title    = "Hourly Wage by Education Level\n(Both Sexes, Full-time, Age 25–64)",
    x        = "Year",
    y        = "Median Hourly Wage (CAD)",
    colour   = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5),
    axis.text.x     = element_text( hjust = 1, size = 10),
    axis.text.y     = element_text(size = 12),
    legend.position = "top",
    legend.text     = element_text(size = 11)
  )

