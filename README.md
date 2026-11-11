# Screen Time and Children's Mental Health

## Project Overview
This project explores patterns between children's weekday screen time and reported mental-health experiences.

## Research Question
What patterns can be observed between children's weekday screen time and reported mental-health experiences?

We also explore differences by age group and sex.

## Dataset Description
The dataset contains 41,890 records and 5 original columns. It includes information about children's screen time, mental-health treatment, age, and sex.

**Dataset file:** `screentimementalhealth.csv`

### Column Descriptions
- `Screentime`: Children's reported screen-time category.
- `Mental Health Treatment`: Information about reported mental-health treatment.
- `Treatment Not Received`: Information about treatment that was not received.
- `Age (Years)`: Child's age in years.
- `Sex`: Child's reported sex.

The analysis also creates an additional age-group variable:
- `Age_Group`: Preschool (0–5), Young Children (6–10), Older Children (11–13), and Adolescents (14–17).

## Analysis
The analysis is performed in `Mentalhealth.R`.

The project includes six visualizations:
1. Screen-time category distribution.
2. Mental-health treatment by screen-time category.
3. Treatment not received by screen-time category.
4. Age distribution by screen-time category.
5. Sex distribution by screen-time category.
6. Mental-health treatment across age groups.

The graphs are saved in the `graphs` folder.

## Project Files
- `Mentalhealth.R`: Data cleaning, analysis, and graph creation.
- `screentimementalhealth.csv`: Dataset used for the analysis.
- `graphs/`: Folder containing the six visualizations.
- `app.R`: Shiny application file (to be added by the app developer).

## Contributors
Leilany Calderon and Ericka Aliaga 

## AI and LLM Use
ChatGPT was used as an AI assistance tool for help with debugging and organizing some of the dataset. Team members reviewed everything. 
## Limitations
This project explores patterns and associations in the data. The results do not establish that screen time causes mental-health outcomes.
