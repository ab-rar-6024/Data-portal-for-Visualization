# 📊 Data Visualization Portal – Built with R & Shiny

An interactive **data portal** built using **R** and **Shiny** that allows users to:

- Upload their own datasets (`.csv`)
- Preview raw data
- Visualize data with various plots:
  - Box Plot
  - Pie Chart
  - Histogram
  - Scatter Plot
  - Bar Chart

---

## 🧰 Technologies Used

- **R** (version ≥ 4.0)
- **Shiny** (web framework for R)
- **ggplot2** (for data visualization)
- **readr / data.table** (for fast CSV parsing)
- **dplyr** (for data manipulation)

---

## 🚀 Features

- 📁 Upload `.csv` dataset
- 👁️ View dataset preview (head and summary)
- 📊 Select plot type from dropdown
- 🧮 Automatically detects numeric/categorical columns
- 🎛️ Customizable axis and color options
- 🔃 Reactive UI: plots update as inputs change

---

## 📦 Installation Instructions

### 1. Install R and RStudio

- Download R: [https://cran.r-project.org/](https://cran.r-project.org/)
- Download RStudio: [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)

How to Run the App
Open app.R in RStudio and click Run App
Or run from the R console:
shiny::runApp('path/to/app-directory')

Available Visualizations
| Type         | Description                                 |
| ------------ | ------------------------------------------- |
| Box Plot     | Summary distribution by group               |
| Pie Chart    | Proportion of categorical variables         |
| Histogram    | Frequency distribution of numeric variables |
| Scatter Plot | Relationship between two numeric variables  |
| Bar Chart    | Counts of categories                        |

Notes
Only .csv files are supported

Ensure the file has a header row

Pie chart works best with categorical variables
