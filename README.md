# 🎓 Student Satisfaction Dashboard

## 📘 Overview

This project builds an interactive **Shiny dashboard** to visualize undergraduate academic satisfaction trends at U.S. public research universities from **2010 to 2021**, using data from the **Student Experience in the Research University (SERU)** survey. The goal is to provide an accessible tool for **policymakers, researchers, and educators** to compare institutional performance and explore demographic disparities in student satisfaction over time.

---

## 🎯 Objectives

* Track and visualize **10-year trends** in overall academic satisfaction.
* Allow users to compare **across universities** or **within a single university** by demographic subgroups (gender, ethnicity, status).
* Build an **interactive, user-friendly interface** using the R `Shiny` framework.
* Enable data export and customized trend analysis for downstream use.

---

## 🛠️ Tech Stack

* **R packages**:

  * `shiny`, `shinydashboard`, `shinythemes`, `shinyWidgets`
  * `tidyverse`, `ggplot2`, `DT`, `bslib`, `thematic`

* **Data Visualization & UX**:

  * Trend line plots with demographic facets
  * Custom theme via `shinythemes::sandstone`
  * Interactive filtering + download buttons

* **Data Source**:

  * **SERU Survey** (2010–2021)
  * 1M+ undergraduate student responses
  * 30+ AAU/R1 public research universities (data protected by NDA)

---

## 🧮 Features

* 📊 Dynamic comparison of academic satisfaction across universities
* 🧑‍🤝‍🧑 Subgroup breakdown by gender, ethnicity, and status
* 🧭 Two dashboard modes:

  * **Cross-University View**
  * **Within-University View**
* 📥 Exportable trend plot (`.png`) and filtered data (`.csv`)

---

## 🚫 Data Privacy Note

Due to confidentiality agreements, the full dataset is not included in this repository. The uploaded Shiny app code is designed for demonstration purposes only.

---

## 👩‍💻 Author

*Caitlyn Cai* — R user and dashboard developer focused on education data and interactive research tools.
