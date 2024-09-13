---
title: 'ExcelToShiny: An R Package for Automating Shiny Dashboard Creation from Excel Templates'
tags:
  - R
  - Shiny
  - open-source
  - data visualisation
  - automation
  - Excel
  - dashboard
  - reproducibility
  - transparency
  - user interface
  - data-driven
  - interactive application
authors:
  - name: Lily Clements
orcid: 0000-0001-8864-0552
affiliation: 1
affiliations:
  - name: IDEMS International
index: 1
date: 13th September
bibliography: paper.bib

---

# Summary

The `ExcelToShiny` package is an open-source, user-friendly tool that automates the creation of Shiny dashboards from structured Excel templates. Designed to simplify the process of building interactive web applications in R, `ExcelToShiny` allows users to convert Excel spreadsheets directly into customisable, fully functional dashboards with minimal coding. This solution promotes reproducibility and transparency while significantly reducing the technical barrier for users seeking data-driven insights through visualisations.

By automating the traditionally labour-intensive process of dashboard generation, `ExcelToShiny` ensures that even non-technical users can engage with dynamic data visualisations. The package plays a role in promoting accessibility and flexibility in data presentation, enabling users to focus on decision-making rather than complex programming tasks.

# Statement of Need

Dashboards provide a powerful way to explore, communicate, and act on data insights, offering dynamic, live data interaction and insights [@dutta2024business]. However, constructing these dashboards from scratch in Shiny typically requires substantial R programming expertise. For non-technical users or those unfamiliar with Shiny, this poses a significant challenge, often leading to reliance on static reports or external proprietary tools like Tableau and Power BI. While these tools are user-friendly, they come with limitations, such as cost, limited customisability, and a lack of reproducibility [@tableau; @powerbi].

`ExcelToShiny` addresses this gap by offering an open-source, R-based alternative that automates the conversion of Excel templates into Shiny dashboards. By reading a user-defined Excel structure, `ExcelToShiny` dynamically generates the corresponding UI and server components for a Shiny application. This approach allows users to create dashboards that are both reproducible and easy to share, with the flexibility to modify and adapt them as needed within the R ecosystem. The integration of Excel templates allows users familiar with spreadsheets to harness the full power of Shiny without needing extensive coding skills.

# Functionality and Features

`ExcelToShiny` leverages a simple, structured Excel format to automatically generate Shiny dashboards, making the process highly accessible. The package interprets Excel template elements like tabs, graphs, tables, and menus, converting them into Shiny UI components and server logic with minimal user input. The dashboards can have regular display pages, display pages with tabs, or have a page that downloads the data. Furthermore, while much of the process is automated, users can further customise their dashboards, enabling fine-tuning of both layout and behavior to meet specific requirements. This allows  users to fine-tune both the appearance and functionality without needing to dive into R code.

Finally, the use of structured templates ensures that dashboards can be easily replicated, modified, and audited, enhancing transparency in data-driven processes. The package integrates with existing R workflows, making it straightforward to incorporate into larger projects, combining it with other packages for a comprehensive analysis.

# Usage

`ExcelToShiny` is simple to use, with the workflow beginning from an Excel template that defines the dashboard’s layout. Users provide the template, and the package generates the dashboard based on the structure defined in the Excel file.

## Example: Building a Basic Dashboard

To create a dashboard from an spreadsheet template:

1. **Create a Template**: Design a file with the necessary structure for the dashboard, including tabs, charts, and tables.

2. **Read in the Data**: As outlined in more detail in the vignette, the next task is to read in the Excel Template and your data to display needed for the dashboard. You can do data manipulation at this stage to the data, if needed. 

```{r}
library(ExcelToShiny)

# Load in the Excel Template from it's location
excel_file <- "path_to_template.xlsx"
```

3. **Launch the Dashboard**: Finally, using the `build_shiny` function, the user can built their shiny dashboard.
```{r}
# Create the Shiny Dashboard
shiny_app <- build_shiny(title = "Testing App",
                         data_list = excel_file,
                         data_frame = data_file)
```

This process generates and launches a complete Shiny dashboard based on the specified spreadsheet template, requiring little to no R programming expertise.

# Future Goals

The ongoing development of `ExcelToShiny` aims to extend its functionality and usability. Planned improvements include optimisation changes on the R code to improve its efficiency. Furthermore, plans include adding more flexibility to the dashboards with additional features to include for the Shiny Dashboards - such as displaying data tables on the dashboard.

Hosted on GitHub, `ExcelToShiny` encourages contributions from the community to ensure the package continues to evolve and meet the diverse needs of its users. This community-driven development fosters an open-source, collaborative approach to refining the tool.

# Conclusion

`ExcelToShiny` represents a significant advancement in simplifying the creation of interactive Shiny dashboards, lowering the barriers to entry for users without extensive programming experience. By automating the generation of dashboards from Excel templates, it ensures accessibility, reproducibility, and transparency in data visualisation. As the package evolves, it aims to serve as a tool for researchers, analysts, and decision-makers in a wide range of fields, providing an easy-to-use yet powerful solution for data presentation.