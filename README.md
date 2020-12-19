# Migration Explorer 
## - An interactive web app for exploring US migration data - 

https://jesso.shinyapps.io/migration/


## Introduction

### About Data 

The data used comes from searchable tables provided by the [US Census Bureau](https://www.census.gov/data.html), which collects data about every resident in the United States every 10 years. According to [2020 Census](https://www.census.gov/programs-surveys/decennial-census/2020-census/about.html#:~:text=It%20is%20mandated%20by%20Article,federal%20funds%20to%20local%20communities), "the data collected by the census "determines the number of seats each state has in the U.S. House of Representatives (a process called apportionment) and is also used to distribute billions in federal funds to local communities".

Two tables from Census Bureau Current Population Survey (CPS) [Geographic mobility](https://www.census.gov/data/tables/time-series/demo/geographic-mobility/historic.html) were parsed and analyzed. 

- Net Migration. The US annual net migration data comes from U.S. Census Bureau, Current Population Survey, Annual Social and Economic Supplement 1981-2019. The data [Table A-2. Annual Inmigration, Outmigration, Net Migration, and Movers from Abroad for Regions: 1981-2019](https://www2.census.gov/programs-surveys/demo/tables/geographic-mobility/time-series/historic/tab-a-2.xls) can be downloaded as an Excel file. The data contains the annual inmigation, outmigration, net migration, and movers from abroad from 1981-2019 for 4 main US regions: west, northeast, midwest, and south. Note that movers from Puerto Rico and the United States Island Areas are counted as movers from abroad. 

- Reason for Move. The reason for move data comes from the same source. The table [Table A-5. Reason for Move (Specific Categories): 1999-2019](https://www2.census.gov/programs-surveys/demo/tables/geographic-mobility/time-series/historic/tab-a-5.xls) is downloadable. The dat contains the annual number of movers who move for specific reasons, categorized under 4 main categories: family-related reasons, housing-related reasons, employment-related reasons, and other reasons, from 1999-2019.

  

### About App 

This interactive dashboard built with Shiny Dashboard in R allows you to explore migration in the US. There are 2 main pages: "Net Migration" and "Reason for Move". The "Net Migration" tab provides an overall timeline of migration trends, as well as the distribution of net population flows by migration type and by region. It allows you to select region(s), the type(s) of migration, and the year to visualize and explore the net migration data. The "Reason for Move" tab provides interactive views of people's reasons to move. You can view the propotion of each aggregate reason by year, as well as the subcategories under each reason for a more closeup look.


The app is published on [Shinyapps](https://jesso.shinyapps.io/migration/)

For a preview of the app, 
![app preview net migration](<preview_net_migration.png>)


### About Project Repo

```
├── R
│   ├── parse_netmigration.R
│   └── parse_reason.R
├── README.md
├── migration
│   ├── app.R
│   ├── custom
│   │   ├── custom.css
│   │   └── introduction.html
│   ├── data
│   │   ├── migration.csv
│   │   ├── reason.csv
│   │   ├── tab-a-2.xls
│   │   └── tab-a-5.xls
│   ├── images
│   │   └── migration-humankind.jpg
│   └── rsconnect
│       └── shinyapps.io
│           └── jesso
│               └── migration.dcf
├── preview_net_migration.png
└── project.Rproj
```

After deleting duplicate entries (coming from different sources) in original excel files, the raw data were loaded in as "tab-a-2/5.xls" in `migration/data/tab-a..xls`. The `parse_netmigration.R` and `parse_reason.R` under R folder were used to parse, clean and reshape the raw data into tibbles and save output as .csv files under `migration/data/..csv`. I wrote `introduction.html` for introduction tab and `custom.css` for customized CSS. These files are in `migration/custom/`. The image `migration-humankind.jpg` is the image used in introduction tab and is available in `migration/images/`. 


## References

1.  Bureau, U. (2020). Data. Retrieved 10 June 2020, from
    [https://www.census.gov/data.html](https://www.census.gov/data.html)
2.  [Shiny Tutorials](https://shiny.rstudio.com/tutorial/)
3.  [R Shiny Dashboard](https://rstudio.github.io/shinydashboard/index.html)
4.  [Plotly-R](https://plotly-r.com/controlling-tooltips.html)
5.  [Datavis with R](https://rkabacoff.github.io/datavis/Multivariate.html#Grouping)
6.  [R Documentation](https://www.rdocumentation.org/packages/janitor/versions/2.0.1/topics/row_to_names)
7.  [musgraveanalytics](https://www.musgraveanalytics.com/blog/2018/8/24/how-to-make-ggplot2-charts-interactive-with-plotly)
8.  [Cran R Project naniar](https://cran.r-project.org/web/packages/naniar/vignettes/replace-with-na.html)
9.  [Cran R project hablar](https://cran.r-project.org/web/packages/hablar/vignettes/convert.html)
10. [RStudio community](https://community.rstudio.com/t/shinydashboard-render-only-the-clicked-tab/36493/2)
11. [Stackoverflow](https://stackoverflow.com/questions/58145084/ggplotly-rename-tooltip-on-hover)
12. [lab3 CSS](https://github.com/sta323-523-su20/lab3/blob/master/lab3.css)
