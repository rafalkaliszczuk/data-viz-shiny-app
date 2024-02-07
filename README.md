## data-viz-shiny-app

This Shiny application is designed to visualize COVID-19 death-related data from Eurostat. <br>
It allows users to execute SQL queries, create interactive plots, and display data on maps and in time series.

You can check out the app here:
https://rkali.shinyapps.io/covid-deaths-shiny-app/ <br>
Please note that due to the shinyapps.io limitations the following functionalities does not work on the website:<br>

• Button `Open http:// connection and download the newest Deaths by week and sex (demo_r_mwk_ts)` in a Start Page. <br>
You can access the raw data here though: https://ec.europa.eu/eurostat/web/population-demography/demography-population-stock-balance/database?node_code=demomwk <br>
• Button `Generate Markdown report` in the Report tab. <br>
You can see the report example below

### Functionalities in Each Tab:

<b>Start Page</b><br>
•	Download raw data from the Eurostat website.<br>
•	Add Eurostat data tables to the RSQLLite database.<br>

![image](https://github.com/rafalkaliszczuk/data-viz-shiny-app/assets/100476888/9a1d8423-f0e0-41be-aaab-e4740a48ee12)

<b>SQL</b><br>
•	Execute custom SQL queries on Eurostat data.<br>
•	Browse query results in an interactive table.<br>
•	Export query results to a CSV file.<br>
•	Create interactive plots based on SQL query results.<br>

![image](https://github.com/rafalkaliszczuk/data-viz-shiny-app/assets/100476888/d1c245c4-af15-4495-8c5e-086eabd15b1c)
![image](https://github.com/rafalkaliszczuk/data-viz-shiny-app/assets/100476888/dd565b29-b185-45d8-bdd5-7a894b0bfa43)

<b>Map</b><br>
•	Display aggregated data on a European map.<br>
•	Choose a specific time frame to view on the map.<br>

![image](https://github.com/rafalkaliszczuk/data-viz-shiny-app/assets/100476888/f18db471-41da-437d-b531-96ae751f623b)

<b>Time series</b><br>
•	Generate interactive time series plots for selected countries and genders.<br>
•	Define a time frame for the time series plot.<br>

![image](https://github.com/rafalkaliszczuk/data-viz-shiny-app/assets/100476888/ac15ec29-f30e-48c0-9c9b-64df203085ff)

<b>Report</b><br>
•	Generate Markdown-format reports based on the analysis and visualizations. <br>
•	Export reports to HTML format. <br>

![image](https://github.com/rafalkaliszczuk/data-viz-shiny-app/assets/100476888/d1df78d3-d008-4542-81fd-2e8a010d141a)

Exemplary generated .html report: <br>

![image](https://github.com/rafalkaliszczuk/data-viz-shiny-app/assets/100476888/4ce76f87-512e-4b5b-9ed4-63cdb918b702)

