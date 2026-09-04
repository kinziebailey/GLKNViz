# GLKNViz (branch `main`)

## Description

The `main` branch is Version 0.0.1 of the `GLKNViz` repository. This is the first 
deployment of the Great Lakes Inventory and Monitoring Network water quality 
shiny data visualizer. Previous archived versions of this RShiny app can be found
at [www.github.com/kinziebailey/GLKNViz](www.github.com/kinziebailey/GLKNViz).

## Live Demo:
[Click here to run the app](https://kinziebailey.shinyapps.io/GLKNViz-Dev/)

## Getting started: Running locally

1. Create a new R Studio project File  -\> New Project -\> Version Control -\> Git -\> <https://github.com/DOI-NPS/GLKNWViz>

2. Create a data folder. In the Console:

```{terminal}
dir.create("data")
```

3. Copy the following files to your `data` folder from (insert link when available)
  - chr_lookup.csv
  - stations.csv
  - thresholds.csv

4. Run the wqp_retrival.R code to retrieve data from the [Water Quality Portal](https://www.waterqualitydata.us/)

5. Confirm that you can run the shiny app.
  - open `global.R`
  - click the "Run App" button at the top of your code editor
  
  or 
  
  - Run the following code in the console (make sure you are in the RProject):

```{terminal}
shiny::runApp()
```

6. If the app runs for you, continue on. Otherwise, contact Kinzie Bailey.