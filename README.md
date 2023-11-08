---
title: Cricket Statistics Shiny App Explanation
output: pdf_document
---

Cricket Statistics Shiny App

Introduction

The Cricket Statistics Shiny App is a web application built using the Shiny framework in R. It provides an interactive platform for cricket enthusiasts and analysts to delve into various aspects of cricket data.

The Github Repository

These are the following folders in repository with brief description

Folders

This file contains

Data sets It contains R data and csv files.

Folders It contains all the .R files.

<!-- -->

Shiny App: Contains the ShinyApp.

pictures: This contains all the png's and other necessary graphs.

Report.Rmd: This file contains the Report of the project.

to view their performance statistics. This is facilitated through a dynamic user interface. The data have been extracted from website Cricmetric.

Team ODI Data

The Team ODI Data tab offers a comprehensive view of team performance. It includes three sub-tabs:

World View: Provides a world map visualization displaying team performance metrics.

Team Overview: Allows users to select a specific team and view detailed performance summaries over a selected range of years.

-Match Up: Enables users to select two teams and view head-to-head statistics.

Happiness Index: It will show correlation between the happiness index of country vs winning-loss ratio.

GDP: This section compares the correlation between the GDP of the country and the win-loose ratio.

Cricket Match

The Cricket Match tab simulates a cricket match between two teams. Users can select players for each team and start the match to view detailed performance metrics.

Usage

Open the repository with the link

Open the app using an R environment with the required packages installed.

Run the app using the shiny::runApp() function in R.

Code Examples

Player ODI Data

```R # UI component for selecting country and player selectInput("country", "Select Country:", choices = Country_name, selected = Country_name[5]), uiOutput("player"), uiOutput("ballorbat")# Project Repository

This repository will contain **ALL** the code, analysis, document and presentation for your group project.
 
