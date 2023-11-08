---
title: Cricket Statistics Shiny App Explanation
output: pdf_document
---

Introduction

The Cricket Statistics Shiny App is a web application built using the Shiny framework in R. It provides an interactive platform for cricket enthusiasts and analysts to delve into various aspects of cricket data.

The Github Repository

These are the following folders in repository with brief description

Folders

This file contains

Data sets: It contains R data and csv files.

Functions: It contains all the .R files.

Country_Coord : represents the coordinate of different countries on map.

GDP_Data_Process : Save GDP data of diffrent countries.

Teams_Data : Is used to scrap different teams' ODI data.

batting scrap function : it is used to scrap batting data.

cricfunc : it runs simulation for a cricket match.

how_list_formed : it is used to a create a list of countries which each contains 15 lists of players in which there are 3 objects.

scrap bowling data function : it is used to scrap bowling data.

Shiny App: Contains the ShinyApp.

pictures: This contains all the png's and other necessary graphs.

Report.Rmd: This file contains the Report of the project.

The data have been extracted from website Cricmetric.

Usage

Open the repository with the link and download file in your system.

Open the app using an R environment with the required libraries installed. list of libraries is -

shiny , ggplot2 , png , magick , shinyjs , shinyalert , dplyr , shinythemes , gt , plotly 

Run the app using the shiny::runApp() function in R.

