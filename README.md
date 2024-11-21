# Welcome to my assignment B3 submission
In this repo, you'll find all the code, images, and files relevant to my Shiny app. You can see the app in action [HERE](https://andrewfullerton.shinyapps.io/assignment-b3/).

### Here's what you need to know:
* `assignment-b3/` is a folder containing the code, images, and files used to create and run the Shiny app.
* `assignment-b3-andrewfullerton.Rproj` contains the project-specific settings you'll need to run everything (smoothly) on your local machine.
* `.gitignore` is there to prevent me from pushing any unneccessary or confidential files to GitHub.

If you want to run the code on your local machine, you can clone the repo using the following git command: `git clone https://github.com/stat545ubc-2024/assignment-b3-andrewfullerton`. 

Once you have the project opened up in **RStudio**, go into the `assignment-b3/` folder and the open the file named `app.R` to view, modify, or tinker with the code used to produce the Shiny app. 

### About the app
This Shiny app uses the `starwars` dataset from the `dplyr` package and allows you to explore and compare Star Wars characters by height (cm) and mass (kg). Filter characters, choose the metric to compare, and explore in both graphical and tabular formats.

*The data used in this app is sourced from the R package dplyr. It was originally sourced from the [Star Wars API (SWAPI)](https://swapi.py4e.com/) and revised to include gender and sex determinations. More information on the dataset can be found [here](https://dplyr.tidyverse.org/reference/starwars.html)*
