3D hillshaded maps of Wytham Woods
==============================
Nilo Merino Recalde
23 March, 2020



![Wytham Woods, Oxford](/reports/figures/angle_1.jpeg)

A very small 3D map project that makes rendering snapshots of Wytham Woods very easy.
This project is based on [Tyler Morgan-Wall](https://www.tylermw.com/)'s fantastic [Rayshader](https://www.rayshader.com/) package.


Project Organization
------------

    ├── LICENSE
    ├── README.md          <- The top-level README.
    ├── data
    │   └── raw            <- The original .asc files. This is created in the notebook.
    │
    │
    ├── notebooks          <- .Rmd notebooks and corresponding .md for GitHub.
    │   ├── 0.1.0_nmr-wytham-map.Rmd 
    │   └── 0.1.0_nmr-wytham-map.md  
    │                                 
    │
    ├── reports            <- Generated analysis as HTML, PDF, LaTeX, etc.
    │   └── figures        <- Generated graphics and figures.
    │
    ├── renv.lock          <- The 'requirements' file for reproducing the environment,
    │                         generated with renv::snapshot()
    │
    ├── .Rprofile          <- Used to activate renv for new R sessions launched in the project.
    │
    ├── 01_gtit_maps.Rproj <- Shortcut for opening the project 
    │
    ├── renv         
    │   └── activate.R     <- The activation script run by the project Rprofile.
    │
    ├── src                <- Source code for use in this project.
    │   └── nmr-map-source.R 
    │




To use this repository, clone it () and open the R project (howw). with renv?? there is a good chance that you will need to install some dependencies I don't know about lolololo

1. Navigate to the folder where you want to install the repository. Then type `git clone https://github.com/nilomr/wytham-maps.git`
2. Open the `01_gtit_maps.Rproj`
3. Install the required libraries with `renv::restore()`
4. Open and run the `0.1.0_nmr-wytham-map.Rmd` file



**To do:**
- [x] Add renv.lock
- [x] Clean notebook
- [x] Make third palette
- [ ] Add water to rivers
- [ ] Overlay orthophoto
- [ ] Add compass



--------

<p><small>A project by Nilo Merino Recalde | based on the <a target="_blank" href="https://drivendata.github.io/cookiecutter-data-science/">cookiecutter data science project template</a>.</small></p>
