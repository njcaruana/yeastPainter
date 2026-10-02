# yeastPainter


## Description

**yeastPainter** 
Package to 'Paint' PDB structures with proteomics logfc intensities within the Yeast Genome. Based off Complexity and ShinyPDBPainter code.  


## Installation

1. Install the current development version from [GitHub](https://github.com/) by downloading the package. To download go to 'Code' above and download the zip.
2. Once downloaded, unzip.
3. Open the terminal if on Mac or powershell if on PC. 

To run the app, run the code below in the terminal, please note that the folder location you have saved the project in needs to be added to the code: 
``` bash
Rscript -e "pkgload::load_all('[folderlocation]/YeastPainter'); run_app()"
```

The application will run with the text below and open a local instance of the program:

``` bash
ℹ Loading yeastPainterApp
There were 14 warnings (use warnings() to see them)
Loading required package: shiny
Warning: package ‘shiny’ was built under R version 4.5.2

Attaching package: ‘shiny’

The following object is masked from ‘package:yeastPainterApp’:

    runExample

Listening on http://1.0.0.1:2457
```
In your browser, copy and paste the html IP address (this will look different to all individuals).



## Additional Info.

Current R version tested - 4.5.1 

yeastPainter requires ShinyNGLVieweR by [nvelden](https://github.com/nvelden), an R interface to the [NGL.js](http://nglviewer.org/ngl/api/) JavaScript library. It can be used to visualize and interact with protein data bank (PDB) and structural files in R and Shiny applications. It includes a set of API functions to manipulate the viewer after creation and makes it possible to retrieve data from the visualization into R.

Please note: This is a version of nveldens NGLVieweR with an updated NGL.js file due to compatibility issues with the removal of the PDB legacy files. 
This to ensure the package ShinyPDBPainter correctly works and this version of NGLvieweR will be removed once the original is updated. 
