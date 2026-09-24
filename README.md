# Cetaceans in the BGTW
This is app shows the distribution of cetaceans in the British Gibraltar Territorial Waters. It was part of my Masters Thesis with the University of Gibraltar.

**Author:** Simon Gendrisch
**Live version:** <https://simongendrisch.shinyapps.io/mastershiny/> (may be inaccessible from some institutional networks; see below for local execution)

## Requirements

- [R](https://cran.r-project.org/) (>= 4.6.0; developed under R 4.6.0)
- [RStudio Desktop](https://posit.co/download/rstudio-desktop/)
- The following R packages: `shiny`, `bslib`, `leaflet`, `leaflet.extras`, `tidyverse`, `htmltools`, `stringr`, `readxl`, `plotly`, `rsconnect`, `shinycssloaders`

## Running the app locally

### 1. Clone the repository in RStudio

1. `File` > `New Project...` > `Version Control` > `Git`
2. Repository URL: `https://github.com/datsimon-design/CetaceansIntheBGTW.git`
3. Choose a local project directory and click `Create Project`.

Alternatively, from a terminal:

```bash
git clone https://github.com/datsimon-design/CetaceansIntheBGTW.git
```

If Git is not installed, the repository can also be downloaded as a ZIP file via the green `Code` button on GitHub (`Download ZIP`) and extracted.

### 2. Install the required packages

In the R console:

```r
install.packages(c(
  "shiny", "bslib", "leaflet", "leaflet.extras", "tidyverse",
  "htmltools", "stringr", "readxl", "plotly", "rsconnect",
  "shinycssloaders"
))
```

### 3. Start the app

Open `app.R` in RStudio and click **Run App** (top right of the editor pane), or run in the console:

```r
shiny::runApp("app.R")
```

The app opens in an RStudio viewer window or in the default web browser. It runs entirely on the local machine and requires no internet connection after package installation.

## Repository structure

```
.
├── app.R        # Shiny application (UI and server logic)
├── data/        # Input data used by the app
├── www/         # Static assets (e.g., images, CSS)
└── README.md
```

## Data

The data was collected during my Masters Project and describes the sightings of Cetaceans in the Waters around Gibraltar.
