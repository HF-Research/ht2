# HjerteTal 2

## Running app from within R environment
To run the app within R, simply open the global.R file and run the runApp() command - or press <ctr><shft><entr>



## Project File Structure
The shiny app consists of three main shiny/R files, all located in the parent directory: `global.R`, `server.R` and `ui.R`. Additionally there are `data/`, `documentation/`, `r/`, `www/` and `rsconnect/` directories.

### Shiny/R code
- `global.R` contains global variables and functions that are accessible to all users across sessions.
- `server.R` calls the server logic. The server logic for this app is kept in two separate files, one for each navbar page of the shiny app. Thus there is on file for the main data page - `main_server.R` and one for the methods page - `about_server.R`. These files are located under the server/ directory
- `ui.R` defines the app's user interface. Like the server logic - the UI is broken up into two files, one for each nabar page. The `main_ui.R` conatins the UI for the main data page and `about_ui.R` contains the UI for the methods page. Both files are located in the ui/ directory
- `ui-dk.R` contains several bits of the shiny UI that are language specific. A parallel English file needs to be created for the English version of the app.

### R code
The `r/` directory contains the file for data preprocessing and validation
- `data-preprocessing.R` takes the data exported from DST and the geographic data downloaded, and transforms them into the shape and formatted needed by the shiny part of the project. This code needs to be run everytime the data from DST is updated
-`dataValidation.R` compares the Danish population as defined by HjerteTal2 with that defined by StatistikBanken.

### Data
The `data/` directory contains data exported from DST, as well as the UI data (text descriptions of outcomes, variables, etc) and the geographic data (shp files for the kommunes). Some key files are the follwing:
#### DST exports
- `export_diag1.txt` and `export_diag2.txt` contain the export from DST for diseases for every outcome, year and aggregation level
- `export_opr.txt` contain the export from DST for operation/procedure for every outcome, year and aggregation level
- `export_med.txt` contain the export from DST for medication regime for every outcome, year and aggregation level
#### UI text
- `outcome_descriptions.csv`contains the text descriptions of the all the outcomes (i.e. diseases/operations/medications), as well as the ICD/ATC codes, diag type and pattype used.
- `variable_ui.csv` contains the text descriptions explaining variables (i.e. what an age standardized rate means). To allow this text to be dynamic and specific to each outcome the user selects, there are several key words that are dynamically replaced within the shiny sessions. These key words are `REPLACE_OUTCOME`, `REPLACE_YEAR`, `REPLACE_AGGR`, `REPLACE_TYPE`.
- `ui_about_text.csv` contains the text descriptions used in the "methods" page of the app.

### CSS
The .css file can be found under the www/ dir
The css template was copied from a bootstrap template found online, and heavily modified for my own needs.

