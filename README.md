# HjerteTal 2

### Running app from within R environment
To run the app within R, simply open the global.R file and run the runApp() command - or press <ctr><shft><entr>

### CSS
The .css file can be found under the www/ dir
The css template was copied from a bootstrap template found online, and heavily modified for my own needs.


### Project File Structure
The shiny app consists of three main R files, all located in the parent directory: `global.R`, `server.R` and `ui.R`.

- `global.R` contains global variables and functions that are accessible to all users across sessions.
- `server.R` calls the server logic. The server logic for this app is kept in two separate files, one for each navbar page of the shiny app. Thus there is on file for the main data page - `main_server.R` and one for the methods page - `about_server.R`. These files are located under the server/ directory
- `ui.R` defines the app's user interface. Like the server logic - the UI is broken up into two files, one for each nabar page. The `main_ui.R` conatins the UI for the main data page and `about_ui.R` contains the UI for the methods page. Both files are located in the ui/ directory

Data from DST is placed in the `data/` directory.
