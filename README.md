# GPSskyrunners
Shiny app to read and interpret GPX files (XML representation of GPS coordinates).
The idea and actually much of the code comes from a great entry in the [Rcrastinate blog](http://rcrastinate.blogspot.de/2014/09/stay-on-track-plotting-gps-tracks-with-r.html).

## How to run it
### Prerequisites
A working R installation plus the following R libraries:

- shiny: the framework used to deploy the app
- XML: parse XML files
- Imap: calculate the geographic distance between two (sets of) points on an ellipsoid
- OpenStreetMap: get maps from the OpenStreetMap project
- grid and gridExtra: display matrix as graphical output

### How to run it
- From the [shinyapps.io hosting service](https://ssayols.shinyapps.io/GPSskyrunners).
- From within RStudio, open the file *app.R* file and click on the *Run app* button in the upper-left corner.
- From an R console, run from the parent folder where you cloned the git-repo:
```
R> shiny::runApp("GPSskyrunners")
```
