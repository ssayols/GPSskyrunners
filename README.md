# GPSskyrunners
Shiny app to read and interpret GPX files (XML representation of GPS coordinates).

## How to run it
### Prerequisites
A working R installation plus the following R libraries:

- shiny: the framework used to deploy the app
- XML: parse XML files
- Imap: calculate the geographic distance between two (sets of) points on an ellipsoid
- OpenStreetMap: get maps from the OpenStreetMap project
- Leaflet: open-source JavaScript libraries for interactive maps
- grid and gridExtra: display matrix as graphical output

### How to run it
- From the [shinyapps.io hosting service](https://ssayols.shinyapps.io/GPSskyrunners).
- From RStudio, open the file *app.R* file and click on the *Run app* button in the upper-right corner.
- From an R console, run from the parent folder where you cloned the git-repo:
```
R> shiny::runApp("GPSskyrunners")
```

## TODO
- highlight km in the map.
- segment speed as a heatmap
- no tabbed UI design
- interactive map, displaying the distance, speed and elevation based on the cursor position
- highlight in map the position based on the cursor position in the speed/elevation summary plot

## Acknowledgements
The idea and actually much of the code comes from a great entry in the [Rcrastinate blog](http://rcrastinate.blogspot.de/2014/09/stay-on-track-plotting-gps-tracks-with-r.html).
