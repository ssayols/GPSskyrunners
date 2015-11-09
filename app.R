######################################
##
##  Shiny app to read and interpret GPX files 
##  (XML representation of GPS coordinates)
##
##  The idea and actually much of the code comes from
##  the great Rcrastinate blog
##  http://rcrastinate.blogspot.de/2014/09/stay-on-track-plotting-gps-tracks-with-r.html
##
######################################
library(shiny)
library(XML)		# parse XML files
library(Imap)       # calculate the geographic distance between two (sets of) points on an ellipsoid
library(OpenStreetMap)
library(gridExtra)

##
## The server side
##
server <- function(input, output) {
 
    ##
    ## read in GPX files and output as data.frame
    ##
    readGPX <- reactive({
        
        # read input file
        FILE <- input$GPXfile
        if(is.null(FILE)) return(NULL)
        FILE <- FILE$datapath

        # Parse the GPX file
        pfile <- htmlTreeParse(FILE, error=function (...) {}, useInternalNodes=T)
        
        # Get all elevations, times and coordinates via the respective xpath
        elevations <- as.numeric(xpathSApply(pfile, path="//trkpt/ele", xmlValue))
        times  <- xpathSApply(pfile, path="//trkpt/time", xmlValue)
        coords <- xpathSApply(pfile, path="//trkpt", xmlAttrs)
        
        # Extract latitude and longitude from the coordinates
        lats <- as.numeric(coords["lat", ])
        lons <- as.numeric(coords["lon", ])
        
        # Put everything in a dataframe and get rid of old variables
        geodf <- data.frame(lat=lats, lon=lons, ele=elevations, time=times)
        
        # Shift vectors for lat and lon so that each row also contains the next position.
        geodf$lat.next <- geodf$lat[2:(nrow(geodf)+1)]	# language abuse: go beyond the boundaries to get an NA
        geodf$lon.next <- geodf$lon[2:(nrow(geodf)+1)]	# language abuse: go beyond the boundaries to get an NA
        
        # Calculate the geographic distance between two (sets of) points on an ellipsoid (lonlat=T, on a plane lonlat=F)
        geodf$dist.to.next <- apply(geodf[, c("lat", "lon", "lat.next", "lon.next")], 1, function(x) {
        	Imap::gdist(x["lon"], x["lat"], x["lon.next"], x["lat.next"], units="m", verbose=F)
        })
        geodf$dist <- cumsum(c(0, geodf$dist.to.next[-nrow(geodf)]))
        
        # Transform the column 'time' so that R knows how to interpret it.
        geodf$time <- strptime(geodf$time, format="%Y-%m-%dT%H:%M:%OS")
        geodf$time.next <- geodf$time[2:(nrow(geodf)+1)]	# Shift the time vector, too.
        
        # Calculate the number of seconds between two positions.
        geodf$time.diff.to.next <- as.numeric(difftime(geodf$time.next, geodf$time))
        
        # Calculate metres per seconds, kilometres per hour and two LOWESS smoothers to get rid of some noise.
        geodf$speed.min.per.km <- (geodf$time.diff.to.next / geodf$dist.to.next) * (1 / 60) * (1000 / 1)	# 1min/60sec, 1000m/1km
        geodf$speed.min.per.km <- ifelse(is.na(geodf$speed.min.per.km), 0, geodf$speed.min.per.km)
        
        # Remove speed spikes (cut everything between [1/2, 2] * avg speed)
        upper.threshold <-   2 * mean(geodf$speed.min.per, na.rm=T)
        lower.threshold <- 1/2 * mean(geodf$speed.min.per, na.rm=T)
        geodf$speed.min.per.km <- ifelse(geodf$speed.min.per > upper.threshold, upper.threshold,
                                  ifelse(geodf$speed.min.per < lower.threshold, lower.threshold,
                                         geodf$speed.min.per))
        
        geodf
    })

    ##   
    ## elevation and speed vs. distance plot
    ##
    plotDist <- function(geodf) {

        # Plot elevations and smoother
        xlab <- pretty(geodf$dist)
        ylab <- pretty(geodf$ele)
        plot(geodf$ele ~ geodf$dist, type="n", bty="n", axes=F, ylab="", xlab="Distance (km)", xlim=range(xlab), ylim=range(ylab))
        lines(lowess(geodf$ele ~ geodf$dist,f=0.2), col="red", lwd=3)
        axis(side=1,at=xlab,labels=xlab/1000)
        axis(side=2,at=ylab)
        mtext("Elevation", side=2, outer=TRUE, line=-1)
        
        # Plot speeds and smoother
        par(new=TRUE)
        ylab <- pretty(if(input$fixsmooth) lowess(geodf$speed.min.per.km ~ geodf$dist, f=input$span, iter=100)$y 
                       else geodf$speed.min.per.km) 
        plot(geodf$speed.min.per.km ~ geodf$dist, type=ifelse(input$fixsmooth,"n","l"), bty="n", 
        	 axes=F, ylab="", xlab="", xlim=range(xlab), ylim=range(ylab))
        lines(lowess(geodf$speed.min.per.km ~ geodf$dist, f=input$span,iter=100), col="blue", lwd=3)
        abline(h=mean(geodf$speed.min.per.km), lty=2, col="blue")
        axis(side=4,at=ylab)
        mtext("Speed (min/km)", side=4, outer=TRUE, line=-4)
        legend(x=paste0(input$legendH, input$legendV), 
               legend=c("elevation", "speed", "avg speed"), 
               col=c("red", "blue", "blue"), lwd=c(3,3,1), lty=c(1,1,2), bty="n")
    }
    
    output$distPlot <- renderPlot({

        # read input file
        geodf <- readGPX()
        if(is.null(geodf)) return(NULL)
        
        plotDist(geodf)
    })
    
    ##   
    ## map plot
    ##
    mapPlotWidth <- reactive( {
        if(is.null(input$mapWidth)) return("auto")
        input$mapWidth
    })
        
    mapPlotHeight <- reactive( {
        if(is.null(input$mapHeight)) return("auto")
        input$mapHeight
    })
    
    plotMap <- function(geodf) {
         # get map
        mar.lat <- diff(range(geodf$lat)) * .1	# margin for latitud
        mar.lon <- diff(range(geodf$lon)) * .1	# margin for longitud
        map <- openmap(c(max(geodf$lat) + mar.lat, min(geodf$lon) - mar.lon),	# upper left corner
        			   c(min(geodf$lat) - mar.lat, max(geodf$lon) + mar.lon),	# lower right corner
        			   type=input$maptype, minNumTiles=input$numTiles)
        transmap <- openproj(map, projection="+proj=longlat")
        
        # plot map (projection) and lines with the gps coordinates
        par(mar=c(0,0,0,0))
        plot(transmap, raster=T)
        lines(geodf$lon, geodf$lat, type="l", col=scales::alpha("red", .5), lwd=4)
    }

    output$mapPlot <- renderPlot({
        # read input file
        geodf <- readGPX()
        if(is.null(geodf)) return(NULL)
        
        plotMap(geodf)
    }, width=mapPlotWidth, height=mapPlotHeight, units="px")
    
    ##
    ## Run Summary
    ##
    runSummary <- function(geodf) {
        x <- sapply(split(geodf, factor(floor(geodf$dist / 1000))), function(x) {
            x$ele.next <- x$ele[2:(nrow(x)+1)]	# language abuse: go beyond the boundaries to get an NA
            data.frame(km=ceiling(x$dist[1] / 1000 + .01),
                       time=format(.POSIXct(difftime(x$time[nrow(x)], x$time[1], units="secs"), tz=NULL), "%M:%S"),
                       slope=round(sum(x$ele.next - x$ele, na.rm=T), 2))
        })
        x <- t(x)
    }
    
    output$runSummary <- renderTable({
        # read input file
        geodf <- readGPX()
        if(is.null(geodf)) return(NULL)
        
        runSummary(geodf)
    }, include.rownames=FALSE)
    
    ##
    ## Download as PDF
    ##
    output$downloadData <- downloadHandler(
        filename="output.pdf",
        content = function(file) {
            # read input file
            geodf <- readGPX()
            if(is.null(geodf)) return(NULL)
        
            pdf(file)
            plotDist(geodf)
            plotMap(geodf)
            x <-runSummary(geodf)
            mode(x) <- "character"
            grid.newpage()
            grid.table(x, rows=NULL)
            dev.off()
        }
    )
}

##
## the client side
##
ui <- fluidPage(
    headerPanel("GPS skyrunner\'s stats"),
    sidebarLayout(
        sidebarPanel(
            fileInput("GPXfile", "Choose GPX file", accept=c("text/xml", ".gpx")),
            tags$hr(),
            sliderInput("span", "Lowess smoothing span:", min=0, max=1, value=.01),
            checkboxInput("fixsmooth", "Show smoothed speed only", TRUE),
            radioButtons("legendH", "Legend:",
                         c("top" = "top",
                           "bottom" = "bottom"), inline=TRUE),
            radioButtons("legendV", NULL,
                         c("left" = "left",
                           "center" = "",
                           "right" = "right"), selected="center", inline=TRUE),
            tags$hr(),
            radioButtons("maptype", "Map type:",
                         c("OpenStreetMap" = "osm",
                           "OpenCycleMap" = "opencyclemap",
                           "Topographic" = "esri-topo")),
            sliderInput("numTiles" , "Num. tiles (+resolution +download time):", min=2, max=100, value=10, ticks=FALSE),
            sliderInput("mapWidth" , "Width (px, default: auto):" , min=100, max=10000, value=1000, step=100, ticks=FALSE),
            sliderInput("mapHeight", "Height (px, default: auto):", min=100, max=10000, value=1000, step=100, ticks=FALSE),
            tags$hr(),
            downloadButton("downloadData", "Download")
        ),
        mainPanel(
            tabsetPanel(type="tabs", 
                        tabPanel("distPlot"  , plotOutput("distPlot")),
                        tabPanel("mapPlot"   , plotOutput("mapPlot")),
                        tabPanel("runSummary", tableOutput("runSummary"))
            )
        )
    )
)

shinyApp(ui=ui, server=server)