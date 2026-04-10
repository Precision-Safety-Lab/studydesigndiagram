
# About this app
This is a small modification to work done by Lars Christian Lund, original work is available at https://source.coderefinery.org/lcl/repeat-diagrams.

# Setup instruction for Shiny application to generate longitdunal design diagrams

Prerequisites:
* Linux server
* Installed Shiny backend (see https://docs.rstudio.com/shiny-server/#installation)
* R 3.5.1 or higher (compatibility has only been tested using 3.5.1)
	* ggplot2 library
	* ggfittext library
	* shiny library
In R run install.packages(c("ggplot2", "ggfittext", "shiny"))

Move app.R and make-diagram.R to a separate folder in /srv/shiny-server/repeat-diagrams/,
e.g. /srv/shiny-server/repeat-diagrams/.

Create a www directory in the folder, i.e. /srv/shiny-server/repeat-diagrams/www/.
Ensure that the "shiny" user has read and write permission for the www folder.

Navigate to http://shiny.your-server.org/repeat-diagrams/ and ensure that everything works as expected.
