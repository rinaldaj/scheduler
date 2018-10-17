#This program is intended to be run once and to set up R to install all the libraries we need and to point to mirror

#set mirror.clarkson.edu as the mirror for our use
local({r <- getOption("repos")
	r["CRAN"] <- "https://mirror.clarkson.edu/cran/"
	options(repos=r)
})

#Install the nessacary packages
install.packages(c('shiny','dplyr','gdata','DT'))
