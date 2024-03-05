#
# (c) 2021 Andreas Geyer-Schulz
#          Simple Genetic Algorithm in R. V 0.1
#          Layer: Top-level main programs.     
#          Package: xega
#

# Distributed Master-Worker Configuration.
#
# @description Provided as a concrete example configuration.
#              The master must be able to launch ssh-authenticated 
#              R-processes (without interactive login) as workers. 
#              The example shown builds a cluster of 4 R-processes 
#              on three machines which communicate via socket connections.
#
# @details A named list with the following elements:
#  \enumerate{
#  \item \code{$port}:    The IP port number of the master. 
#                         E.g. in 1024 -65353.  
#  \item \code{$master}:  The domain name of the master.
#  \item \code{$workers}: A list of domain names of workers.
#  }
#
#
# workers <- c("em-pop.iism.kit.edu", "em-pop.iism.kit.edu", 
#	     "em-folk.iism.kit.edu")
# master <- "em-ags-nb2.iism.kit.edu"
# port <- 10250
#
# @export
# Cluster<-list(
# 	port=10250,
#      master="em-ags-nb2.iism.kit.edu",
#    workers=c("em-pop.iism.kit.edu", 
# 	       "em-pop.iism.kit.edu", 
# 	       "em-pop.iism.kit.edu", 
# 	       "em-folk.iism.kit.edu"))
# 

