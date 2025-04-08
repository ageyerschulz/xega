
#library(filelock)

# (c) 2025 Jens Kleineheismann

#' Create a unique filename.
#'
#' @description Name conflicts in filenames are avoided by 
#'    \itemize{
#'    \item Including the time fractions below a second (\code{tfrac}).
#'    \item Padding the name with 6 random letters.
#'    \item Locking and retrying (as a last resort).
#'          The program stops after 10 unsuccessful attempts of finding 
#'          a unique name.
#'    }
#'    Created by Jens Kleineheismann (2025).
#'    
#' @param fpath    File path. Default: ".".
#' @param prefix   The filename. Default: "data".
#' @param ext      The file extension. Default: ".dat".
#'
#' @return A filename. Components: 
#' [prefix]_[year][month][day]_[h][min][sec]_[node]_[pid]_[pad]_[fracsec].[ext]
#'
#' @family File I/O
#'
#' @examples
#' tmp<-tempdir()
#' fn<-createExclusiveFile(fpath=tmp, prefix="data", ext=".dat") 
#' cat(fn)
#'
#' @importFrom filelock lock
#' @importFrom filelock unlock
#' @export 
createExclusiveFile <- function(fpath=".", prefix="data", ext=".dat") {
makeFilename <- function(prefix="data", ext=".dat", sep="_") {
getRandomString <- function() {
    return(paste(sample(letters, 6), collapse="")) }
    oldOptions<-options(digits.secs=6)
    on.exit(options(oldOptions))
    t<-Sys.time()
    timestamp_fmt = paste("%Y%m%d", "%H%M%S", sep=sep)
    timestamp = format(t, timestamp_fmt)
    hostname = Sys.info()["nodename"]
    pid = Sys.getpid()
    pad = getRandomString()
    tfrac<-unlist(strsplit(paste0(t), "\\."))[2]
    filename = paste(prefix, sep, timestamp, sep, hostname, sep, pid,
                     sep, pad, sep, tfrac, ext, sep="")
    return(filename) }
#### Start
    maxTries = 10
    while(maxTries > 0) {
        maxTries = maxTries - 1
        filename = makeFilename(prefix=prefix, ext=ext)
        path = file.path(fpath, filename)
        lckPath = paste(path, ".lck", sep="")
        lck = lock(lckPath, exclusive=TRUE, timeout=0)
        if(is.null(lck)) {
            # Someone else has locked this filename,
            # so we must try another filename.
            next }
        if(file.exists(path)) {
            # We locked this filename,
            # but the file already exists,
            # so we must remove the lock again
            # and try another filename.
            unlock(lck)
            file.remove(lckPath)
            next }
        # This filename was not locked by someone else (now it is by us)
        # and the file did not exist already,
        # so we can use it (and then remove the lock).
        file.create(path)
        unlock(lck)
        file.remove(lckPath)
        return(path) }
    return(NULL) }

# main <- function() {
#    directory = "."
#    path = createExclusiveFile(fpath=directory, 
#                                 prefix="results", ext=".rds")
#    if(is.null(path))
#        stop("Cannot create an exclusive file.")
#
#    data = 17
#    saveRDS(data, path)
#    cat("Wrote data to file", path, "\n")
#}

# main()
