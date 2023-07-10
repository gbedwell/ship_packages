# Create a CSV file listing all installed packages in the current R version
# dir.path defines the path of the directory where you want the output file created.
get_package_info <- function( dir.path ){
  # Based on https://ibecav.github.io/update_libraries/
  
  pkgs <- as.data.frame( installed.packages() )
  pkgs <- pkgs[ pkgs$Priority != "base" | is.na( pkgs$Priority ), ]
  pkgs <- subset( x = pkgs, select = -c( LinkingTo:Suggests ) )
  pkgs <- subset( x = pkgs, select = -c( Enhances:MD5sum ) )
  pkgs <- droplevels( pkgs )
  
  pkg.source <- function( pkg ){
    x <- as.character( packageDescription( pkg )$Repository )
    if ( length(x)==0 ) {
      y <- as.character( packageDescription( pkg )$GithubRepo )
      z <- as.character( packageDescription( pkg )$GithubUsername )
      if ( length(y) == 0 ) {
        if ( isTRUE( any( grepl( pattern = "bioconductor", x = as.character( packageDescription( pkg ) ), ignore.case = TRUE ) ) ) ){
          return( "Bioconductor" ) } else{
            return( "Other" ) }
      } else {
        return( paste0( "GitHub; repo = ", z, "/", y ) )
      }
    } else {
      return(x)
    }
  }
  
  pkgs$location <- sapply( pkgs$Package, pkg.source )
  
  write.csv( x = pkgs, 
             file = paste0( dir.path, "/", "Rpackages_R-", 
                            R.Version()$major, ".", R.Version()$minor, "_", Sys.Date(), ".csv"),
             row.names = FALSE )
}

# Install previously installed packages in new R version library path.
# Uses the output from get_package_info().
# Installs up-to-date package version. Check package compatibility requirements.
# To omit certain packages from being installed, include them in the 'omit' argument.
install_packages <- function( package.csv, omit = NULL ){
  
  pkgs <- read.csv( file = pkg.csv,
                    header = TRUE )
  
  installed.pkgs <- as.data.frame( installed.packages() )$Package
  
  uninstalled.pkgs <- pkgs[ !pkgs$Package %in% installed.pkgs, ]
  
  if ( !is.null( omit ) ){
    uninstalled.pkgs <- uninstalled.pkgs[ !uninstalled.pkgs$Package %in% omit ]
    }
  
  cat( "\n", "Installing CRAN packages...", "\n" )
  
  cran.pkgs <- uninstalled.pkgs[ uninstalled.pkgs$location == "CRAN", ]$Package
  
  install.packages( cran.pkgs )
  
  cat( "\n", "Installing Bioconductor packages...", "\n" )
  
  bioc.pkgs <- uninstalled.pkgs[ uninstalled.pkgs$location == "Bioconductor", ]$Package
    dplyr::filter( whereat == "Bioconductor" ) |>
    dplyr::pull( Package )  
  
  if ( !require( "BiocManager", quietly = TRUE ) )
    install.packages( "BiocManager" )
  
  BiocManager::install( bioc.pkgs, update = FALSE )
  
  cat( "\n", "Installing GitHub packages...", "\n" )
  
  gh.pkgs <- uninstalled.pkgs[ grepl( x = uninstalled.pkgs$location, pattern = "GitHub" ), ]$Package
  
  gh.pkgs <-   gsub( "GitHub; repo = ", "", gh.pkgs )
  
  invisible( sapply( X = gh.pkgs,
                     FUN = function(x) { devtools::install_github( repo = x, upgrade = FALSE ) } ) )
  
  new.installed <- as.data.frame( installed.packages() )$Package
  new.installed <- new.installed[ !new.installed %in% installed.pkgs ]
  remaining <- uninstalled.pkgs[ !uninstalled.pkgs$Package %in% new.installed, ]
  
  if( length( remaining ) != 0 ){
    warning( "Installed ", nrow( new.installed ), "/", nrow( uninstalled.packages ), " packages.",
             nrow( remaining ), " packages are still uninstalled.",
             "\n",
             "It is possible that they are not on CRAN/Bioconductor/GitHub.",
             "\n",
             "If required, these packages should be installed manually.",
             "\n",
             "Printing the uninstalled package information...")
    return( remaining[ colnames( remaining ) %in% c( "Package", "Version", "location" ) ] )
  } else{
    cat( "Done!" )
  }
}