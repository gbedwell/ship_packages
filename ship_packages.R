# Create a CSV file listing all installed packages in the current R version
# dir.path defines the path of the directory where you want the output file created.
write_package_info <- function( dir.path ){
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
# Uses the output from write_package_info().
# Installs up-to-date package version. Check package compatibility requirements.
# To omit certain packages from being installed, include them in the 'omit' argument.
# Packages on CRAN, Bioconductor, or GitHub can be installed with this function.
# Packages from other repositories should be installed via other means.
# Installation from CRAN or Bioconductor can be done without any additional dependencies.
# Installing from GitHub enforces an install of devtools for the install_github() function.
install_packages <- function( package.csv, 
                              from = c( "CRAN", "Bioconductor", "GitHub" ), 
                              omit = NULL ){
  
  valid.repos <- c( "CRAN", "cran", "Bioconductor",
                    "bioconductor", "GitHub", "github" )
  
  if ( any( !from %in% valid.repos ) ){
    ignore <- from[ !from %in% valid.repos ]
    warning( "The following are not currently supported package repositories and will be ignored:",
             "\n",
             ignore )
  }
  
  pkgs <- read.csv( file = pkg.csv,
                    header = TRUE )
  
  installed.pkgs <- as.data.frame( installed.packages() )$Package
  
  uninstalled.pkgs <- pkgs[ !pkgs$Package %in% installed.pkgs, ]
  
  if ( !is.null( omit ) ){
    uninstalled.pkgs <- uninstalled.pkgs[ !uninstalled.pkgs$Package %in% omit ]
    }
  
  if ( "CRAN" %in% from || "cran" %in% from ){
    cat( "\n", "Installing CRAN packages...", "\n" )
    
    cran.pkgs <- uninstalled.pkgs[ uninstalled.pkgs$location == "CRAN", ]$Package
    
    if ( length( cran.pkgs > 0 ) ){
      install.packages( cran.pkgs )
    } else{
      warning( "No CRAN packages present. Moving on..." )
    }
  }
  
  if ( "Bioconductor" %in% from || "bioconductor" %in% from ){
    cat( "\n", "Installing Bioconductor packages...", "\n" )
    
    bioc.pkgs <- uninstalled.pkgs[ uninstalled.pkgs$location == "Bioconductor", ]$Package
    
    if ( length( bioc.pkgs ) > 0 ){
      
      if ( !require( "BiocManager", quietly = TRUE ) )
        install.packages( "BiocManager" )
      
      BiocManager::install( bioc.pkgs, update = FALSE )
    } else{
      warning( "No Bioconductor packages present. Moving on..." )
    }
  }
  
  if ( "GitHub" %in% from || "github" %in% from ){
    cat( "\n", "Installing GitHub packages...", "\n" )
    
    gh.pkgs <- uninstalled.pkgs[ grepl( x = uninstalled.pkgs$location, pattern = "GitHub" ), ]$Package
    
    if ( length( gh.pkgs ) > 0 ){
      gh.pkgs <-   gsub( "GitHub; repo = ", "", gh.pkgs )
      
      warning( "Enforcing devtools install..." )
      
      if ( !require( "devtools", quietly = TRUE ) )
        install.packages( "devtools" )
      
      invisible( sapply( X = gh.pkgs,
                         FUN = function(x) { 
                           devtools::install_github( repo = x, upgrade = FALSE ) }
                         )
                 )
    } else{
      warning( "No GitHub packages present. Moving on..." )
    }
  }
  
  new.installed <- as.data.frame( installed.packages() )$Package
  new.installed <- new.installed[ !new.installed %in% installed.pkgs ]
  remaining <- uninstalled.pkgs[ !uninstalled.pkgs$Package %in% new.installed, ]
  
  if( nrow( remaining ) > 0 ){
    warning( "Installed ", nrow( new.installed ), "/", nrow( uninstalled.packages ), " packages.",
             nrow( remaining ), " packages are still uninstalled.",
             "\n",
             "If required, these packages should be installed manually.",
             "\n",
             "Printing the uninstalled package information...")
    return( remaining[ colnames( remaining ) %in% c( "Package", "Version", "location" ) ] )
  } else{
    cat( "Done!" )
  }
}