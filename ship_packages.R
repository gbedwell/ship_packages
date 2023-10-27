# Create a data frame of all installed packages in the current R version library
# write is a Boolean that states whether or not to write out a CSV file.
# dir.path defines the path of the directory where you want the output file created.
# lib.path defines the library path. If NULL, .libPaths() is used.
# v defines the R version. 
# v is best input as character string, but can be numeric.
address_packages <- function( write = TRUE, dir.path = getwd(), lib.path = NULL, v = NULL ){
  # Based on https://ibecav.github.io/update_libraries/

  if( !isTRUE( write ) && is.null( lib.path ) ){
    stop( paste( "To use the current library path, you must write out a file.",
                 "To use this function without writing anything, specify a different library path.",
                 sep = "\n" ),
          call. = FALSE )
  }
  
  if( is.null( lib.path ) ){
    lib.path <- .libPaths()
  }
  
  pkgs <- as.data.frame( installed.packages( lib.loc = lib.path ) )
  pkgs <- pkgs[ pkgs$Priority != "base" | is.na( pkgs$Priority ), ]
  pkgs <- subset( x = pkgs, select = -c( LinkingTo:Suggests ) )
  pkgs <- subset( x = pkgs, select = -c( Enhances:MD5sum ) )
  pkgs <- droplevels( pkgs )
  
  pkg.source <- function( pkg ){
    x <- as.character( packageDescription( pkg, lib.loc = lib.path )$Repository )
    if ( length(x)==0 ) {
      y <- as.character( packageDescription( pkg, lib.loc = lib.path )$GithubRepo )
      z <- as.character( packageDescription( pkg, lib.loc = lib.path )$GithubUsername )
      if ( length(y) == 0 ) {
        if ( isTRUE( any( grepl( pattern = "bioconductor", 
                                 x = as.character( packageDescription( pkg, lib.loc = lib.path ) ), 
                                 ignore.case = TRUE ) ) ) ){
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
  
  if( is.null( v ) ){
    v <- paste0( R.Version()$major, ".", 
                 R.Version()$minor )
    } else{
      v <- as.character( v )
    }
  
  # Create a new matching variable based on version.
  # Only includes major and minor values, not patch/dev values.
  # e.g., 4.3, not 4.3.1
  # This is because R doesn't always include patch/dev versions in library paths.
  v.match <- substr( x = v, start = 1, stop = 3 )
  
  # Makes sure that major/minor version values jive with library path.
  if( !isTRUE( grepl( pattern = v.match, x = lib.path ) ) ){
    stop( paste( "Incompatibility between R version and library path.",
                 "Double check that the target R version matches the target library.",
                 sep = "\n" ),
          call. = FALSE )
    }
  
  if( isTRUE( write ) ){
    # Names output file according to provided version.
    # Will include patch/dev values if input.
    write.csv( x = pkgs, 
               file = paste0( dir.path, "/", "packages_R-", 
                              v, "_", Sys.Date(), ".csv"),
               row.names = FALSE )
    } else{
    return( pkgs )
      }
}

# Install previously installed packages in new R version library.
# Uses the output from address_packages().
# Installs the up-to-date package version. Check package compatibility requirements.
# To omit certain packages from being installed, include them in the 'omit' argument.
# Packages on CRAN, Bioconductor, or GitHub can be installed with this function.
# Installation from other repositories is not currently supported.
# Such packages should be installed by other means.
# Installation from CRAN can be done without any additional dependencies.
# Installing from Bioconductor enforces an install of BiocManager.
# Installing from GitHub enforces an install of devtools for the install_github() function.
deliver_packages <- function( package.df = NULL,
                              package.csv = NULL, 
                              from = c( "CRAN", "Bioconductor", "GitHub" ), 
                              omit = NULL ){
  
  if( is.null( package.df ) && is.null( package.csv ) ){
    stop( "One of package.df or package.csv must be defined.",
          call. = FALSE )
  }
  
  if( !is.null( package.df ) && !is.null( package.csv ) ){
    stop( "Both package.df and package.csv cannot be defined.",
          call. = FALSE )
  }
  
  if( !is.null( package.df ) ){
    if( is.data.frame( package.df ) ){
      pkgs <- package.df
      } else{
        stop( "package.df must be a data frame.",
              call. = FALSE )
      }
    } else{
      if( is.character( package.csv ) ){
        pkgs <- read.csv( file = package.csv,
                          header = TRUE )
      } else{
          stop( "package.csv must be a file path.",
                call. = FALSE )
      }
    }
  
  valid.repos <- c( "CRAN", "cran", "Bioconductor",
                    "bioconductor", "GitHub", "github" )
  
  if ( any( !from %in% valid.repos ) ){
    ignore <- from[ !from %in% valid.repos ]
    warning( "The following are not currently supported package repositories and will be ignored:",
             "\n",
             ignore )
  }
  
  # Check to see what packages are already installed.
  installed.pkgs <- as.data.frame( installed.packages() )$Package
  
  # Make install list containing uninstalled packages.
  uninstalled.pkgs <- pkgs[ !pkgs$Package %in% installed.pkgs, ]
  
  # Remove packages listed in omit.
  if ( !is.null( omit ) ){
    uninstalled.pkgs <- uninstalled.pkgs[ !uninstalled.pkgs$Package %in% omit ]
    }
  
  # Install packages from each of the respective repositories.
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
      
      warning( "Enforcing BiocManager install..." )
      
      if ( !require( "BiocManager", quietly = TRUE ) ){
        install.packages( "BiocManager" )
        }
      
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
      
      if ( !require( "devtools", quietly = TRUE ) ){
        install.packages( "devtools" )
        }
      
      invisible( sapply( X = gh.pkgs,
                         FUN = function(x) { 
                           devtools::install_github( repo = x, upgrade = FALSE ) }
                         )
                 )
    } else{
      warning( "No GitHub packages present. Moving on..." )
    }
  }
  
  # Enumerate newly installed packages.
  new.installed <- as.data.frame( installed.packages() )$Package
  new.installed <- new.installed[ !new.installed %in% installed.pkgs ]
  
  # ID still uninstalled packages.
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