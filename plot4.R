load_custom_data <- function (..., datasets = character(), pkg = NULL, lib.loc = NULL, 
                              verbose = getOption("verbose"), target_env = .GlobalEnv, overwrite = TRUE) 
{
  # Helper function to get the file extension, including cases with double extensions (e.g., .tar.gz)
  get_file_ext <- function(x) {
    double_ext <- grepl("\\.[^.]+\\.(gz|bz2|xz)$", x)
    ext <- sub(".*\\.", "", x)
    ext[double_ext] <- sub(".*\\.([^.]+\\.)(gz|bz2|xz)$", "\\1\\2", x[double_ext])
    ext
  }
  
  # Helper function to read tables with a consistent collation setting
  read_table_locale <- function(...) {
    original_locale <- Sys.getlocale("LC_COLLATE")
    on.exit(Sys.setlocale("LC_COLLATE", original_locale))
    Sys.setlocale("LC_COLLATE", "C")
    read.table(...)
  }
  
  stopifnot(is.character(datasets))
  data_names <- c(as.character(substitute(list(...))[-1L]), datasets)
  
  # Validate package input
  if (!is.null(pkg)) {
    if (!is.character(pkg)) 
      stop("'pkg' must be a character vector or NULL")
  }
  
  # Determine data paths from the specified package or search locations
  data_paths <- find.package(pkg, lib.loc, verbose = verbose)
  if (is.null(lib.loc)) 
    data_paths <- c(path.package(pkg, TRUE), if (!length(pkg)) getwd(), data_paths)
  
  data_paths <- unique(normalizePath(data_paths[file.exists(data_paths)]))
  data_paths <- data_paths[dir.exists(file.path(data_paths, "data"))]
  data_file_exts <- tools:::.make_file_exts("data")
  
  # If no dataset names are provided, list available datasets
  if (length(data_names) == 0L) {
    dataset_info <- matrix(character(), nrow = 0L, ncol = 4L)
    for (path in data_paths) {
      entries <- NULL
      pkg_name <- if (file_test("-f", file.path(path, "DESCRIPTION"))) basename(path) else "."
      
      # Check for pre-built data index
      if (file_test("-f", data_index <- file.path(path, "Meta", "data.rds"))) {
        entries <- readRDS(data_index)
      } else {
        data_dir <- file.path(path, "data")
        entries <- tools::list_files_with_type(data_dir, "data")
        if (length(entries)) {
          entries <- unique(tools::file_path_sans_ext(basename(entries)))
          entries <- cbind(entries, "")
        }
      }
      
      # Populate dataset information
      if (NROW(entries)) {
        if (is.matrix(entries) && ncol(entries) == 2L) 
          dataset_info <- rbind(dataset_info, cbind(pkg_name, dirname(path), entries))
        else warning(sprintf("Data index for package %s is invalid and will be ignored", sQuote(pkg_name)), call. = FALSE)
      }
    }
    
    colnames(dataset_info) <- c("Package", "LibPath", "Item", "Title")
    footer_msg <- if (missing(pkg)) 
      paste0("Use ", sQuote(paste("data(package =", ".packages(all.available = TRUE))")), "\n", "to list datasets in all *available* packages.")
    else NULL
    result <- list(title = "Data sets", header = NULL, results = dataset_info, footer = footer_msg)
    class(result) <- "packageIQR"
    return(result)
  }
  
  data_paths <- file.path(data_paths, "data")
  
  # Load each specified dataset
  for (dataset_name in data_names) {
    dataset_found <- FALSE
    for (data_path in data_paths) {
      temp_env <- if (overwrite) target_env else new.env()
      
      # Load from RDS data index if available
      if (file_test("-f", file.path(data_path, "Rdata.rds"))) {
        rds_content <- readRDS(file.path(data_path, "Rdata.rds"))
        if (dataset_name %in% names(rds_content)) {
          dataset_found <- TRUE
          if (verbose) 
            message(sprintf("Dataset '%s' found in Rdata.rds", dataset_name))
          pkg_name <- sub(".*/([^/]*)/data$", "\\1", data_path)
          pkg_name <- sub("_.*$", "", pkg_name)
          pkg_name <- paste0("package:", pkg_name)
          objs <- rds_content[[dataset_name]]
          lazyLoad(file.path(data_path, "Rdata"), envir = temp_env, filter = function(x) x %in% objs)
          break
        } else if (verbose) {
          message(sprintf("Dataset '%s' NOT found in Rdata.rds", dataset_name))
        }
      }
      
      # List files in the data path and filter by dataset name
      data_files <- list.files(data_path, full.names = TRUE)
      data_files <- data_files[grep(dataset_name, data_files, fixed = TRUE)]
      
      # Sort files to prioritize based on extensions
      if (length(data_files) > 1L) {
        order_ext <- match(get_file_ext(data_files), data_file_exts, nomatch = 100L)
        file_paths <- dirname(data_files)
        file_paths <- factor(file_paths, levels = unique(file_paths))
        data_files <- data_files[order(file_paths, order_ext)]
      }
      
      # Load the dataset file based on file extension
      if (length(data_files)) {
        for (data_file in data_files) {
          if (verbose) 
            message("Dataset =", dataset_name, ":\t loading file =", basename(data_file))
          ext <- get_file_ext(data_file)
          if (basename(data_file) != paste0(dataset_name, ".", ext)) 
            dataset_found <- FALSE
          else {
            dataset_found <- TRUE
            switch(ext, 
                   R = , r = { library("utils"); sys.source(data_file, chdir = TRUE, envir = temp_env) },
                   RData = , rdata = , rda = load(data_file, envir = temp_env),
                   TXT = , txt = , tab = , tab.gz = , tab.bz2 = , tab.xz = , txt.gz = , txt.bz2 = , txt.xz = 
                     assign(dataset_name, read_table_locale(data_file, header = TRUE, as.is = FALSE), envir = temp_env),
                   CSV = , csv = , csv.gz = , csv.bz2 = , csv.xz = 
                     assign(dataset_name, read_table_locale(data_file, header = TRUE, sep = ";", as.is = FALSE), envir = temp_env),
                   dataset_found <- FALSE
            )
          }
          if (dataset_found) break
        }
        if (verbose) message(if (!dataset_found) "*NOT* ", "found")
      }
      if (dataset_found) break
    }
    if (!dataset_found) {
      warning(sprintf("Dataset '%s' not found", sQuote(dataset_name)))
    } else if (!overwrite) {
      # Copy objects to target environment if overwrite is disabled
      for (obj in ls(envir = temp_env, all.names = TRUE)) {
        if (exists(obj, envir = target_env, inherits = FALSE)) 
          warning(sprintf("Object '%s' already exists and will not be overwritten", sQuote(obj)))
        else assign(obj, get(obj, envir = temp_env, inherits = FALSE), envir = target_env)
      }
      rm(temp_env)
    }
  }
  invisible(data_names)
}
