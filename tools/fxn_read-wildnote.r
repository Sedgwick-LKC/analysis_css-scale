#' @title Read Excel Sheets in 'WildNote Format'
#' 
#' @description Reads in all sheets in an Excel presumed to be in 'Wildnote format'. This is necessary to retain true "Survey ID" values because WildNote stores these as bizarre hyperlink formulae so special handling is necessary to get the number code that is the actual survey ID (reading in the Excel file with the `readxl` package returns only `NA` for this column which makes it impossible to join the sheets together).
#' 
#' @param wn_path (character) File name (and path if needed) for the Excel file in WildNote format to read in
#' 
read_wildnote <- function(wn_path = NULL){

  # Error checks for 'wn_path'
  if(is.null(wn_path) || is.character(wn_path) != T || length(wn_path) != 1)
    stop("'wn_path' must be a file path to a single Excel file")

  # Make sure file actually exists in file path
  if(file.exists(wn_path) != T)
    stop("File not found at specified file path; check working directory and file name")

  # Identify the sheets in this file (can use `readxl` for this)
  wn_sheets <- readxl::excel_sheets(path = wn_path)

  # Make a list for storing outputs
  out_list <- list()

  # Loop across sheets in the file
  for(wn_tab in wn_sheets){

    # Read in just that sheet
    wn_v01 <- tidyxl::xlsx_cells(path = wn_path, sheets = wn_tab)

    # Parse that into a useable thing
    wn_v02 <- wn_v01 %>% 
      # Pare down to just desired columns
      dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
        .fns = as.character)) %>% 
      dplyr::mutate(content_actual = dplyr::coalesce(logical, numeric, date, character, formula)) %>% 
      dplyr::select(row, col, content_actual) %>% 
      # Regain desired shape/column names
      tidyr::pivot_wider(names_from = col, values_from = content_actual) %>% 
      dplyr::select(-row) %>% 
      stats::setNames(nm = .[1,]) %>% 
      dplyr::filter(`Survey ID` != "Survey ID") %>% 
      dplyr::rename_with(.fn = ~ tolower(gsub(pattern = " |-|\\(|\\)|\\?|\\/", replacement = ".", x = .))) %>% 
      # Extract useable survey ID numbers
      dplyr::mutate(id.num = stringr::str_extract(string = survey.id, pattern = "\"[[:digit:]]{2,20}\"")) %>% 
      dplyr::mutate(id.actual = gsub(pattern = "\"|\\\\", replacement = "", id.num), 
        .before = survey.id) %>% 
      dplyr::select(-survey.id, -id.num) %>% 
      dplyr::rename(survey.id = id.actual) %>% 
      # Remove any columns that are completely empty
      dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0)))

    # Add to output list
    out_list[[wn_tab]] <- wn_v02
  } # Close sheet loop

  # Return the output list
  return(out_list) }

# End ----
