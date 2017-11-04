my_makeImport <- function(script, cut = NULL, print = TRUE, format = "oxygen") {
  rInst <- paste0(row.names(utils::installed.packages()), "::")
  if (inherits(script, "function")) {
    file <- tempfile()
    utils::capture.output(print(script), file = file)
  } else {
    file = script
  }
  pkg = sapply(file, function(f) {
    x <- readLines(f, warn = F)
    x = gsub("^\\s+", "", x)
    x = x[!grepl("^#", x)]
    x = x[!grepl("^<", x)]
    s0 = sapply(paste0("\\b", rInst), grep, x = x, value = TRUE)
    s1 = s0[which(sapply(s0, function(y) length(y) > 0))]
    names(s1) = gsub("\\\\b", "", names(s1))
    ret = sapply(names(s1), function(nm) {
      out = unlist(lapply(s1[[nm]], function(x) {
        y = gsub("[\\\",\\(\\)]", "", unlist(regmatches(x, 
          gregexpr(paste0(nm, "(.*?)[\\)\\(,]"), x))))
        names(y) = NULL
        if (any(y %in% paste0(nm, c("\"", "'")))) 
          y = NULL
        y
      }))
      out = gsub("\\$.*", "", out)
      out = unique(out)
      if (format == "oxygen") {
        ret = paste0("#' @importFrom ", gsub("::", " ", 
          nm), gsub(nm, "", paste(unique(out), collapse = " ")))
        if (!is.null(cut)) {
          if (length(out) >= cut) 
          ret = paste0("#' @import ", gsub("::", "", 
            nm))
        }
        out = ret
      }
      
      
      return(out)
    })
    
    # requires
    lines <- grep("(require|library)", x)
    if (length(lines) > 0) {
      
      L <- unlist(strsplit(x[lines], split = "\\(|\\)"))
      pkgs <- L[L %in% row.names(utils::installed.packages())]
      ret <- c(ret, paste0("#' @import ", pkgs))
      
    }
    
    if (format == "oxygen") {
      if (print) 
        writeLines(paste(" ", f, paste(ret, collapse = "\n"), 
          sep = "\n"))
    }
    return(ret)
  })
  if (format == "oxygen") 
    ret = pkg
  if (format == "namespace") {
    pkg = sort(unique(unlist(pkg)))
    pkgN = gsub(":.*", "", pkg)
    pkgC = table(pkgN)
    ret = paste0("importFrom(", gsub("::", ",", pkg), ")")
    if (!is.null(cut)) {
      ret = sapply(names(pkgC), function(x) {
        if (pkgC[x] >= cut) {
          sprintf("import(%s)", unique(gsub(":.*", "", 
          x)))
        } else {
          paste0("importFrom(", gsub("::", ",", grep(x, 
          pkg, value = T)), ")")
        }
      })
    }
    retWrite = paste(" ", paste(unlist(ret), collapse = "\n"), 
      sep = "\n")
    if (print) 
      writeLines(retWrite)
  }
  if (format == "description") {
    ret = unique(gsub("::(.*?)$", "", unlist(pkg)))
    retWrite = sprintf("Imports: %s", paste(ret, collapse = ","))
    if (print) 
      writeLines(retWrite)
  }
  if (inherits(script, "function")) 
    unlink(file)
  invisible(paste0(ret, collapse = "\n"))
}

my_makeSeeAlso <- function(obj, cutOFF = 3) {
  x <- my_makeImport(obj, cut = cutOFF, print = FALSE)
  x <- grep("importFrom", strsplit(x, "#'")[[1]], value = TRUE)
  x <- sapply(x, function(y) {
    ret = strsplit(gsub("\\n|@importFrom|^\\s+", "", y), 
      " ")[[1]][-1]
    paste0(sprintf("\\code{\\link[%s]{%s}}", ret[1], ret[-1]), 
      collapse = ",")
  }, USE.NAMES = F)
  paste0("\n#'  ", x)
}

my_makeOxygen <- function(obj, add_default = TRUE, add_fields = c("details", 
  "seealso", "export"), use_dictionary = NULL, print = TRUE, 
  ...) {
  header_add <- sinew_opts$get()
  lbl = deparse(substitute(obj))
  lbl = gsub("\"", "", lbl)
  if (is.character(obj)) 
    obj = eval(parse(text = obj))
  if (inherits(obj, c("data.frame", "tibble"))) {
    cl <- sapply(obj, typeof)
    items <- paste0(sprintf("#'   \\item{\\code{%s}}{%s COLUMN_DESCRIPTION}", 
      names(cl), cl), collapse = "\n")
    header = c(title = "#' @title DATASET_TITLE", description = "#' @description DATASET_DESCRIPTION", 
      format = sprintf("#' @format A data frame with %s rows and %s variables:", 
        nrow(obj), length(cl)))
    ret = sprintf("%s\n%s\n%s%s", paste(header, collapse = "\n"), 
      sprintf("#' \\describe{\n%s \n#'}", items), ifelse(!is.null(add_fields), 
        paste(header_add[add_fields], collapse = "\n"), 
        ""), sprintf("\"%s\"", lbl))
  }
  if (inherits(obj, c("function"))) {
    importList = list()
    importList$script = obj
    importList$print = FALSE
    import = do.call("my_makeImport", importList)
    if (import == "list()") 
      import = ""
    cutOFF = switch("cut" %in% names(importList), importList$cut, 
      3)
    if (import == "") 
      add_fields = add_fields[!grepl("seealso", add_fields)]
    if ("seealso" %in% add_fields) 
      header_add = c(header_add, seealso = paste0(my_makeSeeAlso(obj, 
        cutOFF = cutOFF), collapse = "\n"))
    param_desc = NULL
    if (!is.null(use_dictionary)) 
      param_desc = ls_param(obj = obj, dictionary = use_dictionary, 
        print = FALSE)
    fn = as.list(formals(obj))
    if ("rdname" %in% add_fields) 
      header_add["rdname"] = lbl
    out = sapply(names(fn), function(name_y) {
      cl = class(fn[[name_y]])
      out = as.character(fn[[name_y]])
      if (cl == "NULL") 
        out = "NULL"
      if (cl == "character") 
        out = sprintf("'%s'", as.character(fn[[name_y]]))
      if (cl %in% c("if", "call")) 
        out = deparse(fn[[name_y]])
      out = paste0(out, collapse = "\n#'")
      if (add_default) {
        if (nchar(out) > 0) {
          out = sprintf(", Default: %s", out)
        }
        if (!is.null(use_dictionary) && name_y %in% names(param_desc)) {
          p_desc = param_desc[name_y]
        } else {
          p_desc = paste0("(", cl, ")", " PARAM_DESCRIPTION")
        }
        str_out = sprintf("%s%s", p_desc, out)
      }
      return(str_out)
    })
    if (length(out) > 0) {
      
      max_length <- max(sapply(names(out), nchar))
      
    } else {max_length <- 3}
    
    format <- paste0("#' @param %-", max_length + 3, "s %s")
    params = c(sprintf(format, names(out), out), "#'")
    header = c(title = "#' @title FUNCTION_TITLE", "#'", 
      description = "#' @description FUNCTION_DESCRIPTION", 
      "#'")
    footer = c(return = "#' @return OUTPUT_DESCRIPTION", 
      "#'")
    ret = sprintf("%s\n%s\n%s\n", paste(header, collapse = "\n"), 
      paste(params, collapse = "\n"), paste(footer, collapse = "\n"))
    
    if (!is.null(add_fields)) {
      
      ret <- paste0(ret, sprintf("%s", paste(sprintf("#' @%s %s", 
        names(header_add[add_fields]), header_add[add_fields]), 
        collapse = "\n")))
      
    }
    
    ret <- c(ret, import)
    
    ret <- paste(ret, collapse = "\n")
    
  }
  if (print) 
    writeLines(ret)
  invisible(ret)
}

my_makeOxyFile <- function(input = NULL, overwrite = FALSE, verbose = TRUE, 
  ...) {
  
  require(sinew)
  
  if (is.null(input)) 
    input <- file.choose()
  if (length(input) == 1L && file.info(input)$isdir) {
    files <- list.files(path = input, pattern = ".+\\.R$", 
      full.names = TRUE)
    files <- grep("/(?!Oxy)\\w+\\.R$", files, perl = TRUE, 
      value = TRUE)
  } else {
    files <- input
  }
  if (!all(grepl("\\.R$", basename(files)))) 
    stop("Supplied file(s) is not an .R file!", call. = FALSE)
  append_to_lines <- function(.id, .str) {
    unlist(append(.str, strsplit(oxy_lst[[which(ins_id == 
      .id)]], split = "\n", fixed = TRUE), after = .id))
  }
  neg_msg <- "No functions or data frames found in\n"
  for (FILE in files) {
    lines <- readLines(FILE, warn = FALSE)
    lines <- lines[!grepl("^\\s*#'", lines)]
    objs <- gsub("\\s*([[:alnum:]._]+).*", "\\1", grep("^\\s*[[:alnum:]._]+\\s*(<-|=)", 
      lines, value = TRUE))
    if (length(objs) == 0L) 
      stop(neg_msg, normalizePath(FILE), call. = FALSE)
    if (!all(objs %in% ls(envir = parent.frame()))) {
      nenv <- new.env()
      sys.source(FILE, nenv, keep.source = TRUE)
    }
    sel0 <- seq_along(objs)
    if ("nenv" %in% ls()) {
      sel0 <- objs %in% ls(envir = nenv)
      objs <- objs[sel0]
    }
    sel_obj <- vapply(objs, function(x) {
      inherits(get(x, envir = nenv), c("data.frame", "function"))
    }, FUN.VALUE = logical(1))
    if (!any(sel_obj)) 
      warning(neg_msg, normalizePath(FILE))
    objs <- objs[sel_obj]
    oxy_lst <- lapply(objs, function(obj_name, thisenv, ...) {
      assign(obj_name, get(obj_name, envir = thisenv))
      eval(parse(text = sprintf("my_makeOxygen(%s,...)", 
        obj_name)))
    }, thisenv = nenv, ...)
    ins_id <- which(grepl("^\\s*[[:alnum:]._]+\\s*(<-|=)", 
      lines)) - 1L
    ins_id <- ins_id[sel0]
    ins_id <- ins_id[sel_obj]
    for (i in rev(ins_id)) {
      lines <- append_to_lines(i, lines)
    }
    new_name <- if (overwrite) {
      FILE
    } else {
      file.path(dirname(FILE), paste("oxy", basename(FILE), 
        sep = "-"))
    }
    writeLines(lines, new_name)
  }
  oxyfiles <- if (overwrite) {
    files
  } else {
    file.path(dirname(files), paste("oxy", basename(files), 
      sep = "-"))
  }
  if (length(input) > 0L) {
    if (verbose) {
      if (rstudioapi::isAvailable()) {
        for (i in oxyfiles) rstudioapi::navigateToFile(i)
      } else {
        file.show(oxyfiles)
      }
      message("File(s) with roxygen2 comment templates have been written to:\n", 
        paste0(normalizePath(oxyfiles, winslash = "/"), 
          collapse = "\n"))
    }
  }
}
