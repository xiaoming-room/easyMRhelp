right <- function (x, n)
{
  if (any(is.data.frame(x), is.matrix(x))) {
    for (i in 1:ncol(x)) {
      x.i = as.character(x[, i])
      x[, i] = substr(x.i, nchar(x.i) - n + 1, nchar(x.i))
    }
    x
  }
  else if (is.factor(x)) {
    x.i = as.character(x)
    substr(x.i, nchar(x.i) - n + 1, nchar(x.i))
  }
  else {
    substr(x, nchar(x) - n + 1, nchar(x))
  }
}

right <- function (x, n)
{
  if (any(is.data.frame(x), is.matrix(x))) {
    for (i in 1:ncol(x)) {
      x.i = as.character(x[, i])
      x[, i] = substr(x.i, nchar(x.i) - n + 1, nchar(x.i))
    }
    x
  }
  else if (is.factor(x)) {
    x.i = as.character(x)
    substr(x.i, nchar(x.i) - n + 1, nchar(x.i))
  }
  else {
    substr(x, nchar(x) - n + 1, nchar(x))
  }
}

Replace0 <- function(data, ...)
{
  from <- c(...)
  Replace1 <- function(data, from, to) {
    if (any(is.data.frame(data), is.matrix(data))) {
      for (i in 1:ncol(data)) {
        data[, i] = gsub(from, to, data[, i])
      }
    }
    else {
      data = gsub(from, to, data)
    }
    data
  }
  for (i in 1:length(from)) {
    data = Replace1(data, from[i], to = "")
  }
  data
}

Replace <- function (data, from, to, pattern, ignore.case = FALSE)
{
  if (all(!missing(from), !missing(to))) {
    for (i in 1:length(from)) {
      fromi <- from[i]
      data = Replace1(data, fromi, to, ignore.case = ignore.case)
    }
  }
  if (!missing(pattern)) {
    for (j in 1:length(pattern)) {
      from = gsub(":.*", "", pattern[j], ignore.case = ignore.case)
      to = gsub(".*:", "", pattern[j], ignore.case = ignore.case)
      data = Replace1(data, from, to, ignore.case = ignore.case)
    }
  }
  data
}

list1 <- function (x)
{
  x[[1]]
}


is.windows <- function ()
{
  tolower(Sys.info()["sysname"]) == "windows"
}


desc2df <- function (desc)
{
  fields <- desc::desc_fields(desc)
  field_content <- sapply(fields, function(i) desc::desc_get_field(key = i,
                                                                   default = NA, trim_ws = TRUE, file = desc))
  mt <- matrix(field_content, nrow = 1, dimnames = list(NULL,
                                                        fields))
  data.frame(mt, check.names = FALSE, stringsAsFactors = FALSE)
}


rm_nchar <- function (x, least, most)
{
  if (!missing(least))
    x <- x[nchar(x) > least]
  if (!missing(most))
    x <- x[nchar(x) < most]
  x
}

formal_dir <- function (dir, end.slash = FALSE)
{
  if (end.slash) {
    paste0(Replace(Trim_right(dir, c("\\\\", "/")), "\\\\",
                   "/"), "/")
  }
  else {
    Replace(Trim_right(dir, c("\\\\", "/")), "\\\\", "/")
  }
}

Replace1 <- function (data, from, to, ignore.case = FALSE)
{
  if (any(is.data.frame(data), is.matrix(data))) {
    for (i in 1:ncol(data)) {
      data[, i] = gsub(from, to, data[, i], ignore.case = ignore.case)
    }
  }
  else {
    data = gsub(from, to, data, ignore.case = ignore.case)
  }
  data
}

Trim_right <- function (x, pattern = " ")
{
  Trim_vector <- function(x, pattern) {
    x = reverse(x)
    for (j in 1:length(x)) {
      x.j = x[j]
      for (i in 1:nchar(x.j)) {
        if (left(x.j, 1) %in% pattern) {
          x.j = mid(x.j, 2, nchar(x.j))
        }
        else {
          (break)(i)
        }
      }
      x[j] = reverse(x.j)
    }
    x
  }
  if (is.atomic(x)) {
    Trim_vector(x, pattern)
  }
  else if (is.data.frame(x) | is.matrix(x)) {
    for (i in 1:ncol(x)) {
      x[, i] = Trim_vector(as.character(x[, i]), pattern)
    }
    x
  }
}

reverse <- function (x)
{
  x = as.character(x)
  x = sapply(x, function(i) paste0(rev(strsplit(i, "")[[1]]),
                                   collapse = ""))
  names(x) = NULL
  x
}

left <- function (x, n)
{
  if (any(is.data.frame(x), is.matrix(x))) {
    for (i in 1:ncol(x)) {
      x[, i] = substr(x[, i], 1, n)
    }
    x
  }
  else {
    substr(x, 1, n)
  }
}
