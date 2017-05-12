get_time_range <- function(x)
{
  # find start and end of period covered
  #
  # args:
  #   x: array of size (3, nrow, ncol), that represents caption area
  #
  # returns:
  #   POSIXct vector of size 2 if start and end are identified. otherwise NULL

  if (is.null(x)) return(NULL)
  stopifnot(is.array(x))
  stopifnot(length(dim(x)) == 3)
  stopifnot(dim(x)[1] == 3)

  image_list <- extract_caption_letters(x)
  if (length(image_list) < 1) return(NULL)


  # try caption-ja classifier
  cap <- image_list %>%
    # pad
    lapply(pad_crop_image,
           target_rows=caption_ja_classifier$input_size[1],
           target_cols=caption_ja_classifier$input_size[2], value=1) %>%
    # stack
    abind::abind(along=0) %>%
    # predict
    caption_ja_classifier$prediction() %>%
    paste0(collapse='')


  tmp <- stringr::str_match_all(
    cap, '([0-9]{2}/[0-9]{2}/[0-9]{2})[^0-9]*([0-9]{1,2}:[0-9]{1,2}){0,1}')[[1]]
  if (nrow(tmp) >= 2) {
    tmp <- utils::tail(tmp, 2)
    tmp[is.na(tmp[,3]),3] <- '00:00'
    s <- paste(tmp[,2], tmp[,3], sep=' ')
    out <- as.POSIXct(s, format='%y/%m/%d %H:%M', tz='UTC')
    return(out)
  }

  # if caption-ja does not work, try caption-en
  cap <- image_list %>%
    # pad
    lapply(pad_crop_image,
           target_rows=caption_en_classifier$input_size[1],
           target_cols=caption_en_classifier$input_size[2], value=1) %>%
    # stack
    abind::abind(along=0) %>%
    # predict
    caption_en_classifier$prediction() %>%
    paste0(collapse='')

  tmp <- stringr::str_match_all(
    cap, '([A-Za-z0-9]{3})([0-9]{1,2}),([0-9]{4})')[[1]]
  if (nrow(tmp) >= 2) {
    tmp <- utils::tail(tmp, 2)
    yr <- as.integer(tmp[,4])
    dy <- as.integer(tmp[,3])
    mt <- tmp[,2]
    # resolve fuzziness in month names
    if (is.matrix(caption_en_classifier$fuzzy)) {
      for (i in 1:nrow(caption_en_classifier$fuzzy))
      {
        if (caption_en_classifier$fuzzy[i,1] == 'I') next
        # this is to avoid confusion betwen I and l.
        # luckily, we can skip I since I is not used as month name
        mt <- gsub(caption_en_classifier$fuzzy[i,2],
                   caption_en_classifier$fuzzy[i,1],
                   mt)
      }
    }
    mt <- match(mt, month.abb)
    if (any(is.na(mt))) {
      warning('failed to retrieve month from: ', cap)
      return(NULL)
    }
    out <- as.POSIXct(paste(yr,mt,dy, sep='-'), tz='UTC', format='%Y-%m-%d')
    return(out)
  }

  return(NULL)



}


extract_caption_letters <- function(x)
{
  # extract all letters from caption image
  #
  # args:
  #   x: array of size (3, nrow, ncol), that represents caption area
  #
  # returns:
  #   list of matrices, each represents a gray scale images

  if (is.null(x)) return(list())
  stopifnot(is.array(x))
  stopifnot(length(dim(x)) == 3)
  stopifnot(dim(x)[1] == 3)

  thres_dist <- 0.25
  thres_frac <- 1.0

  dist <- rgb_dist(x, .BEIGE)
  is_beige <- (dist < thres_dist)

  # find rows that include non-background
  not_bg_row <- (apply(is_beige, MARGIN=1, FUN=mean) < thres_frac)
  if (all(!(not_bg_row))) return(list()) # all background
  # the first and last index of non-background
  tmp <- range(which(not_bg_row))
  i1 <- tmp[1]
  i2 <- tmp[2]

  # split into letters
  frac <- apply(is_beige[i1:i2, , drop=FALSE], MARGIN=2, FUN=mean)
  is_bg_col <- (frac >= thres_frac)
  tmp <- detect_consecutive_false(is_bg_col)
  if (nrow(tmp) < 1) return(list())
  out <- lapply(1:nrow(tmp), function(k) x[, i1:i2, tmp[k,1]:tmp[k,2], drop=FALSE])

  # to gray scale
  lapply(out, to_gray, white=.BEIGE, black=.GRAY)
}
