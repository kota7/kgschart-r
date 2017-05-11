extract_axis_letters <- function(x)
{
  # Extract all letters from a yaxis image
  #
  # args:
  #   x: array of size (3, nrow, ncol) representing yaxis image
  #
  # return:
  #   list of array of size (nrow, ncol), representing gray scale images

  if (is.null(x)) return(list())
  stopifnot(is.array(x))
  stopifnot(length(dim(x)) == 3)
  stopifnot(dim(x)[1] == 3)

  thres_dist <- 0.1
  thres_frac <- 0.95

  dist <- rgb_dist(x, .BEIGE)
  is_beige <- (dist < thres_dist)

  frac <- apply(is_beige, FUN=mean, MARGIN=1)
  is_bg_row <- (frac >= thres_frac)
  tmp <- detect_consecutive_false(is_bg_row)
  row1 <- tmp[,1]
  row2 <- tmp[,2]

  split_letters <- function(i1, i2)
  {
    frac <- apply(is_beige[i1:i2,], FUN=mean, MARGIN=2)
    is_bg_col <- (frac >= thres_frac)
    tmp <- detect_consecutive_false(is_bg_col)
    start <- tmp[,1]
    end <- tmp[,2]
    Map(function(j1,j2) x[,i1:i2, j1:j2], start, end)
  }

  out <- Map(split_letters, row1, row2)

  # flattens "list of lists of arrays" into list of arrays
  out <- do.call(c, out)

  # convert to gray scale
  lapply(out, to_gray, white=.BEIGE, black=.GRAY)
}



get_label_matrices <- function(x, positions)
{
  # returns a list of matrices representing y-axis labels
  #
  # args:
  #   x: array of size (3, nrow, ncol) representing yaxis image
  #   positions: row indices that roughly identify the location of labels
  #
  # returns:
  #   a list of matrices, each matrix represents a gray scale image
  #

  if (is.null(x)) return(list())
  stopifnot(is.array(x))
  stopifnot(length(dim(x)) == 3)
  stopifnot(dim(x)[1] == 3)

  thres_dist <- 0.1
  thres_frac <- 0.95

  dist <- rgb_dist(x, .BEIGE)
  is_beige = (dist < thres_dist)


  frac <- apply(is_beige, FUN=mean, MARGIN=1)
  is_bg_row <- (frac >= thres_frac)
  tmp <- detect_consecutive_false(is_bg_row)
  row1 <- tmp[,1]
  row2 <- tmp[,2]


  # map positions to (row1, row2)
  # this may potentionally empty (e.g. 10d is not written)
  # but logically cannot be multiple
  p_to_ind <- lapply(positions, function(p) {
    which(p >= row1 & p <= row2)
  })


  split_letters <- function(i1, i2)
  {
    frac <- apply(is_beige[i1:i2,], FUN=mean, MARGIN=2)
    is_bg_col <- (frac >= thres_frac)
    tmp <- detect_consecutive_false(is_bg_col)
    start <- tmp[,1]
    end <- tmp[,2]
    Map(function(j1,j2) x[,i1:i2, j1:j2], start, end)
  }

  tmp <- Map(split_letters, row1, row2)
  # this is a list of lists representing each letter.
  # each letter is 3d array of RGB image

  # convert each image gray scale
  tmp <- lapply(tmp, function(o) {
    lapply(o, to_gray, white=.BEIGE, black=.GRAY)
  })


  # arrange so each element corresponds to the positions
  lapply(p_to_ind, function(i) {
    if (length(i)==1) tmp[[i]] else NULL
  })
}




get_rank_range <- function(x, label_positions)
{
  # find min and max of ratings
  #
  # args:
  #   x: array of size (3, nrow, ncol) representing yaxis image
  #   label_positions: row indices that roughly identify the location of labels
  #
  # returns:
  #   character vector of size 2 if min and max are found. otherwise NULL

  if (is.null(x)) return(NULL)
  stopifnot(is.array(x))
  stopifnot(length(dim(x)) == 3)
  stopifnot(dim(x)[1] == 3)

  labels <- get_label_matrices(x, label_positions) %>%
    # pad all images to the required shape, then stack
    lapply(function(a) {
      abind::abind(lapply(a, pad_crop_image,
                          target_rows=yaxis_classifier$input_size[1],
                          target_cols=yaxis_classifier$input_size[2], value=1),
                   along=0)
    }) %>%
    # predict the letters
    lapply(function(a) if (is.null(a)) '' else yaxis_classifier$prediction(a)) %>%
    lapply(paste0, collapse='') %>%
    unlist()
  #print(labels)

  # find max rank from the top
  num_rank <- str_to_num_rank(labels)
  #print(num_rank)
  if (all(is.na(num_rank))) return(NULL)

  i <- which.max(num_rank)
  max_rank <- num_rank[i] + (i-1L)
  i <- which.min(num_rank)
  min_rank <- num_rank[i] + (i-length(num_rank))


  num_to_str_rank(c(min_rank, max_rank))
}
