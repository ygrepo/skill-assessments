rm(list = ls())


# Create an empty 3x3 matrix to represent the tic-tac-toe board

num_rows <- 3
num_cols <- 3
start_number <- 1


board <- matrix(rep("NA", num_cols * num_cols), nrow = num_rows, byrow = TRUE)


# Function to display the tic-tac-toe board
display_board <- function(board) {
  cat("\n")
  for (row in 1:num_rows) {
    for (col in 1:num_cols) {
      pos <- board[row, col]
      if (pos == "NA")
        cat(board[row, col], " ")
      else
        cat(board[row, col], "  ")
    }
    cat("\n\n")
  }
}

# Display Play Turn Banner
display_round <- function(num_round) {
  cat("##################\n")
  msg <- sprintf("#### Round: %d ####\n", num_round)
  cat(msg)
  cat("##################\n")
}


other_player <- function(symbol) {
  if (symbol == "X") {
    return("O")
  }
  return("X")
}
# Function to read symbol
read_symbol <- function() {
  symbol <- ""
  while ((symbol != "X") & (symbol != "O")) {
    cat("X or O?", fill = F)
    symbol <- toupper(readLines(con = con, n = 1))
    if ((symbol != "X") & (symbol != "O")) {
      cat("Invalid symbol, please enter 'X' or 'O'\n")
    }
  }
  return(symbol)
}
# Function to read row or column
read_row_column <- function(rowToRead) {
  rc <- (-1)
  msg <- "What column?"
  if (rowToRead) {
    msg <- "What row?"
  }
  while ((rc < 1) | (rc > num_rows)) {
    cat(msg)
    rc <- readLines(con = con, n = 1)
    if ((rc < 1) | (rc > num_rows)) {
      cat(paste0("Invalid selection, choose 1,2 or ", num_rows, " for row and column\n"))
    }
  }
  return(as.numeric(rc))
}

# Function to read move y/n
read_yes_no_move <- function(symbol, row, col) {
  ans <- ""
  while ((ans != "y") & (ans != "n")) {
    msg <- paste0("Place '", symbol, "' at row ", row, " column ", col, "? [y/n]")
    cat(msg)
    ans <- tolower(readLines(con = con, n = 1))
    if ((ans != "y") & (ans != "n")) {
      cat("[y]es or [n]o only accepted")
    }
  }
  return(ans)
}

# Function to validate position
is_valid_position <- function(board, row, col) {
  return(board[row, col] == "NA")
}
# Function to update the game board with a player's move
update_board <- function(board, row, col, symbol) {
  pos <- board[row, col]
  if (pos == "NA") {
    board[row, col] <- symbol
  }
  return(board)
}


# Function to return a random position for the computer
# ideally we would want to be smarter here but for now
# sample a row and a column until we find an empty slot
get_random_position <- function(board) {
  for (r in (1:num_rows)) {
    for (c in (1:num_cols)) {
      if (is_valid_position(board, r, c)) {
        return(list(row = r, col = c))
      }
    }
  }
  return(list(row = -1, col = -1))
}

# Function to check if a player has won
check_win <- function(board, symbol) {
  # Check rows, columns, and diagonals for a win
  if (any(apply(board, 1, function(row) all(row == symbol))) ||
    any(apply(board, 2, function(col) all(col == symbol))) ||
    all(diag(board) == symbol) ||
    all(diag(board[, ncol(board):1]) == symbol)) {
    return(TRUE)
  }
  return(FALSE)
}

# Function to check if the game is a draw
check_draw <- function(board) {
  if (all(board != "NA")) {
    return(TRUE)
  }
  return(FALSE)
}


if (interactive()) {
  con <- stdin()
} else {
  con <- "stdin"
}

num_round <- 1
player_symbol <- read_symbol()
computer_sym <- other_player(player_symbol)
curr_symbol <- "X"

last_round <- F

while (TRUE) {
  cat("\n")
  if(!(last_round > 0)){ 
    display_round(num_round)
  }
  cat("\nCurrent board:\n")
  cat("~~~~~~~~~~~~~~\n")
  display_board(board)
  cat("~~~~~~~~~~~~~~~\n\n")
  if (last_round) 
    break
  num_round <- num_round + 1
  cat(paste0("Player '", curr_symbol, "' turn:\n"))
  if (curr_symbol == player_symbol) {
    row <- -1
    col <- -1
    while ((row == -1) & (col == -1)) {
      row <- read_row_column(T)
      col <- read_row_column(F)
      ans <- read_yes_no_move(curr_symbol, row, col)
      if (ans == "y") {
        if (!is_valid_position(board, row, col)) {
          msg <- paste0("Row:", row, "-column:", col, " not available!")
          msg <- paste0(msg, " Please select another position\n")
          cat(msg)
          row <- -1
          col <- -1
        } else {
          board <- update_board(board, row, col, curr_symbol)
          cat("\nMove placed!\n")
        }
      } else {
        cat("\nMove not placed!\n")
        row <- -1
        col <- -1
      }
    }
  } else {
    res <- get_random_position(board)
    board <- update_board(board, res$row, res$col, curr_symbol)
    cat("Computer move registered!\n")
  }
  if (check_win(board, curr_symbol)) {
    cat(paste0(curr_symbol, " wins!!\n\n"))
    last_round <- T
  } else if (check_draw(board)) {
    cat("It's a draw!")
    last_round <- T
  }
  curr_symbol <- other_player(curr_symbol)
}
