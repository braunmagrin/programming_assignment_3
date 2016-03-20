source('utils.R')

rankall <- function (outcome, num='best') {
    df <- load_outcome_data()

    valid_outcomes = c('heart attack', 'heart failure', 'pneumonia')

    name_column <- 2
    state_column <- 7
    columns <- c(11, 17, 23)
    names(columns) <- valid_outcomes
    column_idx = columns[outcome]

    df[, column_idx] <- as.numeric(df[, column_idx])
    df <- df[order(df[,column_idx]),]

    states <- factor(df[,state_column])
    df_by_state <- split(df, states)

    col_names <- c('hospital', 'state')

    res <- sapply(df_by_state,
                  FUN=function(x) {get_nth_element(x=x, n=num,
                                                   c(name_column,state_column),
                                                   col_names)})

    data.frame(t(res))
}

get_nth_element <- function(x, n, columns, col_names) {
    n <- convert_num(n, nrow(x))
    res <- na.omit(x[,columns])[n,]
    names(res) <- col_names
    res
}