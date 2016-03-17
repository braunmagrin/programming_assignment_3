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

    num <- convert_num(num, nrow(df))

    df <- df[order(df[,column_idx]),]
    # df <- na.omit(df[,c(name_column, state_column, column_idx)])

    res <- aggregate(df[,c(name_column,column_idx)],
                     by=list(df[,state_column]), FUN=function(x) {x[num]})
#     aggregate(df[,c(1,3)],
#               by=list(df[,2]), FUN=function(x) {x[num]})
    res <- res[,2:1]
    names(res) <- c('hospital', 'state')
    res
}