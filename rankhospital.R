source('utils.R')

rankhospital <- function(state, outcome, num='best') {
    df <- load_outcome_data()

    valid_outcomes = c('heart attack', 'heart failure', 'pneumonia')

    validate_arguments(list(state=state, outcome=outcome,
                        valid_states=unique(df$State),
                        valid_outcomes=valid_outcomes))

    name_column <- 2
    state_column <- 7
    columns <- c(11, 17, 23)
    names(columns) <- valid_outcomes
    column_idx = columns[outcome]

    df <- df[df$State == state,]

    df[, column_idx] <- as.numeric(df[, column_idx])
    column = df[, column_idx]

    hospitals <- na.exclude(df[order(column), name_column])

    if (num == 'best') num <- 1
    if (num == 'worst') num <- length(hospitals)

    hospitals[num]
}