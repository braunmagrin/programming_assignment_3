best <- function (state, outcome) {
   df <- read.csv('data/outcome-of-care-measures.csv', colClasses = 'character')

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

   na.exclude(df[column == min(column, na.rm=TRUE), name_column])[1]
}

validate_arguments <- function(args) {
    if (!args$state %in% args$valid_states) {
        stop('invalid state')
    }

    if (!args$outcome %in% args$valid_outcomes) {
        stop('invalid outcome')
    }
}