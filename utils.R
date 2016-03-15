load_outcome_data <- function() {
    read.csv('data/outcome-of-care-measures.csv', colClasses = 'character')
}

validate_arguments <- function(args) {
    if (!args$state %in% args$valid_states) {
        stop('invalid state')
    }
    
    if (!args$outcome %in% args$valid_outcomes) {
        stop('invalid outcome')
    }
}