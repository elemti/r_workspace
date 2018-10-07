# max numbers of players, sessions with fewer players will be filled with NA
NUM_PLAYERS = 10

library(jsonlite)

# parse data.csv to R's data frame
data = read.csv('data.csv')

# get the session.code column (represented as R's vector)
sessionCodeColumn = data$'session.code'

# get only unique codes
sessionCodes = unique(sessionCodeColumn)

getLoanAppsInRound = function(roundData) {
    jsonList = roundData$player.decisions
    for (json in jsonList) {
        decisions = tryCatch(
            fromJSON(toString(json)),
            error=function(err) return(NULL)
        )
        if (length(decisions)) return(names(decisions))
    }
    return(NULL)
}

# create empty var for the final table
masterFrame = NULL

# loop through each session
for (seshCode in sessionCodes) {
    # data[data$'session.code' == seshCode,] is filter syntax
    # -> to get a new data frame where the session.code column satisfies the condition
    session = data[data$'session.code' == seshCode,]

    # get number of rounds in this session table
    roundIds = unique(session$'subsession.round_number')
    numRounds = length(roundIds)

    # create a vector (array) of player ids (from 1 -> NUM_PLAYERS)
    playerIds = seq(1, NUM_PLAYERS)

    # initiate var
    seshFormattedFrame = NULL

    # loop through rounds in current session
    for (roundId in roundIds) {
        # same as above: filter to get round data table from the parent session table
        roundData = session[session$'subsession.round_number' == roundId,]

        # create a vector/array of loan ids (to later use as a column in the table)
        loanCol = getLoanAppsInRound(roundData)

        # create a vector/array of round ids (to later use as a column in the table)
        roundCol = rep(roundId, length(loanCol))

        # create a vector/array of session codes/ids (to later use as a column in the table)
        seshCodeCol = rep(seshCode, length(loanCol))

        # initiate a table/dataFrame for this round with 3 columns
        roundFormattedFrame = data.frame('round' = roundCol, 'session_code' = seshCodeCol, 'loan_id' = loanCol)

        # loop each player
        for (playerId in playerIds) {
            # create a column for each player with format p1_dec, p2_dec, ...
            playerColName = paste('p', playerId, '_dec', sep="")

            # filter from roundData table to get playerData table (using playerId)
            playerData = roundData[roundData$'participant.id_in_session' == playerId,]

            # try to parse player decision in this round from JSON
            # will create R's Named List (kinda like a Dictionary with key-value pairs)
            playerDecisions = tryCatch(
                # since playerData$'player.decisions' is a vector/array with length == 1
                # so we have to select the first cell playerData$'player.decisions'[1]
                # use toString() to wrap around the JSON string because god knows it works
                fromJSON(toString(playerData$'player.decisions'[1])),
                # if the JSON string is empty -> an error is thrown
                # -> in will jump into the error handler function `error=function(err) return(NULL)`
                # which returns NULL -> playerDecisions = NULL
                # -> the player did not make any decision that round
                error=function(err) return(NULL)
            )

            # initiate player column var
            playerCol = NULL

            # loop through each loan app
            for (loanId in loanCol) {
                # get player decision for this loanId (Approve/Reject) from the Named List
                # if playerDecisions is NULL from above then decision will equal to NULL
                decision = playerDecisions[[loanId]]

                # check NULL for decision
                if (is.null(decision)) decision = NA

                # append decision to playerCol
                # if playerCol is NULL -> create a new vector/array with decision as the first child
                playerCol = c(playerCol, decision)
            }

            # create a new column with `playerColName` as the name and `playerCol` vector as data
            # -> append to roundFormattedFrame table
            # round  session_code  loan_id         round  session_code  loan_id  p1_dec
            #   x          x          x      --->    x         x           x       x
            #   x          x          x              x         x           x       x
            roundFormattedFrame[[playerColName]] = playerCol
        }

        # append round table to session table
        # if seshFormattedFrame is NULL -> considered empty table
        # -> create new table with roundFormattedFrame's data
        seshFormattedFrame = rbind(seshFormattedFrame, roundFormattedFrame)
    }

    # same as above
    masterFrame = rbind(masterFrame, seshFormattedFrame)
}

write.csv(masterFrame, 'done.csv')
