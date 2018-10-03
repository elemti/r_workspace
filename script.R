# max numbers of players, sessions with fewer players will be filled with NA
NUM_PLAYERS = 10

library(jsonlite)

data = read.csv('data.csv')

sessionCodes = unique(data$'session.code')

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

masterFrame = NULL

for (seshCode in sessionCodes) {
    session = data[data$'session.code' == seshCode,]
    roundIds = unique(session$'subsession.round_number')
    numRounds = length(roundIds)
    playerIds = seq(1, NUM_PLAYERS)

    seshFormattedFrame = NULL

    for (roundId in roundIds) {
        roundData = session[session$'subsession.round_number' == roundId,]

        # roundCol = rep(roundId, NUM_LOANS)
        # loanCol = sprintf("fin_app%d", seq(1:NUM_LOANS))
        loanCol = getLoanAppsInRound(roundData)
        roundCol = rep(roundId, length(loanCol))
        roundFormattedFrame = data.frame('round' = roundCol, 'loan_id' = loanCol)

        for (playerId in playerIds) {
            playerColName = paste('p', playerId, '_dec', sep="")
            playerData = roundData[roundData$'participant.id_in_session' == playerId,]
            playerDecisions = tryCatch(
                fromJSON(toString(playerData$'player.decisions'[1])),
                error=function(err) return(NULL)
            )

            playerCol = NULL
            for (loanId in loanCol) {
                decision = playerDecisions[[loanId]]
                if (is.null(decision)) decision = NA
                playerCol = c(playerCol, decision)
            }

            roundFormattedFrame[[playerColName]] = playerCol
        }

        seshFormattedFrame = rbind(seshFormattedFrame, roundFormattedFrame)
    }

    masterFrame = rbind(masterFrame, seshFormattedFrame)
}

write.csv(masterFrame, 'done.csv')
