library(plotly)

delayForProgress <- function(progress, easinessFactor) {
    ceiling(easinessFactor ^ (progress - 1))
}

# Given currentProgress, returns updated progress
study <- function(currentProgress, correctRate) {
    correct <- rbinom(length(currentProgress), size = 1, prob=correctRate)
    mapply(function(correct, currentProgress) {
        if (correct != 0) {
            currentProgress + 1
        } else {
            1
        }
    }, correct, currentProgress)
}

srsSimulation <- function(newTermsPerDay=10, termCount=1000, schedule=rep(1, 60), correctRate=0.9, easinessFactor=2.5) {
    currentReviews <- sapply(schedule, function(x) vector("numeric"))
    newTermsStudied <-rep(0, length(schedule))
    reviewTermsStudied <- rep(0, length(schedule))
    newTermsRemaining <- termCount
    
    for (today in seq_along(schedule)) {
        tomorrow <- today + 1
        if (schedule[today] == 0) {
            if (today < length(schedule)) {
                currentReviews[[tomorrow]] <- c(currentReviews[[tomorrow]], currentReviews[[today]])
            }
            next
        }
        newTermsToday = newTermsPerDay
         
        currentReviews[[today]] <- c(currentReviews[[today]], rep(0, newTermsToday))
        updatedProgress <- study(currentReviews[[today]], correctRate=correctRate)
        delay <- delayForProgress(updatedProgress, easinessFactor)
        
        mapply(function(updatedProgress, delay) {
            nextDay <- today + delay
            if (nextDay <= length(currentReviews)) {
                currentReviews[[nextDay]] <<- c(currentReviews[[nextDay]], updatedProgress)
            }
        }, updatedProgress, delay)
        
        newTermsStudied[today] <- newTermsToday
        reviewTermsStudied[today] <- length(currentReviews[[today]]) - newTermsToday
    }

    data.frame(newTermsStudied=newTermsStudied,
                reviewTermsStudied=reviewTermsStudied)
}

chartResults <- function(results) {
    days <- seq_along(results$newTermsStudied)
    newTerms <- plot_ly(
        x=days,
        y=results$newTermsStudied,
        name="New",
        type="bar"
    )
    chart <- add_trace(
        newTerms,
        x = days,
        y = results$reviewTermsStudied,
        name = "Review",
        type="bar"
    )
    layout(chart,
          barmode = "stack",
          xaxis=list(title="Days"),
          yaxis=list(title="Terms Studied"))
}