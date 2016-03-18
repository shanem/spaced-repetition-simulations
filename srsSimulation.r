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
    
    for (day in seq_along(schedule)) {
        if (schedule[day] == 0) {
            next
        }
        newTermsToday = newTermsPerDay
         
        currentReviews[[day]] <- c(currentReviews[[day]], rep(0, newTermsToday))
        updatedProgress <- study(currentReviews[[day]], correctRate=correctRate)
        delay <- delayForProgress(updatedProgress, easinessFactor)
        
        mapply(function(updatedProgress, delay) {
            nextDay <- day + delay
            if (nextDay <= length(currentReviews)) {
                currentReviews[[nextDay]] <<- c(currentReviews[[nextDay]], updatedProgress)
            }
        }, updatedProgress, delay)
        
        newTermsStudied[day] <- newTermsToday
        reviewTermsStudied[day] <- length(currentReviews[[day]]) - newTermsToday
    }

    data.frame(newTermsStudied=newTermsStudied,
                reviewTermsStudied=reviewTermsStudied)
}