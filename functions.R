RMSPE <- function(real, prediction){
    
    a <- (real - prediction) / real
    sqrt(sum(a^2) / length(real))
    
}

RMSPE2 <- function(real, prediction){
    
    sum((real - prediction) / real)^2
    
}

ReStructure <- function(train, store, test){
    
    all.training.data <-
        train %>%
        mutate(day.week.1 = DayOfWeek == 1,
               day.week.2 = DayOfWeek == 2,
               day.week.3 = DayOfWeek == 3,
               day.week.4 = DayOfWeek == 4,
               day.week.5 = DayOfWeek == 5,
               day.week.6 = DayOfWeek == 6,
               day.week.7 = DayOfWeek == 7) %>%
        dplyr::select(-DayOfWeek) %>%
        mutate(Date = ymd(Date),
               week = week(Date),
               month = month(Date),
               year = year(Date)) %>%
        dplyr::select(-Date, -Customers) %>%
        mutate(state.holiday.0 = StateHoliday == "0",
               state.holiday.a = StateHoliday == "a",
               state.holiday.b = StateHoliday == "b",
               state.holiday.c = StateHoliday == "c") %>%
        dplyr::select(-StateHoliday) %>%
        left_join(store, by = c("Store")) %>%
        mutate(store.type.a = StoreType == "a",
               store.type.b = StoreType == "b",
               store.type.c = StoreType == "c",
               store.type.d = StoreType == "d") %>%
        dplyr::select(-StoreType) %>%
        mutate(assortment.a = Assortment == "a",
               assortment.b = Assortment == "b",
               assortment.c = Assortment == "c") %>%
        dplyr::select(-Assortment) %>%
        # If there is no competition distance, suppose it is far away
        mutate(CompetitionDistance = ifelse(is.na(CompetitionDistance), 
                                            max(CompetitionDistance, na.rm = T),
                                            CompetitionDistance)) %>%
        mutate(competition.open.months = (2015 - CompetitionOpenSinceYear)*12 +
                   7 - CompetitionOpenSinceMonth) %>%
        dplyr::select(-CompetitionOpenSinceYear, -CompetitionOpenSinceMonth) %>%
        mutate(promo2.weeks = (2015 - Promo2SinceYear)*52 + 
                   31 - Promo2SinceWeek ) %>%
        dplyr::select(-Promo2SinceWeek, -Promo2SinceYear, -PromoInterval) %>%
        # Try to help the nets avoid NA efect
        mutate(unknown.competition.open.months = is.na(competition.open.months),
               unknown.promo2.weeks = is.na(promo2.weeks)) %>%
        mutate(competition.open.months = ifelse(is.na(competition.open.months),
                                                0, competition.open.months),
               promo2.weeks = ifelse(is.na(promo2.weeks), 0, promo2.weeks))
    
    all.training.data
    
}

Evaluate <- function(chromosome, data, index.training, index.cv){
    
    cat("\nEvaluating chromosome [", chromosome[1:5], "...]")

    
    # Create sets
    training.set <- data[index.training, ]
    cv.set <- data[index.cv, ] %>% filter(Sales != 0)
    incomplete.training <- training.set %>% select(-Sales)
    incomplete.cv <- cv.set %>% select(-Sales)
    
    # Change data with importance
    for(col in 1:ncol(incomplete.training)){
        incomplete.training[, col] <- incomplete.training[, col] * chromosome[col]
        incomplete.cv[, col] <- incomplete.cv[, col] * chromosome[col]
    }
    
    # Create model
    model <- nn2(data = incomplete.training, query = incomplete.cv)
    
    # Make prediction 
    sales.prediction.1 <- training.set[model$nn.idx[, 1], "Sales"]
    sales.prediction.2 <- training.set[model$nn.idx[, 2], "Sales"]
    sales.prediction.3 <- training.set[model$nn.idx[, 3], "Sales"]
    sales.prediction <- matrix(c(sales.prediction.1, sales.prediction.2, 
                                 sales.prediction.3), ncol = 3, byrow = FALSE)
    
    # Mean/median of the values
    mean.or.median <- round(tail(chromosome, 1))
    if(mean.or.median == 1)
        function.to.apply <- mean
    else
        function.to.apply <- median
    
    number.to.see <- round(chromosome[length(chromosome) - 1])
    if(number.to.see > 1)
        sales.prediction <- rowMeans(x = sales.prediction[, 1:number.to.see])
    else
        sales.prediction <- sales.prediction.1
    
    cat("\nSales prediction = [", sales.prediction[1:5], "...]")
    
    # Calculate error
    rmspe <- RMSPE(cv.set$Sales, sales.prediction)
    rmspe2 <- RMSPE2(cv.set$Sales, sales.prediction)
    
    cat("\nError = ", rmspe, fill = T)
    rmspe2
    
}

Normalize <- function(data){
    
    for(col in 1:ncol(data)){
        
        c <- data[, col]
        data[, col] <- (c - min(c, na.rm = T)) / (max(c, na.rm = T) - min(c, na.rm = T))
        
    }
    
    data
    
}