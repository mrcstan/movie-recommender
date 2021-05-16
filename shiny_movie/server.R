## server.R
# Based on (1) https://github.com/pspachtholz/BookRecommender 
# and (2) modified Shiny code provided by the instructors https://piazza.com/class/kdf6l5f8bb78j?cid=880

library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)


get_user_ratings = function(value_list) {
    dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                      function(x) ifelse(length(x) > 1, x[[2]], NA)),
                     Rating = unlist(as.character(value_list)))
    dat = dat[!is.null(Rating) & !is.na(MovieID)]
    dat[Rating == " ", Rating := 0]
    dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
    dat = dat[Rating > 0]
    dat = as(dat, 'data.frame')
    dat['UserID'] = 1
    dat = dat[, c('UserID', 'MovieID', 'Rating')]
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

# Read movie ratings from data file and convert into rating matrix
myurl = "https://liangfgithub.github.io/MovieData/"
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
ratings = ratings[, -4]
colnames(ratings) = c('UserID', 'MovieID', 'Rating')
# User IDs are increased by 1 so that the 
# new user ID can be set to 1
ratings$UserID = ratings$UserID+1
# DEBUG
#ratings = ratings[sample(1:nrow(ratings), 10000), ]

################### System 1 ###########################

genres = as.data.frame(movies$Genre, stringsAsFactors=FALSE)
tmp = as.data.frame(tstrsplit(genres[,1], '[|]',
                              type.convert=TRUE),
                    stringsAsFactors=FALSE)
genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")
m = length(genre_list)
genre_matrix = matrix(0, nrow(movies), length(genre_list))
for(i in 1:nrow(tmp)){
    genre_matrix[i,genre_list %in% tmp[i,]]=1
}
colnames(genre_matrix) = genre_list
remove("tmp", "genres")

movies = cbind(movies, genre_matrix)

# Get average rating of each movie
movie_ave_rating = aggregate(ratings$Rating, by=list(Category=ratings$MovieID), FUN=mean)
colnames(movie_ave_rating) = c('MovieID', 'Ave_rating')

# Add average rating to each movie and remove movies without rating
movies = merge(movies, movie_ave_rating, by='MovieID')

# Get the number of times each movie is rated as a measure of popularity
movie_rating_freq = data.frame(table(ratings$MovieID))
colnames(movie_rating_freq) = c('MovieID', 'Rating_freq')

# Add average rating to each movie and remove movies without rating
movies = merge(movies, movie_rating_freq, by='MovieID')

n_top = 5;  # n_top most highly rated/most popular movies for each genre
min_rating = 3; # minimum average rating required for a movie to be recommeded as a top popular movie
min_rating_freq = 100; # minimum number of times a movie is rated to be recommended as a top rated movie
top_rated_movies = data.frame(Genre=rep(genre_list, each=n_top), 
                              MovieID=NA, 
                              Title=NA, 
                              Ave_rating=NA,
                              Rating_freq=NA)

top_popular_movies = data.frame(Genre=rep(genre_list, each=n_top), 
                                MovieID=NA, 
                                Title=NA, 
                                Ave_rating=NA,
                                Rating_freq=NA)


# Get n_top most highly rated and most popular movie for each gnere and store them in a data frame
count_rows = 0
for (genre in genre_list) {
    genre_movies = movies[as.logical(movies[, genre]), c('MovieID', 'Title', 'Ave_rating', 'Rating_freq', 'image_url')]
    
    # Get n_top most highly rated movie for each genre with rating frequency >= min_rating_freq
    inds = order(genre_movies[, 'Ave_rating'], decreasing=TRUE)
    movies_by_rating = genre_movies[inds, ]
    movies_by_rating = movies_by_rating[movies_by_rating[ ,'Rating_freq'] >= min_rating_freq, ]
    movies_by_rating = movies_by_rating[1:n_top, ]
    
    # Get n_top most popular movie for each genre with average rating >= min_rating
    inds = order(genre_movies[, 'Rating_freq'], decreasing=TRUE)
    movies_by_popularity = genre_movies[inds, ]
    movies_by_popularity = movies_by_popularity[movies_by_rating[ ,'Ave_rating'] >= min_rating, ]
    movies_by_popularity = movies_by_popularity[1:n_top, ]
    
    start_row = count_rows+1
    end_row = count_rows+n_top
    top_rated_movies[start_row:end_row, 2:6] = movies_by_rating
    top_popular_movies[start_row:end_row, 2:6] = movies_by_popularity
    
    count_rows = end_row
}

shinyServer(function(input, output, session) {
    
    # display the genre-based recommendations
    output$recom_movies <- renderUI({
        paste("You chose", input$sel_genre)
        
        num_rows <- 2
        num_movies <- 5
 
        sel_movies = rbind(top_rated_movies[top_rated_movies[,'Genre'] == input$sel_genre, ],
                          top_popular_movies[top_popular_movies[,'Genre'] == input$sel_genre, ])
        
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                kk = (i - 1) * num_movies + j
                box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", j),
                    
                    div(style = "text-align:center", 
                        
                        a(img(src = sel_movies$image_url[kk], height = 150))
                    ),
                    div(style="text-align:center; font-size: 100%", 
                        strong(sel_movies$Title[kk])
                    )
                    
                )        
            }))) # columns
        }) # rows
        
    }) # renderUI function
    
    ### Collaborative based reocmmendations
    # show the books to be rated
    output$ratings <- renderUI({
        num_rows <- 40
        num_movies <- 6 # movies per row
        
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                list(box(width = 2,
                         div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                         #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                         div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                         div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
            })))
        })
    })
    
    # Calculate recommendations when the submit button is clicked
    df <- eventReactive(input$btn, {
        withBusyIndicatorServer("btn", { # showing the busy indicator
            # hide the rating container
            useShinyjs()
            jsCode <- "document.querySelector('[data-widget=collapse]').click();"
            runjs(jsCode)
            
            # get the user's rating data
            value_list = reactiveValuesToList(input)
            user_ratings = get_user_ratings(value_list)
            
            # add user's ratings as first column to rating matrix
            Rmat = as(rbind(user_ratings, ratings), 'realRatingMatrix')
            rownames(Rmat) = paste0('u', rownames(Rmat))
            colnames(Rmat) = paste0('m', colnames(Rmat))
            
            #r = Recommender(Rmat, method = "POPULAR", parameter = list(normalize = 'center'))
            r = Recommender(Rmat, method = "UBCF", parameter = list(normalize = 'Z-score',
                                                                    method = 'Cosine', nn = 25))
            

            recom = predict(r, Rmat[1], type='ratings')
            #  Order the ratings from highest to lowest and get 
            # the 10 highest rating
            recom_df = as(recom, 'data.frame')
  
            ordered_rows = order(recom_df$rating, decreasing=TRUE)[1:10]
            user_predicted_ids = recom_df[ordered_rows, 'item']
            user_predicted_ids = as.numeric(sub('.', '', user_predicted_ids))
            user_results = recom_df[ordered_rows, 'rating']
            
            movie_rows = match(user_predicted_ids, movies[, 'MovieID'])
            
            #print(user_predicted_ids)
            df <- data.table(Rank = 1:10, 
                            MovieID = movies$MovieID[movie_rows], 
                            Title = movies$Title[movie_rows], 
                            Image_url = movies$image_url[movie_rows],
                            Predicted_rating =  user_results)
            #print(df)
        }) # still busy
        
    }) # clicked on button
    
    
    # display the recommendations
    output$results <- renderUI({
        num_rows <- 2
        num_movies <- 5
        recom_result <- df()
        
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                kk = (i - 1) * num_movies + j
                box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
                    
                    div(style = "text-align:center", 
                        a(img(src = recom_result$Image_url[kk], height = 150))
                    ),
                    div(style="text-align:center; font-size: 100%", 
                        strong(recom_result$Title[kk])
                    )
                    
                )        
            }))) # columns
        }) # rows
        
    }) # renderUI function
    
}) # server function
