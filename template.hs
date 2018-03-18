import Data.List
import Data.Ord

--
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- Add your student number
--

--
-- Types
--
-- Define Film type here

data Film = Film{
    title :: String,
    director :: String,
    release :: Int,
    usersLiked :: [String],
    usersDisliked :: [String]
} deriving (Show)

--A function that returns the testDatabase. It's not really storing it.
testDatabase :: [Film]
--testDatabase = [Film "A film" "A director" 1048 [] [], Film "Second" "Another director" 2000 [] []]
testDatabase = [Film "Blade Runner" "Ridley Scott" 1982 ["Zoe", "Heidi", "Another", "Someone else"] ["Sam"], Film "The Fly" "David Cronenberg" 1986 ["First"] [], Film "Body of Lies" "Ridley Scott" 2008 ["Something"] ["Third", "Zoe"]]

--Add a film to the database by taking a list of films and the film to add.
addFilm :: [Film] -> Film -> [Film]
addFilm fArray f = f:fArray

--List all the films in the database
listFilms = testDatabase

--Get all the films by a certain director
filmsByDirector :: [Film] -> String -> [Film]
filmsByDirector [] _ = []
filmsByDirector (x:xs) d
    | director x == d = x:filmsByDirector xs d
    | otherwise = filmsByDirector xs d

--Get the total number of ratings a film has
totalRatings :: Film -> Int
totalRatings f = (length $ usersLiked f) + (length $ usersDisliked f)

--Get the website rating for a film.
--This is the percentage of users liked rounded.
getWebsiteRating :: Film -> Int
getWebsiteRating f = round ((fromIntegral (length $ usersLiked f)) / (fromIntegral (totalRatings f)) * 100)
--getWebsiteRating f = 10 / 5

--Get a list of films with a rating of over 75%
getFilmsWith75 :: [Film] -> [Film]
getFilmsWith75 [] = []
getFilmsWith75 (x:xs)
  | getWebsiteRating x >= 75 = x:getFilmsWith75 xs
  | otherwise = getFilmsWith75 xs

--Get the sumative website ratings for a list of films.
getSumativeRatings :: [Film] -> Int
getSumativeRatings [] = 0
getSumativeRatings (x:xs) = (getWebsiteRating x) + getSumativeRatings xs

--Find the average website rating for a director.
averageWebsiteRatingByList :: [Film] -> Int
averageWebsiteRatingByList x = div (getSumativeRatings x) (length x)

--Get the average website rating for a particular director.
averageForDirector :: String -> [Film] -> Int
averageForDirector director films = averageWebsiteRatingByList (filmsByDirector films director)

--Get a list of the films that that user has either liked or disliked.
filmsByUser :: String -> [Film] -> [Film]
filmsByUser _ [] = []
filmsByUser u (x:xs)
  | elem u (usersLiked x) || elem u (usersDisliked x) = x:filmsByUser u xs
  | otherwise = filmsByUser u xs


--By the looks of things these don't return the entire list, just the one film.
--Add a user to a single film's like list.
addUserToFilmLikes :: String -> Film -> Film
addUserToFilmLikes user f = f { usersLiked = user:(usersLiked f) }

--Add the user to the liked list for that film.
likeFilm :: String -> String -> [Film] -> [Film]
likeFilm _ _ [] = []
likeFilm user filmName (x:xs)
  | title x == filmName = (addUserToFilmLikes user x):likeFilm user filmName xs
  | otherwise = likeFilm user filmName xs


addUserToFilmDislikes :: String -> Film -> Film
addUserToFilmDislikes user f = f { usersDisliked = user:(usersDisliked f) }

--Add the user to the liked list for that film.
--User, Filmname, film list
dislikeFilm :: String -> String -> [Film] -> [Film]
dislikeFilm _ _ [] = []
dislikeFilm user filmName (x:xs)
  | title x == filmName = (addUserToFilmDislikes user x):dislikeFilm user filmName xs
  | otherwise = dislikeFilm user filmName xs

--Sort films in decending order.

--Get a list of the films released between two given years.
filmsReleasedBetween :: Int -> Int -> [Film] -> [Film]
filmsReleasedBetween _ _ [] = []
filmsReleasedBetween start end (x:xs)
  | release x >= start && release x <= end = x:(filmsReleasedBetween start end xs)
  | otherwise = filmsReleasedBetween start end xs

--Wrap the films list into a tuple containing the website rating.
--This tuple form can be used to sort the list according to its rating.
wrapFilms :: [Film] -> [(Film, Int)]
wrapFilms [] = []
wrapFilms (x:xs) = (x, getWebsiteRating x):wrapFilms xs

--Remove the tuple from the list
unwrapFilms :: [(Film, Int)] -> [Film]
unwrapFilms [] = []
unwrapFilms (x:xs) = fst x:unwrapFilms xs

--Sort the wrapped films by their second value (the website rating)
sortByRating :: [(Film, Int)] -> [(Film, Int)]
sortByRating = sortBy (comparing snd)

--Wrap the films, sort them, then unwrap them.
sortFinal :: [Film] -> [Film]
sortFinal films = unwrapFilms (sortByRating (wrapFilms films))

--Get the films between two years ordered in ascending order by website rating.
filmsBetweenOrdered :: Int -> Int -> [Film] -> [Film]
filmsBetweenOrdered first second films = reverse (sortFinal (filmsReleasedBetween first second films))


formatFilm :: Film -> String
formatFilm f =
  "Title:  " ++ title f ++ "\n\
  \Director:  " ++ director f ++ "\n\
  \Release: " ++ show (release f) ++ "\n\
  \Website Rating:  " ++ show (getWebsiteRating f) ++ "\n\
  \"

formatFilmList :: [Film] -> String
formatFilmList [] = ""
formatFilmList (x:xs) = formatFilm x ++ formatFilmList xs

mainMenu = "-------------------------------------------------------------\n\
            \1. Add a new film to the database.\n\
            \2. Print all films in the database.\n\
            \3. Print all the films by a given director.\n\
            \4. Print films with a website rating of 75% or higher.\n\
            \5. Print the average rating for a given director.\n\
            \6. Print the films that a particular user has rated.\n\
            \7. Like or dislike a film.\n\
            \8. Print films between two years sorted by website rating.\n\
            \9. Exit\n\
            \-------------------------------------------------------------"

processInput :: String -> String
processInput s
  | s == "1" = "first"
  | otherwise = "Something else"

--input

main = do
    putStrLn "Please enter your name:"
    name <- getLine
    putStrLn ("Hey " ++ name ++ "")
    loop

loop = do
  putStrLn mainMenu
  input <- getLine
  if input == "1"
    then first
    else do
  if input == "2"
    then second
    else do
  if input == "3"
    then third
    else do
  if input == "4"
    then fourth
    else do
  if input == "5"
    then fifth
    else do
    putStrLn "Nothing"
  loop

test = do
  putStrLn $ formatFilmList testDatabase

first = do
    putStrLn "first"

second = do
    putStrLn "second"

third = do
    putStrLn "Please enter the director name:"
    director <- getLine
    putStrLn $ formatFilmList $ filmsByDirector testDatabase director
    loop

fourth = do
    putStrLn $ formatFilmList $ getFilmsWith75 testDatabase
    loop

fifth = do
    putStrLn "Please enter the director name:"
    director <- getLine
    putStrLn $ show $ averageForDirector director testDatabase
--Films as string function which returns a film formatted in a readable form.

-- Demo function to test basic functionality (without persistence - i.e.
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

--Adding a film
--addFilm testDatabase $ Film "" "" 1990 [] []

--Are the demos just if you don't have the user interfaces working?
--What exactly should be stored in the testDatabase and what should be stored in the files.

--demo :: Int -> IO ()
--demo 1  = putStrLn all films after adding 2018 film "Sherlock Gnomes"
--          directed by by "John Stevenson" to testDatabase
--demo 2  = putStrLn (filmsAsString testDatabase)
--demo 3  = putStrLn all films by "Ridley Scott"
--demo 4  = putStrLn all films with website rating >= 75%
--demo 5  = putStrLn average website rating for "Ridley Scott"
--demo 6  = putStrLn titles of films rated by "Emma" (with likes/dislikes)
--demo 7  = putStrLn all films after "Emma" says she likes "Avatar"
--demo 71 = putStrLn all films after "Emma" says she likes "Titanic"
--demo 72 = putStrLn all films after "Emma" says she dislikes "Jaws"
--demo 8  = films between 2000 and 2006 inclusive sorted by website rating

--
--
-- Your user interface code goes here
--
--
