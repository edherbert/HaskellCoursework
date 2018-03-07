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
testDatabase = [Film "Blade Runner" "Ridley Scott" 1982 ["Zoe", "Heidi", "Another", "Someone else"] ["Sam"], Film "The Fly" "David Cronenberg" 1986 ["First"] [], Film "Body of Lies" "Ridley Scott" 2008 ["Something"] ["Third", "Zoe"] ]

--Add a film to the database by taking a list of films and the film to add.
addFilm :: [Film] -> Film -> [Film]
addFilm fArray f = f:fArray

--List all the films in the database\
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

getWebsiteRating :: Film -> Int
getWebsiteRating f = round ((fromIntegral (length $ usersLiked f)) / (fromIntegral (totalRatings f)) * 100)
--getWebsiteRating f = 10 / 5

getFilmsWith75 :: [Film] -> [Film]
getFilmsWith75 [] = []
getFilmsWith75 (x:xs)
  | getWebsiteRating x >= 75 = x:getFilmsWith75 xs
  | otherwise = getFilmsWith75 xs

getSumativeRatings :: [Film] -> Int
getSumativeRatings [] = 0
getSumativeRatings (x:xs) = (getWebsiteRating x) + getSumativeRatings xs

averageforDirector :: [Film] -> Int
averageforDirector x = div (getSumativeRatings x) (length x)

filmsByUser :: String -> [Film] -> [Film]
filmsByUser _ [] = []
filmsByUser u (x:xs)
  | elem u (usersLiked x) || elem u (usersDisliked x) = x:filmsByUser u xs
  | otherwise = filmsByUser u xs

addUserToFilmLikes :: String -> Film -> Film
addUserToFilmLikes user f = f { usersLiked = user:(usersLiked f) }

likeFilm :: String -> String -> [Film] -> [Film]
likeFilm _ _ [] = []
likeFilm user filmName (x:xs)
  | title x == filmName = (addUserToFilmLikes user x):likeFilm user filmName xs
  | otherwise = likeFilm user filmName xs

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
