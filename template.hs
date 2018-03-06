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
testDatabase = [Film "Blade Runner" "Ridley Scott" 1982 ["Zoe", "Heidi"] ["Sam"], Film "The Fly" "David Cronenberg" 1986 [] [], Film "Body of Lies" "Ridley Scott" 2008 ["Something"] ["Third"] ]

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

totalRatings :: Film -> Int
totalRatings f = (length $ usersLiked f) + (length $ usersDisliked f)

getWebsiteRating :: Film -> Float
getWebsiteRating f = (fromIntegral (length $ usersLiked f)) / (fromIntegral (totalRatings f))
--getWebsiteRating f = 10 / 5

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
