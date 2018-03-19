import Data.List
import Data.Ord

--
-- MATHFUN
-- 801869
--

data Film = Film{
    title :: String,
    director :: String,
    release :: Int,
    usersLiked :: [String],
    usersDisliked :: [String]
} deriving (Show, Read)

--A function that returns the testDatabase. It's not really storing it.
testDatabase :: [Film]

--testDatabase = [Film "Blade Runner" "Ridley Scott" 1982 ["Zoe", "Heidi", "Another", "Someone else"] ["Sam", "Zoe"], Film "The Fly" "David Cronenberg" 1986 ["First"] [], Film "Body of Lies" "Ridley Scott" 2008 ["Something"] ["Third", "Zoe"]]
testDatabase = [Film "Blade Runner" "Ridley Scott" 1982 ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Dave"] ["Sam", "Olga", "Tim"], Film "The Fly" "David Cronenberg" 1986 ["Garry", "Dave", "Zoe"] ["Kevin", "Emma", "Heidi", "Jo", "Kate"], Film "Body Of Lies" "Ridley Scott" 2008 ["Garry", "Dave"] ["Bill", "Olga", "Tim", "Zoe", "Paula"], Film "Avatar" "James Cameron" 2009 ["Dave", "Amy", "Liz"] ["Olga", "Tim", "Zoe", "Paula"], Film "Titanic" "James Cameron" 1997 ["Zoe", "Emma", "Paula", "Liz", "Olga", "Dave"] ["Sam", "Wally", "Kate"], Film "The Departed" "Martin Scorsese" 2006 ["Wally", "Liz", "Kevin", "Tim", "Emma"] ["Olga", "Dave", "Kate", "Zoe"], Film "Aliens" "Ridley Scott" 1986 ["Dave", "Garry", "Liz", "Sam", "Wally", "Kate", "Zoe"] ["Tim", "Emma", "Jo", "Olga"], Film "Kingdom Of Heaven" "Ridley Scott" 2005 ["Jo", "Wally", "Emma"] ["Tim", "Garry", "Ian", "Neal"], Film "Alien: Covenant" "Ridley Scott" 2017 ["Kevin", "Tim"] ["Emma", "Jo", "Liz"], Film "E.T. The Extra-Terrestrial" "Steven Spielberg" 1982 ["Dave", "Amy", "Garry", "Ian", "Neal"] ["Jenny", "Kate", "Emma", "Olga"], Film "Bridge of Spies" "Steven Spielberg" 2015 ["Wally", "Sam", "Dave", "Neal"] ["Bill", "Garry", "Ian", "Kate"], Film "Jaws" "Steven Spielberg" 1975 ["Jenny", "Emma", "Bill", "Neal"] ["Sam", "Ian", "Kate"], Film "The Martian" "Ridley Scott" 2015 ["Wally", "Sam", "Dave", "Jo", "Jenny", "Kate", "Emma", "Olga"] ["Ian", "Neal", "Tim", "Liz"], Film "The BFG" "Steven Spielberg" 2016 ["Sam", "Wally", "Dave", "Jo", "Kate"] ["Neal"], Film "The Shawshank Redemption" "Frank Darabont" 1994 ["Dave", "Amy", "Bill", "Garry", "Ian", "Neal", "Kate", "Jenny", "Zoe", "Heidi"] ["Jo"], Film "Gladiator" "Ridley Scott" 2000 ["Olga", "Neal", "Kate", "Garry"] ["Heidi", "Bill", "Sam", "Zoe"], Film "The Green Mile" "Frank Darabont" 1999 ["Kevin", "Tim", "Emma", "Heidi"] ["Kate", "Jenny", "Zoe"], Film "True Lies" "James Cameron" 1994 ["Sam", "Dave"] ["Emma", "Olga", "Jenny", "Zoe"], Film "Super 8" "J J Abrams" 2011 ["Kevin", "Tim", "Emma", "Olga", "Heidi"] ["Emma", "Dave", "Jenny", "Zoe"], Film "Minority Report" "Steven Spielberg" 2002 ["Kevin", "Kate", "Tim", "Emma", "Jenny", "Zoe"] ["Olga", "Heidi"], Film "War Horse" "Steven Spielberg" 2011 ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma", "Tim", "Kate", "Zoe"] ["Heidi", "Jenny", "Sam"], Film "Silence" "Martin Scorsese" 2016 ["Wally", "Emma", "Tim", "Heidi", "Bill", "Jo"] ["Dave", "Olga"], Film "The Terminal" "Steven Spielberg" 2004 ["Kate", "Dave", "Jo", "Wally", "Emma"] ["Heidi"], Film "Star Wars: The Force Awakens" "J J Abrams" 2015 ["Emma", "Wally", "Zoe", "Kate", "Bill", "Dave", "Liz"] ["Olga", "Jo", "Wally"], Film "Hugo" "Martin Scorsese" 2011 ["Wally", "Sam"] ["Kate", "Bill", "Dave"]]

--Add a film to the database by taking a list of films and the film to add.
addFilm :: [Film] -> Film -> [Film]
addFilm fArray f = f:fArray

--Get a film by its title
--Returns nothing if there is film with that name
getFilmByTitle :: String -> [Film] -> Maybe Film
getFilmByTitle _ [] = Nothing
getFilmByTitle filmTitle (x:xs)
  | title x == filmTitle = Just x
  | otherwise = getFilmByTitle filmTitle xs

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
averageWebsiteRatingByList [] = 0
averageWebsiteRatingByList x = div (getSumativeRatings x) (length x)

--Get the average website rating for a particular director.
averageForDirector :: String -> [Film] -> Int
averageForDirector _ [] = 0
averageForDirector director films = averageWebsiteRatingByList (filmsByDirector films director)

--Get a list of the films that that user has either liked or disliked.
filmsByUser :: String -> [Film] -> [Film]
filmsByUser _ [] = []
filmsByUser u (x:xs)
  | elem u (usersLiked x) || elem u (usersDisliked x) = x:filmsByUser u xs
  | otherwise = filmsByUser u xs

--Get a string as to whether a user has liked or disliked a film
likeOrDislike :: String -> Film -> String
likeOrDislike user film
  | elem user $ usersLiked film = "Liked"
  | elem user $ usersDisliked film = "Disliked"
  | otherwise = "Not rated"

--Remove a string from a list
removeStringFromList :: String -> [String] -> [String]
removeStringFromList _ [] = []
removeStringFromList s (x:xs)
  | x == s = removeStringFromList s xs
  | otherwise = x:removeStringFromList s xs

--Remove both positive and negative reviews for a user on a specific film.
unrateFilm :: String -> Film -> Film
unrateFilm user f = f { usersLiked = (removeStringFromList user $ usersLiked f), usersDisliked = (removeStringFromList user $ usersDisliked f) }

--Unrate a film for a user by passing in the entire film database.
--This would be called when setting a new rating, as the old one would need to be removed first.
unrateFilmList :: String -> String -> [Film] -> [Film]
unrateFilmList _ _ [] = []
unrateFilmList user filmTitle (x:xs)
  | title x == filmTitle = (unrateFilm user x):unrateFilmList user filmTitle xs
  | otherwise = x:unrateFilmList user filmTitle xs

--By the looks of things these don't return the entire list, just the one film.
--Add a user to a single film's like list.
addUserToFilmLikes :: String -> Film -> Film
addUserToFilmLikes user f = f { usersLiked = user:(usersLiked f) }

--Add the user to the liked list for that film.
likeFilm :: String -> String -> [Film] -> [Film]
likeFilm _ _ [] = []
likeFilm user filmName (x:xs)
  | title x == filmName = (addUserToFilmLikes user x):likeFilm user filmName xs
  | otherwise = x:likeFilm user filmName xs

--Make a user dislike a single film
addUserToFilmDislikes :: String -> Film -> Film
addUserToFilmDislikes user f = f { usersDisliked = user:(usersDisliked f) }

--Add the user to the disliked list for that film.
--User, Filmname, film list
dislikeFilm :: String -> String -> [Film] -> [Film]
dislikeFilm _ _ [] = []
dislikeFilm user filmName (x:xs)
  | title x == filmName = (addUserToFilmDislikes user x):dislikeFilm user filmName xs
  | otherwise = x:dislikeFilm user filmName xs


--Sort films in decending order code.

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
  "--------------------------------------\n\
  \Title:  " ++ title f ++ "\n\
  \Director:  " ++ director f ++ "\n\
  \Release: " ++ show (release f) ++ "\n\
  \Website Rating:  " ++ show (getWebsiteRating f) ++ "\n\
  \"

formatFilmList :: [Film] -> String
formatFilmList [] = ""
formatFilmList (x:xs) = formatFilm x ++ "--------------------------------------\n\
  \" ++ formatFilmList xs

formatFilmListRating :: String -> [Film] -> String
formatFilmListRating _ [] = ""
formatFilmListRating user (x:xs) = formatFilm x ++ "Rating: " ++ likeOrDislike user x ++ "\n\
  \" ++ formatFilmListRating user xs

mainMenu = "-------------------------------------------------------------\n\
            \1. Add a new film to the database.\n\
            \2. Print all films in the database.\n\
            \3. Print all the films by a given director.\n\
            \4. Print films with a website rating of 75% or higher.\n\
            \5. Print the average rating for a given director.\n\
            \6. Print the films you've rated.\n\
            \7. Like or dislike a film.\n\
            \8. Print films between two years sorted by website rating.\n\
            \9. Exit\n\
            \-------------------------------------------------------------"

--input

stringToFilm :: String -> [Film]
stringToFilm s = read s :: [Film]

main = do
    file <- readFile "./test.txt"
    seq (length file) (return ())
    putStrLn $ formatFilmList $ stringToFilm file

    putStrLn "Please enter your name:"
    name <- getLine
    putStrLn ("Hey " ++ name ++ "")
    loop name $ stringToFilm file

loop :: String -> [Film] -> IO ()
loop name films = do
  putStrLn mainMenu
  input <- getLine
  if input == "1"
    then first name films
    else do
  if input == "2"
    then second name films
    else do
  if input == "3"
    then third name films
    else do
  if input == "4"
    then fourth name films
    else do
  if input == "5"
    then fifth name films
    else do
  if input == "6"
    then sixth name films
    else do
  if input == "7"
    then seventh name films
    else do
  if input == "8"
    then eigth name films
    else do
  if input == "9"
    then exit name films
    else do
    putStrLn "Please enter a number"
    loop name films

first :: String -> [Film] -> IO ()
first name films = do
    putStrLn "Adding new film"
    putStrLn "Film title:"
    title <- getLine
    putStrLn "director:"
    director <- getLine
    putStrLn "Release date:"
    releaseDate <- getLine
    putStrLn "Film added."
    loop name $ (Film title director (read releaseDate) [] []):films

second :: String -> [Film] -> IO ()
second name films = do
    putStrLn $ formatFilmList films
    loop name films

third :: String -> [Film] -> IO ()
third name films = do
    putStrLn "Please enter the director name:"
    director <- getLine
    putStrLn $ formatFilmList $ filmsByDirector films director
    loop name films

fourth :: String -> [Film] -> IO ()
fourth name films = do
    putStrLn $ formatFilmList $ getFilmsWith75 films
    loop name films

fifth :: String -> [Film] -> IO ()
fifth name films = do
    putStrLn "Please enter the director name:"
    director <- getLine
    putStrLn $ show $ averageForDirector director films
    loop name films

sixth :: String -> [Film] -> IO ()
sixth name films = do
  putStrLn $ formatFilmListRating name $ filmsByUser name films
  loop name films

seventh :: String -> [Film] -> IO ()
seventh name films = do
  putStrLn "Enter the title of a film:"
  title <- getLine
  putStrLn "Do you like or dislike this film? \n\ \1. Like \n\ \2. Dislike"
  choice <- getLine
  if choice == "1"
    then
    loop name $ likeFilm name title $ unrateFilmList name title films
    else do
  if choice == "2"
    then
    loop name $ dislikeFilm name title $ unrateFilmList name title films
    else do
    putStrLn "Please choose a number"
    loop name films

eigth :: String -> [Film] -> IO ()
eigth name films = do
  putStrLn "Please enter the start year:"
  first <- getLine
  putStrLn "Please enter the ending year:"
  second <- getLine
  putStrLn $ formatFilmList $ filmsBetweenOrdered (read first) (read second) films
  loop name films

exit :: String -> [Film] -> IO ()
exit name films = do
  putStrLn $ "Bye " ++ name ++ "."
  writeFile "./test.txt" $ show films

--demo :: Int -> IO ()
--demo 1  = putStrLn all films after adding 2018 film "Sherlock Gnomes"
--          directed by by "John Stevenson" to testDatabase
demo1 = do
  putStrLn $ formatFilmList $ addFilm testDatabase (Film "Sherlock Gnomes" "John Stevenson" 2018 [] [])

--demo 2  = putStrLn (filmsAsString testDatabase)
demo2 = do
  putStrLn $ formatFilmList testDatabase

--demo 3  = putStrLn all films by "Ridley Scott"
demo3 = do
  putStrLn $ formatFilmList $ filmsByDirector testDatabase "Ridley Scott"

--demo 4  = putStrLn all films with website rating >= 75%
demo4 = do
  putStrLn $ formatFilmList $ getFilmsWith75 testDatabase

--demo 5  = putStrLn average website rating for "Ridley Scott"
demo5 = do
  putStrLn $ formatFilmList $ filmsByDirector testDatabase "Ridley Scott"

--demo 6  = putStrLn titles of films rated by "Emma" (with likes/dislikes)
demo6 = do
  putStrLn $ formatFilmList $ filmsByUser "Emma" testDatabase

--demo 7  = putStrLn all films after "Emma" says she likes "Avatar"
demo7 = do
  putStrLn $ formatFilmList $ likeFilm "Emma" "Avatar" testDatabase

--demo 71 = putStrLn all films after "Emma" says she likes "Titanic"
demo71 = do
  putStrLn $ formatFilmList $ likeFilm "Emma" "Titanic" testDatabase

--demo 72 = putStrLn all films after "Emma" says she dislikes "Jaws"
demo72 = do
  putStrLn $ formatFilmList $ dislikeFilm "Emma" "Jaws" testDatabase

--demo 8  = films between 2000 and 2006 inclusive sorted by website rating
demo8 = do
  putStrLn $ formatFilmList $ filmsBetweenOrdered 2000 2006 testDatabase
