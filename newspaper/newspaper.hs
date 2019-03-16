{-# LANGUAGE OverloadedStrings #-}
import Data.Text
import Data.Int (Int64)
import Data.ByteString (ByteString)
import Control.Applicative
import Control.Monad.IO.Class (liftIO)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow


data Author = Author { idAuthor :: Integer  
                     , firstNameAuthor :: String  
                     , lastNameAuthor :: String  
                     , phoneNumber :: String  
                     , rank :: Integer  
                     } deriving (Show)

data Reader = Reader { idReader :: Integer
                     , firstNameReader :: String  
                     , lastNameReader :: String  
                     , reviewNumber :: Integer 
                     } deriving (Show)

data Article = Article { idArticle :: Integer
                     , name :: String  
                     , author :: Integer  
                     , annotation :: Integer 
                     } deriving (Show)

data Review = Review { idReview :: Integer
                     , review :: String  
                     , article :: Integer  
                     , reader :: Integer 
                     , score :: Integer
                     } deriving (Show)

data File = File { idFile :: Integer
                     , filename :: String  
                     , filepath :: String
                     } deriving (Show)

data FileToArticle = FileToArticle { idFileToArticle :: Integer
			, idFileConn :: Integer
			, idArticleConn :: Integer
		        } deriving (Show)

data Annotation= Annotation {idAnnotation :: Integer
                             ,annotationText :: String
                             ,article_id :: Integer} deriving (Show)


instance FromRow Annotation where
  fromRow = Annotation <$> field <*> field <*> field

instance ToRow Annotation where
  toRow r = [toField (annotationText r), toField (article_id r)]

instance FromRow File where
  fromRow = File <$> field <*> field <*> field

instance ToRow File where
  toRow r = [toField (filename r), toField (filepath r)]

instance FromRow FileToArticle where
  fromRow = FileToArticle <$> field <*> field <*> field

instance ToRow FileToArticle where
  toRow r = [toField (idFileConn r), toField (idArticleConn r)]

instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field <*> field <*> field

instance ToRow Author where
  toRow r = [toField (firstNameAuthor r), toField (lastNameAuthor r), toField (phoneNumber r), toField (rank r)]

instance FromRow Reader where
  fromRow = Reader <$> field <*> field <*> field <*> field

instance ToRow Reader where
  toRow r = [toField (firstNameReader r), toField (lastNameReader r), toField (reviewNumber r)]

instance FromRow Article where
  fromRow = Article <$> field <*> field <*> field <*> field

instance ToRow Article where
  toRow r = [toField (name r), toField (author r), toField (annotation r)]

instance FromRow Review where
  fromRow = Review <$> field <*> field <*> field <*> field <*> field

instance ToRow Review where
  toRow r = [toField (review r), toField (article r), toField (reader r), toField (score r)]

uri :: ByteString
uri = "postgres://newspaper_user:newspaper@localhost:5432/newspaper"

allFilesToArticles :: Connection -> IO [FileToArticle]
allFilesToArticles c = query_ c "SELECT id, file, article FROM files_to_articles" 

addFileToArticle :: Connection -> Int -> Int -> IO Int64
addFileToArticle c idF idA = execute c "INSERT INTO files_to_articles (file, article) VALUES (?, ?)" $ (idF, idA)

updateFileToArticle :: Connection -> Integer -> Integer -> Integer -> IO Int64
updateFileToArticle c idF idA id = execute c "UPDATE files_to_articles SET file = ?, article = ? WHERE id = ?" (idF, idA, id)

deleteFileToArticle :: Connection -> Integer -> IO Int64
deleteFileToArticle c id = execute c "DELETE FROM files_to_articles WHERE id = ?" $ Only $ (id)

allAnnotations :: Connection -> IO [Annotation]
allAnnotations c = query_ c "SELECT id, text, article_id FROM annotations" 

addAnnotation :: Connection -> Annotation -> IO Int64
addAnnotation c annotation = execute c "INSERT INTO annotations (text, article_id) VALUES (?, ?)" annotation

updateAnnotation :: Connection -> String -> Integer -> Integer -> IO Int64
updateAnnotation c txt id art_id = execute c "UPDATE annotations SET text = ?, article_id = ? WHERE id = ?" (txt, id, art_id)

deleteAnnotation :: Connection -> Integer -> IO Int64
deleteAnnotation c id = execute c "DELETE FROM annotations WHERE id = ?" $ Only $ (id)

allFile :: Connection -> IO [File]
allFile c = query_ c "SELECT id, filename, filepath FROM files"

addFile :: Connection -> File -> IO Int64
addFile c file = execute c "INSERT INTO files (filename, filepath) VALUES (?, ?)" file

updateFile :: Connection -> String -> String -> Integer -> IO Int64
updateFile c flname flpath id = execute c "UPDATE files SET filename = ?, filepath = ? WHERE id = ?" (flname, flpath, id)

deleteFile :: Connection -> Integer -> IO Int64
deleteFile c id = execute c "DELETE FROM files WHERE id = ?" $ Only $ (id)

allAuthors :: Connection -> IO [Author]
allAuthors c = query_ c "SELECT id, firstname, lastname, phonenumber, rank FROM authors"

addAuthor :: Connection -> Author -> IO Int64
addAuthor c author = execute c "INSERT INTO authors (firstname, lastname, phonenumber, rank) VALUES (?, ?, ?, ?)" author

updateAuthor :: Connection -> String -> String -> String -> Integer -> Integer -> IO Int64
updateAuthor c fname lname ph rank id = execute c "UPDATE authors SET firstname = ?, lastname = ?, phonenumber = ?, rank = ? WHERE id = ?" (fname, lname, ph, rank, id)

deleteAuthor :: Connection -> Integer -> IO Int64
deleteAuthor c id = execute c "DELETE FROM authors WHERE id = ?" $ Only $ (id)

allReaders :: Connection -> IO [Reader]
allReaders c = query_ c "SELECT id, firstname, lastname, reviewnumber FROM readers"

addReader :: Connection -> Reader -> IO Int64
addReader c reader = execute c "INSERT INTO readers (firstname, lastname, reviewnumber) VALUES (?, ?, ?)" reader

updateReader :: Connection -> String ->  String -> Integer -> Integer -> IO Int64
updateReader c fsname lsname rv id = execute c "UPDATE readers SET firstname = ?, lastname = ?, reviewnumber = ? WHERE id = ?" (fsname, lsname, rv, id)

deleteReader :: Connection -> Integer -> IO Int64
deleteReader c id = execute c "DELETE FROM readers WHERE id = ?" $ Only $ (id)

allArticles :: Connection -> IO [Article]
allArticles c = query_ c "SELECT id, name, author, annotation FROM articles"

addArticle :: Connection -> Article -> IO Int64
addArticle c article = execute c "INSERT INTO articles (name, author, annotation) VALUES (?, ?, ?)" article

updateArticle :: Connection -> String -> Integer -> Integer -> Integer -> IO Int64
updateArticle c name author_id ann id = execute c "UPDATE articles SET name = ?, author = ?, annotation = ? WHERE id = ?" (name, author_id, ann, id)

deleteArticle :: Connection -> Integer -> IO Int64
deleteArticle c id = execute c "DELETE FROM articles WHERE id = ?" $ Only $ (id)

allReviews :: Connection -> IO [Review]
allReviews c = query_ c "SELECT id, review, article, reader, score FROM reviews"

addReviews :: Connection -> Review -> IO Int64
addReviews c review = execute c "INSERT INTO reviews (review, article, reader, score) VALUES (?, ?, ?, ?)" review

updateReviews :: Connection -> String ->  Integer -> Integer -> Integer -> Integer -> IO Int64
updateReviews c rv art rea sc id = execute c "UPDATE reviews SET review = ?, article = ?, reader = ?, score = ? WHERE id = ?" (rv, art, rea, sc, id)

deleteReviews :: Connection -> Integer -> IO Int64
deleteReviews c id = execute c "DELETE FROM reviews WHERE id = ?" $ Only $ (id)

connectFileToArticle :: Connection -> FileToArticle -> IO Int64
connectFileToArticle c fileToArticle = execute c "INSERT INTO files_to_articles (file, article) VALUES (?, ?)" fileToArticle



main :: IO ()
main = do
    conn <- connectPostgreSQL uri
    putStrLn "all todo"
    putStrLn "What to do?\n 1. Show\n 2. Add\n 3. Delete\n 4. Update\n"
    action <- getLine
    if action == "1"
        then do putStrLn "Which table to show?\n 1. Articles\n 2. Authors\n 3. Readers\n 4. Reviews\n 5. Files\n 6. Files to articles\n 7. Annotations\n"
                choise <- getLine
                case choise of
                    "1" -> mapM_ (putStrLn . show) =<< allArticles conn
                    "2" -> mapM_ (putStrLn . show) =<< allAuthors conn
                    "3" -> mapM_ (putStrLn . show) =<< allReaders conn
                    "4" -> mapM_ (putStrLn . show) =<< allReviews conn
                    "5" -> mapM_ (putStrLn . show) =<< allFile conn
                    "6" -> mapM_ (putStrLn . show) =<< allFilesToArticles conn
                    "7" -> mapM_ (putStrLn . show) =<< allAnnotations conn
                    otherwise -> putStrLn "otherwise"
        else if action == "2"
            then do putStrLn "To which table to add?\n 1. Articles\n 2. Authors\n 3. Readers\n 4. Reviews\n 5. Files\n 6. FileToArticle\n 7. Annotations\n"
                    choisea <- getLine
                    if choisea == "1"
                        then do
                            putStrLn "Article name:"
                            article_name <- getLine
                            putStrLn "Article author id:"
                            article_author_id_line <- getLine
                            let article_author_id = (read article_author_id_line :: Integer)
                            putStrLn "Article annotation id:"
                            article_annotation_str <- getLine
                            let article_annotation = (read article_annotation_str :: Integer)
                            putStrLn . show =<< addArticle conn Article{idArticle=(0), name=article_name, annotation=article_annotation, author=article_author_id}
                        else if choisea == "2"
                            then do
                                putStrLn "Author first name:"
                                author_first_name <- getLine
                                putStrLn "Author last name:"
                                author_last_name <- getLine
                                putStrLn "Author phone:"
                                author_phone <- getLine
                                putStrLn "Author rank:"
                                author_rank_str <- getLine
                                let author_rank = (read author_rank_str :: Integer)
                                putStrLn . show =<< addAuthor conn Author{idAuthor=(0), firstNameAuthor=author_first_name, lastNameAuthor=author_last_name, 
                                phoneNumber=author_phone, rank=author_rank}
                            else if choisea == "3"
                                then do
                                    putStrLn "Reader first name:"
                                    reader_first_name <- getLine
                                    putStrLn "Reader last name:"
                                    reader_last_name <- getLine
                                    putStrLn "Reader number of reviews:"
                                    reader_review_number <- getLine
                                    let reader_reviews = (read reader_review_number :: Integer)
                                    putStrLn . show =<< addReader conn Reader{idReader=(0), firstNameReader=reader_first_name, lastNameReader=reader_last_name, 
                                    reviewNumber=reader_reviews}
                                else if choisea == "4"
                                    then do
                                        putStrLn "Review:"
                                        review_text <- getLine
                                        putStrLn "Article id:"
                                        review_article_id_str <- getLine
                                        let review_article_id = (read review_article_id_str :: Integer)
                                        putStrLn "Reader id:"
                                        review_reader_id_str <- getLine
                                        let review_reader_id = (read review_reader_id_str :: Integer)
                                        putStrLn "Score:"
                                        review_score_str <- getLine
                                        let review_score = (read review_score_str :: Integer)
                                        putStrLn . show =<< addReviews conn Review{idReview=(0), review=review_text, article=review_article_id, 
                                        reader=review_reader_id, score=review_score}
                                    else if choisea == "5"
                                        then do
                                            putStrLn "Filename:"
                                            file_filename <- getLine
                                            putStrLn "Filepath:"
                                            file_filepath <- getLine
                                            putStrLn . show =<< addFile conn File{idFile=(0), filename=file_filename, filepath=file_filepath}
                                        else if choisea == "6"
                                            then do
                                                putStrLn "File id:"
                                                file_id_str <- getLine
                                                let file_id = (read file_id_str)
                                                putStrLn "Article id:"
                                                article_id_str <- getLine
                                                let article_id = (read article_id_str)
                                                putStrLn . show =<< addFileToArticle conn file_id article_id
                                            else if choisea == "7"
                                                then do
                                                    putStrLn "Annotation text:"
                                                    ann_text <- getLine
                                                    putStrLn "Article id:"
                                                    article_id_str_ann <- getLine
                                                    let article_id_ann = (read article_id_str_ann)
                                                    putStrLn . show =<< addAnnotation conn Annotation{idAnnotation=(0), annotationText=ann_text, article_id=article_id_ann}
                                                else putStrLn "else"
            else if action == "4"
                then do putStrLn "To which table to update?\n 1. Articles\n 2. Authors\n 3. Readers\n 4. Reviews\n 5. Files\n 6. FileToArticle\n 7. Annotations\n"
                        choiseb <- getLine
                        if choiseb == "1"
                            then do
                                putStrLn "Article id to update:"
                                article_id_str <- getLine
                                let article_id = (read article_id_str :: Integer)
                                putStrLn "Article name:"
                                article_name <- getLine
                                putStrLn "Article author id:"
                                article_author_id_line <- getLine
                                let article_author_id = (read article_author_id_line :: Integer)
                                putStrLn "Article annotation:"
                                article_annotation_str <- getLine
                                let article_annotation = (read article_annotation_str :: Integer)
                                putStrLn . show =<< updateArticle conn article_name article_author_id article_annotation article_id
                            else if choiseb == "2"
                                then do
                                    putStrLn "Author id to update:"
                                    author_id_str <- getLine
                                    let author_id = (read author_id_str :: Integer)
                                    putStrLn "Author first name:"
                                    author_first_name <- getLine
                                    putStrLn "Author last name:"
                                    author_last_name <- getLine
                                    putStrLn "Author phone:"
                                    author_phone <- getLine
                                    putStrLn "Author rank:"
                                    author_rank_str <- getLine
                                    let author_rank = (read author_rank_str :: Integer)
                                    putStrLn . show =<< updateAuthor conn author_first_name author_last_name author_phone author_rank author_id
                                else if choiseb == "3"
                                    then do
                                        putStrLn "Reader id to update:"
                                        reader_id_str <- getLine
                                        let reader_id = (read reader_id_str :: Integer)
                                        putStrLn "Reader first name:"
                                        reader_first_name <- getLine
                                        putStrLn "Reader last name:"
                                        reader_last_name <- getLine
                                        putStrLn "Reader number of reviews:"
                                        reader_review_number <- getLine
                                        let reader_reviews = (read reader_review_number :: Integer)
                                        putStrLn . show =<< updateReader conn reader_first_name reader_last_name reader_reviews reader_id
                                    else if choiseb == "4"
                                        then do
                                            putStrLn "Review id to update:"
                                            review_id_str <- getLine
                                            let review_id = (read review_id_str :: Integer)
                                            putStrLn "Review:"
                                            review_text <- getLine
                                            putStrLn "Article id:"
                                            review_article_id_str <- getLine
                                            let review_article_id = (read review_article_id_str :: Integer)
                                            putStrLn "Reader id:"
                                            review_reader_id_str <- getLine
                                            let review_reader_id = (read review_reader_id_str :: Integer)
                                            putStrLn "Score:"
                                            review_score_str <- getLine
                                            let review_score = (read review_score_str :: Integer)
                                            putStrLn . show =<< updateReviews conn review_text review_article_id review_reader_id review_score review_id
                                        else if choiseb == "5"
                                            then do
                                                putStrLn "Review id to update:"
                                                file_id_str <- getLine
                                                let file_id = (read file_id_str :: Integer)
                                                putStrLn "Filename:"
                                                file_filename <- getLine
                                                putStrLn "Filepath:"
                                                file_filepath <- getLine
                                                putStrLn . show =<< updateFile conn file_filename file_filepath file_id 
                                            else if choiseb == "6"
                                                then do
                                                    putStrLn "FileToArticle id to update:"
                                                    file_to_article_id_str <- getLine
                                                    let file_to_article_id = (read file_to_article_id_str :: Integer)
                                                    putStrLn "File id:"
                                                    file_id_str <- getLine
                                                    let file_id = (read file_id_str :: Integer)
                                                    putStrLn "Article id:"
                                                    article_id_str <- getLine
                                                    let article_id = (read article_id_str :: Integer)
                                                    putStrLn . show =<< updateFileToArticle conn file_id article_id file_to_article_id
                                                else if choiseb == "7"
                                                    then do
                                                        putStrLn "Annotation id to update:"
                                                        ann_id_str <- getLine
                                                        let ann_id = (read ann_id_str :: Integer)
                                                        putStrLn "Annotation text:"
                                                        ann_text <- getLine
                                                        putStrLn "Article id:"
                                                        article_id_str_ann <- getLine
                                                        let article_id_ann = (read article_id_str_ann)
                                                        putStrLn . show =<< updateAnnotation conn ann_text ann_id article_id_ann
                                                    else putStrLn "else"
                else if action == "3"
                    then do 
                        putStrLn "In which table to delete?\n 1. Articles\n 2. Authors\n 3. Readers\n 4. Reviews\n 5. Files\n 6. FileToArticle\n 7. Annotations\n"
                        choisec <- getLine
                        if choisec == "6"
                            then do
                                putStrLn "FileToArticle id to delete:"
                                file_to_article_id_str <- getLine
                                let file_to_article_id = (read file_to_article_id_str :: Integer)
                                putStrLn . show =<< deleteFileToArticle conn file_to_article_id
                            else if choisec == "5"
                                then do
                                    putStrLn "File id to delete:"
                                    file_id_str <- getLine
                                    let file_id = (read file_id_str :: Integer)
                                    putStrLn . show =<< deleteFile conn file_id
                                else if choisec == "4"
                                    then do
                                        putStrLn "Review id to delete:"
                                        review_id_str <- getLine
                                        let review_id = (read review_id_str :: Integer)
                                        putStrLn . show =<< deleteReviews conn review_id
                                    else if choisec == "3"
                                        then do
                                            putStrLn "Reader id to delete:"
                                            reader_id_str <- getLine
                                            let reader_id = (read reader_id_str :: Integer)
                                            putStrLn . show =<< deleteReader conn reader_id
                                        else if choisec == "2"
                                            then do
                                                putStrLn "Author id to delete:"
                                                author_id_str <- getLine
                                                let author_id = (read author_id_str :: Integer)
                                                putStrLn . show =<< deleteAuthor conn author_id
                                            else if choisec == "1"
                                                then do
                                                    putStrLn "Article id to delete:"
                                                    article_id_str <- getLine
                                                    let article_id = (read article_id_str :: Integer)
                                                    putStrLn . show =<< deleteArticle conn article_id
                                                else if choisec == "7"
                                                    then do
                                                        putStrLn "Annotation id to delete:"
                                                        ann_id_str <- getLine
                                                        let ann_id = (read ann_id_str :: Integer)
                                                        putStrLn . show =<< deleteAnnotation conn ann_id
                                                    else putStrLn "invalid choise"
                    else putStrLn "else"
          



    --mapM_ (putStrLn . show) =<< | choise == "1" = allArticles c
    --             | choise == "2" = allAuthors c
    --mapM_ (putStrLn . show) =<< allFile conn
    --putStrLn . show =<< addFile conn File{idFile=(-1), filename="Fully Connected Perceptrons 3", filepath="/home/newspaper/schemes/fully_con_3.jpg"}
--www.ptmu.test
