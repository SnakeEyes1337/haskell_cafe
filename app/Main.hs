{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import System.IO (withFile, IOMode(ReadMode))
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List (nub, intercalate)
import Control.Monad (unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding (decodeUtf8)
import Data.Aeson (encode)
import Text.Parsec


data Category = Appetizer | MainCourse | Dessert | KidsMeal | NonAlcoholic | Alcoholic 
    deriving (Show, Eq, Enum, Read)

data Dish = Dish {
    name :: String,
    category :: Category,
    ingredients :: [String],
    price :: Double,
    volume :: Double,
    calories :: Double
} deriving (Show)

type Menu = [Dish]

instance FromJSON Category where
    parseJSON (String "Appetizer") = return Appetizer
    parseJSON (String "MainCourse") = return MainCourse
    parseJSON (String "Dessert") = return Dessert
    parseJSON (String "KidsMeal") = return KidsMeal
    parseJSON (String "NonAlcoholic") = return NonAlcoholic
    parseJSON (String "Alcoholic") = return Alcoholic
    parseJSON _ = fail "Invalid category"

instance FromJSON Dish where
    parseJSON (Object v) =
        Dish <$> v .: "name"
             <*> v .: "category"
             <*> v .: "ingredients"
             <*> v .: "price"
             <*> v .: "volume"
             <*> v .: "calories"
    parseJSON _ = fail "Invalid Dish"


readMenuFromFile :: FilePath -> IO Menu
readMenuFromFile filePath = do
    jsonData <- BL.readFile filePath
    case eitherDecode jsonData of
        Left err -> error $ "Error parsing JSON: " ++ err
        Right menu -> return menu

-- Функция перевода категории на русский
categoryToRussian :: Category -> String
categoryToRussian Appetizer    = "Закуска"
categoryToRussian MainCourse   = "Основное блюдо"
categoryToRussian Dessert      = "Десерт"
categoryToRussian KidsMeal     = "Детское меню"
categoryToRussian NonAlcoholic = "Безалкогольный напиток"
categoryToRussian Alcoholic    = "Алкогольный напиток"

-- Функция для красивого вывода информации о блюде в одну строку
prettyPrintDish :: Dish -> String
prettyPrintDish (Dish name category ingredients price volume calories) =
  "Блюдо: " ++ name ++
  ", Категория: " ++ categoryToRussian category ++
  ", Цена: " ++ show price ++ " руб." ++
  ", Объем: " ++ show volume ++ " г/мл" ++
  ", Калорийность: " ++ show calories ++ " ккал" ++
  ", Ингредиенты: " ++ unwords ingredients 


-- Просмотр всего меню
showMenu :: Menu -> IO ()
showMenu menu = mapM_ (\(a0, d) -> putStrLn $prettyPrintDish d) $ zip [1..] menu

-- Фильтрация по категории
filterByCategory :: Category -> Menu -> Menu
filterByCategory cat = filter (\dish -> category dish == cat)

-- Фильтрация по ингредиенту
filterByIngredient :: String -> Menu -> Menu
filterByIngredient ing = filter (\dish -> ing `elem` ingredients dish)

-- Функция для извлечения всех ингредиентов из списка блюд
listAllIngredients :: [Dish] -> String
listAllIngredients dishes = intercalate ", " . nub $ concatMap ingredients dishes

-- Составление списка для банкета
createBanquetList :: Menu -> IO ()
createBanquetList menu = do
    putStrLn "\nСоставление списка для банкета:"
    banquetList <- go [] menu
    putStrLn "\nИтоговый список блюд для банкета:"
    showMenu banquetList
    putStrLn $ "Общая стоимость: " ++ show (sum $ map price banquetList)
  where
    go acc menu = do
        putStrLn "\nТекущий список блюд:"
        showMenu acc
        putStrLn "\nДоступные команды:"
        putStrLn "1. Добавить блюдо"
        putStrLn "2. Фильтровать по категории"
        putStrLn "3. Фильтровать по ингредиенту"
        putStrLn "4. Сбросить фильтрацию"
        putStrLn "5. Завершить"
        choice <- getLine
        case choice of
            "1" -> do
                putStrLn "\nВведите номер блюда для добавления:"
                mapM_ (\(i, d) -> putStrLn $ show i ++ ". " ++ prettyPrintDish d) $ zip [1..] menu
                num <- readLn
                if num > 0 && num <= length menu
                    then go (acc ++ [menu !! (num - 1)]) menu
                    else do
                        putStrLn "Неверный номер блюда"
                        go acc menu
            "2" -> do
                putStrLn "\nВыберите категорию:"
                putStrLn "0 - Закуска, 1 - Основное блюдо, 2 - Десерт"
                putStrLn "3 - Детское меню, 4 - Безалкогольный напиток, 5 - Алкогольный напиток"
                catNum <- readLn
                let cat = toEnum catNum :: Category
                go acc (filterByCategory cat menu)
            "3" -> do
                putStrLn "\nДоступные ингредиенты:"
                putStrLn $ listAllIngredients menu
                putStrLn "\nВведите ингредиент:"
                ing <- getLine
                putStrLn "\n"
                go acc (filterByIngredient ing menu)
            "4" -> do
                fullMenu <- readMenuFromFile "menu.json"
                showMenu fullMenu
                go acc fullMenu
            "5" -> return acc
            _ -> do
                putStrLn "Неверная команда"
                go acc menu

main :: IO ()
main = do
    putStrLn "Начало программы"
    menu <- readMenuFromFile "menu.json"
    putStrLn $ "Загружено блюд: " ++ show (length menu)
    if null menu
        then putStrLn "Внимание: Меню пустое!"
        else putStrLn "Меню кафе загружено успешно."
    mainLoop menu

mainLoop :: Menu -> IO ()
mainLoop menu = do
    putStrLn "\nВыберите действие:"
    putStrLn "1. Показать все меню"
    putStrLn "2. Фильтровать по категории"
    putStrLn "3. Фильтровать по ингредиенту"
    putStrLn "4. Составить список для банкета"
    putStrLn "5. Выйти"
    choice <- getLine
    case choice of
        "1" -> showMenu menu
        "2" -> do
            putStrLn "\nВыберите категорию:"
            putStrLn "0 - Закуска, 1 - Основное блюдо, 2 - Десерт"
            putStrLn "3 - Детское меню, 4 - Безалкогольный напиток, 5 - Алкогольный напиток"
            catNum <- readLn
            let cat = toEnum catNum :: Category
            showMenu $ filterByCategory cat menu
        "3" -> do
            putStrLn "Доступные ингредиенты:"
            putStrLn $ listAllIngredients menu
            putStrLn "\nВведите ингредиент:"
            ing <- getLine
            putStrLn "\n"
            showMenu $ filterByIngredient ing menu
        "4" -> createBanquetList menu
        "5" -> putStrLn "До свидания!"
        _ -> putStrLn "Неверный выбор"
    unless (choice == "5") $ mainLoop menu