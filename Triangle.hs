import Data.Char (toLower)
import System.Environment (getArgs)
import Text.Read (readMaybe)

data Direction =
    Up
    | Down
    | L
    | R

repeatN :: Char -> Int -> String
repeatN char size = take size $ repeat char

upTriangle :: Int -> [String]
upTriangle middle =
    let
        genSide:: Int -> String
        genSide s = (repeatN ' ' $ middle - s) ++ (repeatN '#' $ s)
    in
        [(genSide i) ++ "#" ++ (reverse $ genSide i) | i <- [0..middle]]

triangle :: Int -> Direction -> String
triangle size dir =
    let
        middle = ceiling $ ((fromIntegral size):: Float) / 2
        result :: (Int -> String) -> String
        result genLine =
            unlines $
                [genLine i | i <- ([1..middle] ++ [(middle-1), (middle-2)..1])]
    in case dir of
    R ->
        result (\s -> repeatN '#' s)
    L ->
        let
            genLine:: Int -> String
            genLine s = (repeatN ' ' $ middle - s) ++ (repeatN '#' s)
        in result genLine
    Up -> unlines $ upTriangle middle
    Down -> unlines $ reverse $ upTriangle middle

parseDirection:: String -> Maybe Direction
parseDirection dir = case (map toLower dir) of
    "up" -> Just Up
    "down" -> Just Down
    "left" -> Just L
    "right" -> Just R
    _ -> Nothing


main = do
    args <- getArgs
    case args of
        [dirS, sizeS] ->
            let sizeM = readMaybe sizeS :: Maybe Int
                dirM = parseDirection dirS
            in
                case (dirM, sizeM) of
                    (Just dir, Just size) ->
                        if size < 3 || even size
                        then
                            putStrLn "Size must be an odd number greater than 2"
                        else
                           putStr $ triangle size dir
                    (Nothing, _) ->
                        putStrLn $ dirS ++ " is not a direction"
                    (_, Nothing) ->
                        putStrLn $ sizeS ++ "must be an integer"

        _ -> putStrLn "Usage triangles up|down|left|right size"
