import Test.HUnit
 
main = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            main  
 
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words


testlist = TestList ["Szymon Success" ~: reverseWords "Szymon" ~?= "nomyzS"
    , "Ala ma kota Failure" ~: reverseWords "Ala ma kota" ~?="otak am alA"
    , "Empty" ~: reverseWords "" ~?= ""
                    ]
main :: IO ()
tests = do
  runTestTT testlist
  return ()
