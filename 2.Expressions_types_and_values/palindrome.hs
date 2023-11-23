import Data.Char

palindrome :: IO()
palindrome
    = do {putStrLn "Enter a string:";
          xs <- getLine;
          if isPalindrome xs then putStrLn "Yes"
          else putStrLn "No"}

isPalindrome xs = (ys == reverse ys)
    where ys = map toLower (filter isAlpha xs)

main :: IO()
main = palindrome