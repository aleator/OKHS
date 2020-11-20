
module OKHS.Make where
import OKHS.Types
import Data.Text (strip)
import qualified Data.Text as T
import Data.Makefile
import Data.Char

makeToOk :: Makefile -> OkSection' [OkCommand' Text]
makeToOk makefile = entries makefile |> convert "" |> \cmds -> OkSection "Makefile" cmds Nothing
    where
      convert :: Text -> [Entry] -> [OkCommand' Text]
      convert txt (e:es) = case e of
        Rule (Target target) _ _ -> okCommand ("make "<>target) txt  : convert "" es
        Assignment _ _ _ -> convert "" es
        OtherLine possibleComment -> let stripped = stripStuff possibleComment
                                     in convert (if T.null txt then stripped else txt<>"\n"<>stripped)  es
      convert _ [] = []
      stripStuff = T.dropWhile (\x -> x `elem` ['\n','#'] || isSpace x )
