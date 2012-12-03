import Text.XML.Light 
import System.Environment

data IContent = IContent {
                  content :: Content,
                  nodeID      :: Integer
                } deriving Show 

-- elContent is of type [Content] but we have it as [IContent]
-- solution: implement our own tree type
assignIDContent (Elem el) id = (IContent { content = Elem (el {elContent = children}), nodeID = id }, nextID)
                               where (children, nextID) = assignIDList (elContent el) (id+1)
                                                        
assignIDContent c id = (IContent { content = c, nodeID = id }, id + 1)

assignIDList :: [IContent] -> Integer -> ([IContent],Integer)
assignIDList [] id = ([], id)
assignIDList (x:xs) id = (iContent:(fst (assignIDList xs nextID)), nextID)
                         where (iContent, nextID) = assignIDContent x id

assignID t = assignIDList t 0

main = do
  f <- getArgs
  xml1 <- readFile (f !! 0)
  --xml2 <- readFile (f !! 1)

  print (parseXML xml1)

