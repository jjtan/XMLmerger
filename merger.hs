import Text.XML.Light 
import System.Environment

data XContent = XElem { element :: XElement, nodeID :: Integer }
              | XText { cdata   :: CData,    nodeID :: Integer }
              | XCRef { ref     :: String,   nodeID :: Integer } deriving Show 

data XElement = XElement {
                  xName    :: QName,
                  xAttribs :: [Attr],
                  xContent :: [XContent],
                  xLine    :: Maybe Line
                } deriving Show

xElement el contents = XElement {xName = elName el,
                         xAttribs = elAttribs el,
                         xContent = contents,
                         xLine = elLine el
                       }

-- elContent is of type [Content] but we have it as [IContent]
-- solution: implement our own tree type
assignIDContent (Elem el) id = (XElem { element = xElement el children, nodeID = id }, nextID)
                               where (children, nextID) = assignIDList (elContent el) (id+1)
                                                        
assignIDContent (Text c) id = (XText { cdata = c, nodeID = id }, id + 1)
assignIDContent (CRef r) id = (XCRef { ref = r, nodeID = id }, id + 1)

assignIDList :: [Content] -> Integer -> ([XContent],Integer)
assignIDList [] id = ([], id)
assignIDList (x:xs) id = (xContent:(fst (assignIDList xs nextID)), nextID)
                         where (xContent, nextID) = assignIDContent x id

assignID t = fst (assignIDList t 0)

main = do
  f <- getArgs
  xml1 <- readFile (f !! 0)
  --xml2 <- readFile (f !! 1)
  print (parseXML xml1)
  print (assignID (parseXML xml1))


