import Parsing

-- Already defined in Parsing
-- newtype Parser a = P (String ->  [(a, String)])

-- instance Monad Parser where
--   return  :: a -> Parser a
--   return v = P (\inp -> [(v, inp)])


--   (>>=) :: Parser a -> (a -> Parser b) -> Parser b
--   p >>= f 
--     = P (\inp ->
--           case  parse p inp of
--             [(v, out)] -> parse (f v) out
--             [] -> [])

p :: Parser String
p = do char '['
       d <- digit
       ds <- many (do char ','
                      digit)
       char ']'
       return (d:ds)


