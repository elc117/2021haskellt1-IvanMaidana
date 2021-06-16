import Text.Printf

type Point     = (Float,Float)
--type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)

-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

--paleta de cores 
rgbCirculo :: Int -> [(Int,Int,Int)]
rgbCirculo n = take n $ cycle [(250,0,0),(0,250,0),(0,0,250)]


genCircleInLine :: Int -> Int -> (Float,Float) -> [Circle] 
genCircleInLine n raio (x,y) = [((x-m,y+m), reduz (fromIntegral raio)/m) | m <- [1,2..fromIntegral n]]

reduz :: Float -> Float
reduz

--Gera string com atributos de estilo para uma dada cor
svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b


svgCircle :: Circle -> String -> String 
svgCircle ((x, y), r) style = 
  printf "<circle cx='%f' cy='%f' r='%f' style='%s' />\n" x y r style


svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles


main :: IO ()
main = do
  --putStrLn "Informe a posicao (x,y) do circulo:"
  --x <- getLine 
 -- y <- getLine
 -- putStrLn "Informe o número de circulos:"
  --numCirculos <- getLine
  writeFile "trabalho.svg" $ svgAll
  where svgAll = svgBegin w h ++ svgfigs ++ svgEnd
        svgfigs = svgElements svgCircle circle (map svgStyle cor)
        circle = genCircleInLine ncircle r (x,y)
        cor = rgbCirculo ncircle 
        ncircle = 50
        (w,h) = (1500,1500)
        (x,y,r) =(1060,1060,50)
        --r = 50
