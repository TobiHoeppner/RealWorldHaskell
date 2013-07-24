import Graphics.Gloss

main :: IO ()
main = animate
    (InWindow "Titel" (400, 300) (100, 100))
    white
    (\t -> Pictures [ Line [(0, 100), (0, -100)] , Translate (10*t) 0 (Circle 80) ] )

main' :: IO ()
main' = display
    (FullScreen (1280, 800))
    black
    (
        Pictures [
            Translate (-200) 0 redCircle ,
            Translate 200 0 yellowCircle ,
            whiteCircle,
            Translate 100 100 $ Color blue $ Circle 300
          ]
        
    )

redCircle = Color red $ Circle 100

yellowCircle = Color yellow $ Circle 100

whiteCircle = Color white $ ThickCircle 100 200






