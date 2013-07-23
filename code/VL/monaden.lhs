Wir betrachen das gestrige 'eval':

> eval0 :: Expr -> Float
> eval0 (Const x) = x
> eval0 (Add e1 e2) = eval0 e1 + eval0 e2
> eval0 (Div e1 e2) = eval0 e1 / eval0 e2

Wir werden die 'eval0' Funktion weiter entwickeln. Da wir schreibfaul sind
abstrahieren wir das pattern matching mit Hilfe von 'evalExpr'. Diese
Funktion akzeptiert drei Parameter @fC@, @fA@, und @fD@, wobei @fC@ eine
Funktion ist die eine Konstante ermittelt (vom Typ @fC :: Float -> Float@)),
@fA@ eine Funktion die eine Addition ausführt (vom Typ @fA :: Float -> Float -> Float@),
und eine Funktion @fD@, die die Division ausführt (vom Typ @fD :: Float -> Float -> Float)).

> evalExpr fC fA fD (Const x) = fC x
> evalExpr fC fA fD (Add e1 e2) fA (evalExpr fC fA fD e1) (evalExpr fC fA fD e2)
> evalExpr fC fA fD (Div e1 e2) fD (evalExpr fC fA fD e1) (evalExpr fC fA fD e2)

Eine rudimentäre Fehlerbehandlung lässt sich nun wie folgt formulieren:

> eval1 = evalExpr id (+) fD
>   where fD x y = if y == 0 then error "..." else (x/y)

In diesem Fall wir einfach das Programm abgebrochen mittels 'error',
wenn in der Division ein Divisor vom Wert @0@ angegeben wird. Das ist nicht
so schön.

Wir versuchen eine schönere Funktion zu formulieren, die einen zusätzlichen
Fehlerwert ausgeben kann (Nothing). Alle erfolgreichen Ergebnisse sind
in ein 'Just' verpackt (daher @Just :: Float -> Maybe Float@ statt @id@ zuvor):

> eval2 = evalExpr Just fA fD
>   where
>     fA e1 e2 = case e1 of
>       Nothing -> Nothing
>       Just x -> case e2 of
>           Nothing -> Nothing
>           Just t -> Just (x+y)
> 
>     fD e1 e2 = case e1 of
>       Nothing -> Nothing
>       Just x -> case e2 of
>           Nothing -> Nothing
>           Just y -> if id y == 0 then Nothing else Just (x / y)

Ziemlich lang :-(

Wenn man sich fA und fD genau anschaut sieht man, dass sie immer dem
selben Muster folgen:

< func x ... = case x of
<   Nothing -> Nothing
<   Just x -> ... so something with x ...

Dieses Muster wollen wir abstrahieren in eine Funktion 'op':

> op :: Maybe a -> (a -> Maybe b) -> Maybe b
> op val f = case val of
>   Nothing -> Nothing
>   Just x -> f x

Hier gibt es eine Funktion 'f' die einen Wert vom Typ @a@
in ein @Maybe b@ überführt (also entweder @Nothing@ oder
@Just b@). 'op' schaut sich das Argument 'val' an und wendet
dann entweder @f@ auf den in @Just@ verpackten Wert an, oder
gibt wieder @Nothing@ zurück.

Damit könnne wir die obigen Funktionen @fA@ und @fD@
entsprechend vereinfachen:

> eval3 = eval Expr Just fA fD
>   where
>     fA e1 e2 = e1 `op` (\x ->
>                   e2 `op` (\y -> Just (x+y)))
>     fD e1 e2 = e1 `op` (\x ->
>                   e2 `op` (\y -> if x == 0 then Nothing else Just (x/y)))

Das ist schon etwas angenehmer als vorher (Spoiler: Und 'fA' und 'fD'
sehen wieder /ziemlich/ ähnlich aus).

Unserer Taschenrechner-Auswertungsfunktion wurde jetzt mit einer
Fehlerbehandlung ausgestattet, die nicht bloß das Programm abbricht
sondern eine adequate Fehlerbehandlung durchführt (fehlerhafte Werte
werden durch 'Nothing' symbolisiert).


Zustandsveränderung
-------------------

Taschenrechner können oftmals eine Reihe von vergangenen Werten speichern,
oder sich Werte für Variablen @A@, @B@, @Ans@ merken. Wir wollen zunächst nur
das letzte Ergebnis speichern:

> type State = Float

(type deklariert ein Type-Alias. Wir könnten auch schreiben
 @type State = (Float, Float)@, das könnten wir hier in der
 Definition austauschen).

Eine Funktion die einen Wert berechnen /und/ einen Wert verändert
könnte nun so aussehen:

> State -> (a, State)

(diese Funktion bekommt einen Zustand und gibt einen neuen
 Zustand neben einem Rückgabewert @a@ zurück).

Wir würden solche zustandsverändernden Funktionen gerne
als konkrete Objekte behandeln (bitte nicht durcheinander kommen
mit den Objekten aus der OOP):

> data St a = S (State -> (a, State))

(das ist mehr oder weniger equvialent zu einer Deklaration mit
 @type@, allerdings benötigen wir den Parameter @a@, daher nutzen
 wir @data@. Später werden wir sehen, dass man in Haskell für
 konkret diesen Fall @newtype@ verwenden wird, dazu später mehr).

Mit Hilfe einer Funktion 'apply' kann nun ein State Transformer
(eine in 'S' verpackte Funktion) ausgewertet werden:

> apply :: St a         -- Der State Transformer / die Funktion
>       -> State        -- Der Startzustand
>       -> (a, State)   -- Das Ergebnis und der neue Zustand
> apply (S f) s = f s


> eval4 :: Expr -> St Float
> eval4 = evalExpr fC fA fD
>   where
>     fC x = S (\s -> (x, s)) -- Eine Konstante wird nun einfach
>                             -- in ein 'S' verpackt, das den Zustand 's'
>                             -- nicht verändert.
>     fA sx sy = S (\s -> let (x, s1) = apply sx s
>                             (y, s2) = apply sy s1
>                         in (x+y, update (x+y) s2))
>     fD sx sy = S (\s -> let (x, s1) = apply sx s
>                             (y, s2) = apply sy s1
>                         in (x/y, update (x/y) s2))

Die Verwendung von 'apply' lässt sich genauso wie zuvor 'op' verwendet
wurde durch eine Funktion 'ap' verkürzen / abstrahieren:

> ap :: St a -> (a -> St b) -> St b
> ap st f = S (\s -> let (x, s1) = apply st s
>                    in  apply (f x) s1)
>
> eval5 = evalExpr fC fA fD
>   where
>     fC x = ret x -- 'ret' ist die Funktion 'fC' aus 'eval4'
>     fA sx sy = sx `ap` (\x ->
>                   sy `ap` (\y -> S (\s -> (x+y, update abs (x+y)) )))
>     fD sx sy = sx `ap` (x ->
>                   sy `ap` (\y -> S (\s -> (x/y, update abs (x/y)) ))

(Hinweis: Vergleiche eval3 mit eval5 -- welche Gemeinsamkeiten gibt es?)

Um sinnvoll mit Zuständen umgehen zu können wäre es chic Zustände auslesen
zu können und zu manipulieren:

> get :: St State
> -- ^ Gibt den Zustand zurück und lässt ihn unverändert
> get = S (\s -> (s, s))

> put :: State -> St ()
> -- ^ Setzt den neuen Zustand, gibt nichts (also (), das leere Tupel, zurück)
> put newState = S (\s -> ((), newState) -- der aktuelle State s wird verworfen!


Wir sehen, dass es immer wieder das gleiche Muster gibt (vgl eval3 und eval5).
Dieses Muster ist tatsächlich so häufig in der funktionalen Programmierung,
dass man es in eine einheitliche Schnittstelle verpackt hat. Diese wird
@Monad@ genannt:

> class Monad m where
>   return :: a -> m a               -- vgl. mit fC für eval3 und eval5
>   (>>=)  :: m -> (a -> m b) -> m b -- vgl. mit der Signatur von 'ap' und 'op;

Die Implementierungen für @St@ und @Maybe@ sehen wie folgt aus:

> instance Monad St where
>   return = ret
>   (>>=)  = ap

> instance Monad Maybe where
>   return = Just
>   (>>=)  = op

(NB: Spätestens jetzt sollte klar sein, warum die die zustandsverändernden
 Funktionen in ihren eigenen Datentyp verpacken - damit wir eine Instanz
 der Typklasse Monad für sie definieren können).

Hiermit lassen sich eval3 und eval5 vereinfachen:

> evalM :: Monad m => Expr -> m Float
> evalM = evalExpr fC fA fD
>   where fC = return
>         fA m1 m2 = m1 >>= (\x -> m2 >>= (\y -> return (x + y))
>         fD m1 m2 = m1 >>= (\x -> m2 >>= (\y -> ... {- kommt auf die Monade an -})




