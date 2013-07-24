
> data Bool = True | False
> --data Maybe a = Nothing | Just a
> data List a = Cons a (List a) | Nil
> data Tree a = Branch (Tree a) a (Tree a) | Empty
> data Rose a = Node a [Rose a]
> --data Either a b = Left a | Right b
> --newtype Parser s u t a
> newtype St s a = S (s -> (a,s))

Kind:

Word64									
Maybe									* -> *
Maybe String							*
Tree									* -> *
Either Int32 [Char]						*
Either (Maybe Int)						* -> *
Either									* -> * -> *
Parser									
Parser String [(String, String)]		
St (Map k v) Int32						

Typen:

Just 'c'										Char -> Maybe Char
Nothing											Maybe a		
Left "Hooray"									Left String
Node 7 []										Num a => Rose a
Node 7											Num a => a -> [Rose a] -> Rose a
Node											a -> [Rose a] -> Rose a
Cons True										List Bool -> List Bool
\x -> Just x									a -> Maybe a
()												()
S(\s->(s, s))									St a a 
S												(a -> (b, a)) -> St a b
(\a b -> (+a))									Num a => a -> b -> a -> a
(\[a] -> a) [1]									Num a => a
return 7										(Monad a, Num b) => a b
\x -> if x == 0 then Nothing else return x		Num a => a -> Maybe a
maybe											a -> (b -> a) -> Maybe b -> a
either											(a -> b) -> (c -> b) -> Either a c -> b