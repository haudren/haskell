data Stack a = Nil | Stack { top :: a, stack :: Stack a}
	deriving (Show) 

pop :: Stack a -> (a, Stack a)
pop x = (top x, stack x)

push :: a -> Stack a -> Stack a
push x y = Stack x y


