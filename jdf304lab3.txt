--Jonah Fidel 
--9/24/15
--Theory of Computing 
--Lab #3 
--Professor Mulry 

--1.)
monext f bin e [] = e
monext f bin e (start:rest) = bin((f start), (monext f bin e rest))

--comp (x, y) z = x (y z)
--id x = x 

--2.) 
tspec1 :: Char -> Int -> (Int, String)
tspec1 'a' 0 = (1, "a")
tspec1 'a' 1 = (0, "")
tspec1 'b' 1 = (1, "")
tspec1 'b' 0 = (0, "")

tbin (f, g) state = let(state1, string1) = f state in 
					let(state2, string2) = g state1 in 
					(state2, string1 ++ string2) 
--3.) 
tidentity :: Int -> (Int, String)
tidentity state = (state, "")

trans tspec = monext tspec tbin tidentity 

trans1 = trans tspec1 

--Main> trans1 "abbababbaba" 0 
--(1,"aaa")
--Main> trans1 "abbababbababba" 0 
--(0,"aaa")

--4.) 
tspec2 :: Char -> Int -> (Int, String)
tspec2 'a' 0 = (1, "a")
tspec2 'a' 1 = (0, "a")
tspec2 'b' 1 = (1, "")
tspec2 'b' 0 = (0, "")

trans2 = trans tspec2

--Main> trans2 "abbababbababba" 0 
--(0,"aaaaaa")
--Main> trans2 "abbababbaba" 0 
--(1,"aaaaa")

--5.) 
add1 (x, y) = x + y 

convhelp 'n' = 5 
convhelp 'd' = 10 
convhelp 'q' = 25

convert coins = monext convhelp add1 0 coins 

change coins = (convert coins) - 40

short coins = 40 - (convert coins) 

candy coins = convert coins >= 40 

inlist x [] = False 
inlist x (start:rest) = if (x==start) 
						then True 
						else (inlist x (rest))

tt(tspec, finalstates) string = (inlist (tspec string) finalstates, string)

run(tspec, finalstates) string = let (bool, string) = tt(tspec, finalstates) string
							     in if bool 
							     then "Here comes your candy, Your change coming is " ++ show (change string) ++ " cents."
							     else "Sorry, you need " ++ show (short string) ++ " cents."





