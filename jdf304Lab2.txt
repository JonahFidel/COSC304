-- Jonah Fidel 
-- 9/17/15 
-- prof Mulry
-- COSC 304 
-- Lab #2 

--1.) 
monext f bin e [] = e
monext f bin e (start:rest) = bin((f start), (monext f bin e rest))

add (num1, num2) = num1 + num2 
times (num1, num2) = num1 * num2

f n = 3*n + 2
g n = n + 1

--Main> monext f add 0 [3, 4, 7]
--48
--Main> monext g times 1 [2, 4, 1]
--30

--2.) 
mspec1 'a' 0 = 1 
mspec1 'a' 1 = 2 
mspec1 'a' 2 = 3 
mspec1 'a' 3 = 0  
mspec1 'b' state = state 

--3.) 
comp (x, y) z = x (y z)
fsm spec = monext spec comp id 
fsm1 = fsm mspec1 

--Main> fsm1 "abbababbaba" 0 
--1
--Main> fsm1 "abbababbababba" 0 
--2
--Main> fsm1 "abbababbababba" 1
--3

--4.)
lm mspec str = fsm mspec str 0 
--Main> lm mspec1 "abbababbaba"
--1
--Main> lm mspec1 "abbababbababba"
--2

--5.) 
inlist x [] = False 
inlist x (start:rest) = if (x==start) 
						then True 
						else (inlist x (rest))

mmspec1 = (mspec1, [0,2])
mm (mspec, finalstates) str = inlist (lm mspec str) finalstates 
mm1 = mm mmspec1 

--Main> mm1 "abbababbaba"
--False
--Main> mm1 "abbababbababba"
--True






