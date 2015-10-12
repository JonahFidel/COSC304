--Jonah Fidel 
--10/1/15
--Theory of Computing 
--Prof. Mulry 
--Lab #4

monext f bin e [] = e
monext f bin e (start:rest) = bin((f start), (monext f bin e rest))

flatten ([]) = []
flatten (start:rest) = start ++ flatten(rest)

comp(rel2, rel1) x = fixlist$ flatten (map rel2 (rel1 x))

inlist val [] = False 
inlist val (start:rest)  = if val == start 
					then True 
					else inlist val rest 



fixlist [] = []
fixlist [start] = [start]
fixlist (start:rest) = if inlist start rest 
					then fixlist rest 
					else [start] ++ fixlist (rest)

relidentity rel = [rel]

ndspec1 'b' state = [state] 
ndspec1 'a' 0 = [1, 2]
ndspec1 'a' 1 = [2, 3]
ndspec1 'a' 2 = [0, 3]
ndspec1 'a' 3 = [0, 1]

ndfa ndspec = monext ndspec comp relidentity

ndfa1 = ndfa ndspec1 

--Main> ndfa1 "abbababbaba" 1
--[0,1,2,3]

ndlm ndspec string = ndfa ndspec string 0

--Main> ndlm ndspec1 "abbababbaba" 
--[2,3,0,1]

interS set1 [] = [] 
interS set1 (start:rest) = if inlist start set1 
						 then [start] ++ (interS set1 rest)
						 else interS set1 rest

ndmm (ndspec, finalstates) string = let reststates = ndlm ndspec string in 
								if interS reststates finalstates == []
								then False 
								else True 

--Main> ndmm (ndspec1, [0,2]) "abbababbabaaaa"
--True
--Main> ndmm (ndspec1, [0]) "ab"
--False

ndspec2 'a' 0 = [1, 2]
ndspec2 'a' 1 = []
ndspec2 'a' 2 = [3]
ndspec2 'a' 3 = []
ndspec2 'b' 0 = [3]
ndspec2 'b' 1 = [2]
ndspec2 'b' 2 = []
ndspec2 'b' 3 = []

--4.) we need to add empty brackets as return values for the states where 
-- there is no next state specified by a given character 

--Main> ndlm ndspec2 "abab" 
--[]
--Main> ndlm ndspec2 "bb" 
--[]
--Main> ndmm(ndspec2, [2]) "a"
--True
--Main> ndmm(ndspec2, [2]) "aba"
--False
--Main> ndmm(ndspec2, [2]) "abab"
--False

nndspec3 "a" 0 = [1]
nndspec3 "b" 1 = [2]
nndspec3 "ab" 1 = [3]
nndspec3 "" 2 = [0]
nndspec3 "a" 2 = [3]
nndspec3 "b" 3 = [3]

--5.) in order for nndspec3 to be correct, we need to add double quotes to change the
-- char to a string 

nndfa nndspec = monext nndspec comp relidentity

nndlm nndspec liststrings = nndfa nndspec liststrings 0

nndmm (nndspec, finalstates) liststrings = let reststates = nndlm nndspec liststrings in 
								if interS reststates finalstates == []
								then False 
								else True 

-- couldn't quite get the last part to work. The examples here should be different
-- depending on whether or not the "" empty word is included because in 
-- nndspec3, the empty word transitions to state 0 from state 2 

















