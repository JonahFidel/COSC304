--Jonah Fidel 
--09/10/15
--COSC 304 
--Lab #1

--1.)
minList [] = error "empty list"
minList [start] = start
minList (start:rest) = if start < minList rest 
					then start 
					else minList rest 
					

listSort [] = []
listSort [start] = [start]
listSort (start:rest) = if start < minList rest 
						then start : listSort rest 
						else listSort (rest++[start])
						
inS val [] = False 
inS val(start:rest)  = if val == start 
					then True 
					else inS val rest 
					
norep [] = []
norep [start] = [start]
norep (start:rest) = if inS start rest 
					then norep rest 
					else [start] ++ norep (rest)

fixS [] = []
fixS[start] = [start]
fixS (start:rest) = listSort(norep(start:rest))

addtoSet val [] = [val]
addtoSet val (start:rest) = if inS val (start:rest)
							then (start:rest)
							else val : (start:rest)

unionS [] [] = []
unionS [val1] [val2] = if val1 /= val2 
						then [val1, val2]
						else [val1]
unionS (start1:rest1) (start2:rest2) = norep((start1:rest1)++(start2:rest2))

interS set1 [] = [] 
interS set1 (start:rest) = if inS start set1 
						 then [start] ++ (interS set1 rest)
						 else interS set1 rest

setEq (start1:rest1) (start2:rest2) = if fixS(start1:rest1) == fixS(start2:rest2)
									then True 
									else False
                                    
--2.) 
addA ([]) = []
addA (start:rest) = ("a" ++ start) : addA(rest) 

astar = [""] ++ (addA (astar))  

--Main> take 6 astar
--["","a","aa","aaa","aaaa","aaaaa"]

--3.)
addAB ([]) = []
addAB (start:rest) = (start ++ "a") : (start ++ "b") : addAB(rest)

abstar = [""] ++ addAB(abstar)

--Main> take 15 abstar
--["","a","b","aa","ab","ba","bb","aaa","aab","aba","abb","baa","bab","bba","bbb"]

--4.)
addABC ([]) = []
addABC (start:rest) = (start ++ "a") : (start ++ "b") : (start ++ "c") : addABC(rest)

abcstar = [""] ++ addABC(abcstar)

--Main> take 15 abcstar
--["","a","b","c","aa","ab","ac","ba","bb","bc","ca","cb","cc","aaa","aab"]








