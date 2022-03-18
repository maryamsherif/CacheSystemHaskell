data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)

strToInt x = read x :: Int

convertBinToDec :: Integral a => a -> a
convertBinToDec 0=0
convertBinToDec bin = 2*convertBinToDec(div bin 10)+ mod bin 10

replaceIthItem :: (Eq a, Num a) => t -> [t] -> a -> [t]
replaceIthItem item [] index = []
replaceIthItem item (h:t) index | index==0 = item:t
						        | otherwise = h:(replaceIthItem item t (index-1))
takeFirst _ [] = []
takeFirst n (x:xs) | n==0 = []
				   |otherwise = [x] ++ takeFirst (n-1) xs 
deleteFirst _ [] = [] 
deleteFirst n (x:xs) | n==0 = (x:xs)
				     |otherwise =  deleteFirst (n-1) xs
splitEvery _ [] = []				   
splitEvery n (x:xs) = [takeFirst n (x:xs)] ++ splitEvery n (deleteFirst n (x:xs))
		   
logBase2 :: Floating a => a -> a
logBase2 1 =0	
logBase2 num | num==2 = 1
			|otherwise = 1 + logBase2(num/ 2)
			
getNumBits numOfSets cacheType cache= round (logBase2 numOfSets)
							

fillZeros :: (Eq a, Num a) => [Char] -> a -> [Char]
fillZeros s n = if n==0 then s 
			else "0"++ fillZeros s (n-1)
			

--DirectMap
data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)

getData  (It (T t) (D d) b o) = d
getTag  (It (T t) (D d) b o) = t
getOrder  (It (T t) (D d) b o) = o
getIdx stringAddress bitsNum = convertBinToDec(mod (strToInt(stringAddress)) (10^bitsNum))

isValid  (It (T t) (D d) b o) =  if b==True then True
                                 else False
--We changed Name of the function because it gives us error multiply defined
getDataFromCache1 stringAddress cache "directMap" bitsNum = 
		if isValid (cache!!convertBinToDec(snd(convertAddress (strToInt(stringAddress)) bitsNum "directMap")))==False
		then error("NoOutput")
		else 
		if isValid (cache!!convertBinToDec(snd(convertAddress (strToInt(stringAddress)) bitsNum "directMap"))) ==True &&
		(getTag(cache!!convertBinToDec(snd(convertAddress (strToInt(stringAddress)) bitsNum "directMap")))== 
		convertBinToDec(fst(convertAddress (strToInt(stringAddress)) bitsNum "directMap")))
		then (getData(cache!!convertBinToDec(snd(convertAddress (strToInt(stringAddress)) bitsNum "directMap"))),0)
		else error("NoOutput")

convertAddress binAddress bitsNum "directMap" = (div binAddress (10^bitsNum) , mod binAddress (10^bitsNum))

replaceInCache tag idx memory oldCache "directMap" bitsNum = (memory!!(convertBinToDec(idx)) ,replaceIthItem (It (T tag) (D (memory!!(convertBinToDec(idx)))) True 0) oldCache (convertBinToDec(idx)))


--FullyAssoc
--We changed Name of the function because it gives us error multiply defined
getDataFromCache2 stringAddress (x:xs) "fullyAssoc" bitsNum = getDataFromCache22 stringAddress (x:xs) "fullyAssoc" bitsNum 0

getDataFromCache22 _ [] _ _ _ = error("NoOutput")


getDataFromCache22 stringAddress (x:xs) "fullyAssoc" bitsNum hopsnum =  if isValid x == False 
    then getDataFromCache22 stringAddress xs "fullyAssoc" bitsNum (hopsnum+1)
	else 
	if getTag(x) == strToInt(stringAddress) 
	then (getData (x), hopsnum)
	else error("NoOutput")

--We changed Name of the function because it gives us error multiply defined
convertAddress_fully binAddress bitsNum "fullyAssoc" = (binAddress,0)


--We changed Name of the function because it gives us error multiply defined
replaceInCache2 tag idx memory (x:xs) "fullyAssoc" bitsNum=
    if isValid x == False && (allValid (x:xs) == False )
   then (memory!!(convertBinToDec(tag)) ,replaceIthItem (It (T tag) (D (memory!!(convertBinToDec(tag)))) True 0)(x:xs) (findInValid (x:xs) 0 )) 
    else if allValid (x:xs) == False
	 then replaceInCache2 tag idx memory xs "fullyAssoc" bitsNum
	 else replaceInCache22 tag idx memory (x:xs) "fullyAssoc" bitsNum

replaceInCache22 tag idx memory (x:xs) "fullyAssoc" bitsNum =
	if allValid (x:xs)  ==True 
 then (memory!!(convertBinToDec(tag)),replaceIthItem (It (T tag) (D (memory!!(convertBinToDec(tag)))) True 0) (x:xs) (getHigh (x:xs) 0))
  else error("NoOutput")


		
allValid [] = True
allValid (x:xs) = if isValid x == True 
					then allValid (xs)
					else False
					

findInValid [] c = c 					
findInValid (x:xs) c = if isValid x == False then c 
					 else findInValid (xs) (c+1) 

getHigh [] c = c	
			
getHigh (x:xs) c | getOrder(x)== 3 = c
						|otherwise = getHigh xs (c+1)
			
--SetAssoc
set stringAddress cache bitsNum = (splitEvery (2^bitsNum) cache)!!(convertBinToDec(snd(convertAddress_set (strToInt(stringAddress)) bitsNum "setAssoc" )))

--We changed Name of the function because it gives us error multiply defined
getDataFromCache3 stringAddress (x:xs) "setAssoc" bitsNum= getDataFromCache33 stringAddress (set stringAddress (x:xs) bitsNum) "setAssoc" bitsNum 0

getDataFromCache33 _ [] _ _ _ = error("NoOutput")

getDataFromCache33 stringAddress (x:xs) "setAssoc" bitsNum hopsnum =  if isValid x == False 
    then getDataFromCache33 stringAddress xs "setAssoc" bitsNum (hopsnum+1)
	else 
	if getTag(x) == fst(convertAddress_set (strToInt(stringAddress)) bitsNum "setAssoc" ) 
	then (getData (x), hopsnum)
	else error("NoOutput")

--We changed Name of the function because it gives us error multiply defined					
convertAddress_set binAddress bitsNum "setAssoc" = (div binAddress (10^bitsNum) , mod binAddress (10^bitsNum))

set2 idx cache bitsNum = (splitEvery (2^bitsNum) cache)!!(convertBinToDec(idx))

getHigh2 [] c = c	
			
getHigh2 (x:xs) c | getOrder(x)== 1 = c
						|otherwise = getHigh xs (c+1)
						
app tag idx = show (tag) ++ show (idx)	

--We changed Name of the function because it gives us error multiply defined
replaceInCache3 tag idx memory oldCache "setAssoc" bitsNum =
			if allValid (set2 idx oldCache bitsNum)== False
			then replaceIthItem (It (T tag) (D (memory!!(convertBinToDec(strToInt(app tag idx))))) True 0) oldCache ((findInValid (set2 idx oldCache bitsNum) 0)+2)
			 else replaceInCache33 tag idx memory oldCache "setAssoc" bitsNum
		 

replaceInCache33 tag idx memory oldCache "setAssoc" bitsNum =
          if  allValid ( set2 idx oldCache bitsNum)== True
         then  replaceIthItem (It (T tag) (D (memory!!(convertBinToDec(strToInt(app tag idx))))) True 0) oldCache (getHigh2 (set2 idx oldCache bitsNum) 0)
         else  error("NoOutput")
		 
--Implemented Functions
{-getData stringAddress cache memory cacheType bitsNum
			| x == NoOutput = replaceInCache tag index memory cache cacheType bitsNum
			| otherwise = (getX x, cache)
		where
		x = getDataFromCache stringAddress cache cacheType bitsNum
		address = read stringAddress:: Int
		(tag, index) = convertAddress address bitsNum cacheType
		getX (Out (d, _)) = d

runProgram [] cache _ _ _ = ([], cache)
runProgram (addr: xs) cache memory cacheType numSets =
				((d:prevData), finalCache)
					where
				bitsNum = round(logBase2 numOfSets)
				(d, updatedCache) = getData addr cache memory cacheType bitsNum
				(prevData, finalCache) = runProgram xs updatedCache memory cacheType numSets-}