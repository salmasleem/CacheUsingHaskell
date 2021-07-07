
data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)

convertBinToDec :: Integral a => a -> a
convertBinToDec x = convertBinToDecHelp x 0
convertBinToDecHelp x z | x == 0 = 0
						| x == 1 = 2^z
						| odd x  = (2^z + convertBinToDecHelp (div x 10) (z+1))
						| otherwise = convertBinToDecHelp (div x 10) (z+1)

logBase2 :: Floating a => a -> a
logBase2 x = logBase 2 x

fillZeros :: [Char] -> Int -> [Char]
fillZeros x y | y == 0 = x
			  | otherwise = "0" ++ fillZeros x (y-1)

getNumBits :: (Integral a, RealFloat a1) => a1 -> [Char] -> [c] -> a
getNumBits numOfSets cacheType cache 
										| cacheType == "fullyAssoc" = 0
										| cacheType == "setAssoc" = ceiling (logBase2 numOfSets)
										| cacheType == "directMap" = ceiling (logBase2 numOfSets)


replaceIthItem :: (Eq a, Num a) => t -> [t] -> a -> [t]
replaceIthItem item l index = replaceIthItemHelp item l index 0
replaceIthItemHelp item (x:xs) index c | c == index = (item:xs)
									   | otherwise = (x:(replaceIthItemHelp item xs index (c+1)))
									   
splitEvery n l = splitEveryHelp n l [] 0
splitEveryHelp n (x:xs) y c | xs == [] = [(y++[x])]
							| c == n = y:(splitEveryHelp n (x:xs) [] 0)
							| c < n = splitEveryHelp n xs (y++[x])(c+1)



data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)

getDataFromCache :: (Integral b, Eq a) => [Char] -> [Item a] -> [Char] -> b -> Output a
getDataFromCache stringAddress cache typee bitsNum 
												   | typee == "directMap" = getDataFromCacheDMH (convertAddress (read stringAddress :: Int) bitsNum "DirectMap") cache 0
												   | typee == "fullyAssoc" = getDataFromCacheFA (read stringAddress :: Int) cache 0
												   | typee == "setAssoc" = getDataFromCacheSA (convertAddress (read stringAddress :: Int) bitsNum "setAssoc") cache (div (length cache) 2^bitsNum) 0 0
												   | otherwise = error "NoOutput"
												   
getDataFromCacheFA tag ((It (T t) (D d) b v):xs) acc 
                                                            |t==tag && (b == True) = Out(d,acc)
															|xs == [] = NoOutput
															|t/=tag = getDataFromCacheFA tag xs (acc+1)
															|otherwise = NoOutput

getDataFromCacheDMH (a,index) ((It (T t) (D d) b v):xs) c 
															| (It (T t) (D d) b v) == NotPresent = NoOutput
															| c == (convertBinToDec index)  = getOut a (It (T t) (D d) b v)
															| c /= (convertBinToDec index) = getDataFromCacheDMH (a,index) xs (c+1)
															| otherwise = NoOutput
																	
													
getDataFromCacheSA (tag , index) (x:xs) pos hops currentpos   
													| currentpos < wantedPos = getDataFromCacheSA (tag , index) xs pos hops (currentpos+1)
													| currentpos == wantedPos = checkData tag (x:xs) pos 0 hops

													where 	
														wantedPos = pos*(convertBinToDec index)

checkData tag ((It (T t) (D d) b i):xs) pos curr hops	
														|curr > pos = NoOutput
														|t == tag && b == True = Out(d,hops)
														|xs == [] = NoOutput
														|otherwise = checkData tag xs pos (curr+1) (hops+1)
							
																	
getOut 	tag (It (T t) (D a) b v) | (b == True && t == tag) = Out(a,v)
								 | otherwise = NoOutput
																										
convertAddress :: (Integral b1, Integral b2) => b1 -> b2 -> p -> (b1, b1)											
convertAddress binAddress bitsNum _ = (a,b)
											where 
												  z = 10^bitsNum
												  a = div binAddress z
												  b = mod binAddress z

--replaceInCache :: Integral b =>Int -> Int -> [a] -> [Item a] -> [Char] -> b -> (a, [Item a])

--replaceInCache :: (Eq a, Integral b, Integral c) => Int -> c -> [a] -> [Item a] -> String -> b -> (a,[Item a])
replaceInCache tag idx memory oldCache "directMap" bitsNum = (itemData , newcache) 
			where
				idxPos   = convertBinToDec idx
				newcache = changeAddress tag idxPos itemData 0 oldCache
				itemData = memory !! pos
				pos = convertBinToDec (read ((tagTostring tag bitsNum1) ++ (indexTostring idx bitsNum1)):: Int)
				bitsNum1 = fromIntegral bitsNum 
				
replaceInCache tag idx memory oldCache "fullyAssoc" _ | checkInvalid oldCache = (itemData, newcacheIn)
													  | otherwise = (itemData, newcacheV)
				where 
					itemData = memory !!(convertBinToDec tag)
					newcacheIn = incTrue (replaceInvalid tag itemData oldCache)
					max = getMaxValid oldCache 0 
					newcacheV = incTrue (replaceValid tag itemData oldCache max)
					

replaceInCache tag idx memory oldCache "setAssoc" bitsNum = (itemData , newcache)
				where 
					itemData = memory !! pos
					newcache = concat (replaceHelper new tag itemData (convertBinToDec idx) 0)
					pos = convertBinToDec (read ((tagTostring tag bitsNum1) ++ (indexTostring idx bitsNum1)):: Int)
					new = splitEvery (div (length oldCache) (2^bitsNum1)) oldCache
					bitsNum1 = fromIntegral bitsNum 
					


replaceHelper (x:xs) tag itemData position curr | curr < position = (x:(replaceHelper xs tag itemData position (curr+1)))
												| curr == position = ((change tag itemData x):xs)
											
	

change tag itemData oldCache |checkInvalid oldCache = newcacheIn
							 | otherwise = newcacheV
				where 
					newcacheIn = incTrue (replaceInvalid tag itemData oldCache)
					max = getMaxValid oldCache 0 
					newcacheV = incTrue (replaceValid tag itemData oldCache max)

replaceInvalid tag itemData ((It (T t) (D d) b i):xs) | b == False = (It (T tag) (D itemData) True (-1)):xs
                                                      | otherwise = ((It (T t) (D d) b i):replaceInvalid tag itemData xs)

replaceValid tag itemData ((It (T t) (D d) b i):xs) n 	| i == n = (It (T tag) (D itemData) True (-1)):xs
														| otherwise = (It (T t) (D d) b i):(replaceValid tag itemData xs n)

incTrue [] = []
incTrue ((It (T t) (D d) b i):xs) | b == True = ((It (T t) (D d) b (i+1)):(incTrue xs))
								  | otherwise = ((It (T t) (D d) b i):(incTrue xs))

checkInvalid [] = False					  
checkInvalid ((It (T t) (D d) b i):xs) |b == False = True
									   |otherwise = checkInvalid xs

getMaxValid [] n = n
getMaxValid ((It (T t) (D d) b i):xs) n |i > n  = getMaxValid xs i
										|otherwise = getMaxValid xs n

tagTostring tag bitsNum = fillZeros tagString size
		where
			size = 6 - bitsNum - (length tagString)
			tagString = show tag
			
indexTostring index bitsNum = fillZeros indexString size
		where
			size = bitsNum - (length indexString)
			indexString = show index

changeAddress tag idxPos itemData current (x:xs)| current < idxPos = (x:(changeAddress tag idxPos itemData (current+1) xs))
												| current == idxPos = (t:xs)
												where 
													t = checkReplace tag itemData x
													  
checkReplace tag itemData (It (T t) (D d) b i) =(It (T tag) (D itemData) True 0)


getData stringAddress cache memory cacheType bitsNum
												| x == NoOutput = replaceInCache tag index memory cache cacheType bitsNum
												| otherwise = (getX x, cache)
												where
													x = getDataFromCache stringAddress cache cacheType bitsNum
													address = read stringAddress:: Int
													(tag, index) = convertAddress address bitsNum cacheType
													getX (Out (d, _)) = d
													
													
													
													
runProgram [] cache _ _ _ = ([], cache)
runProgram (addr: xs) cache memory cacheType numSets =((d:prevData), finalCache)
													where
														bitsNum = round(logBase2 numSets)
														(d, updatedCache) = getData addr cache memory cacheType bitsNum
														(prevData, finalCache) = runProgram xs updatedCache memory cacheType numSets
