			-- BabyMoneyBall!
			-- Justin McNamara
			-- CSCI 3330 Dr. Reed
			-- April 21, 2014
			-- mcnamara_moneyball.hs

import Data.List

type Outcome = String
type Grade = String
type Index = Integer
type Wt = Float
type Measure = Float
type TierOutcomes = [(Index, Outcome) ]         -- Indexed outcomes, one per index
type TierMeasures = [(Index, Measure) ]         -- Indexed measures, one per index
type TierOutcomeMeasures = [(Index,Outcome,Measure) ] 
                                                -- Indexed measured outcomes
												--    one per index
type SingleWtMeasures  = [(Index,Wt,Measure)]   -- Set of weighted measures for 
												--   a single index.  Index should
												--   be same for all triples.
												
type SingleWtGrade	   = [(Index, Wt, Grade)]	

												
type MultWtMeasures = [[(Index,Wt,Measure)]]    -- The nth list of triples should map 
												--   nth index in a lower tier 
												--   to each index in upper tier.

type OneToManyMap = [(Index, [(Index,Wt)])]	     -- From each index of one level to
                                                 --   each index of another level
												 --   Wt = 1 means mapped
												 --   Wt = 0 means not mapped
type ManyToOneMap = [([(Index, Wt)],Index)]		 

type UpMap = OneToManyMap	                     -- From lower tier to higher tier
					
					
-- Define integral values for letter grades
integralMeasure grade 
  | grade == "A" 	=	4
  | grade == "B" 	=	3
  | grade == "C" 	=	2
  | grade == "D" 	=	1
  | grade == "F"    	=      -1
  | otherwise		=	0

-- Transform letter grade to a Float Measure  
measure :: String -> Measure
measure s = fromIntegral (integralMeasure s)

--------------------------
--Calculate Max Measure
--------------------------
maxMeasure :: [(Wt, Measure)] -> Measure
maxMeasure []     = error "maximum of empty list"
maxMeasure (x:xs) = maxTail x xs
  where maxTail currentMax [] = (snd currentMax)
        maxTail (m, n) (p:ps)
          | n < (snd p) = maxTail p ps
          | otherwise   = maxTail (m, n) ps


measureGrades :: [(Index,Wt,Grade)] -> [(Index,Wt,Measure)]
measureGrades pgs = [(i,j, measure g ) | (i,j,g) <- pgs]

{- *************************
   Testing measureGrades !!!
   *************************-}
testMeasureGrades :: [(Index, Wt, Grade)]
testMeasureGrades =    [(1,0.0,"A"), (2,0.0,"B"), (3,1.0,"C"), 
						(4,1.0,"A"), (5,1.0,"B"), (6,1.0,"C"), 
						(7,1.0,"A"), (8,1.0,"B")]
{- testMeasureGrades gives  
[(1,0.0,4.0),(2,0.0,3.0),(3,1.0,2.0),(4,1.0,4.0)
,(5,1.0,3.0),(6,1.0,2.0),(7,1.0,4.0),(8,1.0,3.0)]-}


-- Calculate weighted average of a list of weights and measures

sumProd :: [Float] -> [Float] -> Float
sumProd [] ys = 0
sumProd (x:xs) (y:ys) = ((x*y) + (sumProd xs ys))

{- Following represents 4 measures of 4.0, 3.0, 2,0 and -1, with 
corresponding weights 10.0, 2.0. 1.0, and 0.0. The 4.0 is weighted 
significantly heavier than others.  The 0.0 weight for -1 renders it 
irrelevant.
  -}
testSumProdWts :: [Float]
testSumProdWts = [10.0, 1.0, 1.0, 0.0 ] 
testSumProdMeasures  :: [Float]
testSumProdMeasures = [4.0, 3.0, 2.0, -1.0 ]

{-  *************************
	  Testing sumProd  !!!
	*************************
testSumProd gives   12.0  -} 
testSumProd = sumProd testSumProdWts testSumProdMeasures


wtAve :: [Wt] -> [Measure] -> Measure
wtAve ws vs  =
	let d = sum ws
	    n = sumProd ws vs
	in (if (truncate d == 0) then 0.0  else n/d )     -- allows all 0 wts

	
{- testing wtAve below, result gives  3.75 with  ghci -}
testWtAve = wtAve testSumProdWts testSumProdMeasures	



-- Separate off heads from tails	
split :: [[a]] -> ( [a], [[a]] )
split [] = ([], [])
split (xs:xss) =
	let (hds, tls) = split xss
	in  ( (head xs): hds, (tail xs): tls )	


{-	WinHugs Example	-}
testsplit = [[(1,"B"), (2,"B"), (3,"B")],[(1,"X"), (2,"X"),(3,"A")]]	
{-
Main> split testsplit
([(1,"B"),(1,"X")],[[(2,"B"),(3,"B")],[(2,"X"),(3,"A")]])

-}

--  Split a list of ordered triples into a triple of lists
splitTrips :: [(a,b,c)] -> ([a],[b],[c])
splitTrips [] = ([],[],[])
splitTrips ((x,y,z):ts) =
	let (l1,l2,l3) = splitTrips ts
	in  ( x:l1, y:l2, z:l3 )
	
	
	
{- *************************
   Testing splitTrips !!!
   *************************-}
testSplitTrips = [(1,0.0,"A"), (2,0.0,"A"), (3,1.0,"A"), 
				  (4,1.0,"A"), (5,1.0,"A"), (6,1.0,"A"), 
				  (7,1.0,"A"), (8,1.0,"A")]
	
{-  splitTrips testSplitTrips gives the following: 
([1,2,3,4,5,6,7,8],[0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0],
 ["A","A","A","A","A","A","A","A"])
-}			  

-- Calculate weighted average for a list weighted triples.
-- All indexes should be the same:
wtAveTrips :: [(Index,Wt,Measure)] -> (Index,Measure)
wtAveTrips ps = 
	let (is,ws,vs) = splitTrips ps
	in  ( head is, wtAve ws vs) 	


{-  *********************
	Testing wtAveTrips!!! 
	*********************-}	
testWtAveTrips :: [(Index, Wt, Measure)]
testWtAveTrips = [(1, 1.0, 4.0),(1, 1.0, 3.0),(1, 1.0, 3.0),
				  (1, 1.0, 4.0),(1, 1.0, 4.0)]

{- testing testWtAveTrips gives us: (1,3.6) -}	  
	
	
-- Caluclate measures for each index of a tier 
-- from a weighted average for each index from an
-- aggregate list of lists of weighted triples.  
-- Each list in the aggregate contains exactly one weighted triple per index.
--wtAveAgg :: [[(Index,Wt,Measure)]]  -> [(Index,Measure)]
wtAveAgg :: MultWtMeasures  -> TierMeasures
wtAveAgg pmss =  ag0 pmss []

ag0 pmss' ags 
  | null (head pmss') 	=  ags
  | otherwise           =
        let (hds,tls) = split pmss'
        in (wtAveTrips hds): (ag0 tls [])

addv :: Measure -> (Index,Wt) -> (Index,Wt,Measure)
addv v (i,w) = (i,w,v)

{-  *********************
	    Testing addv!!! 
	*********************-}	
testAddvMeasure :: Measure
testAddvMeasure = 4
testAddvIndexWt :: (Index, Wt)
testAddvIndexWt = (1,2.0)

testAddv = addv testAddvMeasure testAddvIndexWt
{- testing addv results in (1,2.0,4.0) -}


-- Take a set of measures of lower level together with an upmap,
-- producing an aggregate list of lists of weighted triples
-- for indexes of the higher level 
-- aggMeasures :: [(Index,Measure)] -> UpMap -> [[(Index,Wt,Measure)]]
aggMeasures :: TierMeasures -> UpMap -> MultWtMeasures
aggMeasures ms us 	=
   let aggMeasures' [] [] cur = cur
       aggMeasures' ((i1,v):ms') ((i2,ws):us') cur =
	        (map (addv v) ws) : aggMeasures' ms' us' cur
   in  aggMeasures' ms us []	

 -- An equivalent rendition of aggMeasures:
aggMeasuresAlt0 [] [] cur = cur
aggMeasuresAlt0 ((i1,v):ms') ((i2,ws):us') cur = 
                       (map (addv v) ws) : aggMeasuresAlt0 ms' us' cur
			
aggMeasuresAlt ms us = aggMeasuresAlt0 ms us []

{-*************************
	Testing aggMeasures!!!
  *************************-}
testAggMeasuresTier :: TierMeasures
testAggMeasuresTier  = [(1,1.222), (2,1.0), (3, 3.54)]
testAggMeasuresMap  :: UpMap
testAggMeasuresMap   = [(1,[(1,1.0),(2,1.0),(3,1.0),(4,1.0)]),
					    (2,[(1,1.0),(2,1.0),(3,1.0),(4,1.0)]),
					    (3,[(1,1.0),(2,1.0),(3,1.0),(4,1.0)])]

testingAggMeasures = aggMeasures testAggMeasuresTier testAggMeasuresMap
  {- gives [[(1,1.0,1.222),(2,1.0,1.222),(3,1.0,1.222),(4,1.0,1.222)],
			[(1,1.0,1.0),(2,1.0,1.0),(3,1.0,1.0),(4,1.0,1.0)],
			[(1,1.0,3.54),(2,1.0,3.54),(3,1.0,3.54),(4,1.0,3.54)]] -}
  
			
----------------------------------------------------------------------
--  An Example --
--  Levels:  Organization, Club, and Team
--  Raw Data from 2 Players:  Chipper Jones and Greg Maddox
----------------------------------------------------------------------
  
   
-- Define the Outcomes  
orgOutcomes  = [(1,"Skill"), (2,"Money")]
clubOutcomes = [(1,"Offense"), (2,"Defense"), (3,"Box Office Appeal")]
teamOutcomes = [(1, "AVG"), (2, "HR"), (3, "E"),(4, "FP"), (5, "ERA"), 
				(6, "W") ,(7,"Awards"), (8,"Fan Appeal")]

-- Define the maps Avg/HR/ etc mapped to Off, Def, Box Office
teamToClub :: UpMap
teamToClub = [(1,[ (1,1.0), (2,0.0), (3, 0.0) ] ),  
			  (2,[ (1,1.0), (2,0.0), (3, 0.0) ] ),  
			  (3,[ (1,0.0), (2,1.0), (3, 0.0) ] ),  
			  (4,[ (1,0.0), (2,1.0), (3, 0.0) ] ),
			  (5,[ (1,0.0), (2,1.0), (3, 0.0) ] ),
			  (6,[ (1,0.0), (2,1.0), (3, 0.0) ] ),
			  (7,[ (1,0.0), (2,0.0), (3, 1.0) ] ),
			  (8,[ (1,0.0), (2,0.0), (3, 1.0) ] )]


			  
clubToOrg :: UpMap
clubToOrg = [(1,[(1,1.0),(2,0.0)]),
			 (2,[(1,1.0),(2,0.0)]),
			 (3,[(1,0.0),(2,1.0)])]


-- Provide raw data for players: Chipper Jones and Greg Maddox

-- First in terms of grades
-- "X" for not relevant
-- *DISCLAIMER*  In rare cases, offensive players can also be effective pitchers and vice versa

chipperWGrades, maddoxWGrades, glavinWGrades, torreWGrades, uptonWGrades, arronWGrades, drReedWGrades :: [(Index, Wt, Grade)]
--Player Grades    (1)Avg        (2)HR        (3)Err	   (4)FP        (5)ERA  	 (6)Wins	(7)Awards   (8)Fan Loyalty

chipperWGrades = [(1,1.0,"B"), (2,1.0,"B"), (3,1.0,"B"), (4,1.0,"B"), (5,0.0,"X"), (6,0.0,"X"), (7,1.0,"B"), (8,1.0,"A")]
maddoxWGrades  = [(1,0.0,"X"), (2,0.0,"X"), (3,1.0,"A"), (4,1.0,"A"), (5,1.0,"A"), (6,1.0,"A"), (7,1.0,"B"), (8,1.0,"A")]
-- 4+ additional players
glavinWGrades  = [(1,0.0,"X"), (2,0.0,"X"), (3,1.0,"A"), (4,1.0,"A"), (5,1.0,"A"), (6,1.0,"A"), (7,1.0,"B"), (8,1.0,"A")]
torreWGrades   = [(1,1.0,"B"), (2,1.0,"B"), (3,1.0,"B"), (4,1.0,"B"), (5,0.0,"X"), (6,0.0,"X"), (7,1.0,"C"), (8,1.0,"B")]
uptonWGrades   = [(1,1.0,"A"), (2,1.0,"A"), (3,1.0,"B"), (4,1.0,"B"), (5,0.0,"X"), (6,0.0,"X"), (7,1.0,"C"), (8,1.0,"A")]  --ambiguous :)
arronWGrades   = [(1,1.0,"B"), (2,1.0,"A"), (3,1.0,"A"), (4,1.0,"A"), (5,0.0,"X"), (6,0.0,"X"), (7,1.0,"A"), (8,1.0,"A")]
drReedWGrades  = [(1,0.0,"A"), (2,0.0,"A"), (3,1.0,"A"), (4,1.0,"A"), (5,1.0,"A"), (6,1.0,"A"), (7,1.0,"A"), (8,1.0,"A")]

-- Transform grades to Float Measures
chipperWMeasures = measureGrades chipperWGrades
maddoxWMeasures  = measureGrades maddoxWGrades
glavinWMeasures  = measureGrades glavinWGrades
torreWMeasures   = measureGrades torreWGrades
uptonWMeasures   = measureGrades uptonWGrades
arronWMeasures   = measureGrades arronWGrades
drReedWMeasures  = measureGrades drReedWGrades


---------------------------------------------------
-- Calculate Team Measures from Player Measures
---------------------------------------------------
-- First aggregate measures for both players into a nested list:
aggPlayerMeasures = [chipperWMeasures, maddoxWMeasures, 
					glavinWMeasures, torreWMeasures, 
					uptonWMeasures, arronWMeasures, drReedWMeasures]


-- Then take the weighted average:
teamMeasures = wtAveAgg aggPlayerMeasures 
{-Main> teamMeasures  =
[(1,3.0),(2,3.0),(3,3.5),(4,3.5),(5,4.0),(6,4.0),(7,3.0),(8,4.0)]
-} 

---------------------------------------------------
-- Calculate Club Measures from Team Measures
---------------------------------------------------
-- First aggregate team measures using the UpMap:
aggClubMeasures = aggMeasures teamMeasures teamToClub
{-Main> aggClubMeasures
[[(1,1.0,3.0),(2,0.0,3.0),(3,0.0,3.0)],
 [(1,1.0,3.0),(2,0.0,3.0),(3,0.0,3.0)],
 [(1,0.0,3.5),(2,1.0,3.5),(3,0.0,3.5)],
 [(1,0.0,3.5),(2,1.0,3.5),(3,0.0,3.5)],
 [(1,0.0,4.0),(2,1.0,4.0),(3,0.0,4.0)],
 [(1,0.0,4.0),(2,1.0,4.0),(3,0.0,4.0)],
 [(1,0.0,3.0),(2,0.0,3.0),(3,1.0,3.0)],
 [(1,0.0,4.0),(2,0.0,4.0),(3,1.0,4.0)]]
-}

-- Then take the weighted average:
clubMeasures = wtAveAgg aggClubMeasures
{-Main> clubMeasures
[(1,3.0),(2,3.75),(3,3.5)]
-}

---------------------------------------------------
--Calculate Organization Measures from Club Measures
---------------------------------------------------
-- First aggregate club measures using the UpMap:
aggOrgMeasures = aggMeasures clubMeasures clubToOrg
{-Main> aggOrgMeasures
[[(1,1.0,3.0),(2,0.0,3.0)],
[(1,1.0,3.75),(2,0.0,3.75)],
[(1,0.0,3.5),(2,1.0,3.5)]]
-}
--Then take the weighted average:
orgMeasures = findMeasures teamMeasures [teamToClub, clubToOrg]
{-Main> orgMeasures
[(1,3.375),(2,3.5)]
-}

{-  Incremental tests and demos -}
s1 = ag0 aggPlayerMeasures []  -- step 1 of wtAvAgg of [chipperMeasures, maddoxMeasures ]

s2 = aggMeasuresAlt0 teamMeasures teamToClub []

{-  ************************
    ***** findMeasures *****
	************************-}
findMeasures :: TierMeasures -> [UpMap] -> TierMeasures
findMeasures [] [] = []
findMeasures m  [] = m
findMeasures  m (u:us) = wtAveAgg(aggMeasures m u) `findMeasures` us 


{-  ************************
    ***** orgMeasures *****
	************************-}
--orgMeasures = findTierMeasures teamMeasures [teamToClub, clubToOrg]

{-  *******************************
    ***** getMeasuredOutcomes *****
	*******************************-}
getMeasuredOutcomes :: TierOutcomes -> TierMeasures -> TierOutcomeMeasures
getMeasuredOutcomes [] [] = []
getMeasuredOutcomes (o:os)(m:ms)
		| fst o == fst m = (fst o, snd o, snd m): getMeasuredOutcomes os ms 
		| otherwise = []

	  

{-  ************************
    ****** reverseMap ******
	************************-}
reverseMap :: UpMap -> UpMap
reverseMap [] = []
reverseMap xs = zip [1..] [zip [1..] (map snd x) 
	| x <- transpose (snd (unzip xs))]
	
orgToClub = reverseMap clubToOrg
clubToTeam = reverseMap teamToClub


{-  ************************
    ***** extraCredit ******
	************************-}
extraCredit = findMeasures orgMeasures [orgToClub, clubToTeam]








