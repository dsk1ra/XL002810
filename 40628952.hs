{- 
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                  SET07106/07406
            Maths for Software Engineering
             Coursework 1: Use and Modify
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

READ ME: Change the name of this file to YOURSTUDENTNUMBER.hs. For example, if your
student number were 123456789, then you would rename the file 123456789.hs.

The names of the functions correspond to the names given in the document cwk23handout-1.pdf. There are also explanations in this file. 

There are two types of question here. Questions 1-5 are "use" questions, 
and questions 6-10 are "modify" questions.

     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
     REMEMBER TO CHECK YOUR FILE COMPILES BEFORE YOU SUBMIT! 
     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%%%%%%%%%%%%%
USE questions
%%%%%%%%%%%%%

For questions 1-5, the functions that you need are given as q1, q2, q3, q4 and q5.

Also included in this file are some lists that you can use, with a note in each question about which list you should use for that question.

To answer questions 1-5, you should load this file. You will be able to find the answer
to each question using the associated function (e.g. q1) and a single line of code in the interpreter.

For questions 4-5, you may have to also create a list yourself, but it should still be possible in a
single line.

You will see that there is a line of code under each question function called "answer", e.g. answer1.

When you have received an answer from the interpreter to the question,
replace the corresponding answer part (i.e. on the right-hand side of the equals sign)
with what you want your answer to be. If you choose not to answer a question, then just leave the code alone.

DO NOT DELETE e.g answer2 IF YOU CHOOSE NOT TO ANSWER THE A QUESTION (in this example, question 2).
-}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Question 1: What is the sum of all the numbers in list1? The function 'q1' adds together the numbers in a list.

q1 :: [Int] -> Int
q1 = sum


answer1 :: Int
answer1 = 36740



-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Question 2: What is the most common character in list2? The function 'q2' returns the most frequent character in a string.

q2 :: String -> Char
q2 ss = gB (mP ss)

answer2 :: Char
answer2 = 'n' {-'X'-} 

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Question 3: What is the product of every seventh element of list1? The function q3 takes a number n and a list xs, and multiplies the numbers found at index n, 2n, 3n,... of xs together.

q3 :: Int -> [Int] -> Int
q3 n xs = product (map (\ x -> eA x xs) ns)
    where  ns = filter (<=t) (map (*n) [1..t])
           t = length xs 

answer3 :: Int
answer3 = 3404963210850205696 {-0-} 
         

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Question 4: What is the largest number in list1 which is strictly less than 167? The function 'q4' returns the biggest element in a list.

q4 :: [Int] -> Int
q4 = maximum

answer4 :: Int
answer4 = 166

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- Question 5: Characters and strings are orderable. How many words in list5 are "less" than the string "this string"? You might like to investigate the function 'words'. The function 'q5' returns the length of a list.

q5 :: [a] -> Int
q5 = length

answer5 :: Int
answer5 = 120 {-0-}{-length (filter(<11)(map(q5)(words list5)))-}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{- 
%%%%%%%%%%%%%%%%
MODIFY questions
%%%%%%%%%%%%%%%%

For questions 6-10, you have to modify a function given to you. Your answer will be the new function.

There are functions (q6-q10), and then copies of each of them (answer6-answer10). You should modify the right-hand side of the
"answer" function. You do not need to worry about the type signatures, that has already been completed for you. Example output is given after each question, so you know how your modified function should behave.

As for questions 1-5, if you choose not to attempt a question, then DO NOT DELETE the corresponding
answer function (e.g. answer7). 

-}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{- Question 6: The function 'q6' takes the even numbers in a given list, and then adds 3 to them.
Modify (in answer6) q6 so that it instead adds 3 to a list of numbers, and then returns the ones that are even.
-}

q6 :: [Int] -> [Int]
q6 xs = map (+3) (filter even xs)

-- modify the part after the = sign if you want to answer the question!
answer6 :: [Int] -> [Int]
answer6 xs = filter even (map (+3) xs)

{- Sample output
answer6 [1..25] gives [4,6,8,10,12,14,16,18,20,22,24,26,28]
answer6 [1,3,5] gives [4,6,8]
answer6 [14,26] gives []
-}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{- 
Question 7: The function 'q7' takes all the numbers in a list greater than or equal to 8 and strictly less than 25, doubles them, and adds 3. Modify (in answer7)
q7 so that it instead takes the numbers in a list strictly greater than 11 and less than or equal to 30, doubles them, and adds 3.
-}

q7 :: [Int] -> [Int]
q7 [] = []
q7 (x:xs)
    | x >= 8 && x < 25 = (2*x + 3) : q7 xs
    | otherwise = q7 xs
    
-- modify the part after each = sign if you want to answer the question!
answer7 :: [Int] -> [Int]
answer7 [] = []
answer7 (x:xs)
    | x > 11 && x <= 30 = (2 * x + 3) : answer7 xs
    | otherwise = answer7 xs
    
{- Sample output
answer7 [1..5] gives []
answer7 [11,12] gives [27]
answer7 [29..31] gives [61,63]
-}
    
	
	


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{-
Question 8: The function 'q8' takes three arguments: an integer n, an integer m, and a list of elements of type a. The function selects elements, with increasing gaps between the selected indices. So, it selects the n-th element, then the (n+1)-th element following that, then the (n+2)-th element following that, and so on. 
Modify (in 'answer8') 'q8' so that it instead selects elements with fixed gaps between the indices. So, it takes the n-th element, then the n-th element after that, then the n-th element after that, and so on, AND it always selects the last element.
-}

q8 :: Int -> Int -> [a] -> [a]
q8 _ _ [] = []
q8 1 n (x:xs) = x : (q8 n (n+1) xs)
q8 m n (x:xs) = q8 (m-1) n xs


-- modify the part after each = sign if you want to answer the question! 
answer8 :: Int -> Int -> [a] -> [a]
answer8 _ _ [last] = [last]
answer8 1 n (x:xs) = x : answer8 n (n) xs {-(n+1)-}
answer8 m n (x:xs) = answer8 (m-1) n xs

{- Sample output
answer8 3 3 [1..10] gives [3,6,9,10]
answer8 4 4 [5..50] gives [8,12,16,20,24,28,32,36,40,44,48,50]
answer8 5 5 [1..40] gives [5,10,15,20,25,30,35,40]
-}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{- Question 9: The function 'q9' takes a list of pairs of integers. If the first number in the pair is strictly greater than the second, then it adds the numbers together, squares the result, and adds that number to a list. If not, then it adds nothing to a list.
Modify (in 'answer9') 'q9' so that if the sum of the squares of the two numbers in the pair is greater than or equal to the product of the numbers multiplied by 3, then it adds the second number to a list. If not, then it subtracts the second number from the first, and adds that to a list.
-}

q9 :: [(Int,Int)] -> [Int]
q9 [] = []
q9 (x:xs)
    | fst x > snd x = (fst x + snd x)^2 : q9 xs
    | otherwise = q9 xs
    
-- modify the part after each = sign if you want to answer the question!
answer9 :: [(Int,Int)] -> [Int]
answer9 [] = []
answer9 (x:xs)
	| (fst x)^2 + (snd x)^2 >= (fst x * snd x)*3 = snd x : answer9 xs
    	| otherwise = (fst x - snd x) : answer9 xs
    
{- Sample output
answer9 [(1,5),(2,4),(3,3),(4,2),(5,1)] gives [5,-2,0,2,1]
answer9 [(2,2),(4,4),(6,6)] gives [0,0,0]
answer9 [(2,7),(4,9),(6,22)] gives [7,-5,22]
-}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{-
Question 10: The function 'q10' reverses the first and last words in a string, and leaves the remainder alone. 
Modify (in 'answer10') 'q10' so that it instead leaves the first and last words in a string alone, and reverses the order of the remaining words.
-}

q10 :: String -> String
q10 xs = unwords ([(reverse.head.words) xs] ++ ys ++ [(reverse.last.words) xs])
    where ys = (init.tail) (words xs)
    
answer10 :: String -> String
answer10 xs = unwords ([head(words xs)] ++ reverse (tail(init(words xs))) ++ [last(words xs)])
    where ys = (init.tail) (words xs)


{- Sample output
answer10 "a man a plan a canal panama" gives "a canal a plan a man panama"
answer10 "can you bring me my chapstick?" gives "can my me bring you chapstick?"
answer10 "my lips hurt real bad" gives "my real hurt lips bad"
-}


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--           END OF COURSEWORK 1
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


{- FUNCTIONS THAT ARE NEEDED FOR THE COURSEWORK. DO NOT EDIT ANYTHING IN THIS SECTION!-}

helper1 :: [a] -> [a] -> [a]
helper1 xs ys = concat (zipWith (\x y -> [x,y]) xs ys)

ls1 = reverse (helper1 [1,6..290] [19,23..157])

list1 = take 300 (cycle (helper1 ls1 [6,20..200])) :: [Int]

list2' = helper1 "long words in question2......do not change" "antidisestablishmentarianism...still, do not change"
list2 = take 300 (cycle list2')

list5' = helper1 "why am I asking you to do these questions?" "so that you can demonstrate the learning outcomes for the module"
list5 = take 1000 (cycle list5')

count :: Char -> String -> Int
count a ss = length (filter (==a) ss)

rD :: String -> String
rD "" = ""
rD (s:ss)
    | elem s ss = rD ss
    | otherwise = s : rD ss
    
mP :: String -> [(Char,Int)]
mP ss = zip (rD ss) (map (\x -> count x ss) (rD ss))

gB :: [(Char,Int)] -> Char
gB ss  = fst (head (filter (\ x -> snd x == y) ss))
    where y = maximum (map snd ss)

eA :: Int -> [Int] -> Int
eA n [] = error "not enough elements"
eA 1 (x:xs) = x
eA n (x:xs) = eA (n-1) xs


