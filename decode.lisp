; *********************************************
; *  341 Programming Languages                *
; *  Fall 2019                                *
; *  Author: Furkan Ozev                      *
; *  Number: 161044036						  *
; *********************************************

;; utility functions 
(load "include.lisp") ;; "c2i and "i2c"

(defun read-as-list (filename)
	; Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."

	(let ((downWord ) (fileList (list )) (file (open filename :if-does-not-exist nil)))
	   (when file
	      (setq fileList (read-recursive file))

	      (close file)
	   )
	   fileList
	)

)

;; -----------------------------------------------------
;; HELPERS

;; A brute force version of the spell checker that just checks whether the word occurs in the dictionary or not.
;; The word searched and the words read from the dictionary are translated into strings and checked to see if they are the same.
;; If the dictionary is ordered, removing the code in the comment line will speed up the process.
(defun spell-checker-0 (word)
	(let ((in (open "dictionary2.txt" :if-does-not-exist nil)) (dictWord) (listWord) (temp nil))
	   (when in
	      (loop for line = (read-line in nil)
	      
	      while line do
	      	(progn
	      		(setq listWord (coerce word 'string))
	      		(setq dictWord (coerce line 'string))
	      		(if (string= listWord dictWord) (progn (setq temp t) (return t)))
		      	;(if (string< listWord dictWord) (return nil))
	      	)
	      )
	      (close in)
	   )
	   temp
	)
)

;; Implements a faster search strategy using hash mapping.
;; It calculates the hash code of the word and checks whether it is in the input hash table.
(defun spell-checker-1 (word table)
 	(let ((res) (newWord))
 		(setq newWord (coerce word 'string))
 		(setq res (gethash (sxhash newWord) table))
 		(if (equal res newWord) t nil)
 	)
)

;; This function is used in the "read-as-line" function to recursive reading from the file.
(defun read-recursive (file)
	(let ((word ) (downWord ) (fileList (list ) ))
		(setq word (read-preserving-whitespace file nil))
		(setq downWord (string-downcase word))
		(if word (append (list (coerce downWord 'list)) (read-recursive file)) ()) 		
	)
)

;; Creates a list of numbers. Counts the letters of the word and adds the amount of that letter to the index corresponding to the letter.
(defun countWord (countList word)
	(if (> (length word) 0)
		(let((letter (first word)) (index ) (val )) 
			(setq index (c2i letter))
			(setq val (elt countList index))
			(prog1 countList (setf (elt countList index) (+ val 1)))
			(countWord countList (cdr word))
			countList
		) ()
	)
)

;; Creates a list of numbers. Counts the letters in the paragraph and adds the amount of that letter to the index corresponding to the letter.
;; This function call countWord function for each word.
(defun countWordInParag (countList paragraph)
	(if (> (length paragraph) 0)
		(let((word (first paragraph))) 
			(setq countList (countWord countList word))
			(countWordInParag countList (cdr paragraph))
			countList
		) ()
	)
)

;; After the letters in the paragraph are counted, the 6 most used letters are determined.
;; According to the frequency analysis, these letters are replaced by the letters "e, t, a, o, i, n" respectively.
;; The new alphabet map created based on these placed letters is returned.
(defun most6Letter (countList &optional (frequentlyList (make-list 26 :initial-element #\-)) (letterList '(#\e #\t #\a #\o #\i #\n)))
	(if (> (length letterList) 0)
		(let((letter ) (index ))

		(setq letter (apply 'max countList))
		(setq index (position letter countList))
		(prog1 countList (setf (elt countList index) 0))
		(prog1 frequentlyList (setf (elt frequentlyList index) (first letterList)))
		(setq frequentlyList (most6Letter countList frequentlyList (cdr letterList)))
		
		)
		frequentlyList
	)
)

;; Finds the permutations of the given list and returns it as a list.
(defun permu (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
             append (mapcar (lambda (l) (cons element l))
                            (permu (remove element list))))))
)

;; Specifies how many letters of permutation are required for the given word.
;; It does not take into account the same letters in the word and the letters that already exist in the alphabet map.
;; For example, if the letters "hello" are not on the alphabet map, function 4 returns.
;; Because there are 2 'l' letters. Also, if the letter 'h' is also in the alphabet map, the function returns 3.
(defun countLetter (word alphabetMap &optional (count 0) (checkList(list )))
    (if (> (list-length word) 0)
        (progn

            (if (or (find (car word) checkList) (not (char= (elt alphabetMap (c2i (car word))) #\-)))
                ()
                (progn (setq count (+ count 1)) (setq checkList (append checkList (list (car word)))) )
            )
            (setq count (countLetter (cdr word) alphabetMap count checkList))))
    count
)

;; This function is used for Gen-Decoder-A and Gen-Decoder-B0 functions.
;; The general purpose is to take the cipher paragraph and return the alphabet map that is suitable for decoding.
;; Basically creates an alphabet map by permutation for each word and tries for other words by going over the same alphabet map.
;; It run recursively until you find the alphabet map that fits all the words.
;; Selects the most optimal word for the solution with some auxiliary functions.
;; This optimal word is a word that is less than the number of "countletter(return function value)" and is longer in length.
;; This will find the right mapping in less trials by reducing the number of permutations possible.
;; In addition, mapping is not required for words whose "countletter(return function value)" is equal to zero, so operations are accelerated.
;; The way the function works is simply:
;; New letters are placed using permutations instead of letters of the word. In this way, it is checked whether the new word is in the dictionary or not.
;; If the new word is in the dictionary, the sequence moves to the next word, assuming the mapping is correct.
;; If the new word is not in the dictionary, a new mapping is created by going to the previous word.
;; For this function to work properly, all the encrypted words must be in the dictionary.
(defun Mapp (paragraph table &optional (alphabetMap (make-list 26 :initial-element #\-)) (letList '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)))
	(setq paragraph (sortList paragraph alphabetMap))
	(if (or (= (list-length paragraph) 0) (= (list-length letList) 0))
		alphabetMap
		(let ((word (car paragraph)) (wordMappingList (list )) (letterCount) (permList (list )) (newWord) (temp) (j 0)  (lenj ) (flag 0) (res) )
			(setq letterCount (countLetter word (copy-list alphabetMap)))

			(if (= letterCount 0)
				(progn
					(setq temp alphabetMap)
					(setq newWord (wordMapping word temp))
					(if (spell-checker-1 newWord table)
						(progn
							(setq res (Mapp (cdr paragraph) table (copy-list temp) (copy-list (makeLetlist temp))))
							(if (equal res nil)
								()
								(progn (setq flag 1) (setq alphabetMap res))
							)	
						)	
					)
				)
				(progn
					(let( (xlist (copy-list letList)) (m letterCount))
					  (labels ((comb1 (l c m)
							  (when (and (>= (length l) m) (equal flag 0))
							    (if (zerop m)
							    	(progn
							    		(setq permList (permu c ))
							    		(setq j 0)
										(setq lenj (list-length permList))
							    		(loop
											(setq temp (alphMap word (elt permList j) (copy-list alphabetMap)))
											(setq newWord (wordMapping word temp))
											(if (spell-checker-1 newWord table)
												(progn
													(setq res (Mapp (cdr paragraph) table (copy-list temp) (copy-list (makeLetlist temp))))
													(if (equal res nil)
														()
														(progn (setq flag 1) (setq alphabetMap res) )
													)	
												)	
											)
											
											(setq j (+ j 1))
											(when (or (= j lenj) (= flag 1)) (return ))		
										)
							    		

							    		(return-from comb1 )
							    		
							    	)
							    )
							    (comb1 (cdr l) c m)
							    (comb1 (cdr l) (cons (first l) c) (1- m)))))
					  	(if (equal flag 0)
					    (comb1 xlist nil m)))
					 )
				)	
			)		
			(if (= flag 1) alphabetMap nil)
		)
	)	
)

;; Permutations are taken according to letlists.
;; Since the permutation of the existing letters in the alphabet map cannot be taken.
;; When creating a letlist, the integer equivalents of these letters cannot be added to the this list.
(defun makeLetlist (alphabetMap)
    (let ((letList '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)))
    	(loop for a from 0 to 25
            do
            (if (char= (elt alphabetMap a) #\-)
                ()
            	(setq letList (remove (c2i (elt alphabetMap a)) letList))
            )
        )
        letList
    )    
)

;; It takes one of the permutation elements produced for the cipher word and creates a new alphabet mapping according to that element and the word.
(defun alphMap (word wordMap alphabetMap &optional (count 0))
    (let ((index) (letter))
    	(if (> (list-length word) count)
    	    (progn (setq letter (elt word count)) (setq index (c2i letter))
        	    (if (char= (elt alphabetMap index) #\-)
            	     (progn (prog1 alphabetMap (setf (elt alphabetMap index) (i2c(elt wordMap count))))
            	        (setq alphabetMap (alphMap word wordMap alphabetMap (+ count 1)))
            	     )
            	    (setq alphabetMap (alphMap (cdr word) wordMap alphabetMap count))
        	    )
    	       
    	    )
    	)
    	alphabetMap
    )
)

;; It is a function that decodes the encrypted word using alphabet mapping and returns the decoded word.
(defun wordMapping (word alphabetMap)
      (let ((letList (list )) (val (- (list-length word) 1)))
    	(loop for a from 0 to val
            do
            	(setq letList (append letList (list (elt alphabetMap (c2i (elt word a))))))
        )
        letList
    ) 
)

;; It is a function that decodes the encrypted paragraph using alphabet mapping and returns the decoded word.
;; This function call wordMapping function for each word.
(defun Finalparagraph (paragraph alphabetMap)
	(let((finalList (list )))
		(loop for word in paragraph
		    do
		        (setq finalList (append finalList (list (wordMapping word alphabetMap))))
		)
		finalList
	)
)

;; Creates a hash table to spell checking faster in brute force method.
;; This table is used in the "spell-checker-1" function.
;; When creating the table, it calculates the hash code of each word and adds it to the table.
(defun HashTable (filename)

	(let ((table (make-hash-table)) (in (open filename :if-does-not-exist nil)) (dictWord))
	   (when in
	      (loop for word = (read-line in nil)
	      
	      while word do
		      (progn
		      	(setq dictWord (coerce word 'string))
		      	(setf (gethash (sxhash dictWord) table) dictWord)
		      )
	      )
	      (close in)
	   )
	   table
	)
)

;; It is a function to accelerate the processes while applying the brute force method.
;; This function tries to sort the word list in the most optimal way for the decode algorithm.
;; In short, it gives priority to the word that has fewer letters to get permutations.
;; In this way, the words that are easy to mapping are given priority.
;; For words with the same number of letters required, it gives priority to the word length.
;; This reduces the number of appropriate words for this mapping in the dictionary and strengthens possibilities.
;; In short, alphabet mapping will be completed more quickly by making smaller permutations with this method.
(defun sortList (paragraph alpMap)
	(let ((newList) (list0)(list1) (list2) (list3) (list4) (list5) (list6) (sortedList (list)))

		(setq newList (sort (copy-seq paragraph) #'> :key #'list-length))

		(loop for word in newList
			do
			(cond
				((< (countLetter word alpMap) 3) (setq list0 (append list0 (list word))))
				((< (countLetter word alpMap) 4) (setq list1 (append list1 (list word))))
				((< (countLetter word alpMap) 5) (setq list2 (append list2 (list word))))
				((< (countLetter word alpMap) 6) (setq list3 (append list3 (list word))))
				((< (countLetter word alpMap) 8) (setq list4 (append list4 (list word))))
				((< (countLetter word alpMap) 10) (setq list5 (append list5 (list word))))
				((< (countLetter word alpMap) 27)  (setq list6 (append list6 (list word))))
			)
		)
		(setq sortedList(append list0 list1 list2 list3 list4 list5 list6))
		sortedList
	)
)

;; This function is used for the "Gen-Decoder-B1" function.
;; It attempts to decode the encoded paragraph by taking the encrypted paragraph and the most commonly used 6-letter alphabet mapping.
;; The general purpose is to take the cipher paragraph and return the alphabet map that is suitable for decoding.
;; Basically creates an alphabet map by appropriate word in dictionary for each word and tries for other words by going over the same alphabet map.
;; It run recursively until you find the alphabet map that fits all the words.
;; For this function to work correctly, the file must be prepared in accordance with the frequency analysis.
;; Assume that the most frequent six letters are (in the correct order) 'e', 't', 'a', ‘o’, ‘i’ and ‘n’
(defun Helper-B1 (paragraph alphabetMap dict)
	(if (= (list-length paragraph) 0) alphabetMap
		(let ((word (car paragraph)) (alpList) (res) (totallist) (flag 0))
			(if (= (countLetter word (copy-list alphabetMap)) 0) 
				(progn
					(setq res (Helper-B1 (cdr paragraph) alphabetMap dict))
					(if (equal res nil)
						()
						(progn (setq flag 1) (setq totallist res))
					)
				)
				(progn
					(setq alpList (possibleMap word (elt dict (- (list-length word) 1)) (copy-list alphabetMap)))
					(loop for newlist in alpList
						do
						(progn
							(setq res (Helper-B1 (cdr paragraph) newList dict))
							(if (equal res nil)
								()
								(progn (setq flag 1) (setq totallist res))
							)
						)
					)
				)

			)
																		
			(if (= flag 1) totallist nil)
		)
	)
)

;; Finds the encrypted word by the corresponding possible words in the dictionary.
;; According to these words are found alphabet mapping and keeps these in a list.
;; Thus, it will determine possible mapping for this encrypted word. The function returns this list.
;; It should not conflict new alphabet mapping with the existing alphabet mapping when determining the possible words for encrypted word.
;; If there is a conflict, the alphabet mapping generated for that word is not added to the list.
(defun possibleMap (word dictList alpmap)
	(let ((i) (tempmap) (charr) (flag) (newlist) (tempword))
		(loop for dictword in dictList
			do
			(progn
				(setq i 0)
				(setq tempmap (copy-list alpmap))
				(setq flag 1)
				(setq tempword (coerce dictword 'list))
				(loop for letter in tempword
					do
					(progn
						(setq charr (c2i (elt word i)))
						(if (or (char= (elt tempmap charr) #\-) (char= (elt tempmap charr) letter))
							(setf (elt tempmap charr) letter)
							(setq flag 0)
						)
						(setq i (+ i 1))
					)
				)
				(if (= flag 1) (setq newlist (append newList (list tempmap))))
			)
		)
		newList
	)
)

;; Reads the words in the dictionary file.
;; Adds these words to separate lists based on the length of the word.
;; It then returns a list of them.
(defun makeWordList (filename)
	(let ((in (open filename :if-does-not-exist nil)) (dictWord) (listWord (make-list 28)) (len))
	   (when in
	      (loop for line = (read-line in nil)
	      
	      while line do
	      	(progn
	      		(setq dictWord (coerce line 'string))
	      		(setq len (- (length dictWord) 1))
	      		(setf (elt listWord len) (append (elt listWord len) (list dictWord)))
	      	)
	      )
	      (close in)
	   )
	   listWord
	)
)

;; Randomly encrypts the paragraph.
;; To do this, he first shuffles the alphabet list, then applies this alphabet list to this paragraph.
;; Thus, the character of the letters are changed.
(defun encode (paragraph)
	(let((alpmap '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)) (res))
		(loop for i from (length alpmap) downto 2
        	do
        	(rotatef (elt alpmap (random i (make-random-state t))) (elt alpmap (1- i)))
        )
        (setq res (Finalparagraph paragraph alpmap))
        res
	)
  
)

;; -----------------------------------------------------
;; DECODE FUNCTIONS

;; For this function to work properly, all the encrypted words must be in the dictionary.
(defun Gen-Decoder-A (paragraph dictionaryName)
	;you should implement this function
	(let ((alpmap) (res nil) (table))
		(setq table (HashTable dictionaryName))
		(setq alpmap (Mapp (copy-list paragraph) table))
		(if (equal alpmap nil) () (setq res (Finalparagraph paragraph alpmap)))
		res
	)
)

;; For this function to work properly, all the encrypted words must be in the dictionary.
;; For this function to work correctly, the file must be prepared in accordance with the frequency analysis.
;; Assume that the most frequent six letters are (in the correct order) 'e', 't', 'a', ‘o’, ‘i’ and ‘n’
(defun Gen-Decoder-B-0 (paragraph dictionaryName)
  	;you should implement this function
  	(let ((countList (make-list 26 :initial-element 0)) (*alphabet* ) (*letlist*) (alpmap) (table))
  		(setq table (HashTable dictionaryName))
  		(setq *alphabet* (most6Letter (countWordInParag countList paragraph)))
  		(setq *letlist* (makeLetlist *alphabet*))
  		(setq alpmap (Mapp (copy-list paragraph) table *alphabet*))
		(if (equal alpmap nil) (setq res nil) (setq res (Finalparagraph paragraph alpmap)))
		res
  	)
)

;; For this function to work properly, all the encrypted words must be in the dictionary.
;; For this function to work correctly, the file must be prepared in accordance with the frequency analysis.
;; Assume that the most frequent six letters are (in the correct order) 'e', 't', 'a', ‘o’, ‘i’ and ‘n’
(defun Gen-Decoder-B-1 (paragraph dictionaryName)
  	;you should implement this function
  	(let ((wordList) (countList (make-list 26 :initial-element 0)) (*alphabet*) (*letlist*) (alpmap) (wordList) (temppar))
  		(setq wordList (makeWordList dictionaryName))
  		(setq *alphabet* (most6Letter (countWordInParag countList paragraph)))
  		(setq temppar (sort (copy-seq paragraph) #'> :key #'list-length))
  		(setq alpmap (Helper-B1 (copy-list temppar) *alphabet* wordList))
		(if (equal alpmap nil) (setq res nil) (setq res (Finalparagraph paragraph alpmap)))
		res
  	)
)

;; The function Code-Breaker takes an encoded document and a decoding function as input, and returns the entire document in plain text.
(defun Code-Breaker (document decoder dictionaryName)
  	;you should implement this function
  	(let ((decodedtext))
  		(setq decodedtext (funcall decoder document dictionaryName))
  		decodedtext
  	)
)

;; -----------------------------------------------------
;; Test code...
;; You can run the program by changing the file parameters(not necessary).
;; document1.txt is my test document
;; Also, you can try document2.txt or document3.txt
;; dictionary2.txt is big dictionary with 45000 words.
(defun test_on_test_data ()
	(terpri)
	(print "....................................................")
	(print "Testing ....")
	(print "....................................................")
	(let ((doc (read-as-list "document1.txt")) (encodedDoc) (decodedDoc-A) (decodedDoc-B-0) (decodedDoc-B-1) (dictionary))
		(setq dictionary "dictionary2.txt")

		(terpri) (terpri)
		(print "ENCODED DOCUMENT:")
		(setq encodedDoc (encode doc))
		(print encodedDoc)

		(terpri) (terpri)
		(print "Gen-Decoder-A:")
		(terpri)
		(setq decodedDoc-A (Code-Breaker encodedDoc #'Gen-Decoder-A dictionary))
		(if (equal decodedDoc-A nil) (print "NOT FOUND!") (print decodedDoc-A))

		(terpri) (terpri)
		(print "Gen-Decoder-B-0:")
		(terpri)
		(setq decodedDoc-B-0 (Code-Breaker encodedDoc #'Gen-Decoder-B-0 dictionary))
		(if (equal decodedDoc-B-0 nil) (print "NOT FOUND!") (print decodedDoc-B-0))

		(terpri) (terpri)
		(print "Gen-Decoder-B-1:")
		(terpri)
		(setq decodedDoc-B-1 (Code-Breaker encodedDoc #'Gen-Decoder-B-1 dictionary))
		(if (equal decodedDoc-B-1 nil) (print "NOT FOUND!") (print decodedDoc-B-1))

		(terpri) (terpri)
		(print "		PROGRAM IS FINISHED			")
	)
)

;;;;;;; TEST FUNCTIONS
;;;; These are test functions.
;;;; It will not work unless you call.
;;;; Function calls are in the comment line at the bottom.
;;;; You can use these functions to configure and test your own test cases.

(defun test-read-as-list ()
	(let ((doc "document1.txt"))
		(terpri)
		(print "Test Read As List ....") (terpri) (terpri)
		(write "Document: ") (terpri)
		(write (read-as-list doc)) (terpri) (terpri)
	)
)

(defun test-spell-checker-0 ()
	(let ((word1 "hello") (word2 "furkan"))
		(terpri)
		(print "Test Spell Check 0 ....") (terpri) (terpri)
		(write "Word: ") (write word1) (terpri) (write "Result: ") (write (spell-checker-0 word1)) (terpri) (terpri)
		(write "Word: ") (write word2) (terpri) (write "Result: ") (write (spell-checker-0 word2)) (terpri) (terpri)
	)
)

(defun test-spell-checker-1 ()
	(let ((word1 "hello") (word2 "furkan") (dictionary "dictionary2.txt") (table))
		(terpri)
		(setq table (HashTable dictionary))
		(print "Test Spell Check 1 ....") (terpri) (terpri)
		(write "Word: ") (write word1) (terpri) (write "Result: ") (write (spell-checker-1 word1 table)) (terpri) (terpri)
		(write "Word: ") (write word2) (terpri) (write "Result: ") (write (spell-checker-1 word2 table)) (terpri) (terpri)
	)
)

(defun test-countWord ()
	(let ((word1 "hello") (word2 "furkan") (countList (make-list 26 :initial-element 0)))
		(terpri)
		(print "Test Count Word ....") (terpri) (terpri)
		(write "Word: ") (write word1) (terpri) (write "Result: ") (write (countWord (copy-list countList) (coerce word1 'list))) (terpri) (terpri)
		(write "Word: ") (write word2) (terpri) (write "Result: ") (write (countWord (copy-list countList) (coerce word2 'list))) (terpri) (terpri)
	)
)

(defun test-countWordInParag ()

	(let ((paragraph '((#\h #\e #\l #\l #\o) (#\t #\h #\i #\s) (#\i #\s) (#\a) (#\t #\e #\s #\t))) (countList (make-list 26 :initial-element 0)))
		(terpri)
		(print "Test Count Word In Paragraph ....") (terpri) (terpri)
		(write "Paragraph: ") (write paragraph) (terpri) (write "Result: ") (write (countWordInParag (copy-list countList) paragraph)) (terpri) (terpri)
	)
)

(defun test-most6Letter ()

	(let ((paragraph '((#\h #\e #\l #\l #\o) (#\t #\h #\i #\s) (#\i #\s) (#\a) (#\t #\e #\s #\t))) (countList (make-list 26 :initial-element 0)))
		(terpri)
		(setq countList (countWordInParag (copy-list countList) paragraph))
		(print "Test Most 6 Letter ....") (terpri) (terpri)
		(write "Paragraph: ") (write paragraph) (terpri) (write "Result: ") (write (most6Letter (copy-list countList))) (terpri) (terpri)
	)
)

(defun test-permu ()

	(let ((testList '(1 2 3)))
		(terpri)
		(print "Test Permutation ....") (terpri) (terpri)
		(write "List: ") (write testList) (terpri) (write "Result: ") (write (permu testList)) (terpri) (terpri)
	)
)

(defun test-countLetter ()
	(let ((word1 "hello") (word2 "furkan") (alpmap (make-list 26 :initial-element #\-)))
		(terpri)
		(print "Test Count Letter ....") (terpri) (terpri)
		(write "Word: ") (write word1) (terpri) (write "Result: ") (write (countLetter (coerce word1 'list) (copy-list alpmap))) (terpri) (terpri)
		(write "Word: ") (write word2) (terpri) (write "Result: ") (write (countLetter (coerce word2 'list) (copy-list alpmap))) (terpri) (terpri)
	)
)

(defun test-Mapp ()

	(let ((paragraph '((#\h #\u #\n #\g) (#\c #\a #\b) (#\h #\i #\g #\h) (#\y #\a #\g #\i) )) (dictionary "dictionary2.txt") (table) (encoded) (res))
		(terpri)
		(setq encoded (encode (copy-list paragraph)))
		(setq table (HashTable dictionary))
		(setq res (Mapp encoded table))
		(print "Test Mapping ....") (terpri) (terpri)
		(write "Encoded Paragraph: ") (write encoded) (terpri) (write "Result Map: ") (write (Mapp encoded table)) (terpri) (terpri)
		(write "Decoded Paragraph: ") (write (Finalparagraph encoded res)) (terpri) (terpri)
	)
)

(defun test-makeLetlist ()

	(let ((alpmap '(#\u #\n #\a #\- #\- #\- #\g #\- #\- #\- #\- #\- #\h #\- #\y #\- #\i #\- #\c #\- #\- #\- #\- #\- #\b #\-)))
		(terpri)
		(print "Test Make Let List ....") (terpri) (terpri)
		(write "Alphabet Map: ") (write alpmap) (terpri) (write "Result: ") (write (reverse (makeLetlist alpmap))) (terpri) (terpri)
	)
)

(defun test-alphMap ()
	(let ((word '(#\x #\y #\z #\z #\h)) (mapp (make-list 26 :initial-element #\-)) (wordMap '(7 4 11 14)) (res))
		(terpri)
		(setq res (alphMap word wordMap mapp))
		(print "Test Alphabet Mapping ....") (terpri) (terpri)
		(write "Encoded Word: ") (write word) (terpri) (write "Word Map: ") (write wordMap) (terpri) (write "Result: ") (write res) (terpri)
		(write "Decoded Word: ") (write (Finalparagraph (list word) res)) (terpri) (terpri)
	)
)

(defun test-wordMapping ()
	(let ((word '(#\x #\y #\z #\z #\h)) (mapp '(#\- #\- #\- #\- #\- #\- #\- #\o #\- #\- #\- #\- #\- #\- #\- #\- #\- #\- #\- #\- #\- #\- #\- #\h #\e #\l)))
		(terpri)
		(print "Test Word Mapping ....") (terpri) (terpri)
		(write "Encoded Word: ") (write word) (terpri) (write "Alphabet Map: ") (write mapp) (terpri) (write "Decoded Word: ") (write (wordMapping word mapp)) (terpri) (terpri)
	)
)

(defun test-Finalparagraph ()
	
	(let ((paragraph '((#\j #\k #\b #\g) (#\q #\v #\a) (#\j #\e #\g #\j) (#\i #\v #\g #\e))) (mapp '(#\b #\n #\- #\- #\i #\- #\g #\- #\y #\h #\u #\- #\- #\- #\- #\- #\c #\- #\- #\- #\- #\a #\- #\- #\- #\-)))
		(terpri)
		(print "Test Final Paragraph ....") (terpri) (terpri)
		(write "Encoded Paragraph: ") (terpri) (write paragraph) (terpri) (write "Alphabet Map: ") (terpri) (write mapp) (terpri) (write "Decoded Paragraph: ") (write (Finalparagraph paragraph mapp)) (terpri) (terpri)
	)

)

(defun test-HashTable ()
	(let ((filename "dictionary2.txt"))
		(print "Test Hash Table ....") (terpri) (terpri)
		(print "Result: ") (terpri) (write (HashTable filename)) (terpri) (terpri)
	)
)

(defun test-sortList ()
	(let ((paragraph '((#\j #\k #\b #\g #\a) (#\q #\v #\a) (#\j #\e #\g #\j) (#\i #\v #\g #\e))) (mapp (make-list 26 :initial-element #\-)))
		(terpri)
		(print "Test Sort List ....") (terpri) (terpri)
		(write "Paragraph: ") (terpri) (write paragraph) (terpri) (write "Alphabet Map: ") (terpri) (write mapp) (terpri) (write "Sorted Paragraph: ") (write (sortList paragraph mapp)) (terpri) (terpri)
	)
)

(defun test-encode ()
	(let ((paragraph '((#\h #\e #\l #\l #\o) (#\t #\h #\i #\s) (#\i #\s) (#\a) (#\t #\e #\s #\t))))
		(terpri)
		(print "Test Encode ....") (terpri) (terpri)
		(write "Paragraph: ") (terpri) (write paragraph) (terpri) (write "Encoded Paragraph: ") (terpri) (write (encode paragraph)) (terpri) (terpri)
	)
)

(defun test-makeWordList ()
	(let ((filename "dictionary2.txt"))
		(print "Test Make Word List ....") (terpri) (terpri)
		(print "Result: ") (terpri) (write (makeWordList filename)) (terpri) (terpri)
	)
)


;; Program will start.
(test_on_test_data)

;You can operate the following test functions as desired.
;Before executing these test functions, configure code and run them according to your desire.

;(test-read-as-list)
;(test-spell-checker-0)
;(test-spell-checker-1)
;(test-countWord)
;(test-countWordInParag)
;(test-most6Letter)
;(test-permu)
;(test-countLetter)
;(test-Mapp)
;(test-makeLetlist)
;(test-alphMap)
;(test-wordMapping)
;(test-Finalparagraph)
;(test-HashTable)
;(test-sortList)
;(test-encode)
;(test-makeWordList)