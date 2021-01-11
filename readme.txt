3 test files were prepared for the program.
These test files are document1.txt, document2.txt and document3.txt.
You can run the program with any of these test files.
If you want change test file, You need to modify the document file name in the "test_on_test_data ()" function.


Dictionary file is dictionary2.txt.
This dictionary is the largest dictionary and contains about 45 thousand words.
Test documents are prepared according to this dictionary.
You can run the program with other dictionary and document files.
If you want change dictionary and document file, You need to modify the dictionary and document file name in the "test_on_test_data ()" function.


!!! If you are going to try your own test file, there are a few things to consider: !!!
The high number of words increases the accuracy. So keep as many words as possible.
Having different lengths of words speeds up the program's work. For example, you can have some short words.
For Gen-Decoder functions to work properly, all the encrypted words must be in the dictionary.
For Gen-Decoder-B functions to work correctly, the file must be prepared in accordance with the frequency analysis.
The most frequent six letters of file must be (in the correct order) 'e', 't', 'a', ‘o’, ‘i’ and ‘n’.
There should be certainty about the six most letters, because some equality can lead to wrong results.

Because there are too many words in the dictionary, there may be more than one appropriate result for the Gen-Decoder-B-1 function.
For example, it print "poker" instead of "joker" for only 1 word in paragraph.
Only the first result of appropriate results will be printed on the screen.
This does not indicate that the function was operating incorrectly.

You can see the paragraph by opening the file.
The paragraph is randomly encrypted within the program.

There are test functions.
It will not work unless you call.
Function calls are in the comment line at the bottom.
You can use these functions to configure and test your own test cases.

You can get an idea of the algorithm by reading the comment lines.


FURKAN OZEV 161044036