Netids (include all members of group, if parternering):
  yechenh, lt7

Report your work and span numbers:
----------------------------------
Fill in the ? with your actual numbers.

Running: main_index.native data/test_index_1000.txt for a year
make_index work=15558288 span=11378
search work=10212454 span=40

Running: population.native data/CenPop2010.txt 20 40 26 4 30 7
slow:precompute work=660999 span=39
slow:compute work=220333 span=19
fast:precompute work=14540392 span=822
fast:compute work=8 span=8

Analyze your work and span numbers:
----------------------------------
Fill in the ? with an asymptotic complexity formula (see instructions).

N = total number of words in all documents
make_index work=O(NlogN) span=O(log^2 N)

N = length of the query string
M = average number of occurences of each query word in all documents
search work=O(N * M^2) span=O(logN * logM)


N = length of groups (i.e. the length of the census data)
slow:precompute work=O(N) span=O(log N)
slow:compute work=O(N) span=O(log N)
fast:precompute work=O(NlogN) span=O(log^2 N)
fast:compute work=O(1) span=O(1)

Comments, Problems and Design Decisions:
----------------------------------------
We have also implemented a more brute-force or sequential version of making
a one dimensional grid in sequence.ml. The function has the name 
make_one_dim_grid_v2. Using this function, we get the same result as the 
reference and our other implementation, but the work and span is different.
This bruteforce version has 

precompute work=353417472 span=81
compute work=8 span=8

which has a much smaller span than our other implementation (the implementation
suggested by the assignment specification). However, the work is much larger
than our other implementation. In general, using the time complexity as above, 
its complexity would be 

N = length of groups (i.e. the length of the census data)
R = number of rows
C = number of columns
precompute work=O(N*R*C) span=O(log N)
compute work=O(1) span=O(1)

which explains why this bruteforce implementation has smaller span and larger
work. We commented out this implementation, although it also uses the Sequence 
library and is also parallel, simply because it does not follow what is 
suggested in the assignment specification and it is a more bruteforce way
of forming a one-dimensional grid.


Suggestions for the Future or Random Comments:
----------------------------------------------





In doing this homework I used the following sources:

1. Sources I don't need to mention   [see notes 1 and 4]
2. Authorized sources  [see notes 2 and 4]
   [list here]
3. Unauthorized sources [see notes 3 and 4]
   [list here]

This paper represents my own work in accordance with University regulations.

Signed, [your name(s):] Yechen Hu, Lauren Tange

------------------------------------------------------------------------

NOTE 1:   Sources you don't need to mention
  (unless you paste code from them verbatim) include: 
  this semester's lectures and precepts,  the course web site, 
  the assignment handout (download), Real World OCaml, and the OCaml manual.

NOTE 2:  Authorized sources include:
    professors and preceptors (outside of lecture or precept);
    advice from other students (but not looking at their solutions
    to this course's homeworks);
    other books, and (within reason) web sites such as stackoverflow.com
    (for general questions about OCaml bugs, not for solutions to
    the homework).
  List by name, by URL, or by bibliographic citation, as appropriate.

NOTE 3:  "Why would I list an unauthorized source?"
  Using an unauthorized source without citing it is an Academic Violation
  under Princeton University's disciplinary code, and can result
  in suspension from the University for one or more semesters.

  Using an unauthorized source and citing it clearly is not an Academic
  Violation, it is "merely" a violation of this homework's instructions,
  and can result in (at most) getting a zero on this homework.

  Unauthorized sources include, at least: other people's solutions
  to these (or similar) homework problems.

NOTE 4.  If you paste in code from from these sites,
  you should clearly cite it at the point of use, in accordance with
  Section 2.4.6 of RRR  for "direct quotation or extensive paraphrase".
  https://rrr.princeton.edu/students#comp246
  Please limit the amount of this that you do in accordance with the
  principle that the purpose of these homeworks is so that you
  can learn how to do things yourself.
