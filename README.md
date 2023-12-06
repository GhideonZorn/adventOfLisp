# adventOfLisp
Making advent of code in Common Lisp to learn this language.

# Year 2022

I did a part of advent of code in python this year. As I was learning
a little about lisp in my school, I tried to do the first days in Lisp.
However, it was quite more a traduction python -> lisp than a reflexion
of how to resolve each problem in lisp.

# Year 2023

For 2023 I took the challenge to try to finish advent of code in Lisp.
I use SBCL (Steel Bank Common Lisp) compiler and some lisp packages that
will ease my task.

## Day 1

The first part was easy, I just had to recall myself Lisp syntax.
I found the second part quite annoying for a first day  as I had to
parse the string to found some words and I had no idea how to it in lisp.

## Day 2

I could do a lot of factoring but I don't know how.
I begin to read 'Land of Lisp' of **Conrad Barski** and hope I
will found new ways to design my solutions.

## Day 3

Part 1 was easy in lisp but damnit I screamed for the parsing
for part 2.

I'm quite happy for my manipulation of my final list with
special removing, sorting and reduce using lambda but I know
I don't get well the informations from the input file which
results in a parsing/analyzing function quite ugly 
(check-line and get-numbers-with-gear).

I will search more efficient ways to treat my input files for
the next days.

## Day 4

Quite a fun day. I use an extern library to parse my file and it
was a lot more easier.
I took time to understand that the part 2 was counting the total of
cards (original + copies) and not of points...

## Day 5

Part 2 took me some times to understand how to do it quite clean.
I choose to start from the end, and compute every location and 
check if it gives a valid seed.
I find this AOC is harder that the 2022 AOC.

## Day 6

A fun day, more easier than the 5. 
I like the use of the cl-ppcre library to parse the inputs
