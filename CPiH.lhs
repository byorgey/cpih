% -*- mode: LaTeX; compile-command: "stack runhaskell --package shake -- Shake.hs" -*-

\documentclass{book}

%include polycode.fmt

%format >>> = ">\!\!>\!\!>"

\title{Competitive Programming in Haskell}
\author{Brent A. Yorgey}

\begin{document}

\maketitle

\tableofcontents

\chapter{Introduction}
\label{chap:intro}

\chapter{Basics}
\label{chap:basics}

As a running example, we will consider the problem
\url{https://open.kattis.com/problems/twosum}, by Johann Sannemo:

\begin{quote}
Per-Magnus is trying to add two integers, but he never learned how to.

Write a program to help him with this most difficult task!

\textbf{Input} \\
The input consists of a single line with two integers $0 \leq a \leq
1000$ and $0 \leq b \leq 1000$.

\textbf{Output} \\
Output a single integer, the sum $a+b$.

Sample Input 1	Sample Output 1
1 1
2
Sample Input 2	Sample Output 2
2 2
4
\end{quote}

\subsection*{Competitive programming is functional}

You can see that this problem specifies an \emph{input} in a
particular format (which will be provided on the standard input of our
program), and requires us to produce a specified \emph{output} (via
standard output).  Most competitive programming problems are of this
form.  There certainly do exist problems requiring interaction beyond
standard input and output, such as reading from a certain file, doing
network I/O, and so on, but we will not consider such problems in this
book.  An imperative approach to such a problem involves doing a
sequence of input commands, some computation, and a sequence of output
commands, possibly all interleaved with one another. However, the
functional perspective is much nicer: we are simply being asked to
implement a particular (partial) function of type |String -> String|!
The fact that the function's input and output should be on the
program's standard input and output is just an implementation detail.
Competitive programming is functional at heart.

In fact, Haskell already has the perfect built-in function for this scenario:
\begin{code}
interact :: (String -> String) -> IO ()
\end{code}
|interact| takes a pure |String -> String| function and turns it into
an |IO ()| action which reads from standard input, passes the input to
the given function, and prints the result to standard output.  It even
does this using \emph{lazy} I/O, which can be strange and problematic
in some scenarios, but is perfect for this situation: the input is
read lazily, as demanded by the function, so that the output and input
can be automatically interleaved depending on which parts of the
output depend on which parts of the input. In particular, this means
that (unless required by the logic of the problem) we need not read
the entire input into memory at once.

Thus, the |interact| function lets us immediately pass to a functional
view of a problem, worrying only about the essential details of
transforming the given input into the requested output.  This is the
last time |IO| will appear in this book!

\subsection*{A basic solution pipeline}

With this in mind, we can write the basic skeleton of a solution.
Typically, we will compose the solution out of three phases:
\begin{itemize}
\item a function to |parse| the initial |String| into a more structured
  representation of the input,
\item a function to |solve| the problem,
  producing a structured representation of the requested output, and
\item a function to |format| the output to a |String|.
\end{itemize}

For our example problem, it might look something like this:
\begin{code}
  import Control.Arrow ((>>>))

  main = interact $ parse >>> solve >>> format

  parse :: String -> [Int]
  parse = _

  solve :: [Int] -> Int
  solve = _

  format :: Int -> String
  format = show
\end{code}

XXX write about use of |>>>|


\subsection*{Parsing and formatting}



\chapter{Wholemeal Programming}
\label{chap:wholemeal}

\chapter{Trees}
\label{chap:trees}

\chapter{Monoids}
\label{chap:monoids}

\chapter{Data Structures}
\label{chap:data-structures}

\chapter{Dynamic Programming}
\label{chap:dp}

\chapter{Number Theory}
\label{chap:number-theory}

\chapter{Combinatorics}
\label{chap:combinatorics}

\chapter{Geometry}
\label{chap:geometry}

\chapter{Graphs}
\label{chap:graphs}

\chapter{Strings}
\label{chap:strings}

\chapter{Mutability}
\label{chap:mutability}

\chapter{Miscellaneous Topics}
\label{chap:misc}




\end{document}
