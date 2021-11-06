% -*- mode: LaTeX; compile-command: "stack runhaskell --package shake -- Shake.hs" -*-

\documentclass{book}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% lhs2TeX formatting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% XXX switch to monospace font, with nice symbols?
%include polycode.fmt

%format >>> = ">\!\!>\!\!>"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Packages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\usepackage{hyperref}
\usepackage{url}
\usepackage{xcolor}

\hypersetup{
    colorlinks,
    linkcolor={green!50!black},
    citecolor={blue!50!black},
    urlcolor={blue!80!black}
}

\usepackage{graphicx}
\graphicspath{{images/}}

\usepackage{etoolbox}
\usepackage{mdframed}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Kattis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand{\kattislogo}{\raisebox{-0.2em}{\includegraphics[height=0.9\baselineskip]{Kattis}}}

\newcommand{\kattislist}[1]{%
  \def\nextitem{\def\nextitem{, }}
  \renewcommand*{\do}[1]{\nextitem\kattislink{##1}}
  \docsvlist{#1}
}

\newcommand{\kattis}[1]{%
  \begin{mdframed}
    \kattislogo
    \kattislist{#1}
  \end{mdframed}
}

\newcommand{\kattislink}[1]{\href{https://open.kattis.com/problems/#1}{\texttt{#1}}}

\newcommand{\inlinekattis}[1]{\kattislogo\!\!\kattislist{#1}\!}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Title page
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\title{Competitive Programming in Haskell}
\author{Brent A. Yorgey}

\begin{document}

\maketitle

\tableofcontents

\chapter{Introduction}
\label{chap:intro}

\chapter{Getting Started}
\label{chap:getting-started}

As a basic running example, we will consider the same example problem
that Open Kattis uses in its \href{https://open.kattis.com/help}{help
  section}, A Different Problem \mbox{(\inlinekattis{different})}.  In
this problem, we are told that the input will consist of a number of
pairs of integers between $0$ and $10^{15}$, one pair per line, and we
should output the absolute value of the difference between each
pair. The given example is that if the input looks like this:
\begin{verbatim}
10 12
71293781758123 72784
1 12345677654321
\end{verbatim}
then our program should produce output that looks like this:
\begin{verbatim}
2
71293781685339
12345677654320
\end{verbatim}

This is of course an extremely simple problem, so we can just focus on
the mechanics of solving a problem in Haskell.

\subsection*{Competitive programming is functional}

You can see that this problem specifies an \emph{input} in a
particular format (which will be provided via our program's standard
input), and requires us to produce a specified \emph{output} (via
standard output).  Most competitive programming problems are of this
form.\footnote{There certainly can exist problems requiring
  interaction beyond standard input and output, such as reading from a
  certain file, doing network I/O, and so on, but we will not consider
  such problems in this book.} An imperative approach to such a
problem involves doing a sequence of input commands, some computation,
and a sequence of output commands---possibly interleaved with one
another---and we might immediately think to start using functions like
|getLine| and |putStrLn| to do the required I/O in Haskell.  However,
there is a much more fruitful functional perspective: we are simply
being asked to implement a particular (partial) function of type
|String -> String|.\footnote{If you are worried about the use of
  |String|, fear not: in Chapter XXX, once we develop a simple parsing
  framework, we will switch to |ByteString| for efficiency.}  The fact
that the function's input and output should be hooked up to the
program's standard input and output is just an implementation detail.
Competitive programming is functional at heart.

It turns out that Haskell's standard library already has the perfect
built-in function for this scenario:
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
that we need not worry about storing the entire input in memory at
once.

Thus, the |interact| function lets us immediately pass to a functional
view of a problem, worrying only about the essential details of
transforming the given input into the requested output.  This is the
last time |IO| will appear in this book!

\subsection*{A basic solution pipeline}

So now we need to write a pure function which transforms the input
into the output.  Of course, in true Haskell fashion, we will do this
by constructing a chained pipeline of functions to do the job
incrementally.  The general plan of attack (for any problem) is
as follows:
\begin{enumerate}
\item First, parse the input, that is, transform the raw input
  into some more semantically meaningful representation.
\item Next, solve the problem, turning a semantically meaningful
   representation of the input into a semantically meaningful
   representation of the output.
\item Finally, format the output into a |String|.
\end{enumerate}
Figure~\ref{fig:skeleton-different} has a simple skeleton solution along
these lines.

\begin{figure}
  \centering
\begin{code}
  import Control.Arrow ((>>>))

  main = interact $ parse >>> map solve >>> format

  parse :: String -> [[Integer]]
  parse = _

  solve :: [Integer] -> Integer
  solve = _

  format :: [Integer] -> String
  format = _
\end{code}
% $
  \caption{A skeleton solution for A Different Problem}
  \label{fig:skeleton-different}
\end{figure}

There are a few things to point out.

\begin{enumerate}
\item Notice the use of the backwards function composition operator
  |(>>>)| from |Control.Arrow| (essentially, |f >>> g = g . f|,
  although in actuality |(>>>)| is a bit more general than that).
  When solving programming problems, I like to be able to type
  function pipelines from left to right as I think about data flowing
  through the pipeline from beginning to end.  This is of course a
  personal preference, and one could also write |format . solve
  . parse| instead of |parse >>> solve >>> format|.
\item If the machine on which our solution will run has a 64-bit
  architecture (this is always true for Open Kattis, but not
  necessarily so for other platforms such as Codeforces), for this
  problem we could technically get away with using |Int| instead of
  |Integer|. On a 64-bit architecture, |maxBound :: Int| is
  $2^{63} - 1$, which is a bit more than $9 \times 10^{18}$, plenty
  big enough for inputs up to $10^{15}$. For more computationally
  intensive problems, using |Int| instead of |Integer| can be an
  important optimization; however, for simple problems, using
  |Integer| is preferred since it eliminates the potential for bugs
  due to overflow.

\item In simple problems such as this, the |solve| function could
  probably be inlined (it's a fun challenge to solve easy problems in
  a single line of code!).  However, I prefer to split it out
  explicitly in order to specify its type, which both prevents
  problems with |read|/|show| ambiguity, and also serves as a sanity
  check on the parsing and formatting code.

\item And one last thing: I said we were going to parse the input into
  a ``semantically meaningful representation'', but I lied a teensy
  bit: the problem says we are going to get a \emph{pair} of integers,
  but the type of |solve| says that it takes a \emph{list} of
  integers.  This is a bigger discussion XXX see later.
\end{enumerate}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




\subsection*{Parsing and formatting}


\subsection*{Using partial functions}

You might wonder about using a list as the output of |parse| and the
input of |solve|.  Shouldn't we use something like |(Int,Int)| since
we know we will be given exactly two integers? XXX paste in blog post
about this


  The fact is that I almost never use actual Haskell tuples in my
  solutions, because they are too awkward and
  inconvenient. Representing homogeneous tuples as Haskell lists of a
  certain known length allows us to read and process ``tuples'' using
  standard functions like |words| and |map|, to combine them using
  |zipWith|, and so on.  And since we get to assume that the input
  always precisely follows the specification---which will never
  change---this is one of the few situations where, in my opinion, we
  are fully justified in writing partial functions like this if it
  makes the code easier to write.  So I always represent homogeneous
  tuples as lists and just pattern match on lists of the appropriate
  (known) length.  (If I need heterogeneous tuples, on the other hand,
  I create an appropriate |data| type.)

XXX practice problems

* [Job Expenses](https://open.kattis.com/problems/jobexpenses)
* [Judging Moose](http://open.kattis.com/problems/judgingmoose)
* [Quick Estimate](http://open.kattis.com/problems/quickestimate)

Of course you can also try solving any of the other problems (as of
this writing, over 2400 of them!) on Kattis as well.

\chapter{Wholemeal Programming}
\label{chap:wholemeal}

XXX already touched on this a bit in previous section re: interact,
lazy IO, etc.

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
