% -*- mode: LaTeX; compile-command: "stack runhaskell --package shake -- Shake.hs" -*-

\documentclass{book}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% lhs2TeX formatting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% XXX switch to monospace font, with nice symbols?
% XXX switch to just using minted?
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

\usepackage{prettyref}

\usepackage{minted}

\usepackage{todonotes}

\usepackage[style=authoryear]{biblatex}
\addbibresource{references.bib}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% prettyref
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newrefformat{fig}{Figure~\ref{#1}}
\newrefformat{sec}{section~\ref{#1}}
\newrefformat{chap}{Chapter~\ref{#1}}
\newrefformat{eq}{equation~\eqref{#1}}
\newrefformat{prob}{Problem~\ref{#1}}
\newrefformat{tab}{Table~\ref{#1}}
\newrefformat{thm}{Theorem~\ref{#1}}
\newrefformat{lem}{Lemma~\ref{#1}}
\newrefformat{prop}{Proposition~\ref{#1}}
\newrefformat{defn}{Definition~\ref{#1}}
\newrefformat{cor}{Corollary~\ref{#1}}
\newrefformat{ex}{Exercise~\ref{#1}}
\newrefformat{alg}{Algorithm~\ref{#1}}

\newcommand{\pref}[1]{\prettyref{#1}}

% \Pref is just like \pref but it uppercases the first letter; for use
% at the beginning of a sentence.
\newcommand{\Pref}[1]{%
  \expandafter\ifx\csname r@@#1\endcsname\relax {\scriptsize[ref]}
    \else
    \edef\reftext{\prettyref{#1}}\expandafter\MakeUppercase\reftext
    \fi
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Misc semantic markup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand{\module}[1]{\emph{#1}}
\newcommand{\pkg}[1]{\texttt{#1}}
\newcommand{\term}[1]{\emph{#1}}

\newcommand{\h}[1]{\mintinline{haskell}{#1}}

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

\section{Competitive programming is functional}
\label{sec:cp-functional}

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
\h{getLine} and \h{putStrLn} to do the required I/O in Haskell.  However,
there is a much more fruitful functional perspective: we are simply
being asked to implement a particular (partial) function of type
\h{String -> String}.\footnote{If you are worried about the use of
  \h{String}, fear not: in Chapter XXX, once we develop a simple parsing
  framework, we will switch to \h{ByteString} for efficiency.}  The fact
that the function's input and output should be hooked up to the
program's standard input and output is just an implementation detail.
Competitive programming is functional at heart.

It turns out that Haskell's standard library already has the perfect
built-in function for this scenario:
\begin{code}
interact :: (String -> String) -> IO ()
\end{code}
\h{interact} takes a pure \h{String -> String} function and turns it into
an \h{IO} action which reads from standard input, passes the input to
the given function, and prints the result to standard output.  It even
does this using \emph{lazy} I/O, which can be strange and problematic
in some scenarios, but is perfect for this situation: the input is
read lazily, as demanded by the function, so that the output and input
can be automatically interleaved depending on which parts of the
output depend on which parts of the input. In particular, this means
that that the entire input need not be stored in memory at once. If
the inputs can be processed into outputs in a streaming fashion---as
is the case in the example problem we are currently
considering---then the input and output will be interleaved.

Thus, the \h{interact} function lets us immediately pass to a functional
view of a problem, worrying only about the essential details of
transforming the given input into the requested output.  This is the
last time \h{IO} will appear in this book! \todo{Check this---maybe I
  want to write about interactive problems.}

\section{A basic solution pipeline}
\label{sec:pipeline}

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
\item Finally, format the output into a \h{String}.
\end{enumerate}
Figure~\ref{fig:skeleton-different} has a simple skeleton solution along
these lines. There are a few things to point out.

\begin{figure}
  \centering
\begin{minted}{haskell}
  import Control.Arrow ((>>>))

  main = interact $ parse >>> map solve >>> format

  parse :: String -> [[Integer]]
  parse = _

  solve :: [Integer] -> Integer
  solve = _

  format :: [Integer] -> String
  format = _
\end{minted}
% $
  \caption{A skeleton solution for A Different Problem}
  \label{fig:skeleton-different}
\end{figure}

\begin{itemize}
\item Notice the use of the backwards function composition operator
  \h{(>>>)} from \h{Control.Arrow} (essentially, \h{f >>> g = g . f},
  although in actuality \h{(>>>)} is a bit more general than that).
  When solving programming problems, I like to be able to type
  function pipelines from left to right as I think about data flowing
  through the pipeline from beginning to end.  This is of course a
  personal preference, and one could also write%
  \h{format . solve . parse} instead of%
  \h{parse >>> solve >>> format}.
\item If the machine on which our solution will run has a 64-bit
  architecture (this is always true for Open Kattis, but not
  necessarily so for other platforms such as Codeforces), for this
  problem we could technically get away with using \h{Int} instead of
  \h{Integer}. On a 64-bit architecture, \h{maxBound :: Int} is $2^{63} -
  1$, which is a bit more than $9 \times
  10^{18}$, plenty big enough for this problem, with inputs only up to
  $10^{15}$. For more computationally intensive problems, using \h{Int}
  instead of \h{Integer} can be an important optimization; however, for
  simple problems, using \h{Integer} is preferred since it eliminates
  the potential for bugs due to overflow.

\item In simple problems such as this, the \h{solve} function could
  probably be inlined (it can be a fun challenge to solve easy
  problems in a single line of code!).  However, in general, I prefer
  to split it out explicitly in order to specify its type, which both
  prevents problems with \h{read}/\h{show} ambiguity, and also serves as a
  sanity check on the parsing and formatting code.

\item And one last thing: I said we were going to parse the input into
  a ``semantically meaningful representation'', but I lied a teensy
  bit: the problem says we are going to get a \emph{pair} of integers,
  but the type of \h{solve} says that it takes a \emph{list} of
  integers.  We will discuss and justify this choice in more detail in
  \pref{sec:partial}.
\end{itemize}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Parsing and formatting}
\label{sec:parse-format}

The outer shell of our basic solution pipeline deals with converting
between the raw strings used as input and output, and more
semantically meaningful representations.  In many cases, it suffices
to use basic list-processing functions from \module{Prelude} or
\module{Data.List}.

\begin{itemize}
\item For parsing input, some of the most frequently useful functions
  include \h{lines}, \h{words}, \h{drop}, and \h{read}.  The \h{split} function
  from the \h{Data.List.Split} module (from the \pkg{split}
  package) can also be helpful.
\item For formatting output, some of the most frequently useful
  functions include \h{unlines}, \h{unwords}, \h{show}, \h{concat},
  \h{intersperse}, and \h{intercalate}.
\end{itemize}

If any of these functions are unfamiliar to you, it's worth spending
some time reading their documentation and trying them on some
examples.  In \pref{chap:parsing} we will consider more sophisticated
tools for parsing in particular, but until then we will stick to
problems that only require these basic tools.  Fortunately, there are
many such problems.

Returning to our running example solving A Different Problem, the
given input consists of a number of lines, each containing two
numbers.  This is easily parsed using a combination of \h{lines},
\h{words}, \h{read}, and \h{map} (\pref{fig:parse-different}).

\begin{figure}
  \centering
\begin{minted}{haskell}
  parse :: String -> [[Integer]]
  parse = lines >>> map (words >>> map read)
\end{minted}
  \caption{\h{parse} function for A Different Problem}
  \label{fig:parse-different}
\end{figure}

As another example, consider the problem Pot
\mbox{(\inlinekattis{pot})}.  The input description for this problem says

\begin{quote}
  The first line of input contains the integer $N (1 \leq N \leq 10)$, the number
  of the addends from the task. Each of the following $N$ lines contains
  the integer $P_i$ ($10 \leq P_i \leq 9999$, $i = 1, \dots, N$) from the task.
\end{quote}

At first glance, it looks like we need a more sophisticated parsing
framework for this problem: the first line is a number which tells us
\emph{how many} additional lines of input we need to parse.  The need
for intermediate values to be able to affect later parsing is exactly
what necessitates the use of a \emph{monadic} parsing framework.
However, in this case, we can do something much simpler: just ignore
the number entirely, and process all the lines besides the first!  We
don't actually need the number to tell us how many lines to expect
since the end of the input is delimited by EOF.

\begin{minted}{haskell}
parse :: String -> [Integer]
parse = lines >>> drop 1 >>> map read
\end{minted}

This approach often works. For example, even if we have a variable
number of lines, each of which has a variable number of items (with a
number at the beginning saying how many), we can ignore all the
counts: just split into lines, drop the first line, and then split
each line into words, dropping the first.  Besides the lines being
delimited by EOF overall, each individual line is delimited by the end
of line character, so we don't need XXX.  It doesn't work when we
really have no way to tell when a certain section ends other than
using the number at the beginning. For example, XXX multiple sections
with variable numbers of lines.  We'll get to that in chapter XXX.

\section{Using partial functions}
\label{sec:partial}

You might wonder about using a list as the output of \h{parse} and the
input of \h{solve}.  Shouldn't we use something like \h{(Int,Int)}, since
we know we will be given exactly two integers? XXX paste in blog post
about this.  Note you can skip if you're not interested in the
philosophy

  The fact is that I almost never use actual Haskell tuples in my
  solutions, because they are too awkward and
  inconvenient. Representing homogeneous tuples as Haskell lists of a
  certain known length allows us to read and process ``tuples'' using
  standard functions like \h{words} and \h{map}, to combine them using
  \h{zipWith}, and so on.  And since we get to assume that the input
  always precisely follows the specification---which will never
  change---this is one of the few situations where, in my opinion, we
  are fully justified in writing partial functions like this if it
  makes the code easier to write.  So I often represent homogeneous
  tuples as lists and just pattern match on lists of the appropriate
  (known) length.  (If I need heterogeneous tuples, on the other hand,
  I create an appropriate \h{data} type.)

\section{Explicit recursion}
\label{sec:explicit-recursion}

\section{Practice problems}
\label{sec:gs-practice}

Here are a few simple problems for you to practice on.

\kattis{jobexpenses, judgingmoose, quickestimate}

This is just a very small sample; there are many, many problems on
Open Kattis that can be solved with not much more than the techniques
explained in this chapter, and I encourage you to just solve a bunch
of them!  To start, look for problems with a difficulty rating less
than 2.0.  To find such problems easily, you can go to the list of all
problems (\url{http://open.kattis.com/problems/}) and sort by difficulty.

\chapter{Basic Data Structures and Wholemeal Programming}
\label{chap:wholemeal}

As we have seen in the previous chapter, functional programming
encourages us to think in terms of assembling functions into pipelines
that incrementally transform an input into an output.  \term{Wholemeal
  programming} is a closely related concept, explained best by Ralf
Hinze \parencite*{hinze2009tour}:
\begin{quote}
  Functional languages excel at wholemeal programming, a term coined
  by Geraint Jones. Wholemeal programming means to think big: work
  with an entire list, rather than a sequence of elements; develop a
  solution space, rather than an individual solution; imagine a graph,
  rather than a single path.
\end{quote}
This is often a very fruitful approach to solving competitive
programming problems.  It helps us avoid getting lost---or making
mistakes!---dealing with trivial low-level details.

\section{Example: processing a list of strings}

As a simple example, consider the problem of writing a function of
type \h{[String] -> Int} which adds up the lengths of all strings that
do not contain the letter \h{'e'}.  For example, given the input
\h{["dog", "sheep", "horse", "capybara", "lemur"]}, the function
should return 11 (the combined length of \h{"dog"} and
\h{"capybara"}).

A standard imperative approach to this problem involves looping over
the list of words, updating an accumulator variable every time we see
a string that does not contain the letter e.  An inexperienced
Haskeller, steeped in imperative thinking, might produce something
similar:
\begin{minted}{haskell}
lengthWithoutE :: [String] -> Int
lengthWithoutE [] = 0
lengthWithoutE (s:ss)
  | 'e' `elem` s = lengthWithoutE ss
  | otherwise = length s + lengthWithoutE ss
\end{minted}
This code works, but there is a better way.  Instead of thinking in
terms of processing the list one item at a time, an experienced
Haskeller will think in terms of incrementally transforming the input
into the desired output: first, filter out the strings we don't want;
next, turn each string into its length; and finally sum the lengths.
\begin{minted}{haskell}
lengthWithoutEWholemeal :: [String] -> Int
lengthWithoutEWholemeal = filter ('e' `notElem`) >>> map length >>> sum
\end{minted}
This is better in almost every way: it is shorter, easier to read and
understand, easier to refactor, harder to get wrong, and easier to
prove correct.

Experienced programmers might worry about efficiency: doesn't this do
three passes over the list instead of just one?  Even worse, doesn't
it construct several intermediate lists from scratch?  Actually,
Haskell's laziness, combined with optimization technology like list
fusion, make this very efficient in practice.  And in any case, we
usually don't need to worry much about constant factors as long as we
have the right asymptotic complexity.

\section{Basic data structures}

\chapter{Parsing}
\label{chap:parsing}

\chapter{Monoids}
\label{chap:monoids}

\chapter{Trees}
\label{chap:trees}

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

\printbibliography

\end{document}
