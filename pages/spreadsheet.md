---
title: Spreadsheet
---

What it\'s about
================
Having spent 30 years of my life using spreadsheets I decided to try to write one of my own in Haskell.
 
I was promted to do this after reading about GADTs in Tim Philip Williams' blog and also 
reading about loeb and cellular automata on Dan Pipone's blog. It seemed as though GADT's
would be useful when parsing the cell input and I thought it would be neat if someting like
loeb or some of Dan's comonad stuff could be used to do the spreadsheet recalculation.

So, here is the list of posts:

1. Types for cells to parse to (GADTs)

2. Parsing cell input to an Expr type

3. What is a Sheet going to be?

4. Is loeb useful (or cfix?)

5. Displaying the sheet using three-penny-gui


N


LaTex
-----

Let\'s try some LaTex:

$\begin{equation}
\left(
c \alpha \cdot \hat{p} + \beta m c^2
\right) \psi = i \hbar \frac{\partial \psi}{\partial t}
\end{equation}$

Code
----

Now some code:

    main :: IO ()
    main = hakyll $ do

    -- This creates an include file for the scoreTable page
        scoreTable <- match "csv/scoreTable.csv" $ do
            compile $ scoreTableCompiler
                >>> addDefaultFields

Did that look OK?

Lincoln Student Accommodation
============================

We own [Brayford Lets](http://www.brayfordlets.co.uk) which provides student accommodation for students at Lincoln University

