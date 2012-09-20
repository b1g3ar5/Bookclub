---
title: Home
---

What it\'s about
================
This is a web site built so that I could learn Hakyll and how to draw graphs on a web site, and at the same 
time produce some statistics about the books that we have read.

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

To do
=====

Add index of markdown blog pages, write some blog pages.

