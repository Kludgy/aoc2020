# Advent of Code 2020

So far:
* These are only the cleaned up solutions as they become available. I may be able to show more work-in-progress now that this is in a repo.
* Problems have been solved alternating between vanilla Haskell and SWI-Prolog (DCG), depending on perceived parsing requirements. Take my judgment with a healthy grain of salt; the choice of tool is a snap decision and you could achieve results in many different languages. (Scripting languages widely used in data scraping, for example?)
* Haskell solutions have problem input in source; quick copy-paste munging reduces ceremony of passing loaded file argument around due to Haskell's adherence to purity. Good for many reasons, but brevity is not one.

Time to complete are not competitive; more like half-hour per star, rather than the crazy fast 1-minute-per-star that board leaders are achieving. (Wow!)  But times of hour+ are perfectly normal for even very seasoned programmers, depending on what sort of work we normally do, and other differences.

Have fun all.

How do I run the source?
========================

* Haskell (```*.hs```) files can be compiled anywhere Haskell '98 is supported. Honestly, I have just been pasting into https://repl.it/lm/haskell online. (The GHC toolchain can be a bit bulky to set up locally for one-off use.) Every Haskell solution has a ```main``` function that prints the part 1 solution on the first line, and the part 2 solution on the second line.
* SWI-Prolog (```*.pl``` - no not Perl) files can be consulted via ```swipl``` (https://www.swi-prolog.org/download/stable). For every problem there are two queries matching part 1 and part 2, ```sol1/1``` and ```sol2/1```, respectively.
