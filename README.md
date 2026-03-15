
Inspirations:
```
https://www.youtube.com/watch?v=v68zYyaEmEA
https://sonorouschocolate.com/notes/index.php/The_best_strategies_for_Wordle
https://sonorouschocolate.com/notes/index.php/The_best_strategies_for_Wordle,_part_2
https://sonorouschocolate.com/notes/index.php/The_best_strategies_for_Wordle,_part_3_(July_2023)
https://alexpeattie.com/blog/establishing-minimum-guesses-wordle/
https://www.poirrier.ca/notes/wordle/
https://www.poirrier.ca/notes/wordle-optimal/
https://www.poirrier.ca/notes/wordle-fixed/
```

Dicts are from https://github.com/seanpatlan/wordle-words  
They are stale, most probably  
But that's ok by me

Use `cabal run wordle-solver -O2 -- +RTS -sstderr -pj` to run and save profiling data to `./wordle-solver.prof`  
Or `cabal build wordle-solver -O2 && time cabal run wordle-solver -O2` to measure without profiling obstructions
Also you need to comment first 2 lines in `./cabal.project` with ` -- ` to disable profiling


