Bots and Doxes 
Audrey Tollett
Noah Wenig
Daisy Nieto 
Neil Flores 

# Project Grade: 101/100
## Functionality: 75/73
* Game mechanics:              20
* Exact game solver:           15
* Cut-off depth solver:        13
* Evaluation function:         2
* Avoiding unnecessary work:   3
* Command-line interface:      10
  * Don't get all the outputs exactly right (you don't output a move the -m flag can take, etc) (-1)
* Move and verbose flags:      5
  * You are verbose all the time in -w and default, but works for -m. Move works. (-2)
  * Interactive flag works (+5 ec)
* Error-handling:              5
  * Beautifully done

## Design                      26/27
* Well-designed data types:    8
* Well-decomposed functions:   10
* Good module decomposition:   2
* Good variable names:         2
* Efficient/idiomatic code:    5
  * Beatiful code
  * Some lines are very long, but overall incredibly clean code.
  * Not fond of the 'zip' in bestMove, versus list comprehensions, but that's a quibble.
  * Minimax repeats whoMightWin nearly identically, but that does make it a bit quicker.
