# TextComplexity

### What does TextComplexity do? 
TextComplexity analyzes an English text and returns the list of complex words and their percentage up to the whole text.

### When is a word complex?
The criterion is very easy: _is the word inside Oxford3000 list?_. Oxford made indeed a more or less 3000 word list that contains the most used words in English. This list can commonly supply the 90% of ordinary vocabulary words.

### How can I use it?
1. Install `cabal`, Haskell's package manager, if you haven't it yet
2. Clone this repo on your computer and `cd` into it
3. Build the project with: `cabal new-build TextComplexity`
4. Run the executable with: `cabal new-run TextComplexity < inputs/...` (output will be on `stdout` so you can redirect it with `>`)

### Example of an analysis
With this command`cabal new-run TextComplexity < inputs/a-study-in-scarlet.txt` output will be:  
```
12 % of the words are not in the Oxford 3000 list (6030 over 47520).

Here are the words:
abide, aboard, abruptly, absentee, abstraction, absurd, ...
```

### Features
- plural words convertion ("cats" => "cat", "boxes" => "box")
- verb conjugations convertion ("is" => "be", "stopped" => "stop", "goes" => "go")
- contractions convertion
- saxon genitive support
