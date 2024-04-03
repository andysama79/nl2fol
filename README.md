# nl2fol

## General Information
Please put all your inputs in the `input.txt` file. \
All outputs are stored in `output.txt` file.

At the time of writing this piece of software, some part-of-speech (POS) tags were only used, namely:

S -> Sentence \
VP -> Verb Phrase \
NP -> Noun Phrase \
NNP -> Proper Nouns \
NN -> Common Nouns \
VBZ -> Verbalizers \
DT -> Determiner 

The software is heavily inspired by StanfordCoreNLP's Stanza.

## Setting up the code
1. Make sure you have `conda` installed. To install `conda` follow the documentation [here](https://docs.anaconda.com/free/miniconda/).
2. Use the `environment.yml` file to create the environment:
```python
conda env create --name envname --file=environment.yml
```
3. Build the cabal project:
```bash
cd interpreter
cabal update
cabal build
```

## Running the code
To run the code:
```bash
python main.py -f input.txt
```

## References
- [Phrase Structure Rules](https://en.wikipedia.org/wiki/Phrase_structure_rules)
- [Montague Grammar](https://en.wikipedia.org/wiki/Montague_grammar)
- [Stanza](https://stanfordnlp.github.io/stanza/)
- [Stanza Github](https://github.com/stanfordnlp/stanza)
- [Constituency Parsing](https://www.bing.com/search?q=constituency+parsing&qs=n&form=QBRE&sp=-1&ghc=1&lq=0&sm=u&pq=constituency+parsing&sc=10-20&sk=&cvid=372BD46ADBD646F99E2F6628E02ED930&ghsh=0&ghacc=0&ghpl=)
- [Penn TreeBank Project](https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html)
