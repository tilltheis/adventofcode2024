# Advent of Code 2024

In Haskell!

## Setup

1. Install Haskell with Stack using [GHCup](https://www.haskell.org/ghcup/).
2. Create an `inputs/` folder and add your puzzle input files there, named `input1.txt`, `input2.txt` and so forth.

## Running

```text
Usage: stack run <day> <part>
Example: stack run 1 2
```

## Testing

`stack test`

## Developing

Run tests on file change (requires `inotify-tools` on Ubuntu):
```shell
while inotifywait -e close_write app src test; do stack test; done
```