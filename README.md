# first-lab

A Clojure library designed to realize cluster estimation algorithm

## Usage

$> lein run <distance-calculation-type> <file-path>

Distance calculation types:
* euclidean
* hamming

example

$> lein run "hamming" "files/irises.txt"

## Tests

$> lein test
