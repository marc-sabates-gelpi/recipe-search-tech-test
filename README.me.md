# rf-search

RF Search

A serach engine to search on any file in a directory (resources/recipes)
The indexing uses a hash map to store each word to each file with the frequency of the word in the file.
To implement multi word search it uses the context of each word (2 words on each side) and then those collocated words are used
to increase the match importance.
After implementing the collocation indexing and search the results are not very promising.. indexing time went up by a ten fold
and search is many times slower...

Collocation could be improved if the collocated words were more meaningful.. e.g. by filtering any non noun word.. or by using 
a meaning proximity instead of location proximity.

The program watches any changes in the recipes directory and though there is no partial indexing implemented some investigations
were done and could be implemented with some more time.. improving the indexing time..

The program runs on the CLI with the clojure/clj command line.

Some tests have been written but they are not exhaustive.. 

There are 3 namespaces
`core` -> The main fn and all the IO fns sit
`indexing` -> Indexing related fns
`search` -> Search related fns

## Usage

   $ clj -Astart

## Testing

   $ clj -Atest

## Timekeeping

03/03/2020 1h 15min
04/03/2020 4h 15min
05/03/2020 8h 30min

## TODO

* Partial indexing
* Performance improvement (perhaps using transducers and reducing as much as possible the loops over long colls)
* The collocations doesn't seem to be very useful; use another multi word search technique
* Keep only nouns when indexing
* Further work would need to take into account other indexing structures, e.g. Binary trees, Ternaries trees 

## License

Distributed under the GNU General Public License (GPL) version 3
https://www.gnu.org/licenses/gpl.html
