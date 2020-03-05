# rf-search

RF Search

[done] A first version will be attempted using a hashmap index of all the 'words' (`#"[a-zA-Z]{4,}"`)
This should allow to use fast 1 word searches

A further approach will be to implement >1 words' search

Further work would need to take into account other indexing structures, e.g. Binary trees, Ternaries trees

## Usage

   $ clj -Astart

## Testing

   $ clj -Atest

## Timekeeping

03/03/2020 1h 15min
04/03/2020 4h 15min
05/03/2020 3h 30min
05/03/2020 Start 21:50

## TODO

Partial indexing
Performance
The collocations doesn't seem to be very useful; use another multi word search technique
Keep only nouns when indexing 

## License

Distributed under the GNU General Public License (GPL) version 3
https://www.gnu.org/licenses/gpl.html
