# Geohash programs

These programs implement a couple of the assignments from my CS 5278 course
at Vanderbilt. The Geohashes here are a little different from what you will
find described on Wikipedia. First, the latitude bits come first instead of
the longitude bits. Second, they are never converted into a shorter
character-based representation.

## geohash.scm and geohashtests.scm
The geohash.scm file implements the first assignment, which is simple geohash
computation. The geohashtests.scm file contains Scheml versions of the unit
tests for the assignment. To run the tests, from this directory run:
```shell
../../scheml run-a1-tests.scm
```

## geohash-assignment2.scm and geohash-assignment2-tests.scm
The geohash-assignment2.scm file implements the second assignment in Scheml.
It adds the idea of a GeoDB database where locations are stored in some
kind of data structure (a tree in this case). The
geohash-assignment2-tests.scm implements the unit tests for the second assignment.
To run the tests from this directory:
```shell
../../scheml run-a2-tests.scm
```