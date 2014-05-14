
# TODO

* unit tests
* property-based testing (clojure.test.check)

## pooling

* try spatio-temporal pooling by extending (column/cell?) activation
> "The trick to adding TP to the SP is the following. When the input to
the SP was correctly predicted in the previous layer we want the cells
in the column to remain active long enough to learn to respond to the
next input pattern."

* allow columns arrayed in 2 dimensions
* target 2% activation

## sequence memory

* limit connections to one cell of bursting columns ("learning state")
* start empty?
* only update synapses when cells turn on/off? (not when continuing?)
* define connected permanence value to be 1.0? parameter is then max-perm
* limit lateral synapses to within a radius 

## code architecture

* protocols




## test problems

what is "hello world" for CLA / HTM?

### sequence

* 1 2 3 1 1 8, 1 2 3 1 1 8

```
1 100110 100110 
2 010000 010000
3 001000 001000
4 000000 000000
5 000000 000000
6 000000 000000
7 000000 000000
8 000001 000001
```

### shooting spaceship

```
000100000001
001000000010
010000000100
111111111111
000001000000
000000100000
000000010000
```

### moving shooting spaceship

```
00010011000010000000010000
00101100110100000000100000
01110100001100000001000000
11000010000011000010000011
00000001000000110100001100
00000000100000101100110100
00000000010000010011000010
```
motor signals:
```
00101010000000000000101010 move up
00000000101010101010000000 move down
01000000000000000100000000 fire up
00000100000000000000000100 fire down
```

### two-d bouncing ball

* motor control of ball
  * proprioceptive sense












