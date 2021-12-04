# Scheml API

### lists
| Function           | Signature                                 | Description                                     |
|--------------------|-------------------------------------------|-------------------------------------------------|
| (cons a l)         | 'a -> cons 'a -> cons 'a                  | insert an element in the front of a list        | 
| (->list a)         | 'a -> cons 'a                             | creates a single-element list                   | 
| (range from to)    | int -> int -> int                         | creates a list between from and to (inclusive)  |
| (head l)           | cons 'a -> 'a                             | returns first element in a list                 |
| (tail l)           | cons 'a -> cons 'a                        | returns rest of list after first element        |
| (empty? l)         | cons 'a -> bool                           | returns true if list is empty                   |
| (nil? l)           | cons 'a -> bool                           | returns true if list is empty                   |
| (map f l)          | ('a -> 'b) -> cons 'a -> cons 'b          | applies f to each element of l                  |
| (pmap f l)         | ('a -> 'b) -> cons 'a -> cons 'b          | applies f in parallel to each element of l      |
| (map-optional f l) | ('a -> 'b) -> cons (option 'a) -> cons 'b | applies f in parallel to each Just element of l |
| (reverse l)        | cons 'a -> cons 'a                        | reverses list                                   |
| (append l1 l2)     | cons 'a -> cons 'a -> cons 'a             | appends l2 onto end of l1                       |
| (member elem l)    | 'a -> cons 'a -> bool                     | is elem in list l?                              |
| (remove elem l)    | 'a -> cons 'a -> cons 'a                  | removes first occurrence of elem in l           |
| (take n l)         | int -> cons 'a -> cons 'a                 | returns list of first n elements in l           |
| (drop n l)         | int -> cons 'a -> cons 'a                 | returns list l without first n elements         |
| fold f elem l      | ('a -> 'b -> 'b) -> 'b -> cons 'a -> b


### int
| Function   | Signature               | Description                      |
|------------|-------------------------|----------------------------------|
| (+ a b)    | int -> int -> int       | adds a and b                     |
| (- a b)    | int -> int -> int       | subtracts b from a               |
| (* a b)    | int -> int -> int       | multiplies a and b               |
| (/ a b)    | int -> int -> int       | divides a by b                   |
| (% a b)    | int -> int -> int       | remainder of a divided by b      |
| (div a b)  | int -> int -> int       | divides a by b                   |
| (mod a b)  | int -> int -> int       | remainder of a divided by b      |
| (min a b)  | int -> int -> int       | minimum of a and b               |
| (max a b)  | int -> int -> int       | maximum of a and b               |
| (neg a)    | int -> int              | negates a                        |
| (= a b)    | int -> int -> bool      | are a and b equal?               |
| (!= a b)   | int -> int -> bool      | are a and b not equal?           |
| (< a b)    | int -> int -> bool      | is a less than b?                |
| (<= a b)   | int -> int -> bool      | is a less than or equal to b?    |
| (> a b)    | int -> int -> bool      | is a greater than b?             |
| (>= a b)   | int -> int -> bool      | is a greater than or equal to b? |

### double
| Function   | Signature                    | Description                      |
|------------|------------------------------|----------------------------------|
| (+. a b)   | double -> double -> double   | adds a and b                     |
| (-. a b)   | double -> double -> double   | subtracts b from a               |
| (*. a b)   | double -> double -> double   | multiplies a and b               |
| (/. a b)   | double -> double -> double   | divides a by b                   |
| (%. a b)   | double -> double -> double   | remainder of a divided by b      |
| (div. a b) | double -> double -> double   | divides a by b                   |
| (mod. a b) | double -> double -> double   | remainder of a divided by b      |
| (min. a b) | double -> double -> double   | minimum of a and b               |
| (max. a b) | double -> double -> double   | maximum of a and b               |
| (neg. a)   | double -> double             | negates b                        |
| (=. a b)   | double -> double -> bool     | are a and b equal?               |
| (!=. a b)  | double -> double -> bool     | are a and b not equal?           |
| (<. a b)   | double -> double -> bool     | is a less than b?                |
| (<=. a b)  | double -> double -> bool     | is a less than or equal to b?    |
| (>. a b)   | double -> double -> bool     | is a greater than b?             |
| (>=. a b)  | double -> double -> bool     | is a greater than or equal to b? |

### bignum
| Function   | Signature                    | Description                      |
|------------|------------------------------|----------------------------------|
| (+b a b)   | bignum -> bignum -> bignum   | adds a and b                     |
| (-b a b)   | bignum -> bignum -> bignum   | subtracts b from a               |
| (*b a b)   | bignum -> bignum -> bignum   | multiplies a and b               |
| (/b a b)   | bignum -> bignum -> bignum   | divides a by b                   |
| (%b a b)   | bignum -> bignum -> bignum   | remainder of a divided by b      |
| (divb a b) | bignum -> bignum -> bignum   | divides a by b                   |
| (modb a b) | bignum -> bignum -> bignum   | remainder of a divided by b      |
| (minb a b) | bignum -> bignum -> bignum   | minimum of a and b               |
| (maxb a b) | bignum -> bignum -> bignum   | maximum of a and b               |
| (negb a)   | bignum -> bignum             | negates b                        |
| (=b a b)   | bignum -> bignum -> bool     | are a and b equal?               |
| (!=b a b)  | bignum -> bignum -> bool     | are a and b not equal?           |
| (<b a b)   | bignum -> bignum -> bool     | is a less than b?                |
| (<=b a b)  | bignum -> bignum -> bool     | is a less than or equal to b?    |
| (>b a b)   | bignum -> bignum -> bool     | is a greater than b?             |
| (>=b a b)  | bignum -> bignum -> bool     | is a greater than or equal to b? |

### Conversions
| Function              | Signature        | Description                   |
|-----------------------|------------------|-------------------------------|
| (int->double a)       | int -> double    | Converts int to double        |
| (double->int a)       | double -> int    | Converts double to int        |
| (int->bignum a)       | int -> bignum    | Converts int to bignum        |
| (bignum->int a)       | bignum -> int    | Converts bignum to int        |
| (bignum->string a)    | bignum -> string | Converts bignum to string     |
| (string->bignum a)    | string -> bignum | Converts string to bignum     |
| (bignum->hexstring a) | bignum -> string | Converts bignum to hex string |
| (hexstring->bignum a) | string -> bignum | Converts hex string to bignum |
| (int->char a)         | int -> char      | Converts int to char          |
| (char->int a)         | char -> int      | Converts char to int          |

