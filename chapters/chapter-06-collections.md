# Chapter 6: Collections - Organizing Your Data

So far, we've worked with individual objects: numbers, strings, characters. But what if you need to work with *groups* of objects? A list of names? A set of numbers? A mapping of keys to values?

That's what **collections** are for. Collections are objects that contain other objects. They're some of the most powerful and frequently used tools in Smalltalk, and they come with an incredibly rich set of operations.

In this chapter, we'll explore the main collection types and learn the common protocol that makes them so useful.

## What is a Collection?

A **collection** is an object that holds other objects. Think of it like a container:

- A shopping list is a collection of items
- A phonebook is a collection of names and numbers
- A set of unique numbers is a collection

Smalltalk provides several types of collections, each optimized for different uses:

- **Arrays** - Fixed-size, ordered collections
- **OrderedCollections** - Growable, ordered collections (like lists)
- **Sets** - Unordered collections of unique elements
- **Dictionaries** - Key-value pairs (maps)
- **Bags** - Like sets, but allow duplicates
- And many more specialized types

## Arrays

An **Array** is a fixed-size collection where elements are stored in order and accessed by index.

### Creating Arrays

The simplest way to create an array is with the literal syntax:

```smalltalk
#(1 2 3 4 5)
```

The `#(...)` syntax creates an array. Try this:

```smalltalk
#(1 2 3 4 5) class
```

Returns `Array`.

You can put any objects in an array:

```smalltalk
#('hello' 'world' 'from' 'Smalltalk')
```

```smalltalk
#(1 'two' 3.14 true)  "Mixed types!"
```

**Important**: The literal array syntax `#()` can only contain literal values (numbers, strings, symbols, other literal arrays). For more complex objects, use the dynamic creation method:

```smalltalk
Array with: Date today
```

```smalltalk
Array with: 1 with: 2 with: 3
```

Or create an empty array of a specific size:

```smalltalk
Array new: 5  "Creates an array with 5 elements (all nil initially)"
```

### Accessing Array Elements

Arrays are indexed starting at 1 (not 0!):

```smalltalk
| arr |
arr := #(10 20 30 40 50).
arr at: 1       "10 (first element)"
arr at: 3       "30 (third element)"
arr at: 5       "50 (fifth element)"
arr first       "10"
arr last        "50"
```

### Array Size

```smalltalk
#(1 2 3 4 5) size       "5"
#() size                "0 (empty array)"
```

### Modifying Arrays

You can change elements in an array:

```smalltalk
| arr |
arr := #(10 20 30) copy.  "Make a copy to modify"
arr at: 2 put: 99.
arr  "Returns #(10 99 30)"
```

**Note**: We use `copy` because literal arrays like `#(10 20 30)` are actually immutable in the image. When you want to modify an array, make a copy first, or create it with `Array with:` or `Array new:`.

### Multi-dimensional Arrays

You can have arrays of arrays:

```smalltalk
#(
    (1 2 3)
    (4 5 6)
    (7 8 9)
)
```

This is a 3x3 grid represented as an array of three arrays.

```smalltalk
| grid |
grid := #((1 2 3) (4 5 6) (7 8 9)).
(grid at: 2) at: 3  "6 (second row, third element)"
```

## OrderedCollections

An **OrderedCollection** is like an array, but it can grow and shrink dynamically. This is probably the collection type you'll use most often when you need an ordered list.

### Creating OrderedCollections

```smalltalk
OrderedCollection new
```

Creates an empty OrderedCollection.

```smalltalk
OrderedCollection with: 10 with: 20 with: 30
```

Creates an OrderedCollection with three elements.

You can also convert an array to an OrderedCollection:

```smalltalk
#(1 2 3) asOrderedCollection
```

### Adding Elements

```smalltalk
| list |
list := OrderedCollection new.
list add: 10.
list add: 20.
list add: 30.
list  "Returns an OrderedCollection(10 20 30)"
```

You can add at the beginning or end:

```smalltalk
| list |
list := OrderedCollection with: 20.
list addFirst: 10.      "Adds at the beginning"
list addLast: 30.       "Adds at the end"
list  "OrderedCollection(10 20 30)"
```

### Removing Elements

```smalltalk
| list |
list := OrderedCollection with: 10 with: 20 with: 30.
list remove: 20.
list  "OrderedCollection(10 30)"
```

```smalltalk
| list |
list := OrderedCollection with: 10 with: 20 with: 30.
list removeFirst.  "Removes and returns 10"
list removeLast.   "Removes and returns 30"
list  "OrderedCollection(20)"
```

### Accessing Elements

Just like arrays:

```smalltalk
| list |
list := OrderedCollection with: 10 with: 20 with: 30.
list at: 2       "20"
list first       "10"
list last        "30"
list size        "3"
```

### Why Use OrderedCollection Instead of Array?

- **Dynamic sizing**: OrderedCollections grow and shrink as needed
- **Efficient insertion/removal**: Especially at the ends
- **More flexible**: Better for lists that change size frequently

Use Arrays when you know the size won't change. Use OrderedCollections when you need flexibility.

## Sets

A **Set** is an unordered collection that contains no duplicates. Each element can appear only once.

### Creating Sets

```smalltalk
Set new
```

```smalltalk
Set with: 10 with: 20 with: 30
```

Converting an array to a set (removes duplicates):

```smalltalk
#(1 2 3 2 1 4 3) asSet  "Returns a Set with 1, 2, 3, 4"
```

### Adding to Sets

```smalltalk
| set |
set := Set new.
set add: 10.
set add: 20.
set add: 10.  "Adding 10 again has no effect"
set size  "2 (not 3!)"
```

Sets automatically ignore duplicates.

### Testing Membership

```smalltalk
| set |
set := Set with: 10 with: 20 with: 30.
set includes: 20     "true"
set includes: 99     "false"
```

### Removing from Sets

```smalltalk
| set |
set := Set with: 10 with: 20 with: 30.
set remove: 20.
set includes: 20     "false"
```

### Set Operations

Sets support mathematical set operations:

```smalltalk
| setA setB |
setA := Set with: 1 with: 2 with: 3.
setB := Set with: 3 with: 4 with: 5.

setA union: setB         "Set(1 2 3 4 5)"
setA intersection: setB  "Set(3)"
setA difference: setB    "Set(1 2)"
```

### When to Use Sets

- When you don't care about order
- When you want to ensure no duplicates
- When you need to test membership efficiently
- When you want set operations (union, intersection, etc.)

## Dictionaries

A **Dictionary** is a collection of key-value pairs. You look up values by their keys, like a real dictionary where you look up definitions by words.

### Creating Dictionaries

```smalltalk
Dictionary new
```

```smalltalk
| dict |
dict := Dictionary new.
dict at: 'name' put: 'Alice'.
dict at: 'age' put: 30.
dict at: 'city' put: 'Wonderland'.
dict
```

### Accessing Values

```smalltalk
| dict |
dict := Dictionary new.
dict at: 'name' put: 'Alice'.
dict at: 'name'  "Returns 'Alice'"
```

If a key doesn't exist, you'll get an error. To handle this, use:

```smalltalk
dict at: 'unknown' ifAbsent: [ 'Not found' ]
```

This returns `'Not found'` if the key doesn't exist.

### Convenient Dictionary Creation

There's a more concise way to create dictionaries:

```smalltalk
{ 'name' -> 'Alice' .
  'age' -> 30 .
  'city' -> 'Wonderland' } asDictionary
```

The `->` creates an **Association** (a key-value pair), and the `{...}` creates a dynamic array of associations, which we convert to a dictionary.

### Testing for Keys

```smalltalk
| dict |
dict := { 'name' -> 'Alice' . 'age' -> 30 } asDictionary.
dict includesKey: 'name'     "true"
dict includesKey: 'email'    "false"
```

### Removing Entries

```smalltalk
| dict |
dict := { 'name' -> 'Alice' . 'age' -> 30 } asDictionary.
dict removeKey: 'age'.
dict includesKey: 'age'  "false"
```

### Iterating Over Dictionaries

```smalltalk
| dict |
dict := { 'name' -> 'Alice' . 'age' -> 30 . 'city' -> 'Wonderland' } asDictionary.
dict keysAndValuesDo: [ :key :value |
    Transcript show: key; show: ': '; show: value printString; cr ].
```

Check your Transcript - you'll see all the key-value pairs!

### Common Dictionary Operations

```smalltalk
dict keys       "Returns all keys as a collection"
dict values     "Returns all values as a collection"
dict size       "Number of key-value pairs"
```

### When to Use Dictionaries

- When you want to look up values by meaningful keys (not just numeric indices)
- For configuration data
- For caching
- Anywhere you'd use a "map" or "hash table" in other languages

## The Collection Protocol

Here's where Smalltalk really shines: **most collections understand the same messages**. This common protocol makes collections incredibly powerful and consistent.

### Iteration: `do:`

The `do:` message executes a block for each element:

```smalltalk
#(1 2 3 4 5) do: [ :each |
    Transcript show: each printString; cr ].
```

This works for arrays, OrderedCollections, sets, and more!

```smalltalk
(Set with: 10 with: 20 with: 30) do: [ :each |
    Transcript show: each printString; cr ].
```

### Transformation: `collect:`

The `collect:` message transforms each element and returns a new collection:

```smalltalk
#(1 2 3 4 5) collect: [ :each | each * 2 ]
"Returns #(2 4 6 8 10)"
```

```smalltalk
#('hello' 'world') collect: [ :each | each capitalized ]
"Returns #('Hello' 'World')"
```

### Selection: `select:`

The `select:` message keeps only elements that match a condition:

```smalltalk
#(1 2 3 4 5 6 7 8) select: [ :each | each even ]
"Returns #(2 4 6 8)"
```

```smalltalk
#('apple' 'banana' 'apricot' 'cherry') select: [ :each | each beginsWith: 'a' ]
"Returns #('apple' 'apricot')"
```

### Rejection: `reject:`

The `reject:` message is the opposite of `select:` - it removes elements that match:

```smalltalk
#(1 2 3 4 5 6) reject: [ :each | each even ]
"Returns #(1 3 5)"
```

### Detection: `detect:`

The `detect:` message finds the first element matching a condition:

```smalltalk
#(1 2 3 4 5) detect: [ :each | each > 3 ]
"Returns 4"
```

If nothing is found, you get an error. To handle this:

```smalltalk
#(1 2 3) detect: [ :each | each > 10 ] ifNone: [ 'Not found' ]
"Returns 'Not found'"
```

### Testing: `allSatisfy:` and `anySatisfy:`

```smalltalk
#(2 4 6 8) allSatisfy: [ :each | each even ]
"true (all elements are even)"
```

```smalltalk
#(1 2 3 4) anySatisfy: [ :each | each > 3 ]
"true (at least one element is greater than 3)"
```

### Reduction: `inject:into:`

This is one of the most powerful operations. It "injects" an initial value and combines it with each element:

```smalltalk
#(1 2 3 4 5) inject: 0 into: [ :sum :each | sum + each ]
"Returns 15 (the sum of all elements)"
```

Let's break this down:
- Start with `0` (the initial value)
- For the first element (1): `0 + 1` = 1
- For the second element (2): `1 + 2` = 3
- For the third element (3): `3 + 3` = 6
- And so on...

Another example - finding the maximum:

```smalltalk
#(3 7 2 9 1) inject: 0 into: [ :max :each |
    max max: each ]
"Returns 9"
```

### Counting: `count:`

```smalltalk
#(1 2 3 4 5 6) count: [ :each | each even ]
"Returns 3 (there are 3 even numbers)"
```

### Checking Inclusion

```smalltalk
#(1 2 3 4 5) includes: 3      "true"
#(1 2 3 4 5) includes: 10     "false"
```

### Sum, Average, Max, Min

For numeric collections:

```smalltalk
#(1 2 3 4 5) sum              "15"
#(1 2 3 4 5) average          "3"
#(1 2 3 4 5) max              "5"
#(1 2 3 4 5) min              "1"
```

### Sorting

```smalltalk
#(3 1 4 1 5 9 2 6) sorted     "#(1 1 2 3 4 5 6 9)"
```

```smalltalk
#('zebra' 'apple' 'mango') sorted
"#('apple' 'mango' 'zebra')"
```

Custom sorting:

```smalltalk
#(3 1 4 1 5 9) sorted: [ :a :b | a > b ]
"#(9 5 4 3 1 1) - descending order"
```

### Flattening

```smalltalk
#( (1 2) (3 4) (5 6) ) flatten
"#(1 2 3 4 5 6)"
```

### Removing Duplicates

```smalltalk
#(1 2 3 2 1 4 3 5) asSet asArray
"Returns an array with duplicates removed"
```

## Method Chaining

Because collection methods return collections, you can chain them:

```smalltalk
#(1 2 3 4 5 6 7 8 9 10)
    select: [ :each | each even ]
    collect: [ :each | each squared ]
    sum
"Returns 220"
```

This reads beautifully:
1. Start with numbers 1-10
2. Select the even ones: (2 4 6 8 10)
3. Square each: (4 16 36 64 100)
4. Sum them: 220

Try breaking this down step by step in your Playground!

## Strings are Collections

Remember strings from Chapter 5? They're actually collections of characters!

```smalltalk
'Hello' size                            "5"
'Hello' at: 1                           "$H"
'Hello' do: [ :char | Transcript show: char; cr ]
'Hello' collect: [ :char | char asUppercase ]  "'HELLO'"
'Hello' select: [ :char | char isVowel ]        "'eo'"
```

This is polymorphism in action - strings respond to the same collection protocol as arrays and lists!

## Bags

A **Bag** is like a Set, but it allows duplicates and tracks how many times each element appears.

```smalltalk
| bag |
bag := Bag new.
bag add: 'apple'.
bag add: 'banana'.
bag add: 'apple'.
bag add: 'apple'.
bag occurrencesOf: 'apple'   "3"
bag occurrencesOf: 'banana'  "1"
```

Bags are useful for counting occurrences:

```smalltalk
| words |
words := 'the quick brown fox jumps over the lazy dog' substrings.
words asBag
```

Now you can see which words appear how many times!

## Intervals

An **Interval** is a collection representing a range of numbers:

```smalltalk
1 to: 10                "An interval from 1 to 10"
```

```smalltalk
(1 to: 10) asArray      "#(1 2 3 4 5 6 7 8 9 10)"
```

With a step:

```smalltalk
1 to: 10 by: 2          "1, 3, 5, 7, 9"
```

```smalltalk
10 to: 1 by: -1         "10, 9, 8, ..., 1"
```

Intervals are efficient - they don't create all the numbers in memory; they generate them as needed.

```smalltalk
(1 to: 1000000) select: [ :each | each \\ 7 = 0 ]
```

This efficiently finds all multiples of 7 up to a million without creating a million-element array first!

## Practical Examples

Let's put collections to work with some real-world examples.

### Example 1: Grade Calculator

```smalltalk
| grades |
grades := #(85 92 78 90 88).

"Average grade"
grades average

"Highest grade"
grades max

"How many grades above 85?"
grades count: [ :grade | grade > 85 ]

"All grades above 80"
grades select: [ :grade | grade > 80 ]
```

### Example 2: Word Counter

```smalltalk
| text words uniqueWords |
text := 'the quick brown fox jumps over the lazy dog the quick fox'.
words := text substrings.  "Split into words"
uniqueWords := words asSet.

"How many total words?"
words size

"How many unique words?"
uniqueWords size

"Words that appear more than once"
uniqueWords select: [ :word |
    (words occurrencesOf: word) > 1 ]
```

### Example 3: Phone Book

```smalltalk
| phoneBook |
phoneBook := Dictionary new.
phoneBook at: 'Alice' put: '555-1234'.
phoneBook at: 'Bob' put: '555-5678'.
phoneBook at: 'Charlie' put: '555-9012'.

"Look up a number"
phoneBook at: 'Alice'

"Check if someone is in the book"
phoneBook includesKey: 'David'

"List all names"
phoneBook keys

"Print the whole phone book"
phoneBook keysAndValuesDo: [ :name :number |
    Transcript show: name; show: ': '; show: number; cr ]
```

### Example 4: Shopping Cart

```smalltalk
| cart prices |
cart := OrderedCollection new.
cart add: 'apples'.
cart add: 'milk'.
cart add: 'bread'.
cart add: 'cheese'.

prices := Dictionary new.
prices at: 'apples' put: 3.50.
prices at: 'milk' put: 2.99.
prices at: 'bread' put: 2.50.
prices at: 'cheese' put: 5.99.

"Total price"
cart inject: 0 into: [ :total :item |
    total + (prices at: item) ]
"Returns 14.98"
```

## Performance Considerations

Different collections have different performance characteristics:

- **Arrays**: Fast random access (`at:`), fixed size
- **OrderedCollections**: Fast at ends (addFirst, addLast), flexible size
- **Sets**: Very fast membership testing (`includes:`), no duplicates
- **Dictionaries**: Very fast key lookup, perfect for mappings

Choose the right collection for your needs!

## Try This!

Time to practice with collections:

1. **Array exploration:**
   ```smalltalk
   #(5 2 8 1 9 3) sorted
   #(5 2 8 1 9 3) reversed
   #(5 2 8 1 9 3) sum
   ```

2. **Filtering:**
   ```smalltalk
   #(1 2 3 4 5 6 7 8 9 10)
       select: [ :n | n isPrime ]
   ```

3. **Transforming:**
   ```smalltalk
   #('hello' 'world' 'from' 'smalltalk')
       collect: [ :word | word size ]
   ```
   What does this give you?

4. **String as collection:**
   ```smalltalk
   'programming'
       select: [ :char | char isVowel ]
   ```

5. **Build a collection:**
   ```smalltalk
   | names |
   names := OrderedCollection new.
   names add: 'Alice'.
   names add: 'Bob'.
   names add: 'Charlie'.
   names collect: [ :name | 'Hello, ' , name , '!' ]
   ```

6. **Dictionary lookup:**
   ```smalltalk
   | capitals |
   capitals := Dictionary new.
   capitals at: 'France' put: 'Paris'.
   capitals at: 'Japan' put: 'Tokyo'.
   capitals at: 'USA' put: 'Washington DC'.
   capitals at: 'Japan'
   ```

7. **Complex chaining:**
   ```smalltalk
   (1 to: 20)
       select: [ :n | n odd ]
       collect: [ :n | n squared ]
       sum
   ```

8. **Remove duplicates:**
   ```smalltalk
   #(1 2 3 2 4 3 5 1 6) asSet asSortedCollection asArray
   ```

## Coming Up Next

You now have powerful tools for organizing and manipulating data! Collections are fundamental to programming in Smalltalk, and you'll use them constantly.

But we've been using variables without fully explaining them. In Chapter 7, we'll dive deep into variables: what they are, how they work, different types of variables, and the important concept of scope.

Then we'll be ready to tackle control flow - making decisions and repeating actions - in Chapters 8, 9, and 10.

The foundation is solid. Let's keep building!

---

**Key Takeaways:**
- Collections hold groups of objects
- **Arrays** are fixed-size, ordered collections with literal syntax `#(1 2 3)`
- **OrderedCollections** are dynamic, growable lists
- **Sets** contain unique elements, no duplicates
- **Dictionaries** map keys to values for efficient lookup
- Most collections share a common protocol: `do:`, `collect:`, `select:`, `reject:`, `detect:`, `inject:into:`
- Strings are collections of characters
- Method chaining creates readable, powerful expressions
- Choose collections based on your needs (order, uniqueness, lookup efficiency)
- Collections make Smalltalk incredibly expressive and powerful

---

[Previous: Chapter 5 - Numbers, Strings, and Basic Types](chapter-05-numbers-strings-and-basic-types.md) | [Next: Chapter 7 - Variables](chapter-07-variables.md)
