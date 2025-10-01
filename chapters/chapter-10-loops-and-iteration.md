# Chapter 10: Loops and Iteration

Loops are fundamental to programming. You need them to process lists of items, repeat actions multiple times, continue until a condition is met, and perform many other common tasks.

In most programming languages, loops are special syntax: `for`, `while`, `do-while`, etc. In Smalltalk, as you've probably guessed by now, loops are just **messages sent to objects**, and they work using **blocks**.

This chapter brings together everything you've learned about objects, messages, blocks, and collections to show you Smalltalk's elegant approach to repetition.

## The Simple Loop: `timesRepeat:`

The simplest loop repeats an action a specific number of times:

```smalltalk
5 timesRepeat: [ Transcript show: 'Hello!'; cr ]
```

Check your Transcript - you'll see "Hello!" five times.

The number `5` receives the `timesRepeat:` message with a block. The number executes the block 5 times.

```smalltalk
10 timesRepeat: [ Transcript show: '*' ].
Transcript cr.
"Prints: **********"
```

### How It Works

The `SmallInteger` class has a `timesRepeat:` method that looks something like:

```smalltalk
timesRepeat: aBlock
    | count |
    count := 1.
    [ count <= self ] whileTrue: [
        aBlock value.
        count := count + 1 ]
```

It's just a method sending messages! No special syntax needed.

## The Counting Loop: `to:do:`

When you need to know which iteration you're on, use `to:do:`:

```smalltalk
1 to: 5 do: [ :n |
    Transcript show: n printString; cr ]
```

This prints numbers 1 through 5, each on its own line.

The syntax: `start to: end do: block`

```smalltalk
10 to: 20 do: [ :i |
    Transcript show: i printString; show: ' squared is '; show: (i * i) printString; cr ]
```

### Counting Backwards

Use `to:by:` with a negative step:

```smalltalk
10 to: 1 by: -1 do: [ :n |
    Transcript show: n printString; show: '... '; cr ]
```

Prints: `10... 9... 8... ` etc., like a countdown.

### Counting with Custom Steps

You can use any step value:

```smalltalk
0 to: 100 by: 10 do: [ :n |
    Transcript show: n printString; cr ]
```

Prints: 0, 10, 20, 30, ..., 100.

```smalltalk
1 to: 20 by: 2 do: [ :n |
    Transcript show: n printString; cr ]
```

Prints odd numbers from 1 to 19.

## The While Loop: `whileTrue:` and `whileFalse:`

For loops where you don't know in advance how many iterations you need, use `whileTrue:` or `whileFalse:`.

### `whileTrue:`

```smalltalk
| counter |
counter := 1.
[ counter <= 5 ] whileTrue: [
    Transcript show: counter printString; cr.
    counter := counter + 1 ]
```

The first block is the **condition**. The second block is the **body**.

As long as the condition block evaluates to `true`, the body block executes.

Another example:

```smalltalk
| sum n |
sum := 0.
n := 1.
[ sum < 100 ] whileTrue: [
    sum := sum + n.
    n := n + 1 ].
sum  "Returns 105 (sum of 1+2+3+...+14)"
```

### `whileFalse:`

Continues while the condition is false:

```smalltalk
| done counter |
done := false.
counter := 0.
[ done ] whileFalse: [
    counter := counter + 1.
    Transcript show: counter printString; cr.
    counter >= 5 ifTrue: [ done := true ] ]
```

### Infinite Loops (Be Careful!)

```smalltalk
[ true ] whileTrue: [ Transcript show: 'Forever! ' ]
```

This runs forever! To stop it, you might need to interrupt Pharo (platform-dependent - often Cmd+. on Mac, Alt+. on Windows).

**Pro tip**: Don't run infinite loops in the Playground unless you're prepared to interrupt them!

## Collection Iteration

The most common loops in Smalltalk iterate over collections. You've seen these in Chapter 6, but let's explore them more deeply.

### `do:` - Execute for Each Element

```smalltalk
#(10 20 30 40 50) do: [ :each |
    Transcript show: each printString; show: ' '; cr ]
```

The block receives each element in turn.

Works with any collection:

```smalltalk
'Hello' do: [ :char |
    Transcript show: char; cr ]
```

Strings are collections of characters!

```smalltalk
| set |
set := Set with: 'apple' with: 'banana' with: 'cherry'.
set do: [ :fruit |
    Transcript show: fruit; cr ]
```

### `collect:` - Transform Each Element

```smalltalk
#(1 2 3 4 5) collect: [ :n | n * 2 ]
"Returns #(2 4 6 8 10)"
```

The block is executed for each element, and a new collection is created with the results.

```smalltalk
#('hello' 'world') collect: [ :str | str capitalized ]
"Returns #('Hello' 'World')"
```

### `select:` - Filter Elements

```smalltalk
#(1 2 3 4 5 6 7 8) select: [ :n | n even ]
"Returns #(2 4 6 8)"
```

Creates a new collection containing only elements for which the block returns `true`.

```smalltalk
#('apple' 'banana' 'apricot' 'cherry') select: [ :fruit |
    fruit beginsWith: 'a' ]
"Returns #('apple' 'apricot')"
```

### `reject:` - Filter Out Elements

The opposite of `select:`:

```smalltalk
#(1 2 3 4 5 6) reject: [ :n | n even ]
"Returns #(1 3 5)"
```

### `detect:` - Find First Match

```smalltalk
#(1 2 3 4 5) detect: [ :n | n > 3 ]
"Returns 4 (first number greater than 3)"
```

If nothing matches, you get an error. To handle that:

```smalltalk
#(1 2 3) detect: [ :n | n > 10 ] ifNone: [ 'Not found' ]
"Returns 'Not found'"
```

### `inject:into:` - Accumulate a Result

This is one of the most powerful iteration patterns:

```smalltalk
#(1 2 3 4 5) inject: 0 into: [ :sum :each | sum + each ]
"Returns 15 (sum of all elements)"
```

Let's trace through this:
- Start with `0` (the initial value)
- First iteration: `sum` is `0`, `each` is `1`, result is `0 + 1 = 1`
- Second iteration: `sum` is `1`, `each` is `2`, result is `1 + 2 = 3`
- Third iteration: `sum` is `3`, `each` is `3`, result is `3 + 3 = 6`
- Fourth iteration: `sum` is `6`, `each` is `4`, result is `6 + 4 = 10`
- Fifth iteration: `sum` is `10`, `each` is `5`, result is `10 + 5 = 15`

Another example - find the maximum:

```smalltalk
#(3 7 2 9 4) inject: 0 into: [ :max :each |
    max max: each ]
"Returns 9"
```

Build a string:

```smalltalk
#('Hello' 'from' 'Smalltalk') inject: '' into: [ :result :word |
    result , word , ' ' ]
"Returns 'Hello from Smalltalk '"
```

### `with:do:` - Iterate Two Collections Together

```smalltalk
| names ages |
names := #('Alice' 'Bob' 'Charlie').
ages := #(30 25 35).
names with: ages do: [ :name :age |
    Transcript show: name , ' is ' , age printString , ' years old'; cr ]
```

Both collections are iterated in parallel.

### `keysAndValuesDo:` - Iterate with Indices

For indexed collections:

```smalltalk
#('a' 'b' 'c' 'd') keysAndValuesDo: [ :index :value |
    Transcript show: index printString , ': ' , value; cr ]
```

Prints:
```
1: a
2: b
3: c
4: d
```

For dictionaries:

```smalltalk
| dict |
dict := Dictionary new.
dict at: 'name' put: 'Alice'.
dict at: 'age' put: 30.
dict keysAndValuesDo: [ :key :value |
    Transcript show: key , ': ' , value printString; cr ]
```

## Nested Loops

You can nest loops inside each other:

```smalltalk
1 to: 3 do: [ :i |
    1 to: 3 do: [ :j |
        Transcript show: i printString , ',' , j printString; cr ] ]
```

Prints:
```
1,1
1,2
1,3
2,1
2,2
2,3
3,1
3,2
3,3
```

### Multiplication Table

```smalltalk
1 to: 10 do: [ :i |
    1 to: 10 do: [ :j |
        Transcript show: (i * j) printString; tab ].
    Transcript cr ]
```

## Breaking Out of Loops

Smalltalk doesn't have `break` or `continue` keywords. Instead, you use conditions or exceptions.

### Using a Flag

```smalltalk
| found collection |
collection := #(1 2 3 4 5 6 7 8 9 10).
found := false.
collection do: [ :n |
    found ifFalse: [
        n > 5 ifTrue: [
            Transcript show: 'Found: ' , n printString; cr.
            found := true ] ] ]
```

### Using `detect:` Instead

Often, `detect:` is cleaner than manually breaking:

```smalltalk
| collection result |
collection := #(1 2 3 4 5 6 7 8 9 10).
result := collection detect: [ :n | n > 5 ].
Transcript show: 'Found: ' , result printString; cr
```

### Using Exceptions (Advanced)

You can use exceptions to break out early:

```smalltalk
[ #(1 2 3 4 5 6 7 8 9 10) do: [ :n |
      n > 5 ifTrue: [ Error signal: 'Found: ' , n printString ] ] ]
    on: Error
    do: [ :ex | Transcript show: ex messageText; cr ]
```

This is generally not recommended for normal control flow, but it works!

## Practical Loop Patterns

### Sum Pattern

```smalltalk
| numbers sum |
numbers := #(1 2 3 4 5).
sum := 0.
numbers do: [ :n | sum := sum + n ].
sum  "Returns 15"
```

Or more elegantly:

```smalltalk
#(1 2 3 4 5) sum  "Returns 15"
```

Or with `inject:into:`:

```smalltalk
#(1 2 3 4 5) inject: 0 into: [ :total :n | total + n ]
```

### Counting Pattern

```smalltalk
| words count |
words := #('apple' 'banana' 'apricot' 'cherry' 'avocado').
count := 0.
words do: [ :word |
    (word beginsWith: 'a') ifTrue: [ count := count + 1 ] ].
count  "Returns 3"
```

Or:

```smalltalk
words count: [ :word | word beginsWith: 'a' ]  "Returns 3"
```

### Building Collections

```smalltalk
| squares |
squares := OrderedCollection new.
1 to: 10 do: [ :n |
    squares add: (n * n) ].
squares
```

Or:

```smalltalk
(1 to: 10) collect: [ :n | n * n ]
```

### Finding Max/Min

```smalltalk
| numbers max |
numbers := #(3 7 2 9 4 1 6).
max := numbers first.
numbers do: [ :n |
    n > max ifTrue: [ max := n ] ].
max  "Returns 9"
```

Or simply:

```smalltalk
#(3 7 2 9 4 1 6) max  "Returns 9"
```

## Intervals

Intervals are special collections representing ranges:

```smalltalk
1 to: 10  "An Interval from 1 to 10"
```

```smalltalk
(1 to: 10) asArray  "#(1 2 3 4 5 6 7 8 9 10)"
```

Intervals are lazy - they don't create all the numbers in memory:

```smalltalk
| bigRange |
bigRange := 1 to: 1000000.
bigRange size  "Returns 1000000, but the numbers aren't all stored"
```

You can iterate intervals:

```smalltalk
(1 to: 5) do: [ :n |
    Transcript show: n printString; cr ]
```

With steps:

```smalltalk
(1 to: 20 by: 3) do: [ :n |
    Transcript show: n printString; cr ]
"Prints: 1, 4, 7, 10, 13, 16, 19"
```

Backwards:

```smalltalk
(10 to: 1 by: -1) do: [ :n |
    Transcript show: n printString; cr ]
"Counts down from 10 to 1"
```

## Practical Examples

### Example 1: Fibonacci Sequence

```smalltalk
| fibonacci a b |
fibonacci := OrderedCollection new.
a := 0.
b := 1.
10 timesRepeat: [
    fibonacci add: a.
    | temp |
    temp := a + b.
    a := b.
    b := temp ].
fibonacci  "Returns first 10 Fibonacci numbers"
```

### Example 2: Prime Numbers

```smalltalk
| primes |
primes := (2 to: 100) select: [ :n |
    (2 to: n - 1) allSatisfy: [ :divisor | n \\ divisor ~= 0 ] ].
primes  "All prime numbers from 2 to 100"
```

This works, but it's inefficient. A better implementation:

```smalltalk
| primes |
primes := (2 to: 100) select: [ :n | n isPrime ].
primes
```

The `isPrime` method is already defined on integers!

### Example 3: Word Frequency Counter

```smalltalk
| text words frequency |
text := 'the quick brown fox jumps over the lazy dog the quick fox'.
words := text substrings.
frequency := Dictionary new.

words do: [ :word |
    | count |
    count := frequency at: word ifAbsent: [ 0 ].
    frequency at: word put: count + 1 ].

frequency keysAndValuesDo: [ :word :count |
    Transcript show: word , ': ' , count printString; cr ]
```

### Example 4: Matrix Operations

```smalltalk
| matrix |
matrix := Array new: 5.
1 to: 5 do: [ :i |
    matrix at: i put: (Array new: 5) ].

"Fill with row * column"
1 to: 5 do: [ :row |
    1 to: 5 do: [ :col |
        (matrix at: row) at: col put: (row * col) ] ].

"Print the matrix"
matrix do: [ :row |
    row do: [ :value |
        Transcript show: value printString; tab ].
    Transcript cr ]
```

### Example 5: Password Validator

```smalltalk
| validatePassword |
validatePassword := [ :password |
    | hasDigit hasLetter hasSpecial |
    hasDigit := password anySatisfy: [ :char | char isDigit ].
    hasLetter := password anySatisfy: [ :char | char isLetter ].
    hasSpecial := password anySatisfy: [ :char | '!@#$%^&*' includes: char ].
    (password size >= 8) and: [ hasDigit and: [ hasLetter and: [ hasSpecial ] ] ] ].

validatePassword value: 'weak'           "false"
validatePassword value: 'Strong1!'       "true"
```

### Example 6: Binary Search (Using Loops)

```smalltalk
| binarySearch numbers |
numbers := #(1 3 5 7 9 11 13 15 17 19).

binarySearch := [ :array :target |
    | low high found result |
    low := 1.
    high := array size.
    found := false.
    result := nil.

    [ found not and: [ low <= high ] ] whileTrue: [
        | mid |
        mid := (low + high) // 2.
        (array at: mid) = target ifTrue: [
            found := true.
            result := mid ].
        (array at: mid) < target ifTrue: [ low := mid + 1 ].
        (array at: mid) > target ifTrue: [ high := mid - 1 ] ].

    result ].

binarySearch value: numbers value: 7   "Returns 4 (index of 7)"
binarySearch value: numbers value: 99  "Returns nil (not found)"
```

## Performance Considerations

Different iteration approaches have different performance characteristics:

### `do:` vs `to:do:`

For arrays, both are fast and comparable:

```smalltalk
"Both are efficient:"
#(1 2 3 4 5) do: [ :n | n * 2 ]
1 to: 5 do: [ :i | (#(1 2 3 4 5) at: i) * 2 ]
```

But `do:` is cleaner and works with any collection.

### `select:` Creates New Collections

Each `select:` or `collect:` creates a new collection. If you're chaining many operations, this can be inefficient:

```smalltalk
"Three new collections created:"
(1 to: 1000)
    select: [ :n | n even ]
    collect: [ :n | n squared ]
    select: [ :n | n > 100 ]
```

For large datasets, consider doing everything in one pass:

```smalltalk
(1 to: 1000) inject: OrderedCollection new into: [ :result :n |
    | squared |
    n even ifTrue: [
        squared := n squared.
        squared > 100 ifTrue: [ result add: squared ] ].
    result ]
```

But honestly, for most cases, the readable version is fine. Optimize only when profiling shows it's needed!

## Try This!

Practice with loops and iteration:

1. **Simple repetition:**
   ```smalltalk
   10 timesRepeat: [
       Transcript show: 'Coding is fun!'; cr ]
   ```

2. **Print numbers:**
   ```smalltalk
   1 to: 20 do: [ :n |
       Transcript show: n printString; show: ' ' ].
   Transcript cr
   ```

3. **Countdown:**
   ```smalltalk
   10 to: 1 by: -1 do: [ :n |
       Transcript show: n printString; show: '... ' ].
   Transcript show: 'Blast off!'; cr
   ```

4. **Sum with while:**
   ```smalltalk
   | sum n |
   sum := 0.
   n := 1.
   [ n <= 100 ] whileTrue: [
       sum := sum + n.
       n := n + 1 ].
   sum  "Sum of 1 to 100"
   ```

5. **Filter and transform:**
   ```smalltalk
   (1 to: 20)
       select: [ :n | n odd ]
       collect: [ :n | n squared ]
   ```

6. **Build a multiplication table:**
   ```smalltalk
   (1 to: 12) collect: [ :n |
       (1 to: 12) collect: [ :m | n * m ] ]
   ```

7. **Word length counter:**
   ```smalltalk
   | text lengths |
   text := 'The quick brown fox jumps over the lazy dog'.
   lengths := Dictionary new.
   text substrings do: [ :word |
       | len count |
       len := word size.
       count := lengths at: len ifAbsent: [ 0 ].
       lengths at: len put: count + 1 ].
   lengths
   ```

8. **Find all factors:**
   ```smalltalk
   | number factors |
   number := 24.
   factors := (1 to: number) select: [ :n | number \\ n = 0 ].
   factors
   ```

9. **Generate patterns:**
   ```smalltalk
   1 to: 5 do: [ :row |
       row timesRepeat: [ Transcript show: '*' ].
       Transcript cr ]
   ```

10. **Collatz sequence:**
    ```smalltalk
    | n steps |
    n := 27.
    steps := OrderedCollection new.
    [ n ~= 1 ] whileTrue: [
        steps add: n.
        n even
            ifTrue: [ n := n / 2 ]
            ifFalse: [ n := (3 * n) + 1 ] ].
    steps add: 1.
    steps
    ```

## Common Mistakes

### Modifying Collection While Iterating

```smalltalk
| list |
list := OrderedCollection with: 1 with: 2 with: 3 with: 4 with: 5.
list do: [ :n |
    n even ifTrue: [ list remove: n ] ].  "Dangerous!"
```

This can cause errors! Instead, collect items to remove, then remove them:

```smalltalk
| list toRemove |
list := OrderedCollection with: 1 with: 2 with: 3 with: 4 with: 5.
toRemove := list select: [ :n | n even ].
toRemove do: [ :n | list remove: n ].
```

Or use `reject:` to create a new collection:

```smalltalk
list := list reject: [ :n | n even ]
```

### Forgetting Block Arguments

```smalltalk
#(1 2 3) do: [ Transcript show: 'Number'; cr ]  "Wrong! Ignores the numbers"
```

Should be:

```smalltalk
#(1 2 3) do: [ :n | Transcript show: n printString; cr ]
```

### Off-by-One Errors

```smalltalk
0 to: 10 do: [ :n | "..." ]  "0 to 10 is 11 iterations!"
```

If you want exactly 10 iterations:

```smalltalk
1 to: 10 do: [ :n | "..." ]  "or"
0 to: 9 do: [ :n | "..." ]
```

## Coming Up Next

Congratulations! You've now completed Part III - you understand how to make decisions with conditionals and how to repeat actions with loops and iteration.

You have all the fundamental programming concepts:
- Objects and messages
- Basic types (numbers, strings, etc.)
- Collections
- Variables
- Conditionals
- Blocks
- Loops and iteration

Now it's time to create your own objects! In Part IV (Chapters 11-15), you'll learn:

- **Chapter 11**: How to define your own classes
- **Chapter 12**: How to add methods (behavior) to your classes
- **Chapter 13**: How to give your objects state with instance variables
- **Chapter 14**: How `self` and `super` work
- **Chapter 15**: How to use inheritance to build on existing classes

This is where you transform from using Smalltalk's built-in objects to creating your own custom objects. This is where programming becomes truly creative!

Let's go!

---

**Key Takeaways:**
- Loops in Smalltalk are messages sent to objects with blocks
- `timesRepeat:` repeats an action a specific number of times
- `to:do:` iterates with a counter
- `whileTrue:` and `whileFalse:` loop while conditions hold
- Collections have rich iteration methods: `do:`, `collect:`, `select:`, `reject:`, `detect:`, `inject:into:`
- Intervals represent ranges and can be iterated efficiently
- Nested loops are just loops inside loops
- Choose the right iteration method for readability and clarity
- Most iteration patterns already have elegant Smalltalk methods
- When in doubt, use `do:` for simple iteration and build from there

---

[Previous: Chapter 9 - Blocks](chapter-09-blocks.md) | [Next: Chapter 11 - Classes](chapter-11-classes.md)
