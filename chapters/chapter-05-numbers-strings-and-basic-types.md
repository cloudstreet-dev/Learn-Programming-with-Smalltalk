# Chapter 5: Numbers, Strings, and Basic Types

Now that you understand that everything is an object and objects communicate through messages, let's explore the basic types that Smalltalk provides. These are the building blocks you'll use constantly: numbers, strings, characters, booleans, and nil.

Each type is a class of objects, and each object responds to certain messages. Let's dive in!

## Numbers

In Smalltalk, numbers are objects - full-fledged objects with methods and behavior. There are several types of numbers, each optimized for different purposes.

### Integers

**Integers** are whole numbers without decimal points.

```smalltalk
42
-17
0
999999999999999999999
```

Try this:

```smalltalk
42 class
```

You'll probably see `SmallInteger`. This is the class for small integers (roughly -1073741824 to 1073741823, depending on your platform).

Now try this:

```smalltalk
999999999999999999999 class
```

You'll see `LargePositiveInteger`. When integers get too big for SmallInteger, Smalltalk automatically uses LargePositiveInteger (or LargeNegativeInteger for negative large numbers).

**This automatic switching is invisible to you!** You don't need to worry about it - Smalltalk handles it seamlessly. This is one of the beauties of the object-oriented approach.

### Arithmetic with Integers

All the standard arithmetic operations work as messages:

```smalltalk
10 + 5      "Addition → 15"
10 - 5      "Subtraction → 5"
10 * 5      "Multiplication → 50"
10 / 5      "Division → 2 (or 2.0 in some cases)"
10 // 5     "Integer division → 2"
10 \\ 3     "Modulo (remainder) → 1"
```

Try these in your Playground!

**Division Note**: Regular division `/` can return a Fraction or Float. For pure integer division, use `//`.

```smalltalk
7 / 2       "Returns a Fraction: 7/2 or converts to 3.5"
7 // 2      "Returns an Integer: 3"
7 \\ 2      "Returns the remainder: 1"
```

### Powers and Roots

```smalltalk
2 ** 8      "2 to the 8th power → 256"
5 squared   "5 squared → 25"
16 sqrt     "Square root of 16 → 4.0"
```

The `**` operator raises a number to a power. Note that `sqrt` returns a Float.

### Interesting Integer Messages

Integers respond to many useful messages:

```smalltalk
5 factorial         "5! = 120"
100 factorial       "Huge number! Try it!"
7 isPrime           "true"
8 isPrime           "false"
-5 abs              "Absolute value → 5"
5 negated           "Negation → -5"
10 even             "true"
11 odd              "true"
5 max: 10           "Maximum → 10"
5 min: 10           "Minimum → 5"
7 gcd: 21           "Greatest common divisor → 7"
```

Try calculating `100 factorial` and then converting it to a string to see how many digits it has:

```smalltalk
100 factorial asString size
```

158 digits! And Smalltalk handled it effortlessly.

### Iteration Messages

Integers understand messages for iteration (repeating actions):

```smalltalk
5 timesRepeat: [ Transcript show: 'Hello!'; cr ]
```

This executes the block (the code in square brackets) 5 times. Check your Transcript!

```smalltalk
1 to: 10 do: [ :each | Transcript show: each printString; cr ]
```

This iterates from 1 to 10, executing the block for each number. The `:each` is a block parameter (we'll cover this in Chapter 9).

### Floats

**Floats** (floating-point numbers) represent numbers with decimal points.

```smalltalk
3.14
-2.5
0.001
1.0e6       "Scientific notation: 1.0 × 10^6 = 1000000.0"
```

```smalltalk
3.14 class  "Returns Float"
```

Arithmetic works the same way:

```smalltalk
3.14 + 2.86    "6.0"
10.5 * 2       "21.0"
9.5 / 2        "4.75"
```

### Float Messages

```smalltalk
3.14159 rounded        "3"
3.14159 roundTo: 0.01  "3.14"
3.7 floor              "3"
3.2 ceiling            "4"
3.5 truncated          "3"
-3.5 abs               "3.5"
```

### Converting Between Number Types

```smalltalk
42 asFloat             "42.0"
3.14 asInteger         "3"
3.14 rounded           "3"
7 / 2 asFloat          "3.5"
```

### Fractions

Smalltalk has a Fraction class for representing exact rational numbers:

```smalltalk
3 / 4           "Creates a Fraction: (3/4)"
1/2 + 1/3       "5/6"
2/3 asFloat     "0.6666666666666666"
```

Fractions maintain exactness:

```smalltalk
(1/3) + (1/3) + (1/3)   "Returns exactly 1, not 0.9999999"
```

This is much more accurate than floating-point arithmetic for some calculations!

### Comparing Numbers

```smalltalk
5 = 5           "true"
5 = 6           "false"
5 ~= 6          "true (not equal)"
5 < 10          "true"
5 > 10          "false"
5 <= 5          "true"
10 >= 5         "true"
```

The `=` message tests for equality. The `~=` message tests for inequality (not equal).

### Random Numbers

```smalltalk
10 atRandom          "Random integer from 1 to 10"
100 atRandom         "Random integer from 1 to 100"
Float random         "Random float between 0.0 and 1.0"
```

Try running `10 atRandom` several times - you'll get different results!

## Strings

**Strings** are sequences of characters, enclosed in single quotes.

```smalltalk
'Hello, World!'
'Smalltalk is fun'
'42'            "This is a string, not a number"
''              "Empty string"
```

### String Basics

```smalltalk
'Hello' class   "Returns ByteString (usually)"
```

### String Messages

```smalltalk
'Hello' size                "5"
'Hello' at: 1               "$H (first character)"
'Hello' at: 5               "$o (fifth character)"
'Hello' first               "$H"
'Hello' last                "$o"
```

**Important**: In Smalltalk, collection indices start at 1, not 0!

### String Concatenation

```smalltalk
'Hello' , ' ' , 'World'     "'Hello World'"
'Small' , 'talk'            "'Smalltalk'"
```

The comma `,` message concatenates strings.

### String Case Conversion

```smalltalk
'hello' capitalized         "'Hello'"
'hello' asUppercase         "'HELLO'"
'HELLO' asLowercase         "'hello'"
```

### String Manipulation

```smalltalk
'Hello' reversed            "'olleH'"
'hello' sorted              "'ehllo' (alphabetically sorted)"
```

### Extracting Substrings

```smalltalk
'Hello World' copyFrom: 1 to: 5           "'Hello'"
'Hello World' copyFrom: 7 to: 11          "'World'"
```

### Testing String Content

```smalltalk
'Hello' includesSubstring: 'ell'          "true"
'Hello' includesSubstring: 'xyz'          "false"
'Hello' beginsWith: 'Hel'                 "true"
'Hello' endsWith: 'lo'                    "true"
```

### Finding in Strings

```smalltalk
'Hello World' findString: 'World'         "7 (position where 'World' starts)"
'Hello World' findString: 'xyz'           "0 (not found)"
```

### String Repetition

```smalltalk
'Ha' repeat: 3              "'HaHaHa'"
'*' repeat: 10              "'**********'"
```

Wait, that's not quite right. Let me check... Actually, in Pharo, you'd typically do:

```smalltalk
'Ha' , 'Ha' , 'Ha'          "Concatenation"
```

Or using a collection approach (we'll cover this more in Chapter 6):

```smalltalk
(String new: 10) atAllPut: $*   "Creates '**********'"
```

### Replacing in Strings

```smalltalk
'Hello World' copyReplaceAll: 'World' with: 'Smalltalk'
"Returns 'Hello Smalltalk'"
```

### Trimming Whitespace

```smalltalk
'  hello  ' trimBoth        "'hello'"
'  hello' trimLeft          "'hello'"
'hello  ' trimRight         "'hello'"
```

### Converting to/from Numbers

```smalltalk
'42' asInteger              "42 (the number)"
'3.14' asNumber             "3.14 (the float)"
42 asString                 "'42' (the string)"
42 printString              "'42' (same as asString)"
```

### Multiline Strings

You can write multiline strings directly:

```smalltalk
'This is
a multiline
string'
```

The line breaks are part of the string!

## Characters

Individual characters are objects too! They're represented with a dollar sign followed by the character:

```smalltalk
$A              "The character A"
$z              "The character z"
$5              "The character 5 (not the number)"
$               "The space character"
$$              "The dollar sign character itself"
```

### Character Messages

```smalltalk
$A asLowercase              "$a"
$z asUppercase              "$Z"
$A isVowel                  "true"
$B isVowel                  "false"
$A isLetter                 "true"
$5 isDigit                  "true"
$A isDigit                  "false"
$ isS eparator              "true"
```

### Character Codes

```smalltalk
$A asciiValue               "65"
$a asciiValue               "97"
Character value: 65         "$A"
```

### Strings are Collections of Characters

```smalltalk
'Hello' at: 1               "$H"
'Hello' first               "$H"
'Hello' do: [ :char | Transcript show: char; cr ]
```

This last example iterates through each character in the string.

## Booleans

Smalltalk has two boolean objects: `true` and `false`. Yes, they're objects!

```smalltalk
true class      "True"
false class     "False"
```

Notice the classes are capitalized: `True` and `False`. There's only one instance of each, and those instances are named `true` and `false` (lowercase).

### Boolean Operations

```smalltalk
true not        "false"
false not       "true"
true & false    "false (logical AND)"
true | false    "true (logical OR)"
true xor: false "true (exclusive OR)"
```

### Comparison Results are Booleans

```smalltalk
5 > 3           "true"
5 < 3           "false"
5 = 5           "true"
5 = 6           "false"
5 ~= 6          "true (not equal)"
```

### Conditionals

Booleans understand conditional messages (we'll explore these deeply in Chapter 8):

```smalltalk
true ifTrue: [ Transcript show: 'It is true!' ]
```

```smalltalk
false ifFalse: [ Transcript show: 'It is false!' ]
```

```smalltalk
(5 > 3) ifTrue: [ Transcript show: 'Yes!' ] ifFalse: [ Transcript show: 'No!' ]
```

This is how conditionals work in Smalltalk - they're messages sent to boolean objects!

## Nil - The Absence of Value

`nil` represents "nothing" or "no value". It's the default value for uninitialized variables and is returned when something has no meaningful value to return.

```smalltalk
nil class       "UndefinedObject"
```

There's only one instance of UndefinedObject, and it's called `nil`.

### Testing for Nil

```smalltalk
nil isNil       "true"
42 isNil        "false"
nil notNil      "false"
42 notNil       "true"
```

### Nil in Conditionals

```smalltalk
nil ifNil: [ Transcript show: 'It is nil!' ]
```

```smalltalk
42 ifNotNil: [ Transcript show: 'It is not nil!' ]
```

### Nil as a Placeholder

When you declare a variable but don't assign it a value, it contains `nil`:

```smalltalk
| x |
x isNil  "true"
```

## Symbols

**Symbols** are like strings, but they're unique and immutable. They're primarily used as identifiers in Smalltalk.

```smalltalk
#hello
#mySymbol
#AnotherSymbol
```

Symbols start with a hash/pound sign `#`.

### Why Symbols?

Symbols are guaranteed to be unique. If you use the symbol `#hello` in two different places, they're the exact same object in memory.

```smalltalk
#hello == #hello        "true (identical objects)"
'hello' == 'hello'      "false (different string objects)"
```

The `==` operator tests for object identity (same object), while `=` tests for value equality.

Symbols are more efficient than strings for comparison because they only need to check if they're the same object, not compare character by character.

### Converting Between Symbols and Strings

```smalltalk
#hello asString         "'hello'"
'hello' asSymbol        "#hello"
```

### When to Use Symbols

You'll typically use symbols for:
- Keys in dictionaries (Chapter 6)
- Method selectors
- Constants or enumeration values
- Anywhere you need a unique identifier

For user-facing text, use strings. For internal identifiers, use symbols.

## Type Conversions

Smalltalk makes it easy to convert between types:

```smalltalk
42 asString             "'42'"
42 asFloat              "42.0"
42 printString          "'42'"
'42' asInteger          "42"
'3.14' asNumber         "3.14"
true asString           "'true'"
#hello asString         "'hello'"
'hello' asSymbol        "#hello"
$A asString             "'A'"
```

The `asString` or `printString` message works on almost everything, making it easy to convert values for display.

## Comparing Objects

We've seen several comparison operators. Let's consolidate:

### Value Equality: `=`

Tests if two objects have the same value:

```smalltalk
5 = 5           "true"
5 = 6           "false"
'Hello' = 'Hello'   "true"
#hello = #hello     "true"
```

### Value Inequality: `~=`

Tests if two objects have different values:

```smalltalk
5 ~= 6          "true"
5 ~= 5          "false"
```

### Identity Equality: `==`

Tests if two objects are the exact same object in memory:

```smalltalk
| x y |
x := 'Hello'.
y := 'Hello'.
x = y           "true (same value)"
x == y          "might be false (different objects)"
```

```smalltalk
#hello == #hello    "true (symbols are unique)"
```

Most of the time, you want `=` (value equality). Use `==` only when you specifically need to know if two variables point to the exact same object.

### Ordering: `<`, `>`, `<=`, `>=`

Compare magnitude:

```smalltalk
5 < 10          "true"
5 > 10          "false"
5 <= 5          "true"
10 >= 5         "true"
'apple' < 'banana'  "true (alphabetical)"
```

## Polymorphism in Action

Here's something beautiful: many messages work across different types of objects because they all respond to the same message:

```smalltalk
42 printString              "'42'"
3.14 printString            "'3.14'"
'Hello' printString         "'Hello'"
true printString            "'true'"
#symbol printString         "'symbol'"
```

Different objects respond to `printString` in their own appropriate way. This is **polymorphism** - one message, many implementations.

Similarly:

```smalltalk
42 + 3                      "Integer addition"
3.14 + 2.86                 "Float addition"
'Hello' , ' World'          "String concatenation (not +, but similar concept)"
(1@2) + (3@4)               "Point addition"
```

Each class defines what `+` means for its objects.

## The Object Protocol

Most objects in Smalltalk respond to certain common messages:

```smalltalk
object class            "What class is this object?"
object printString      "Convert to string"
object inspect          "Open an inspector"
object isNil            "Is this nil?"
object notNil           "Is this not nil?"
object yourself         "Return the object itself"
object copy             "Make a copy"
object = another        "Test equality"
```

This common protocol makes Smalltalk objects work together smoothly.

## Try This!

Time to experiment with all these types:

1. **Number exploration:**
   ```smalltalk
   42 inspect
   ```
   In the inspector, try messages like `factorial`, `isPrime`, `squared`, `sqrt`.

2. **Large numbers:**
   ```smalltalk
   2 ** 1000
   ```
   How many digits does this have? (Use `printString size`)

3. **String puzzles:**
   ```smalltalk
   'racecar' = 'racecar' reversed
   ```
   Why is this true?

   ```smalltalk
   'Was it a car or a cat I saw' asLowercase copyWithout: $
   ```
   What does this produce?

4. **Character iteration:**
   ```smalltalk
   'ABCDE' do: [ :char |
       Transcript show: char;
       show: ' is ';
       show: char asciiValue printString;
       cr ]
   ```
   Run this and check your Transcript.

5. **Boolean logic:**
   ```smalltalk
   (true & false) | (true & true)
   ```
   What's the result? Can you explain why?

6. **Type conversions:**
   ```smalltalk
   | x |
   x := '42'.
   x asInteger + 8
   ```
   What happens? Now try without `asInteger`.

7. **Comparing values:**
   ```smalltalk
   5 = 5.0
   5 == 5.0
   ```
   Are these results the same? Why or why not?

8. **Symbol uniqueness:**
   ```smalltalk
   | x y |
   x := #hello.
   y := #hello.
   x == y
   ```
   What's the result?

9. **Random experiments:**
   ```smalltalk
   10 timesRepeat: [ Transcript show: 10 atRandom printString; cr ]
   ```
   Run this a few times - you'll get different sequences!

10. **Nil handling:**
    ```smalltalk
    | x |
    x ifNil: [ Transcript show: 'x is nil' ] ifNotNil: [ Transcript show: 'x is not nil' ]
    ```
    What happens? Now assign a value to `x` and try again.

## Common Pitfalls

### String vs Symbol Confusion

```smalltalk
'hello' = #hello        "false"
```

Strings and symbols are different types! Convert if needed:

```smalltalk
'hello' = #hello asString   "true"
```

### Integer Division Surprises

```smalltalk
5 / 2           "Might return Fraction (5/2) or Float (2.5)"
5 // 2          "Always returns Integer (2)"
```

Use `//` when you specifically want integer division.

### Comparison vs Assignment

```smalltalk
x = 5           "This is a comparison, returns true or false"
x := 5          "This is assignment"
```

Remember: `:=` assigns, `=` compares!

### Case Sensitivity

```smalltalk
'Hello' = 'hello'       "false"
```

Strings are case-sensitive! To do case-insensitive comparison:

```smalltalk
'Hello' asLowercase = 'hello' asLowercase   "true"
```

## Why These Types Matter

These basic types - numbers, strings, characters, booleans, nil, and symbols - are your fundamental building blocks. Every program you write will use them extensively.

Understanding:
- What messages each type responds to
- How to convert between types
- The differences between types (strings vs symbols, = vs ==)
- Common operations for each type

...will make you productive in Smalltalk.

And remember: these aren't primitive types built into the language. They're classes, implemented in Smalltalk itself, and you can browse their code to see how they work!

## Coming Up Next

We've covered individual objects - numbers, strings, and so on. But often you need to work with *groups* of objects. How do you store a list of names? A collection of numbers? A mapping of keys to values?

That's what Chapter 6 is all about: **Collections** - arrays, lists, sets, dictionaries, and more. Collections are some of Smalltalk's most powerful features, and you'll use them constantly.

Let's keep building!

---

**Key Takeaways:**
- Numbers (integers, floats, fractions) are objects with rich behavior
- Smalltalk handles arbitrarily large integers automatically
- Strings are sequences of characters, indexed from 1
- Characters are individual objects denoted with `$`
- Booleans (`true` and `false`) are objects that understand conditional messages
- `nil` represents absence of value
- Symbols are unique, immutable identifiers starting with `#`
- Use `=` for value equality, `==` for identity equality
- Most objects respond to common messages like `printString`, `class`, `inspect`
- Type conversion is easy with messages like `asString`, `asInteger`, `asFloat`
- Everything is discoverable - inspect objects to see what they contain

---

[Previous: Chapter 4 - Everything is an Object](chapter-04-everything-is-an-object.md) | [Next: Chapter 6 - Collections](chapter-06-collections.md)
