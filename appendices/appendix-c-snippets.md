# Appendix C: Useful Code Snippets

A collection of handy Smalltalk code snippets for common tasks. Keep this reference nearby for quick copy-paste solutions!

## Collections

### Creating Collections

```smalltalk
"Array - fixed size"
#(1 2 3 4 5)
Array with: 1 with: 2 with: 3

"OrderedCollection - dynamic"
OrderedCollection new
OrderedCollection with: 1 with: 2 with: 3
{ 1. 2. 3 } asOrderedCollection

"Set - unique elements"
Set new add: 1; add: 2; add: 1; yourself  "-> Set(1 2)"

"Dictionary - key-value pairs"
Dictionary new at: 'name' put: 'Alice'; yourself
{ 'name' -> 'Alice'. 'age' -> 30 } asDictionary

"Bag - counts occurrences"
Bag new add: 'apple'; add: 'apple'; add: 'banana'; yourself
```

### Collection Operations

```smalltalk
"Iterate"
collection do: [ :each | Transcript show: each; cr ].

"Transform"
collection collect: [ :each | each * 2 ].

"Filter"
collection select: [ :each | each even ].
collection reject: [ :each | each odd ].

"Find"
collection detect: [ :each | each > 10 ] ifNone: [ nil ].
collection anySatisfy: [ :each | each > 10 ].
collection allSatisfy: [ :each | each > 0 ].

"Reduce"
collection inject: 0 into: [ :sum :each | sum + each ].

"Chain operations"
collection
    select: [ :each | each > 10 ]
    thenCollect: [ :each | each squared ].

"Sort"
collection sorted.
collection sorted: [ :a :b | a > b ].  "Descending"
collection sorted: #size ascending.    "By property"

"Group"
collection groupedBy: #even.  "-> Dictionary with true/false keys"

"Convert"
collection asArray.
collection asSet.
collection asOrderedCollection.
collection asSortedCollection.
```

### Working with Dictionaries

```smalltalk
"Create"
dict := Dictionary new.
dict at: 'name' put: 'Alice'.
dict at: 'age' put: 30.

"Access"
dict at: 'name'.                    "-> 'Alice'"
dict at: 'missing' ifAbsent: [ 'default' ].
dict at: 'age' ifPresent: [ :v | v + 1 ].

"Check"
dict includesKey: 'name'.           "-> true"
dict includes: 'Alice'.             "-> true (checks values)"

"Iterate"
dict keysAndValuesDo: [ :key :value |
    Transcript show: key, ': ', value asString; cr ].

dict keys.                          "All keys"
dict values.                        "All values"
dict associations.                  "Key-value pairs"

"Remove"
dict removeKey: 'age'.
dict removeKey: 'missing' ifAbsent: [ ].

"Get with default"
dict at: 'count' ifAbsentPut: [ 0 ].
```

## Strings

### String Manipulation

```smalltalk
"Concatenation"
'Hello', ' ', 'World'.
String streamContents: [ :s |
    s nextPutAll: 'Hello';
      space;
      nextPutAll: 'World' ].

"Formatting"
'Hello {1}!' format: { 'Alice' }.
'x={1}, y={2}' format: { 10. 20 }.

"Case"
'hello' asUppercase.                "-> 'HELLO'"
'WORLD' asLowercase.                "-> 'world'"
'hello world' capitalized.          "-> 'Hello world'"

"Trimming"
'  hello  ' trimBoth.               "-> 'hello'"
'  hello  ' trimLeft.               "-> 'hello  '"
'  hello  ' trimRight.              "-> '  hello'"

"Splitting"
'one,two,three' splitOn: $,.       "-> #('one' 'two' 'three')"
'one two  three' substrings.       "-> #('one' 'two' 'three')"
'a-b-c' splitOn: '-'.

"Substring"
'Hello World' copyFrom: 1 to: 5.   "-> 'Hello'"
'Hello World' allButFirst: 6.      "-> 'World'"
'Hello World' first: 5.            "-> 'Hello'"
'Hello World' last: 5.             "-> 'World'"

"Search"
'Hello World' includesSubstring: 'World'.
'Hello World' findString: 'World'. "-> 7 (index)"
'Hello World' beginsWith: 'Hello'.
'Hello World' endsWith: 'World'.

"Replace"
'Hello World' copyReplaceAll: 'World' with: 'Universe'.
```

### String Building

```smalltalk
"Efficient string building"
String streamContents: [ :stream |
    1 to: 100 do: [ :n |
        stream
            nextPutAll: 'Number: ';
            print: n;
            cr ] ].

"Join collection"
#('one' 'two' 'three') joinUsing: ', '.  "-> 'one, two, three'"

"Repeat"
'abc' repeat: 3.                    "-> 'abcabcabc'"
```

## Numbers

### Arithmetic

```smalltalk
"Basic operations"
5 + 3.                              "-> 8"
5 - 3.                              "-> 2"
5 * 3.                              "-> 15"
5 / 3.                              "-> 1.66666... (Float)"
5 // 3.                             "-> 1 (Integer division)"
5 \\ 3.                             "-> 2 (Modulo/remainder)"
2 ** 8.                             "-> 256 (Power)"

"Rounding"
3.7 rounded.                        "-> 4"
3.7 floor.                          "-> 3"
3.2 ceiling.                        "-> 4"
3.7 truncated.                      "-> 3"
3.14159 roundTo: 0.01.             "-> 3.14"

"Testing"
5 even.                             "-> false"
4 even.                             "-> true"
5 odd.                              "-> true"
7 isPrime.                          "-> true"

"Range"
5 max: 10.                          "-> 10"
5 min: 10.                          "-> 5"
5 between: 1 and: 10.              "-> true"

"Absolute value"
-5 abs.                             "-> 5"

"Sign"
-5 sign.                            "-> -1"
5 sign.                             "-> 1"
0 sign.                             "-> 0"
```

### Random Numbers

```smalltalk
"Random integer"
100 atRandom.                       "1 to 100"
Random new nextInt: 100.           "0 to 99"

"Random float"
Random new next.                    "0.0 to 1.0"
Random new next: 5.                "Array of 5 random floats"

"Random from collection"
#(1 2 3 4 5) atRandom.
```

### Math Functions

```smalltalk
"Trigonometry"
45 degreesToRadians sin.
Float pi cos.
1 arcTan.

"Exponential"
2.718281828 ln.                     "Natural log"
100 log.                            "Base 10 log"
Float e.                            "Euler's number"

"Square root"
25 sqrt.                            "-> 5"
2 sqrt.                             "-> 1.41421..."

"Other"
5 squared.                          "-> 25"
5 factorial.                        "-> 120"
```

## Dates and Times

### Current Date/Time

```smalltalk
"Now"
DateAndTime now.
Date today.
Time now.

"Components"
DateAndTime now hour.
DateAndTime now minute.
DateAndTime now dayOfWeek.
DateAndTime now monthName.
```

### Creating Dates

```smalltalk
"Specific date"
Date year: 2024 month: 12 day: 25.
Date fromString: '2024-12-25'.

"Specific time"
Time hour: 14 minute: 30 second: 0.
Time fromString: '14:30:00'.

"Date and time"
DateAndTime
    year: 2024 month: 12 day: 25
    hour: 14 minute: 30 second: 0.
```

### Date Arithmetic

```smalltalk
"Add/subtract"
Date today + 7 days.
Date today - 1 week.
Date today + 1 month.
Date today + 1 year.

"Difference"
(Date today - Date yesterday) days.  "-> 1"

"Comparison"
Date today < Date tomorrow.          "-> true"
Date today between: Date yesterday and: Date tomorrow.
```

### Formatting

```smalltalk
"Print formats"
Date today printFormat: #(1 2 3 $- 1 1).  "YYYY-MM-DD"
Date today mmddyyyy.                      "MM/DD/YYYY"
DateAndTime now asString.
```

## Files and Streams

### File Operations

```smalltalk
"Read entire file"
'data.txt' asFileReference contents.

"Read lines"
'data.txt' asFileReference readStreamDo: [ :stream |
    stream contents lines ].

"Write file"
'output.txt' asFileReference writeStreamDo: [ :stream |
    stream nextPutAll: 'Hello, World!' ].

"Append to file"
'log.txt' asFileReference appendStreamDo: [ :stream |
    stream
        nextPutAll: DateAndTime now asString;
        nextPutAll: ': Log entry';
        cr ].

"File info"
'data.txt' asFileReference exists.
'data.txt' asFileReference size.
'data.txt' asFileReference modificationTime.

"Delete file"
'temp.txt' asFileReference ensureDelete.
```

### Directory Operations

```smalltalk
"List files"
'.' asFileReference children.
'.' asFileReference files.
'.' asFileReference directories.

"Create directory"
'new-folder' asFileReference ensureCreateDirectory.

"Recursive list"
'.' asFileReference allFiles.
'.' asFileReference allDirectories.

"Navigate"
FileLocator home / 'Documents' / 'data.txt'.
FileLocator workingDirectory / 'src' / 'Main.st'.
```

### Stream Processing

```smalltalk
"Line by line"
'large-file.txt' asFileReference readStreamDo: [ :stream |
    [ stream atEnd ] whileFalse: [
        | line |
        line := stream nextLine.
        "Process line..." ] ].

"Build string"
String streamContents: [ :stream |
    stream
        nextPutAll: 'Name: ';
        nextPutAll: person name;
        cr;
        nextPutAll: 'Age: ';
        print: person age ].
```

## Blocks

### Common Block Patterns

```smalltalk
"Simple block"
[ 2 + 2 ] value.                    "-> 4"

"Block with arguments"
[ :x | x * 2 ] value: 5.           "-> 10"
[ :x :y | x + y ] value: 3 value: 4.  "-> 7"

"Multiple statements"
[
    | result |
    result := 10 * 2.
    result + 5
] value.                            "-> 25"

"Conditional execution"
condition ifTrue: [ "do this" ] ifFalse: [ "do that" ].

"Error handling"
[ riskyOperation ] on: Error do: [ :ex |
    Transcript show: 'Error: ', ex messageText; cr ].

"Cleanup"
[ doWork ] ensure: [ cleanup ].

"Timing"
[ expensiveOperation ] timeToRun.
[ operation ] bench.
```

## Error Handling

### Try-Catch Pattern

```smalltalk
"Basic try-catch"
[ self dangerousOperation ]
    on: Error
    do: [ :ex |
        Transcript show: 'Error: ', ex messageText; cr ].

"Specific exceptions"
[ self parseNumber: input ]
    on: NumberParserError
    do: [ :ex | 0 ].

"Multiple exception types"
[ self operation ]
    on: NetworkError, TimeoutError
    do: [ :ex | self handleNetworkIssue: ex ].

"Re-raise"
[ self operation ]
    on: Error
    do: [ :ex |
        self logError: ex.
        ex pass ].  "Re-raise"
```

### Custom Exceptions

```smalltalk
"Define"
Error subclass: #ValidationError
    instanceVariableNames: 'field'
    classVariableNames: ''
    package: 'MyApp'

"Raise"
ValidationError signal: 'Invalid email format'.
(ValidationError new field: 'email') signal.

"Catch specific"
[ self validateUser ]
    on: ValidationError
    do: [ :ex | self showError: ex field ].
```

## Testing

### SUnit Test Template

```smalltalk
TestCase subclass: #MyClassTest
    instanceVariableNames: 'fixture'
    classVariableNames: ''
    package: 'MyApp-Tests'

setUp
    "Run before each test"
    fixture := MyClass new

tearDown
    "Run after each test"
    fixture := nil

testSomething
    "Test that something works"
    | result |
    result := fixture doSomething: 42.
    self assert: result equals: 84

testError
    "Test that error is raised"
    self should: [ fixture badOperation ] raise: Error

testNoError
    "Test that no error is raised"
    self shouldnt: [ fixture goodOperation ] raise: Error
```

### Common Assertions

```smalltalk
"Equality"
self assert: actual equals: expected.
self deny: actual equals: notExpected.

"Boolean"
self assert: condition.
self deny: negativeCondition.

"Nil"
self assert: result isNil.
self deny: result isNil.

"Collections"
self assert: collection isEmpty.
self assert: collection size equals: 3.
self assert: (collection includes: item).

"Exceptions"
self should: [ code ] raise: ErrorClass.
self shouldnt: [ code ] raise: ErrorClass.

"Floats (with tolerance)"
self assert: 3.14159 closeTo: Float pi.
```

## Object Inspection

### Debugging Snippets

```smalltalk
"Inspect object"
myObject inspect.

"Halt execution"
self halt.
self halt: 'Breakpoint here'.

"Trace execution"
Transcript show: 'Value: ', value asString; cr.
Transcript show: self printString; cr.

"Log to Transcript"
[ heavyOperation ] timeToRun.       "See how long it takes"

"Check type"
object class.
object class name.
object isKindOf: Array.
object respondsTo: #size.
```

### Object Creation Patterns

```smalltalk
"Basic instantiation"
MyClass new.

"With initialization"
MyClass new initialize; yourself.

"With setup"
MyClass new
    name: 'Alice';
    age: 30;
    yourself.

"Copy"
original copy.
original deepCopy.

"Singleton"
MyClass uniqueInstance.
```

## System Queries

### Finding Code

```smalltalk
"Find implementors"
SystemNavigation default allImplementorsOf: #size.

"Find senders"
SystemNavigation default allSendersOf: #collect:.

"Find references"
SystemNavigation default allReferencesTo: Array.

"Find classes"
Smalltalk globals allClasses select: [ :c |
    c name includesSubstring: 'Test' ].

"Browse class"
Array browse.
Object browse.
```

### System Information

```smalltalk
"Image info"
SystemVersion current version.
Smalltalk platform name.
Smalltalk vm version.

"Memory"
Smalltalk garbageCollect.
Smalltalk vm totalMemory.
Smalltalk vm freeMemory.

"Performance"
Time millisecondsToRun: [ heavyOperation ].
```

## Useful One-Liners

### Data Processing

```smalltalk
"Word frequency"
text substrings asBag sortedCounts.

"Unique elements"
collection asSet.

"Remove duplicates preserving order"
collection asOrderedCollection removeDuplicates.

"Transpose 2D array"
matrix first size timesCollect: [ :i |
    matrix collect: [ :row | row at: i ] ].

"Flatten nested collection"
nested flattened.
```

### Quick Utilities

```smalltalk
"Generate UUID"
UUID new asString.

"Random password"
(String newFrom: ((1 to: 16) collect: [ :i |
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789' atRandom ])).

"Benchmark comparison"
{ [ method1 ]. [ method2 ] } collect: #timeToRun.

"Quick web fetch"
ZnClient new get: 'https://api.example.com/data'.
```

## Common Idioms

### Lazy Initialization

```smalltalk
accessor
    accessor ifNil: [ accessor := self computeExpensiveValue ].
    ^ accessor
```

### Cascading Messages

```smalltalk
collection
    add: item1;
    add: item2;
    add: item3;
    yourself.
```

### Safe Navigation

```smalltalk
object ifNotNil: [ :o | o doSomething ].
object ifNil: [ default ] ifNotNil: [ :o | o value ].
```

### Builder Pattern

```smalltalk
User new
    name: 'Alice';
    email: 'alice@example.com';
    role: #admin;
    yourself.
```

### Ensuring Resources

```smalltalk
file := 'data.txt' asFileReference.
[
    stream := file readStream.
    stream contents
] ensure: [
    stream ifNotNil: [ stream close ] ].
```

## Performance Patterns

### Pre-sizing Collections

```smalltalk
"Slow"
result := OrderedCollection new.
1 to: 10000 do: [ :i | result add: i ].

"Fast"
result := OrderedCollection new: 10000.
1 to: 10000 do: [ :i | result add: i ].
```

### Using Streams for Strings

```smalltalk
"Slow"
result := ''.
1 to: 1000 do: [ :i | result := result, i asString ].

"Fast"
result := String streamContents: [ :s |
    1 to: 1000 do: [ :i | s nextPutAll: i asString ] ].
```

### Caching

```smalltalk
expensiveResult
    "Cache the result"
    ^ expensiveResult ifNil: [
        expensiveResult := self computeExpensiveResult ]
```

## Quick Reference

Keep this handy:

```smalltalk
"Execute code"
Ctrl+D                          "Do it"
Ctrl+P                          "Print it"
Ctrl+I                          "Inspect it"

"Common collections"
#(1 2 3)                        "Array"
{ 1. 2. 3 }                     "Dynamic array"
OrderedCollection new           "Mutable list"
Set new                         "Unique elements"
Dictionary new                  "Key-value"

"Iteration"
do: [ :each | ... ]
collect: [ :each | ... ]
select: [ :each | ... ]
reject: [ :each | ... ]
detect: [ :each | ... ]
inject: 0 into: [ :sum :each | ... ]

"Conditionals"
ifTrue: [ ... ]
ifFalse: [ ... ]
ifNil: [ ... ]
ifNotNil: [ :value | ... ]

"Loops"
timesRepeat: [ ... ]
to:do: [ :i | ... ]
whileTrue: [ ... ]

"Files"
'file.txt' asFileReference contents
'file.txt' asFileReference writeStream: 'text'
```

---

[Previous: Appendix B - Keyboard Shortcuts Reference](appendix-b-shortcuts.md) | [Next: Appendix D - Glossary](appendix-d-glossary.md)
