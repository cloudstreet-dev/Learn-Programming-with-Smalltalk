# Chapter 37: Performance and Optimization

You've mastered Smalltalk fundamentals, built real applications, and learned design patterns. Now let's explore **performance optimization** - making your code faster and more efficient!

Smalltalk's live environment makes performance analysis interactive and visual. You can profile code, find bottlenecks, and optimize - all while your program runs. This chapter teaches you practical optimization techniques and shows you how to measure and improve performance systematically.

## The First Rule of Optimization

> **"Premature optimization is the root of all evil."** - Donald Knuth

Always remember:

1. **Make it work** - Correct code first
2. **Make it right** - Clean, maintainable design
3. **Make it fast** - Only optimize what matters

Don't optimize until you:
- Have working, tested code
- **Measure** to find actual bottlenecks
- Know the optimization will matter

**Measure, don't guess!**

## Measuring Performance

Smalltalk provides excellent tools for measuring performance.

### Time It

Measure execution time:

```smalltalk
[ 1000 factorial ] timeToRun
"-> 0:00:00:00.003 (3 milliseconds)"
```

More precise:

```smalltalk
[ 1000 factorial ] bench
"-> '39,800 per second'"
```

Compare alternatives:

```smalltalk
"Which is faster?"
[ (1 to: 1000) select: [ :n | n even ] ] timeToRun.
"vs"
[ (1 to: 1000) select: #even ] timeToRun
```

### The Profiler

Find where time is spent:

```smalltalk
[ self expensiveOperation ] profile
```

Or use the **Time Profiler** tool:

```smalltalk
TimeProfiler spyOn: [ self myMethod ]
```

This opens a browser showing:
- Which methods consume most time
- Call tree
- Percentage breakdowns

### The MessageTally

Detailed profiling:

```smalltalk
MessageTally spyOn: [ self complexCalculation ]
```

Shows complete call graph with timing.

### Memory Usage

Check memory consumption:

```smalltalk
| before after |
before := Smalltalk garbageCollect.
"Run your code here"
self createManyObjects.
after := Smalltalk garbageCollect.
(after - before) asStringWithCommas, ' bytes allocated'
```

Monitor specific objects:

```smalltalk
Object allSubInstances size.  "How many instances?"
OrderedCollection allInstances size.
```

## Common Performance Pitfalls

### 1. String Concatenation in Loops

**Slow:**
```smalltalk
| result |
result := ''.
1 to: 1000 do: [ :n |
    result := result, n asString, ' ' ].
result
```

Each concatenation creates a **new string**! For 1000 iterations, this creates 1000 intermediate strings.

**Fast:**
```smalltalk
String streamContents: [ :stream |
    1 to: 1000 do: [ :n |
        stream nextPutAll: n asString; space ] ]
```

Streams are **mutable** - much faster!

**Benchmark:**
```smalltalk
"Slow version"
[ | result |
  result := ''.
  1 to: 1000 do: [ :n | result := result, n asString ].
] timeToRun.  "-> ~150ms"

"Fast version"
[ String streamContents: [ :stream |
      1 to: 1000 do: [ :n | stream nextPutAll: n asString ] ]
] timeToRun.  "-> ~3ms"
```

**50x faster!**

### 2. Growing Collections

**Slow:**
```smalltalk
| collection |
collection := OrderedCollection new.
1 to: 10000 do: [ :n |
    collection add: n * 2 ]
```

Collection resizes multiple times.

**Fast:**
```smalltalk
| collection |
collection := OrderedCollection new: 10000.  "Pre-size it!"
1 to: 10000 do: [ :n |
    collection add: n * 2 ]
```

Or even better:

```smalltalk
(1 to: 10000) collect: [ :n | n * 2 ]
```

Let the collection implementation optimize!

### 3. Repeated Expensive Calculations

**Slow:**
```smalltalk
1 to: 100 do: [ :i |
    1 to: 100 do: [ :j |
        | distance |
        distance := ((i - 50) squared + (j - 50) squared) sqrt.
        distance < 25 ifTrue: [ "do something" ] ] ]
```

Recalculates `sqrt` every time!

**Fast:**
```smalltalk
1 to: 100 do: [ :i |
    | di |
    di := (i - 50) squared.  "Hoist invariant"
    1 to: 100 do: [ :j |
        | distance |
        distance := (di + (j - 50) squared) sqrt.
        distance < 25 ifTrue: [ "do something" ] ] ]
```

Even better - avoid `sqrt`:

```smalltalk
1 to: 100 do: [ :i |
    | di |
    di := (i - 50) squared.
    1 to: 100 do: [ :j |
        | distanceSquared |
        distanceSquared := di + (j - 50) squared.
        distanceSquared < 625 ifTrue: [ "do something" ] ] ]
```

Compare squared distance to squared threshold!

### 4. Using `do:` When Other Methods Exist

**Slow:**
```smalltalk
| result |
result := OrderedCollection new.
collection do: [ :item |
    item > 10 ifTrue: [
        result add: item * 2 ] ].
result
```

**Fast:**
```smalltalk
collection select: [ :item | item > 10 ] thenCollect: [ :item | item * 2 ]
```

Or:
```smalltalk
(collection select: [ :item | item > 10 ]) collect: [ :item | item * 2 ]
```

The built-in methods are **optimized**!

### 5. Checking Membership Repeatedly

**Slow:**
```smalltalk
collection := OrderedCollection with: 1 with: 2 with: 3.

1 to: 1000 do: [ :n |
    (collection includes: n) ifTrue: [ "do something" ] ]
```

`includes:` is O(n) for OrderedCollection!

**Fast:**
```smalltalk
set := collection asSet.  "O(n) once"

1 to: 1000 do: [ :n |
    (set includes: n) ifTrue: [ "do something" ] ]  "O(1) each time"
```

**Use the right data structure!**

### 6. Creating Temporary Objects

**Slow:**
```smalltalk
1 to: 1000 do: [ :n |
    | point |
    point := Point x: n y: n * 2.
    self process: point ]
```

Creates 1000 Point objects.

**Fast (if possible):**
```smalltalk
1 to: 1000 do: [ :n |
    self processX: n y: n * 2 ]
```

Avoid object creation when you can pass values directly.

## Collection Performance

Different collections have different performance characteristics:

### Time Complexity

| Operation | Array | OrderedCollection | Set | Dictionary |
|-----------|-------|-------------------|-----|------------|
| **Access by index** | O(1) | O(1) | N/A | N/A |
| **Access by key** | N/A | N/A | N/A | O(1) |
| **Add** | O(n)* | O(1)* | O(1) | O(1) |
| **Remove** | O(n) | O(n) | O(1) | O(1) |
| **Search** | O(n) | O(n) | O(1) | O(1) |

*May need to resize

### Choose the Right Collection

**For indexed access:**
```smalltalk
Array or OrderedCollection
```

**For membership testing:**
```smalltalk
Set  "Not OrderedCollection!"
```

**For key-value lookup:**
```smalltalk
Dictionary
```

**For ordered unique elements:**
```smalltalk
SortedCollection
```

### Example: Finding Duplicates

**Slow:**
```smalltalk
| duplicates |
duplicates := OrderedCollection new.
collection do: [ :item |
    (collection count: [ :each | each = item ]) > 1 ifTrue: [
        (duplicates includes: item) ifFalse: [
            duplicates add: item ] ] ]
```

O(n³) - terrible!

**Fast:**
```smalltalk
| counts |
counts := Bag new.
collection do: [ :item | counts add: item ].
counts asSet select: [ :item | (counts occurrencesOf: item) > 1 ]
```

O(n) - much better!

## Block Optimization

Blocks are powerful but have overhead.

### Block Arguments vs. Instance Variables

**Slower:**
```smalltalk
items do: [ :item |
    item process: self data using: self options ]
```

**Faster:**
```smalltalk
| data options |
data := self data.
options := self options.
items do: [ :item |
    item process: data using: options ]
```

Reduce method sends inside loops!

### Avoid Blocks When Possible

**Slower:**
```smalltalk
collection select: [ :item | item even ]
```

**Faster:**
```smalltalk
collection select: #even
```

Symbol-based selection is **optimized**!

### Inline Small Blocks

The VM can inline small blocks:

```smalltalk
collection do: [ :item | item doSomething ]  "Can inline"
collection do: [ :item |
    item doSomething.
    item doSomethingElse.
    item doYetAnotherThing ]  "Cannot inline - too big"
```

Keep blocks small for best performance.

## Method Optimization

### Cache Expensive Computations

**Before:**
```smalltalk
expensiveResult
    ^ self complexCalculation
```

Called multiple times = recalculates every time!

**After:**
```smalltalk
expensiveResult
    expensiveResult ifNil: [
        expensiveResult := self complexCalculation ].
    ^ expensiveResult

resetCache
    expensiveResult := nil
```

Lazy initialization - calculate once, cache the result!

### Avoid Deep Call Chains

**Slower:**
```smalltalk
level1
    ^ self level2

level2
    ^ self level3

level3
    ^ 42
```

**Faster:**
```smalltalk
result
    ^ 42
```

Each method call has overhead. Eliminate unnecessary indirection.

### Use Primitives When Available

Some operations are implemented as VM primitives:

```smalltalk
"Fast (primitive):"
1 + 2
3 * 4
array at: 5

"Slower (message send):"
1 + 2.0  "Mixed types - not primitive"
```

Keep arithmetic with same types when possible!

## Algorithmic Optimization

The **biggest** performance gains come from better algorithms!

### Example: Finding Intersection

**Naive (O(n²)):**
```smalltalk
| intersection |
intersection := OrderedCollection new.
collection1 do: [ :item |
    (collection2 includes: item) ifTrue: [
        intersection add: item ] ].
intersection
```

**Better (O(n)):**
```smalltalk
collection1 asSet intersection: collection2 asSet
```

**Much faster** for large collections!

### Example: Sorting

**Don't:**
```smalltalk
"Bubble sort - O(n²)"
| sorted swapped |
sorted := collection copy.
swapped := true.
[ swapped ] whileTrue: [
    swapped := false.
    1 to: sorted size - 1 do: [ :i |
        (sorted at: i) > (sorted at: i + 1) ifTrue: [
            sorted swap: i with: i + 1.
            swapped := true ] ] ].
sorted
```

**Do:**
```smalltalk
collection sorted  "O(n log n) - built-in quicksort!"
```

**Use the library!** It's optimized and tested.

## Memory Optimization

### Avoid Creating Unnecessary Objects

**Before:**
```smalltalk
1 to: 1000 do: [ :n |
    | rect |
    rect := Rectangle origin: 0@0 corner: n@n.
    self processArea: rect area ]
```

**After:**
```smalltalk
1 to: 1000 do: [ :n |
    self processArea: n squared ]
```

Skip object creation entirely!

### Release References

```smalltalk
processLargeData: data
    | result |
    result := self expensiveCalculation: data.
    data := nil.  "Release reference so GC can collect"
    ^ result
```

Help the garbage collector!

### Use Weak References

For caches that shouldn't prevent garbage collection:

```smalltalk
Object subclass: #Cache
    instanceVariableNames: 'storage'

initialize
    super initialize.
    storage := WeakValueDictionary new

at: key put: value
    storage at: key put: value

at: key
    ^ storage at: key ifAbsent: [ nil ]
```

Objects in `WeakValueDictionary` can be garbage collected!

## Practical Optimization Example

Let's optimize a word frequency counter:

### Version 1: Naive

```smalltalk
wordFrequency: text
    | words frequencies |
    words := text substrings.
    frequencies := OrderedCollection new.

    words do: [ :word |
        | count existing |
        existing := frequencies detect: [ :pair | pair key = word ] ifNone: [ nil ].
        existing
            ifNil: [ frequencies add: word -> 1 ]
            ifNotNil: [ existing value: existing value + 1 ] ].

    ^ frequencies
```

**Problems:**
- `detect:` is O(n) - called for every word!
- Repeated searches through frequencies
- OrderedCollection not optimal

**Performance:** ~500ms for 10,000 words

### Version 2: Use Dictionary

```smalltalk
wordFrequency: text
    | words frequencies |
    words := text substrings.
    frequencies := Dictionary new.

    words do: [ :word |
        frequencies at: word put: (frequencies at: word ifAbsent: [ 0 ]) + 1 ].

    ^ frequencies
```

**Improvements:**
- Dictionary lookup is O(1)
- Much simpler code

**Performance:** ~50ms for 10,000 words - **10x faster!**

### Version 3: Use Bag

```smalltalk
wordFrequency: text
    | words |
    words := text substrings.
    ^ Bag withAll: words
```

**Improvements:**
- Bag is designed for counting!
- One line of code

**Performance:** ~30ms for 10,000 words - **17x faster!**

**Lesson:** Use the right data structure!

## Real-World Optimization Story

Consider this actual optimization from a project:

### Before

```smalltalk
buildReport: data
    | report |
    report := ''.
    report := report, 'Report Header', String cr.
    report := report, '============', String cr.
    data do: [ :item |
        report := report, item name, ': '.
        report := report, item value asString, String cr ].
    report := report, '============', String cr.
    report := report, 'Total: ', data size asString.
    ^ report
```

**Performance:** 2.5 seconds for 10,000 items

### After

```smalltalk
buildReport: data
    ^ String streamContents: [ :stream |
        stream
            nextPutAll: 'Report Header'; cr;
            nextPutAll: '============'; cr.
        data do: [ :item |
            stream
                nextPutAll: item name;
                nextPutAll: ': ';
                nextPutAll: item value asString;
                cr ].
        stream
            nextPutAll: '============'; cr;
            nextPutAll: 'Total: ';
            nextPutAll: data size asString ]
```

**Performance:** 0.03 seconds for 10,000 items - **83x faster!**

Single change: Use streams instead of concatenation.

## Profiling Example

Let's profile and optimize real code:

```smalltalk
processData: items
    "Process a collection of items"
    | results |
    results := OrderedCollection new.
    items do: [ :item |
        | processed |
        processed := self validateAndTransform: item.
        (self meetsThreshold: processed) ifTrue: [
            results add: processed ] ].
    ^ results sorted
```

**Profile it:**

```smalltalk
TimeProfiler spyOn: [
    processor processData: largeCollection ]
```

**Results:**
- 40% in `validateAndTransform:`
- 30% in `sorted`
- 20% in `meetsThreshold:`
- 10% other

**Optimizations:**

1. Pre-size results:
```smalltalk
results := OrderedCollection new: items size.
```

2. Cache threshold calculation if it's constant:
```smalltalk
| threshold |
threshold := self calculateThreshold.  "Once, before loop"
items do: [ :item |
    (processed > threshold) ifTrue: [ ... ] ]
```

3. Use `select:thenCollect:` instead of manual loop:
```smalltalk
^ (items collect: #validateAndTransform) select: [ :processed |
    self meetsThreshold: processed ] sorted
```

## Optimization Checklist

Before optimizing:
- ✅ **Measure** - Profile to find real bottlenecks
- ✅ **Set goals** - Know your performance target
- ✅ **Benchmark** - Record current performance

When optimizing:
- ✅ **Use right collections** - Array vs Set vs Dictionary
- ✅ **Avoid string concatenation** - Use streams
- ✅ **Pre-size collections** - Avoid resizing
- ✅ **Cache expensive calculations** - Don't recompute
- ✅ **Use built-in methods** - They're optimized
- ✅ **Choose better algorithms** - O(n) vs O(n²)
- ✅ **Reduce object creation** - Reuse when possible
- ✅ **Profile again** - Verify improvement

After optimizing:
- ✅ **Test** - Ensure correctness
- ✅ **Measure again** - Confirm speedup
- ✅ **Document** - Explain non-obvious optimizations

## When NOT to Optimize

Don't optimize if:
- Code is rarely executed
- Performance is already acceptable
- Optimization makes code much harder to understand
- You're guessing, not measuring

**Readable code > Fast code** (until you need fast code!)

## Advanced Topics

### Compiled Methods

Critical loops can be compiled to machine code:

```smalltalk
"Some VMs support JIT compilation automatically"
```

### Primitives

Write primitives in C for ultimate speed (advanced):

```smalltalk
Object subclass: #FastMath

"VM primitive for fast square root"
sqrt: number
    <primitive: 'primitiveSqrt'>
    ^ self primitiveFailed
```

### Memory-Mapped Files

For huge data sets:

```smalltalk
file := 'huge-data.bin' asFileReference.
mapped := file binaryReadStream.
"Access data without loading entire file"
```

## Try This!

Practice optimization:

1. **Benchmark Collection Operations**
   ```smalltalk
   "Compare Array, OrderedCollection, Set, Dictionary"
   "for add, remove, search operations"
   ```

2. **Optimize String Building**
   ```smalltalk
   "Compare concatenation vs streams"
   "for building large strings"
   ```

3. **Profile Your Code**
   ```smalltalk
   "Use TimeProfiler on your projects"
   "Find and fix bottlenecks"
   ```

4. **Algorithm Comparison**
   ```smalltalk
   "Implement bubble sort vs using sorted"
   "Measure the difference"
   ```

5. **Memory Analysis**
   ```smalltalk
   "Track object creation in loops"
   "Optimize to reduce allocations"
   ```

## What You Learned

Exploring performance, you've mastered:

1. **Measurement**
   - timeToRun, bench
   - TimeProfiler
   - MessageTally

2. **Common Pitfalls**
   - String concatenation
   - Growing collections
   - Wrong data structures
   - Repeated calculations

3. **Collection Performance**
   - Time complexity
   - Choosing right collection
   - Set vs OrderedCollection

4. **Optimization Techniques**
   - Caching
   - Pre-sizing
   - Reducing object creation
   - Using built-in methods

5. **Algorithmic Thinking**
   - O(n) vs O(n²)
   - Better algorithms > micro-optimization
   - Use the library!

6. **The Process**
   - Measure first
   - Optimize bottlenecks
   - Test and verify
   - Document tricky optimizations

## Performance in Smalltalk

Smalltalk performance is **good** because:
- **Modern VMs** - JIT compilation
- **Optimized primitives** - Core operations are fast
- **Efficient garbage collection** - Generational GC
- **Good libraries** - Collections are optimized
- **Live profiling** - Find bottlenecks easily

Smalltalk is fast enough for:
- Web applications
- Business software
- Data processing
- Real-time systems (with care)

## Looking Ahead

You now understand performance optimization! You know:
- How to measure and profile
- Common performance pitfalls
- Optimization techniques
- When to optimize (and when not to)

In Chapter 38, we'll explore **The Smalltalk Community** - connecting with other Smalltalkers worldwide!

Then Chapter 39 covers **Beyond Smalltalk** - taking your skills to other languages and domains!

Part X is bringing your Smalltalk journey to completion!

---

**Key Takeaways:**
- **Measure, don't guess** - Use profiling tools
- **Premature optimization is evil** - Make it work first
- **Use timeToRun and bench** for quick measurements
- **TimeProfiler** shows where time is spent
- **String concatenation in loops** is slow - use streams
- **Pre-size collections** to avoid resizing
- **Use right data structures** - Set for membership, Dictionary for lookup
- **Cache expensive calculations** - don't recompute
- **Use built-in methods** - they're optimized
- **Better algorithms** beat micro-optimizations
- **Bag** is great for counting/frequencies
- **Reduce object creation** in tight loops
- **Profile, optimize, measure again** - verify improvements
- **Readable code first** - optimize only when needed
- Smalltalk has **excellent profiling tools**

---

[Previous: Chapter 36 - Design Patterns in Smalltalk](chapter-36-design-patterns.md) | [Next: Chapter 38 - The Smalltalk Community](chapter-38-community.md)
