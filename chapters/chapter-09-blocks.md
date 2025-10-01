# Chapter 9: Blocks - Code You Can Hold

You've been using blocks since Chapter 3, but we haven't fully explained what they are or why they're so powerful. Now it's time to understand one of Smalltalk's most important and elegant features: **blocks**.

A **block** is an object that contains code. Let that sink in: code itself is an object! You can create blocks, pass them around, store them in variables, and execute them whenever you want.

This might sound abstract, but blocks are what make conditionals, loops, collections, and many other Smalltalk features possible. They're also called **closures** in other languages, and they're fundamental to functional programming.

## What is a Block?

A block is code wrapped in square brackets:

```smalltalk
[ 2 + 2 ]
```

That's a block! It contains the code `2 + 2`, but the code hasn't executed yet. The block is like a recipe - it describes what to do, but doesn't actually do it until you tell it to.

## Executing Blocks: `value`

To execute a block, send it the `value` message:

```smalltalk
[ 2 + 2 ] value  "Returns 4"
```

Now the code inside the block executes, and you get the result.

```smalltalk
[ Transcript show: 'Hello from a block!'; cr ] value
```

Try this - check your Transcript. The block executes when you send it `value`, and the code inside runs.

## Blocks are Objects

Let's prove that blocks are objects:

```smalltalk
[ 2 + 2 ] class  "Returns BlockClosure"
```

Blocks are instances of the `BlockClosure` class. They're real objects with methods and behavior.

You can store blocks in variables:

```smalltalk
| myBlock |
myBlock := [ 2 + 2 ].
myBlock value  "Returns 4"
```

You can pass blocks as arguments to messages (you've been doing this with conditionals and collections!):

```smalltalk
true ifTrue: [ Transcript show: 'Yes!'; cr ]
```

The `ifTrue:` message receives a block as its argument.

## Blocks with Multiple Statements

Blocks can contain multiple statements separated by periods:

```smalltalk
[
    Transcript show: 'First line'; cr.
    Transcript show: 'Second line'; cr.
    Transcript show: 'Third line'; cr
] value
```

Try this - you'll see all three lines in the Transcript.

The block returns the value of its **last expression**:

```smalltalk
| result |
result := [
    2 + 2.
    3 + 3.
    4 + 4
] value.
result  "Returns 8 (the result of 4 + 4)"
```

The intermediate results (4 and 6) are computed but discarded. Only the last expression's value is returned.

## Block Arguments

Blocks can accept arguments - values passed to them when they execute.

### Single Argument

```smalltalk
[ :x | x * 2 ] value: 5  "Returns 10"
```

Let's break this down:

- `[ :x | x * 2 ]` - A block that takes one argument called `x`
- The `:x |` part declares the argument (colon before the name, pipe after)
- `value: 5` - Execute the block with the argument `5`
- Inside the block, `x` is `5`, so `x * 2` is `10`

Another example:

```smalltalk
[ :name | 'Hello, ' , name , '!' ] value: 'Alice'
"Returns 'Hello, Alice!'"
```

### Multiple Arguments

Blocks can take multiple arguments:

```smalltalk
[ :x :y | x + y ] value: 5 value: 3  "Returns 8"
```

Each argument is declared with a colon, and you provide values with multiple `value:` messages.

```smalltalk
[ :a :b :c | (a + b) * c ] value: 2 value: 3 value: 4
"Returns 20  (because (2 + 3) * 4 = 20)"
```

### Block Arguments in Collections

This is how collection methods work:

```smalltalk
#(1 2 3 4 5) collect: [ :each | each * 2 ]
"Returns #(2 4 6 8 10)"
```

The `collect:` method calls your block once for each element, passing that element as the argument:

- First call: block receives `1`, returns `2`
- Second call: block receives `2`, returns `4`
- Third call: block receives `3`, returns `6`
- And so on...

```smalltalk
#(1 2 3 4 5) select: [ :each | each even ]
"Returns #(2 4)"
```

The block receives each number and returns `true` or `false`. `select:` keeps the elements where the block returns `true`.

## Closures: Blocks Remember Their Environment

Blocks have access to variables from the context where they were created. This is called **closure** - the block "closes over" its surrounding variables.

```smalltalk
| multiplier |
multiplier := 10.
[ :x | x * multiplier ] value: 5  "Returns 50"
```

The block was created in a context where `multiplier` is `10`, and it remembers that! Even though `multiplier` isn't passed as an argument, the block can use it.

### Powerful Example

```smalltalk
| counter makeIncrementer |
counter := 0.
makeIncrementer := [ counter := counter + 1. counter ].

makeIncrementer value.  "Returns 1"
makeIncrementer value.  "Returns 2"
makeIncrementer value.  "Returns 3"
```

Each time you execute the block, it increments `counter` and returns the new value. The block remembers `counter` from its creation context!

### Another Example

```smalltalk
| greeting makeGreeter |
greeting := 'Hello'.
makeGreeter := [ :name | greeting , ', ' , name , '!' ].

makeGreeter value: 'Alice'.  "Returns 'Hello, Alice!'"
makeGreeter value: 'Bob'.    "Returns 'Hello, Bob!'"
```

The block remembers the `greeting` variable.

## Blocks and Conditionals (Revisited)

Now you understand how conditionals really work! When you write:

```smalltalk
(5 > 3) ifTrue: [ Transcript show: 'Yes!'; cr ]
```

You're:
1. Creating a block: `[ Transcript show: 'Yes!'; cr ]`
2. Passing that block to the `ifTrue:` message
3. The boolean object (`true`) receives the message and decides whether to execute the block

The `True` class implements `ifTrue:` something like this:

```smalltalk
ifTrue: aBlock
    "Execute the block because I'm true"
    ^ aBlock value
```

The `False` class implements it like this:

```smalltalk
ifTrue: aBlock
    "Don't execute the block because I'm false"
    ^ nil
```

Beautiful, right? Control flow implemented with objects and blocks!

## Blocks in Loops (Preview)

Blocks are also how loops work:

```smalltalk
10 timesRepeat: [ Transcript show: '*' ]
```

The number `10` receives the `timesRepeat:` message with a block. It executes that block 10 times.

```smalltalk
1 to: 5 do: [ :n |
    Transcript show: n printString; cr ]
```

This creates an interval (1 to 5) and executes the block for each number, passing the number as an argument.

We'll explore loops in depth in Chapter 10, but now you understand what's happening under the hood!

## Returning from Blocks

Blocks return the value of their last expression:

```smalltalk
[ 10 + 5 ] value  "Returns 15"
```

```smalltalk
[
    | x y |
    x := 10.
    y := 20.
    x + y
] value  "Returns 30"
```

Blocks can declare their own temporary variables using the same `| varName |` syntax.

### Explicit Return: `^`

You can explicitly return from a block using the caret `^`:

```smalltalk
[
    Transcript show: 'About to return'; cr.
    ^ 42.
    Transcript show: 'This never executes'; cr
] value  "Returns 42"
```

**Important**: The `^` in a block doesn't just return from the block - it returns from the **method** that created the block. This can be confusing at first!

In practice, you rarely need `^` in blocks. The implicit return (last expression's value) is usually what you want.

## Storing Blocks for Later

You can store blocks in variables and execute them whenever you want:

```smalltalk
| greetingBlock farewell |
greetingBlock := [ Transcript show: 'Hello!'; cr ].
farewell := [ Transcript show: 'Goodbye!'; cr ].

greetingBlock value.
Transcript show: 'Some other code...'; cr.
farewell value.
```

This flexibility is powerful. You're storing code as data!

## Blocks in Collections

You can even put blocks in collections:

```smalltalk
| operations |
operations := OrderedCollection new.
operations add: [ :x | x + 10 ].
operations add: [ :x | x * 2 ].
operations add: [ :x | x squared ].

"Apply all operations to 5"
operations collect: [ :operation | operation value: 5 ]
"Returns an OrderedCollection(15 10 25)"
```

This opens up amazing possibilities for organizing code!

## Common Block Patterns

### Callback Pattern

```smalltalk
| processData |
processData := [ :data :callback |
    | result |
    result := data * 2.
    callback value: result ].

processData value: 10 value: [ :result |
    Transcript show: 'Result is: ' , result printString; cr ]
```

### Builder Pattern

```smalltalk
| configBuilder config |
configBuilder := [ :block |
    | settings |
    settings := Dictionary new.
    block value: settings.
    settings ].

config := configBuilder value: [ :settings |
    settings at: 'host' put: 'localhost'.
    settings at: 'port' put: 8080.
    settings at: 'debug' put: true ].

config  "A dictionary with the settings"
```

### Retry Pattern

```smalltalk
| retryBlock result |
retryBlock := [ :block :times |
    | attempts success |
    attempts := 0.
    success := false.
    [ success not and: [ attempts < times ] ] whileTrue: [
        attempts := attempts + 1.
        [ block value. success := true ]
            on: Error
            do: [ :ex | Transcript show: 'Attempt ' , attempts printString , ' failed'; cr ] ] ].

retryBlock value: [ 10 / 0 ] value: 3  "Tries 3 times, fails each time"
```

## Blocks with Side Effects vs Pure Blocks

Some blocks have **side effects** - they change things outside the block:

```smalltalk
[ Transcript show: 'Hello'; cr ] value  "Side effect: writes to Transcript"
```

```smalltalk
| counter |
counter := 0.
[ counter := counter + 1 ] value  "Side effect: modifies counter"
```

Other blocks are **pure** - they just compute and return a value:

```smalltalk
[ 2 + 2 ] value  "No side effects, just returns 4"
```

```smalltalk
[ :x | x * 2 ] value: 5  "No side effects, just computes and returns"
```

Pure blocks are easier to reason about and test, but side effects are often necessary (like updating the UI or writing to files).

## Blocks vs Methods (Preview)

Blocks are similar to methods, but they're not the same:

- **Blocks** are objects you create and pass around. They're anonymous (no name).
- **Methods** are named pieces of code attached to classes. They're called by sending messages to objects.

When we get to Chapter 12, you'll see that methods can use blocks, and blocks often work together with methods.

For now, just remember: blocks are pieces of code you can create and execute on the fly.

## The `whileTrue:` and `whileFalse:` Messages

Blocks understand messages for looping:

```smalltalk
| counter |
counter := 1.
[ counter <= 5 ] whileTrue: [
    Transcript show: counter printString; cr.
    counter := counter + 1 ].
```

The first block (the condition) is evaluated repeatedly. As long as it returns `true`, the second block (the body) executes.

```smalltalk
| counter |
counter := 10.
[ counter > 0 ] whileTrue: [
    Transcript show: counter printString; cr.
    counter := counter - 1 ].
```

And `whileFalse:`:

```smalltalk
| done |
done := false.
[ done ] whileFalse: [
    Transcript show: 'Working...'; cr.
    done := true ].  "Eventually becomes true"
```

We'll explore these in detail in Chapter 10.

## Performance: Blocks are Efficient

You might worry that blocks add overhead. Don't! Smalltalk's virtual machine is highly optimized for blocks. In many cases, blocks are compiled to efficient machine code.

The elegance and readability you gain far outweigh any minimal performance cost (which is typically negligible).

## Practical Examples

### Example 1: Retry Logic

```smalltalk
| attemptWithRetry result |
attemptWithRetry := [ :block :maxAttempts |
    | attempts |
    attempts := 0.
    [ attempts < maxAttempts ] whileTrue: [
        attempts := attempts + 1.
        [ ^ block value ]
            on: Error
            do: [ :ex |
                Transcript show: 'Attempt ' , attempts printString , ' failed: ' , ex messageText; cr.
                attempts = maxAttempts ifTrue: [ ex pass ] ] ] ].

"Usage (will fail 3 times):"
attemptWithRetry value: [ 10 / 0 ] value: 3
```

### Example 2: Timing Code

```smalltalk
| timeBlock duration |
timeBlock := [ :block |
    | start |
    start := Time millisecondClockValue.
    block value.
    Time millisecondClockValue - start ].

duration := timeBlock value: [
    1000000 timesRepeat: [ 1 + 1 ] ].
Transcript show: 'Duration: ' , duration printString , 'ms'; cr.
```

### Example 3: Lazy Evaluation

```smalltalk
| expensive lazyValue |
expensive := [ "Simulate expensive computation"
    Transcript show: 'Computing...'; cr.
    (1 to: 1000000) sum ].

"The block is created but NOT executed yet"
lazyValue := expensive.

"Now we need the value, so execute it"
lazyValue value  "Computation happens now"
```

### Example 4: Custom Control Structures

```smalltalk
| repeatUntil |
repeatUntil := [ :conditionBlock :bodyBlock |
    [ bodyBlock value.
      conditionBlock value ] whileFalse ].

"Count from 1 to 5"
| counter |
counter := 0.
repeatUntil
    value: [ counter >= 5 ]
    value: [
        counter := counter + 1.
        Transcript show: counter printString; cr ].
```

### Example 5: Menu System

```smalltalk
| menu |
menu := Dictionary new.
menu at: 'add' put: [ :a :b | a + b ].
menu at: 'subtract' put: [ :a :b | a - b ].
menu at: 'multiply' put: [ :a :b | a * b ].
menu at: 'divide' put: [ :a :b | a / b ].

"Use the menu"
(menu at: 'add') value: 10 value: 5.     "Returns 15"
(menu at: 'multiply') value: 10 value: 5. "Returns 50"
```

## Try This!

Practice with blocks:

1. **Simple block:**
   ```smalltalk
   | block |
   block := [ 2 + 2 ].
   block value
   ```

2. **Block with argument:**
   ```smalltalk
   | double |
   double := [ :x | x * 2 ].
   double value: 5.
   double value: 10.
   double value: 42
   ```

3. **Block with multiple arguments:**
   ```smalltalk
   | calculator |
   calculator := [ :a :b :operation |
       operation = #add ifTrue: [ a + b ].
       operation = #multiply ifTrue: [ a * b ] ].
   calculator value: 5 value: 3 value: #add.
   calculator value: 5 value: 3 value: #multiply
   ```

4. **Closure example:**
   ```smalltalk
   | makeMultiplier triple quintuple |
   makeMultiplier := [ :factor |
       [ :x | x * factor ] ].
   triple := makeMultiplier value: 3.
   quintuple := makeMultiplier value: 5.
   triple value: 10.     "Returns 30"
   quintuple value: 10   "Returns 50"
   ```

5. **Block in collection:**
   ```smalltalk
   | numbers doubled |
   numbers := #(1 2 3 4 5).
   doubled := numbers collect: [ :n | n * 2 ].
   doubled
   ```

6. **Filtering with blocks:**
   ```smalltalk
   | numbers evens |
   numbers := 1 to: 20.
   evens := numbers select: [ :n | n even ].
   evens
   ```

7. **Block with side effects:**
   ```smalltalk
   | counter incrementer |
   counter := 0.
   incrementer := [ counter := counter + 1. counter ].
   incrementer value.
   incrementer value.
   incrementer value.
   counter  "What's the value?"
   ```

8. **While loop:**
   ```smalltalk
   | sum i |
   sum := 0.
   i := 1.
   [ i <= 10 ] whileTrue: [
       sum := sum + i.
       i := i + 1 ].
   sum  "Returns 55 (sum of 1 to 10)"
   ```

9. **Conditional stored in variable:**
   ```smalltalk
   | checkAge result |
   checkAge := [ :age |
       (age >= 18)
           ifTrue: [ 'adult' ]
           ifFalse: [ 'minor' ] ].
   checkAge value: 15.
   checkAge value: 25
   ```

10. **Block factory:**
    ```smalltalk
    | makeGreeter greetInSpanish greetInFrench |
    makeGreeter := [ :greeting |
        [ :name | greeting , ', ' , name , '!' ] ].
    greetInSpanish := makeGreeter value: 'Hola'.
    greetInFrench := makeGreeter value: 'Bonjour'.
    greetInSpanish value: 'Maria'.
    greetInFrench value: 'Pierre'
    ```

## Common Mistakes

### Forgetting to Send `value`

```smalltalk
| block |
block := [ 2 + 2 ].
block  "This returns the block itself, NOT 4!"
```

You need:

```smalltalk
block value  "Now it returns 4"
```

### Wrong Number of Arguments

```smalltalk
[ :x :y | x + y ] value: 5  "Error! Block expects 2 arguments"
```

You need:

```smalltalk
[ :x :y | x + y ] value: 5 value: 3  "Correct"
```

### Confusing Block Variables with Outer Variables

```smalltalk
| x block |
x := 10.
block := [ :x | x * 2 ].  "This :x shadows the outer x!"
block value: 5  "Returns 10, uses the argument, not the outer x"
```

The block's parameter `:x` hides the outer variable `x`.

### Expecting Immediate Execution

```smalltalk
| result |
result := [ 2 + 2 ].  "Assigns the BLOCK, not 4!"
result  "Returns a block, not 4"
```

If you want the value:

```smalltalk
result := [ 2 + 2 ] value.  "Assigns 4"
```

## Why Blocks Matter

Blocks are fundamental to Smalltalk's elegance and power:

1. **Higher-order functions**: You can write functions that take functions as arguments
2. **Custom control structures**: You can create your own `if`, `while`, and other control flows
3. **Lazy evaluation**: Delay computation until it's needed
4. **Callbacks and event handling**: Store code to be executed later
5. **Functional programming**: Use blocks to write in a functional style
6. **Clean APIs**: Design readable, expressive interfaces

Many patterns and practices in Smalltalk rely on blocks. Understanding them deeply will make you a much better Smalltalker.

## Looking Ahead

You now understand blocks - code as objects, closures, arguments, and how they make conditionals and many other features work.

In Chapter 10, we'll explore **Loops and Iteration** in detail. You'll see how blocks enable all sorts of iteration patterns: `timesRepeat:`, `to:do:`, collection iteration, and more.

Then in Part IV (Chapters 11-15), we'll start creating our own classes and methods. You'll use blocks constantly when defining methods, and you'll see how blocks and methods work together.

Blocks are one of Smalltalk's superpowers. Master them, and you've mastered a core concept that applies across many modern programming languages!

---

**Key Takeaways:**
- Blocks are **objects that contain code**, enclosed in square brackets `[ ]`
- Execute blocks by sending them the `value` message
- Blocks can take arguments: `[ :x | x * 2 ] value: 5`
- Blocks are **closures** - they remember variables from their creation context
- Blocks return the value of their last expression
- Blocks can declare their own temporary variables
- Conditionals and loops are implemented using blocks
- Blocks enable functional programming patterns in Smalltalk
- You can store blocks in variables, pass them as arguments, and put them in collections
- Blocks are what make Smalltalk's control flow elegant and uniform

---

[Previous: Chapter 8 - Conditionals](chapter-08-conditionals.md) | [Next: Chapter 10 - Loops and Iteration](chapter-10-loops-and-iteration.md)
