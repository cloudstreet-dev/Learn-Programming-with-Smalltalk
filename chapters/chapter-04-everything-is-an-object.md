# Chapter 4: Everything is an Object (No, Really!)

This is where Smalltalk's philosophy truly reveals itself. In this chapter, we'll explore what might be the most important concept in Smalltalk: **everything is an object, and objects communicate by sending messages to each other**.

This isn't just a technical detail - it's a fundamentally different way of thinking about programming. Once you understand it, programming in Smalltalk becomes incredibly intuitive and consistent.

## What is an Object?

Let's start with the basics. An **object** is a thing - a bundle of data and behavior packaged together.

Think of objects like things in the real world:

- A **car** is an object. It has data (color, speed, fuel level) and behavior (accelerate, brake, turn).
- A **book** is an object. It has data (title, author, number of pages) and behavior (open, close, turn page).
- Your **phone** is an object. It has data (battery level, contacts, photos) and behavior (make call, send text, take picture).

In Smalltalk, everything you work with is an object:

- The number `42` is an object
- The string `'Hello'` is an object
- The value `true` is an object
- Even classes themselves are objects!

Each object knows things (data) and can do things (behavior).

## What is a Message?

Objects don't work in isolation - they interact by sending **messages** to each other.

A message is a request for an object to do something or tell you something. It's like asking a question or giving a command.

In the real world:
- You send the message "What's your name?" to a person
- You send the message "Go faster" to your car (by pressing the gas pedal)
- You send the message "What time is it?" to your watch

In Smalltalk:
- You send the message `+ 3` to the number `5`
- You send the message `size` to the string `'Hello'`
- You send the message `today` to the class `Date`

### Message Syntax

When you write `5 + 3` in Smalltalk, you're not using a mathematical operator. You're sending the message `+` with the argument `3` to the object `5`.

Let's break down the anatomy:

```smalltalk
5 + 3
```

- **Receiver**: `5` - the object receiving the message
- **Message selector**: `+` - the name of the message
- **Argument**: `3` - additional information for the message

The receiver (`5`) gets the message (`+`) with an argument (`3`) and responds with a result (`8`).

### The Message-Sending Metaphor

Think of it like this: you're not *calculating* 5 + 3. You're *asking* the number 5 "Hey, what's your value plus 3?" and the number 5 responds "8".

This might seem like an odd distinction at first, but it's powerful. It means every operation in Smalltalk is an object doing something, not the computer executing an operation. The object is in control.

## Three Types of Messages

Smalltalk has three kinds of messages, each with different syntax. Understanding these is key to reading and writing Smalltalk code.

### 1. Unary Messages

**Unary messages** have no arguments - just the receiver and the message name.

```smalltalk
Date today
```

- **Receiver**: `Date` (the Date class)
- **Message**: `today`
- **Result**: Today's date

```smalltalk
'hello' capitalized
```

- **Receiver**: `'hello'` (a string)
- **Message**: `capitalized`
- **Result**: `'Hello'`

```smalltalk
42 negated
```

- **Receiver**: `42`
- **Message**: `negated`
- **Result**: `-42`

Unary messages are the simplest. They're just `receiver messageName`.

Try these in your Playground:

```smalltalk
Time now
```

```smalltalk
'Smalltalk' size
```

```smalltalk
100 factorial
```

That last one calculates 100! (100 factorial) - try it! Smalltalk handles huge numbers effortlessly.

### 2. Binary Messages

**Binary messages** use operator-like symbols and take exactly one argument.

```smalltalk
5 + 3
```

- **Receiver**: `5`
- **Message**: `+`
- **Argument**: `3`
- **Result**: `8`

```smalltalk
10 * 4
```

- **Receiver**: `10`
- **Message**: `*`
- **Argument**: `4`
- **Result**: `40`

```smalltalk
'Hello' , ' World'
```

- **Receiver**: `'Hello'`
- **Message**: `,` (comma - concatenation)
- **Argument**: `' World'`
- **Result**: `'Hello World'`

Common binary messages:

- Arithmetic: `+`, `-`, `*`, `/`, `//` (integer division), `\\` (modulo)
- Comparison: `=`, `~=` (not equal), `<`, `>`, `<=`, `>=`
- Logical: `&` (and), `|` (or)
- Other: `,` (concatenation), `@` (point creation)

Try these:

```smalltalk
15 / 3
```

```smalltalk
17 \\ 5  "Remainder of 17 divided by 5"
```

```smalltalk
10 @ 20  "Creates a Point with x=10, y=20"
```

### 3. Keyword Messages

**Keyword messages** are the most powerful and most common. They can take one or more arguments, and the message name includes colons.

**Single keyword:**

```smalltalk
'Hello' at: 1
```

- **Receiver**: `'Hello'`
- **Message**: `at:`
- **Argument**: `1`
- **Result**: `$H` (the character at position 1)

In Smalltalk, collections typically start at index 1, not 0.

**Multiple keywords:**

```smalltalk
'Hello World' copyFrom: 1 to: 5
```

- **Receiver**: `'Hello World'`
- **Message**: `copyFrom:to:` (this is one message name!)
- **Arguments**: `1` and `5`
- **Result**: `'Hello'`

The message selector `copyFrom:to:` includes both keywords. You read it as: "copy from 1 to 5".

More examples:

```smalltalk
10 max: 20
```

Returns `20` (the maximum of 10 and 20).

```smalltalk
'hello' includesSubstring: 'ell'
```

Returns `true`.

```smalltalk
42 between: 40 and: 50
```

Returns `true`. The message selector is `between:and:`.

Try these in your Playground:

```smalltalk
'Smalltalk' copyFrom: 6 to: 9
```

```smalltalk
100 min: 50
```

```smalltalk
'Programming' at: 5
```

## Message Precedence

When you have multiple messages in one expression, Smalltalk evaluates them in a specific order:

**1. Unary messages** (highest precedence)
**2. Binary messages**
**3. Keyword messages** (lowest precedence)

Within each category, evaluation is **left to right**.

### Examples

```smalltalk
3 + 4 * 5
```

Both `+` and `*` are binary messages. They evaluate left to right:
1. `3 + 4` → `7`
2. `7 * 5` → `35`

Result: `35`

This might surprise you if you're used to mathematical precedence where multiplication comes before addition! In Smalltalk, it's strictly left to right for messages of the same precedence.

```smalltalk
'hello' size + 3
```

`size` is unary (highest precedence), `+` is binary:
1. `'hello' size` → `5`
2. `5 + 3` → `8`

Result: `8`

```smalltalk
10 + 20 max: 5
```

`+` is binary (higher precedence), `max:` is keyword:
1. `10 + 20` → `30`
2. `30 max: 5` → `30`

Result: `30`

### Using Parentheses

You can use parentheses to override the default precedence:

```smalltalk
3 + (4 * 5)
```

1. `4 * 5` → `20` (parentheses first)
2. `3 + 20` → `23`

Result: `23`

```smalltalk
(10 + 20) max: 35
```

1. `10 + 20` → `30`
2. `30 max: 35` → `35`

Result: `35`

**Important**: When in doubt, use parentheses! They make your code clearer and ensure it does what you intend.

## Chaining Messages

You can send multiple messages in sequence, each operating on the result of the previous one:

```smalltalk
'hello world' capitalized reversed
```

1. `'hello world' capitalized` → `'Hello world'`
2. `'Hello world' reversed` → `'dlrow olleH'`

Result: `'dlrow olleH'`

This is called **message chaining**. The result of one message becomes the receiver of the next.

```smalltalk
100 factorial asString size
```

1. `100 factorial` → huge number
2. Result `asString` → converts to string
3. Result `size` → number of digits

Try it! 100! has 158 digits.

```smalltalk
Date today addDays: 30
```

1. `Date today` → today's date
2. Result `addDays: 30` → date 30 days from now

Result: A date one month in the future.

## Everything Really IS an Object

Let's prove this with some surprising examples.

### Numbers are Objects

```smalltalk
5 + 3
```

The number `5` is an object that understands the `+` message.

```smalltalk
10 timesRepeat: [ Transcript show: 'Hi! '; cr ]
```

Try this! The number `10` understands `timesRepeat:`, which executes the block (the code in square brackets) 10 times.

(We'll learn about blocks in Chapter 9, but for now, just know they're chunks of code you can execute.)

### Booleans are Objects

```smalltalk
true not
```

Returns `false`. The object `true` understands the `not` message.

```smalltalk
false | true
```

Returns `true`. The object `false` understands the `|` (or) message.

Even more surprisingly:

```smalltalk
true ifTrue: [ Transcript show: 'It is true!' ]
```

The boolean object `true` understands the `ifTrue:` message! Conditionals in Smalltalk aren't special syntax - they're messages sent to boolean objects.

This is mind-bending: **control flow is implemented as messages to objects**. We'll explore this deeply in Chapter 8.

### Classes are Objects

```smalltalk
Date today
```

`Date` is an object (specifically, it's a class, but classes are objects too). You're sending the `today` message to the Date object.

```smalltalk
String new
```

You're sending the `new` message to the String class, asking it to create a new string instance.

Classes aren't special syntax - they're objects you can send messages to.

### Even `nil` is an Object

```smalltalk
nil isNil
```

Returns `true`.

`nil` represents "nothing" or "no value", but even `nil` is an object! It's an instance of the class `UndefinedObject`.

### Literal Objects

When you type literals in your code, you're creating objects:

```smalltalk
42          "A SmallInteger object"
3.14        "A Float object"
'Hello'     "A String object"
true        "The True object"
false       "The False object"
nil         "The UndefinedObject instance"
#symbol     "A Symbol object"
#(1 2 3)    "An Array object"
```

Each of these is an object with its own behavior.

## The Power of Consistency

Why does "everything is an object" matter? **Consistency**.

There are no special cases to memorize. You don't have to remember "numbers work this way, but objects work that way, and classes are different". Everything works the same way:

1. You have an object
2. You send it a message
3. It responds

This makes Smalltalk remarkably easy to learn once you grasp this concept. There's less to memorize because the rules are uniform.

### No Primitive Operators

In many languages, `+` is a special operator handled by the compiler. In Smalltalk, `+` is just a message. You can even implement it yourself in your own classes (and we will in later chapters).

This means you can create objects that behave like numbers, strings, or any built-in type, because they're all just objects sending messages.

### Everything is Discoverable

Since everything is an object, you can inspect everything:

```smalltalk
42 inspect
```

Opens an inspector showing the internal structure of the number 42.

```smalltalk
'Hello' inspect
```

Opens an inspector on the string.

```smalltalk
Date inspect
```

Opens an inspector on the Date class itself!

You can look inside anything and see how it works. This makes Smalltalk an incredibly transparent learning environment.

## Finding What Messages an Object Understands

How do you know what messages you can send to an object? There are several ways:

### 1. Inspect and Explore

Inspect an object:

```smalltalk
'Hello' inspect
```

In the inspector, look at the bottom pane. Type `self` and then start typing a message name. Pharo will show you autocomplete suggestions of messages the object understands.

### 2. Browse the Class

Use the Spotter (`Shift+Enter`) to find a class, then browse its methods. For example:

1. Press `Shift+Enter`
2. Type "String"
3. Press Enter to open the String class browser
4. Look at all the methods (messages) defined

We'll explore the browser in detail in Chapter 19.

### 3. Experiment in the Playground

Just try things!

```smalltalk
'Hello' reversed
```

```smalltalk
'Hello' asUppercase
```

```smalltalk
'Hello' size
```

If you try to send a message an object doesn't understand, you'll get an error. That's okay - close the error and try something else.

### 4. Read the Documentation

Pharo has extensive documentation. Right-click in the Playground and choose `Help` to access it.

## Common Messages to Know

Here are some messages you'll use frequently across many types of objects:

### Universal Messages

Almost every object understands these:

- `printString` - Returns a string representation
- `inspect` - Opens an inspector
- `class` - Returns the object's class
- `yourself` - Returns the object itself (useful with cascades)

```smalltalk
42 printString
```

Returns `'42'` (the string, not the number).

```smalltalk
'Hello' class
```

Returns `ByteString` (the class of the string).

### Number Messages

- `+`, `-`, `*`, `/` - Arithmetic
- `abs` - Absolute value
- `negated` - Negation
- `squared` - Square
- `sqrt` - Square root
- `even`, `odd` - Tests
- `max:`, `min:` - Comparison

```smalltalk
-5 abs
```

Returns `5`.

```smalltalk
16 sqrt
```

Returns `4.0`.

```smalltalk
7 even
```

Returns `false`.

### String Messages

- `size` - Length
- `at:` - Character at position
- `capitalized`, `asUppercase`, `asLowercase` - Case conversion
- `reversed` - Reverse the string
- `,` - Concatenation
- `includesSubstring:` - Substring test
- `copyFrom:to:` - Extract substring

```smalltalk
'hello' asUppercase
```

Returns `'HELLO'`.

```smalltalk
'Smalltalk' copyFrom: 6 to: 9
```

Returns `'talk'`.

### Boolean Messages

- `not` - Negation
- `&`, `|` - Logical AND, OR
- `ifTrue:`, `ifFalse:`, `ifTrue:ifFalse:` - Conditionals

```smalltalk
true & false
```

Returns `false`.

```smalltalk
(5 > 3) not
```

Returns `false` (because 5 > 3 is true, and true not is false).

## The "Does Not Understand" Error

What happens when you send a message an object doesn't understand?

```smalltalk
'Hello' fly
```

Try this in your Playground (use "Print it").

You'll get a **"MessageNotUnderstood"** error. A debugger window opens showing that the String object doesn't understand the message `fly`.

This is Smalltalk's way of saying "Hey, strings don't know how to fly!"

Close the debugger and try something else. This error is common when learning - you just guessed wrong about what messages an object understands. No harm done!

## Objects All the Way Down

Here's something profound: in Smalltalk, there's no "bottom". Everything is implemented as objects sending messages.

When you write:

```smalltalk
5 + 3
```

The number `5` has a method (a piece of code) that defines what to do when it receives the `+` message. That method is written in Smalltalk, and you can look at it!

Let's peek:

1. Open the Spotter (`Shift+Enter`)
2. Type "SmallInteger"
3. Open it
4. Find the method `+`
5. Read the code

It's all there - the entire implementation is visible and understandable. Nothing is hidden.

This transparency is one of Smalltalk's greatest teaching tools. Want to know how something works? Just look at it!

## Try This!

Time to experiment with objects and messages:

1. **Explore numbers:**
   ```smalltalk
   42 inspect
   ```
   In the inspector, try sending different messages: `squared`, `factorial`, `isPrime`, `timesRepeat:`.

2. **Chain messages:**
   ```smalltalk
   'programming' capitalized reversed asUppercase
   ```
   What's the result? Can you explain why?

3. **Play with points:**
   ```smalltalk
   10 @ 20
   ```
   This creates a Point. Now try:
   ```smalltalk
   (10 @ 20) + (5 @ 3)
   ```
   What happens? Try `inspect` on a point to see what messages it understands.

4. **Test precedence:**
   ```smalltalk
   5 + 3 * 2
   ```
   What's the result? Now try:
   ```smalltalk
   5 + (3 * 2)
   ```
   Is it different?

5. **Discover messages:**
   ```smalltalk
   'Hello' inspect
   ```
   In the inspector's bottom pane, type `self ` (note the space) and look at the autocomplete suggestions. Try sending various messages you discover.

6. **Break things intentionally:**
   ```smalltalk
   42 fly
   ```
   Read the error message. What is it telling you?

7. **Everything is an object:**
   ```smalltalk
   true class
   false class
   nil class
   42 class
   'Hello' class
   ```
   What classes are these objects instances of?

8. **Create interesting chains:**
   ```smalltalk
   Date today dayOfWeek asString
   ```
   ```smalltalk
   Time now asSeconds asInteger
   ```

## The Object-Oriented Mindset

As you work with Smalltalk, try to think in terms of objects and messages:

- **Don't think**: "I'm calling the function `size()` on a string"
- **Do think**: "I'm asking the string object to tell me its size"

- **Don't think**: "I'm using the `+` operator"
- **Do think**: "I'm asking this number to add another number to itself"

This mindset shift takes time, but once it clicks, Smalltalk becomes incredibly intuitive. You're not invoking functions or using operators - you're communicating with objects.

## Why This Matters

Understanding that everything is an object and that all computation happens through message sending gives you:

1. **Consistency** - One way of thinking about all code
2. **Discoverability** - You can inspect and explore everything
3. **Extensibility** - You can create your own objects that work just like built-in ones
4. **Simplicity** - Fewer concepts to learn and remember

This isn't just philosophical - it has practical benefits. When you're stuck or confused, you can always inspect an object, look at its class, browse its methods, and understand what's happening.

## Coming Up Next

You now understand Smalltalk's fundamental model: objects sending messages to each other. But what objects do we have to work with?

In Chapter 5, we'll explore the basic types that come with Smalltalk: numbers (integers and floats), strings, characters, booleans, and nil. You'll learn what messages each type understands and how to work with them effectively.

Then in Chapter 6, we'll look at collections - powerful objects for organizing groups of other objects.

The foundation is in place. Now let's build on it!

---

**Key Takeaways:**
- **Everything** in Smalltalk is an object - numbers, strings, booleans, classes, even nil
- Objects communicate by sending **messages** to each other
- There are three types of messages: **unary** (no arguments), **binary** (operators), and **keyword** (named with colons)
- Message precedence: unary, then binary, then keyword; within each, left to right
- Use parentheses to override precedence
- The pattern is always: **receiver** sends **message** (with optional arguments), gets back **result**
- Consistency means fewer special cases to memorize
- Everything is inspectable and discoverable
- "MessageNotUnderstood" errors just mean you guessed wrong - try something else!

---

[Previous: Chapter 3 - The Workspace](chapter-03-the-workspace.md) | [Next: Chapter 5 - Numbers, Strings, and Basic Types](chapter-05-numbers-strings-and-basic-types.md)
