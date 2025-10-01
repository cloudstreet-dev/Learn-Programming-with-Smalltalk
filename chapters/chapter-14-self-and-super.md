# Chapter 14: The Mystery of 'self' and 'super'

You've seen `self` and `super` in code examples, but what are they really? Why do they exist? How do they work?

`self` and `super` are special **pseudo-variables** - they look like variables, but they're actually keywords with special meaning. Understanding them is crucial to mastering Smalltalk and object-oriented programming.

In this chapter, we'll demystify `self` and `super`, exploring how they enable objects to refer to themselves and collaborate with their superclasses.

## What is `self`?

**`self`** always refers to the object that received the current message - the object whose method is currently executing.

Think of it as "me" or "myself":
- When a Person object's method runs, `self` is that Person
- When a Rectangle object's method runs, `self` is that Rectangle
- `self` changes depending on which object's method is running

### A Simple Example

```smalltalk
Object subclass: #Person
    instanceVariableNames: 'firstName lastName'
    ...
```

```smalltalk
fullName
    "Return the full name"
    ^ firstName , ' ' , lastName
```

```smalltalk
greet
    "Return a greeting"
    ^ 'Hello, my name is ' , self fullName
```

When you call `greet`:

```smalltalk
| person |
person := Person new firstName: 'Alice'; lastName: 'Smith'; yourself.
person greet  "Returns 'Hello, my name is Alice Smith'"
```

Inside the `greet` method, `self` refers to that Person object. When it says `self fullName`, it's calling the `fullName` method on itself.

### Why Not Just Call `fullName`?

Why write `self fullName` instead of just `fullName`? Because **you can't call methods directly** in Smalltalk - you must send messages to objects.

This won't work:

```smalltalk
greet
    ^ 'Hello, my name is ' , fullName  "Error! fullName isn't defined!"
```

You need:

```smalltalk
greet
    ^ 'Hello, my name is ' , self fullName  "Correct - sends fullName message to self"
```

## `self` for Accessing Methods

Any time a method needs to call another method in the same class, it uses `self`:

```smalltalk
Object subclass: #Rectangle
    instanceVariableNames: 'width height'
    ...
```

```smalltalk
area
    ^ width * height
```

```smalltalk
perimeter
    ^ 2 * (width + height)
```

```smalltalk
describe
    ^ 'Rectangle with area ' , self area printString ,
      ' and perimeter ' , self perimeter printString
```

The `describe` method uses `self` to call `area` and `perimeter`.

## `self` is the Receiver

When you write:

```smalltalk
person greet
```

The object `person` becomes `self` inside the `greet` method. Every method execution has its own `self` - the object that received the message.

## Method Lookup

Understanding `self` requires understanding **method lookup** - how Smalltalk finds the method to execute when you send a message.

When you send a message:

```smalltalk
object someMessage
```

Here's what happens:

1. Look in the object's class for a method named `someMessage`
2. If found, execute it (with `self` set to `object`)
3. If not found, look in the superclass
4. Keep going up the hierarchy until you find it or reach the top
5. If never found, call `doesNotUnderstand:`

### Example

```smalltalk
| person |
person := Person new.
person greet
```

1. `person` is a Person, so look in the Person class for `greet`
2. Found it! Execute Person>>greet with `self` = person
3. Inside greet, `self fullName` sends `fullName` to person
4. Look in Person for `fullName`
5. Found it! Execute Person>>fullName with `self` = person

## `self` Enables Polymorphism

Different objects respond to the same message in different ways:

```smalltalk
Object subclass: #Circle
    instanceVariableNames: 'radius'
    ...

area
    ^ Float pi * radius * radius
```

```smalltalk
Object subclass: #Rectangle
    instanceVariableNames: 'width height'
    ...

area
    ^ width * height
```

Now:

```smalltalk
| shapes |
shapes := Array with: (Circle new radius: 5) with: (Rectangle new width: 10; height: 20).

shapes do: [ :shape |
    Transcript show: 'Area: '; show: shape area printString; cr ]
```

Each object responds to `area` according to its own class. This is **polymorphism** - same message, different behavior.

## Methods Calling Other Methods with `self`

This is extremely common:

```smalltalk
Object subclass: #BankAccount
    instanceVariableNames: 'balance'
    ...
```

```smalltalk
deposit: amount
    balance := balance + amount.
    self logTransaction: 'Deposit: ' , amount printString
```

```smalltalk
withdraw: amount
    balance := balance - amount.
    self logTransaction: 'Withdrawal: ' , amount printString
```

```smalltalk
logTransaction: description
    Transcript show: description; cr
```

Both `deposit:` and `withdraw:` use `self` to call `logTransaction:`.

## `self` in Chains

```smalltalk
fullAddress
    ^ self street , ', ' , self city , ', ' , self zipCode
```

Multiple `self` sends in one expression - all to the same object.

## What is `super`?

**`super`** is similar to `self`, but with a twist: it changes where method lookup starts.

- `self` says: "Start looking for the method in my class"
- `super` says: "Start looking for the method in my superclass"

Both `self` and `super` refer to the same object - they just change where the method search begins.

### When to Use `super`

The most common use is in `initialize`:

```smalltalk
initialize
    "Initialize a new person"
    super initialize.
    firstName := ''.
    lastName := ''
```

`super initialize` calls the superclass's `initialize` method first, then adds our own initialization.

Why is this important? Because superclasses might do important setup!

### A Concrete Example

```smalltalk
Object subclass: #Vehicle
    instanceVariableNames: 'make model year'
    ...
```

```smalltalk
initialize
    "Initialize a vehicle"
    super initialize.
    make := ''.
    model := ''.
    year := 0
```

```smalltalk
Vehicle subclass: #Car
    instanceVariableNames: 'numberOfDoors'
    ...
```

```smalltalk
initialize
    "Initialize a car"
    super initialize.
    numberOfDoors := 4
```

When you create a Car:

```smalltalk
Car new
```

1. `new` sends `initialize` to the new Car
2. Car>>initialize executes with `self` = the new Car
3. `super initialize` looks for `initialize` in the superclass (Vehicle)
4. Vehicle>>initialize executes (still with `self` = the Car)
5. `super initialize` in Vehicle calls Object>>initialize
6. Then Vehicle sets its variables
7. Then Car sets its variables

All with `self` referring to the same Car object throughout!

## `super` for Method Extension

Sometimes you want to extend a superclass's behavior, not replace it:

```smalltalk
Object subclass: #Animal
    instanceVariableNames: 'name'
    ...
```

```smalltalk
speak
    ^ 'I am an animal'
```

```smalltalk
Animal subclass: #Dog
    instanceVariableNames: ''
    ...
```

```smalltalk
speak
    ^ super speak , ' and I bark!'
```

Now:

```smalltalk
| dog |
dog := Dog new.
dog speak  "Returns 'I am an animal and I bark!'"
```

The Dog's `speak` calls the Animal's `speak` via `super`, then adds to it.

## The Difference Between `self` and `super`

They both refer to the same object, but:

- **`self`**: Method lookup starts in the receiver's class
- **`super`**: Method lookup starts in the superclass of the class where `super` appears

### Example

```smalltalk
Object subclass: #A
    instanceVariableNames: ''
    ...
```

```smalltalk
foo
    ^ 'A'
```

```smalltalk
A subclass: #B
    instanceVariableNames: ''
    ...
```

```smalltalk
foo
    ^ 'B'
```

```smalltalk
test
    ^ self foo , ' / ' , super foo
```

```smalltalk
| b |
b := B new.
b test  "Returns 'B / A'"
```

- `self foo` looks in B first, finds B>>foo, returns 'B'
- `super foo` skips B and looks in A, finds A>>foo, returns 'A'

Both are sent to the same B object!

## Why `super` in `initialize`

Every class should call `super initialize` in its `initialize` method. Why?

Because the superclass might have important initialization:

```smalltalk
Object subclass: #Timestamped
    instanceVariableNames: 'createdAt'
    ...
```

```smalltalk
initialize
    super initialize.
    createdAt := DateAndTime now
```

```smalltalk
Timestamped subclass: #LogEntry
    instanceVariableNames: 'message'
    ...
```

```smalltalk
initialize
    super initialize.  "Essential! Sets createdAt"
    message := ''
```

If LogEntry didn't call `super initialize`, the `createdAt` variable would never be set!

## `super` in Other Methods

You can use `super` in any method, not just `initialize`:

```smalltalk
printOn: aStream
    "Print a nice representation"
    super printOn: aStream.  "First, do the default printing"
    aStream nextPutAll: ' ['.
    aStream nextPutAll: self description.
    aStream nextPutAll: ']'
```

This extends the superclass's printing with additional info.

## Method Lookup Detailed

Let's trace a complete example:

```smalltalk
Object subclass: #A
    ...

foo
    ^ 'A-foo'

bar
    ^ 'A-bar'
```

```smalltalk
A subclass: #B
    ...

foo
    ^ 'B-foo'

baz
    ^ self foo , ' / ' , self bar , ' / ' , super foo
```

```smalltalk
| b |
b := B new.
b baz
```

What happens?

1. `b baz` - Look for `baz` in B. Found! Execute with `self` = b
2. `self foo` - Look for `foo` in B (self's class). Found B>>foo. Returns 'B-foo'
3. `self bar` - Look for `bar` in B. Not found. Look in A. Found A>>bar. Returns 'A-bar'
4. `super foo` - Look for `foo` in A (superclass of where super appears). Found A>>foo. Returns 'A-foo'
5. Concatenate: 'B-foo / A-bar / A-foo'

## Common Patterns with `self`

### Delegation

```smalltalk
area
    ^ width * height

perimeter
    ^ 2 * (width + height)

description
    ^ 'Rectangle with area ' , self area printString
```

Methods calling other methods.

### Template Method

```smalltalk
processRequest
    self validate.
    self execute.
    self cleanup
```

One method defines the structure, others implement the steps.

### Chaining

```smalltalk
x: newX
    x := newX.
    ^ self  "Return self for chaining"

y: newY
    y := newY.
    ^ self
```

Now you can write:

```smalltalk
point x: 10; y: 20
```

The semicolon cascade works because each setter returns `self`.

## Common Patterns with `super`

### Initialization

```smalltalk
initialize
    super initialize.
    "My initialization here"
```

Always do this!

### Extension

```smalltalk
someMethod
    super someMethod.  "Do the superclass's work"
    "Do additional work"
```

### Wrapping

```smalltalk
someMethod
    "Before work"
    | result |
    result := super someMethod.
    "After work"
    ^ result
```

## Blocks and `self`

Inside a block, `self` refers to the same object as outside the block:

```smalltalk
greetEveryone: names
    names do: [ :name |
        Transcript show: self fullName; show: ' greets '; show: name; cr ]
```

`self` in the block is the same Person object whose method is running.

## Class Methods and `self`

On the class side, `self` refers to the class:

```smalltalk
"Class side of Point2D:"
x: xValue y: yValue
    ^ self new x: xValue; y: yValue; yourself
```

Here, `self` is Point2D (the class), and `self new` creates an instance.

## You Cannot Assign to `self` or `super`

```smalltalk
foo
    self := SomethingElse new  "Error! Can't assign to self"
```

```smalltalk
bar
    super := AnotherThing new  "Error! Can't assign to super"
```

`self` and `super` are pseudo-variables, not real variables.

## Try This!

Practice with `self` and `super`:

1. **Create a Counter with reset:**
   ```smalltalk
   Object subclass: #Counter
       instanceVariableNames: 'count'
       ...

   initialize
       super initialize.
       self reset

   reset
       count := 0

   increment
       count := count + 1.
       ^ self

   value
       ^ count
   ```

2. **Create a BoundedCounter that extends Counter:**
   ```smalltalk
   Counter subclass: #BoundedCounter
       instanceVariableNames: 'maximum'
       ...

   initialize
       super initialize.
       maximum := 100

   increment
       count < maximum ifTrue: [ super increment ].
       ^ self

   maximum: value
       maximum := value
   ```

3. **Create shapes with area and description:**
   ```smalltalk
   Object subclass: #Shape
       instanceVariableNames: ''
       ...

   area
       self subclassResponsibility  "Must be overridden"

   describe
       ^ 'A shape with area ' , self area printString
   ```

   Then subclass it:
   ```smalltalk
   Shape subclass: #Circle
       instanceVariableNames: 'radius'
       ...

   area
       ^ Float pi * radius squared

   describe
       ^ super describe , ' (circle)'
   ```

4. **Trace method lookup:**
   Create classes and methods, then trace exactly what happens when you send messages.

## Common Mistakes

### Forgetting `self`

```smalltalk
greet
    ^ 'Hello, ' , fullName  "Error! fullName is not a variable or method"
```

Should be:

```smalltalk
greet
    ^ 'Hello, ' , self fullName
```

### Calling Instance Variable Instead of Method

If you have an instance variable `name` and a method `name`:

```smalltalk
greet
    ^ 'Hello, ' , name  "Direct variable access"
```

vs:

```smalltalk
greet
    ^ 'Hello, ' , self name  "Method call"
```

The first accesses the variable directly. The second sends a message. Usually, you want the method (especially if it might be overridden in subclasses).

### Forgetting `super initialize`

```smalltalk
initialize
    firstName := ''.  "Missing super initialize!"
    lastName := ''
```

Should be:

```smalltalk
initialize
    super initialize.
    firstName := ''.
    lastName := ''
```

### Using `super` When You Mean `self`

```smalltalk
fullName
    ^ super firstName , ' ' , super lastName  "Wrong!"
```

You want:

```smalltalk
fullName
    ^ self firstName , ' ' , self lastName
```

Use `super` only when you specifically want to bypass your class's implementation.

## The Power of `self`

`self` is what makes polymorphism work. When one object sends a message to another, it doesn't know or care what class that other object is - it just sends the message and trusts the receiver to respond appropriately.

This is the essence of object-oriented programming: objects communicating through messages, with `self` allowing objects to collaborate with themselves and their parts.

## Looking Ahead

You now understand `self` and `super` - how they work, when to use them, and why they're fundamental to Smalltalk's design.

In Chapter 15, we'll explore **Inheritance** in depth - how classes build on other classes, sharing and extending behavior. You'll see how `self`, `super`, and method lookup work together to create flexible, reusable class hierarchies.

Then in Part V, we'll dive into the Image system and how Smalltalk manages code persistence. Part VI will cover the development tools in detail.

You're mastering the core concepts of object-oriented programming. The foundation is solid!

---

**Key Takeaways:**
- **`self`** always refers to the object that received the current message
- Use `self` to send messages to the current object
- Methods can't be called directly - use `self` to send messages
- **`super`** refers to the same object, but changes where method lookup starts
- `super` starts lookup in the superclass of the class where `super` appears
- Always call `super initialize` in `initialize` methods
- Use `super` to extend or wrap superclass behavior
- **Method lookup**: Start in the receiver's class, go up the hierarchy
- `self` and `super` are pseudo-variables - you can't assign to them
- `self` enables polymorphism - different objects respond differently
- Both work in class methods too, where `self` is the class itself

---

[Previous: Chapter 13 - Instance Variables](chapter-13-instance-variables.md) | [Next: Chapter 15 - Inheritance](chapter-15-inheritance.md)
