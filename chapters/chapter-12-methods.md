# Chapter 12: Methods - Teaching Objects New Tricks

In the last chapter, you created classes - blueprints for objects. But those objects couldn't do much because they didn't have any behavior. Now we'll add **methods** - the code that defines what your objects can do.

A **method** is a named piece of code attached to a class. When you send a message to an object, the object looks up the corresponding method and executes it.

Remember: in Smalltalk, when you write `5 + 3`, you're sending the message `+` with argument `3` to the object `5`. The object looks up its `+` method and executes it. Now you'll define methods for your own classes!

## Your First Method

Let's add a simple method to the Point2D class you created in Chapter 11. If you didn't create it, do so now:

```smalltalk
Object subclass: #Point2D
    instanceVariableNames: 'x y'
    classVariableNames: ''
    package: 'MyFirstClasses'
```

### Adding a Method

1. **Open the System Browser** (Ctrl+O then B, or Cmd+O then B)
2. **Select your package** `MyFirstClasses`
3. **Select your class** `Point2D`
4. **Make sure you're on the instance side** (there should be a button or indicator showing "instance side" vs "class side")
5. **Look at the protocol pane** (third from left) - it might show "-- all --" or be empty

In the bottom code pane, you'll see a method template:

```smalltalk
messageSelectorAndArgumentNames
    "comment stating purpose of message"
    | temporary variable names |
    statements
```

Replace this with your first method:

```smalltalk
distanceFromOrigin
    "Calculate the distance from this point to the origin (0,0)"
    ^ ((x * x) + (y * y)) sqrt
```

Now **Accept** this (Ctrl+S or Cmd+S, or right-click and choose "Accept").

**You've just defined a method!**

### What This Method Does

- **Method name**: `distanceFromOrigin` (no arguments)
- **Comment**: "Calculate the distance..." (documentation)
- **Body**: The actual code
- **Returns**: `^` means "return this value"

The formula `((x * x) + (y * y)) sqrt` is the Pythagorean theorem: distance = √(x² + y²)

### Trying Your Method

Now let's try to use it. Open a Playground and... wait, there's a problem:

```smalltalk
| point |
point := Point2D new.
point distanceFromOrigin  "This won't work yet!"
```

Why not? Because `x` and `y` are still `nil`! We need a way to set them.

## Accessor Methods

**Accessor methods** (also called getters) allow you to read instance variables from outside the object.

Add these methods to Point2D (one at a time, accepting each):

```smalltalk
x
    "Return the x coordinate"
    ^ x
```

```smalltalk
y
    "Return the y coordinate"
    ^ y
```

These methods simply return the value of the instance variable.

Now try:

```smalltalk
| point |
point := Point2D new.
point x  "Returns nil"
```

It works! But the value is `nil` because we haven't set it yet.

## Mutator Methods

**Mutator methods** (also called setters) allow you to change instance variables.

Add these methods:

```smalltalk
x: aNumber
    "Set the x coordinate"
    x := aNumber
```

```smalltalk
y: aNumber
    "Set the y coordinate"
    y := aNumber
```

Note the colon `:` in the method name - this indicates it takes an argument.

Now try:

```smalltalk
| point |
point := Point2D new.
point x: 10.
point y: 20.
point x  "Returns 10"
point y  "Returns 20"
```

Excellent! Now let's calculate the distance:

```smalltalk
| point |
point := Point2D new.
point x: 3.
point y: 4.
point distanceFromOrigin  "Returns 5.0"
```

The 3-4-5 triangle! It works!

## The Caret: Returning Values

The `^` symbol means "return from this method with this value."

```smalltalk
distanceFromOrigin
    ^ ((x * x) + (y * y)) sqrt
```

Without `^`, the method returns `self` (the object itself). More on `self` in Chapter 14.

### Multiple Return Points

You can have multiple return statements:

```smalltalk
absoluteValue
    x < 0 ifTrue: [ ^ x negated ].
    ^ x
```

As soon as a `^` is executed, the method exits immediately and returns that value.

### Return vs No Return

```smalltalk
x: aNumber
    "Set the x coordinate"
    x := aNumber
```

This setter doesn't explicitly return anything, so it returns `self`. This allows **method chaining**:

```smalltalk
| point |
point := Point2D new.
point x: 10; y: 20; distanceFromOrigin
```

The semicolon `;` (cascade) sends multiple messages to the same object.

## Method Categories (Protocols)

Methods are organized into **protocols** (categories). This is just for organization - it doesn't affect how methods work.

In the System Browser, look at the protocol pane (third from left). You might see "as yet unclassified" or "--all--".

Let's organize your methods:

1. **Click the protocol pane**
2. **Right-click** and choose "New protocol..."
3. **Name it** `accessing`
4. **Press Enter**

Now, drag your accessor and mutator methods (`x`, `x:`, `y`, `y:`) into the `accessing` protocol.

Create another protocol called `calculating` and put `distanceFromOrigin` in it.

Common protocol names:
- `accessing` - Getters and setters
- `initialization` - Setup methods
- `testing` - Methods that return booleans
- `comparing` - Methods like `=` and `<`
- `converting` - Methods like `asString`
- `printing` - Methods like `printString`
- `calculating` - Computational methods
- `private` - Internal methods not meant for public use

## Adding More Sophisticated Methods

Let's add more interesting methods to Point2D.

### A Method with Multiple Arguments

```smalltalk
distanceTo: anotherPoint
    "Calculate the distance to another point"
    | dx dy |
    dx := anotherPoint x - x.
    dy := anotherPoint y - y.
    ^ ((dx * dx) + (dy * dy)) sqrt
```

Put this in the `calculating` protocol.

Try it:

```smalltalk
| point1 point2 |
point1 := Point2D new x: 0; y: 0.
point2 := Point2D new x: 3; y: 4.
point1 distanceTo: point2  "Returns 5.0"
```

### A Method That Returns a New Object

```smalltalk
midpointTo: anotherPoint
    "Return a new point halfway between this point and another"
    | newX newY |
    newX := (x + anotherPoint x) / 2.
    newY := (y + anotherPoint y) / 2.
    ^ Point2D new x: newX; y: newY
```

Try it:

```smalltalk
| point1 point2 mid |
point1 := Point2D new x: 0; y: 0.
point2 := Point2D new x: 10; y: 10.
mid := point1 midpointTo: point2.
mid x  "Returns 5.0"
mid y  "Returns 5.0"
```

### A Testing Method

Methods that return booleans often start with `is` or end with a question:

```smalltalk
isAtOrigin
    "Return true if this point is at the origin"
    ^ (x = 0) and: [ y = 0 ]
```

Put this in a `testing` protocol.

```smalltalk
| point |
point := Point2D new x: 0; y: 0.
point isAtOrigin  "Returns true"
```

## The `printString` and `printOn:` Methods

When you print an object, Smalltalk calls its `printOn:` method. Let's customize this for Point2D:

```smalltalk
printOn: aStream
    "Print a readable representation of this point"
    super printOn: aStream.
    aStream nextPutAll: '('.
    x printOn: aStream.
    aStream nextPutAll: '@'.
    y printOn: aStream.
    aStream nextPutAll: ')'
```

Now try:

```smalltalk
| point |
point := Point2D new x: 10; y: 20.
point printString  "Returns 'a Point2D(10@20)'"
```

Much better than just "a Point2D"!

(Don't worry about understanding `super` yet - we'll cover it in Chapter 14.)

## Initializing Objects

Currently, creating a Point2D leaves x and y as `nil`. Let's add an initialization method:

```smalltalk
initialize
    "Set default values for a new point"
    super initialize.
    x := 0.
    y := 0
```

Put this in an `initialization` protocol.

The `initialize` method is special - it's automatically called when you create an object with `new`.

Now:

```smalltalk
| point |
point := Point2D new.
point x  "Returns 0, not nil!"
```

### A Custom Creation Method

What if you want to create a point with specific coordinates in one line? Add a **class method**:

1. In the System Browser, **switch to the class side** (look for a button or toggle)
2. Add this method:

```smalltalk
x: xCoord y: yCoord
    "Create a new point with the given coordinates"
    ^ self new x: xCoord; y: yCoord; yourself
```

Put it in an `instance creation` protocol.

Now you can write:

```smalltalk
Point2D x: 10 y: 20
```

Much cleaner! `Point2D` (the class) receives the message, creates a new instance, sets its coordinates, and returns it.

## A Complete Example: BankAccount

Let's create a more substantial class with multiple methods. First, define the class:

```smalltalk
Object subclass: #BankAccount
    instanceVariableNames: 'accountNumber balance owner'
    classVariableNames: ''
    package: 'MyFirstClasses'
```

Now add methods (all on the instance side unless noted):

### Initialization

```smalltalk
initialize
    "Initialize a new bank account"
    super initialize.
    balance := 0.
    accountNumber := ''
```

### Accessors

```smalltalk
accountNumber
    ^ accountNumber
```

```smalltalk
accountNumber: aString
    accountNumber := aString
```

```smalltalk
balance
    ^ balance
```

```smalltalk
owner
    ^ owner
```

```smalltalk
owner: aString
    owner := aString
```

### Operations

```smalltalk
deposit: amount
    "Deposit money into the account"
    amount > 0 ifFalse: [ ^ self error: 'Amount must be positive' ].
    balance := balance + amount
```

```smalltalk
withdraw: amount
    "Withdraw money from the account"
    amount > 0 ifFalse: [ ^ self error: 'Amount must be positive' ].
    amount > balance ifTrue: [ ^ self error: 'Insufficient funds' ].
    balance := balance - amount
```

### Testing

```smalltalk
canWithdraw: amount
    "Can the given amount be withdrawn?"
    ^ (amount > 0) and: [ amount <= balance ]
```

### Printing

```smalltalk
printOn: aStream
    "Print a readable representation"
    super printOn: aStream.
    aStream nextPutAll: '('.
    owner printOn: aStream.
    aStream nextPutAll: ': '.
    balance printOn: aStream.
    aStream nextPutAll: ')'
```

### Class Method for Creation

Switch to **class side** and add:

```smalltalk
forOwner: ownerName
    "Create a new account for the given owner"
    ^ self new owner: ownerName; yourself
```

### Using BankAccount

Now try it all together:

```smalltalk
| account |
account := BankAccount forOwner: 'Alice'.
account accountNumber: '12345'.
account deposit: 1000.
account withdraw: 250.
account balance  "Returns 750"
account printString  "Returns 'a BankAccount(Alice: 750)'"
account canWithdraw: 500  "Returns true"
account canWithdraw: 1000  "Returns false"
```

Try to withdraw too much:

```smalltalk
| account |
account := BankAccount forOwner: 'Bob'.
account deposit: 100.
account withdraw: 200  "Error: Insufficient funds"
```

## Method Naming Conventions

Good method names are crucial for readable code.

### Unary Messages (No Arguments)

Start with a lowercase letter, use camelCase:

```smalltalk
distanceFromOrigin
accountNumber
isEmpty
asString
```

### Keyword Messages (With Arguments)

Each keyword ends with a colon:

```smalltalk
x: aNumber
distanceTo: anotherPoint
deposit: amount
at: index put: value
```

The parameter names should be descriptive:

**Good:**
```smalltalk
withdraw: amount
setName: aString
addItem: anItem
```

**Bad:**
```smalltalk
withdraw: a
setName: s
addItem: x
```

### Boolean Methods

Methods that return booleans often start with `is`, `has`, `can`, or end with a description:

```smalltalk
isEmpty
hasNext
canWithdraw:
isAtOrigin
```

### Intention-Revealing Names

Choose names that reveal what the method does:

**Good:**
```smalltalk
deposit: amount
calculateInterest
displayOnScreen
```

**Bad:**
```smalltalk
d: a  "What does this mean?"
calc
show
```

## Temporary Variables in Methods

Methods can declare temporary variables:

```smalltalk
distanceTo: anotherPoint
    "Calculate the distance to another point"
    | dx dy |
    dx := anotherPoint x - x.
    dy := anotherPoint y - y.
    ^ ((dx * dx) + (dy * dy)) sqrt
```

The `| dx dy |` declares two temporary variables used within the method.

## Common Method Patterns

### Guard Clause Pattern

Check preconditions at the start:

```smalltalk
withdraw: amount
    amount > 0 ifFalse: [ ^ self error: 'Amount must be positive' ].
    amount > balance ifTrue: [ ^ self error: 'Insufficient funds' ].
    balance := balance - amount
```

### Delegation Pattern

Call other methods to do parts of the work:

```smalltalk
transfer: amount to: anotherAccount
    self withdraw: amount.
    anotherAccount deposit: amount
```

### Query Pattern

Methods that return information without changing the object:

```smalltalk
balance
    ^ balance
```

```smalltalk
canWithdraw: amount
    ^ (amount > 0) and: [ amount <= balance ]
```

### Command Pattern

Methods that change the object's state:

```smalltalk
deposit: amount
    balance := balance + amount
```

```smalltalk
clear
    balance := 0
```

## Comments in Methods

Always add a comment explaining what the method does:

```smalltalk
distanceFromOrigin
    "Calculate the Euclidean distance from this point to the origin (0,0).
     Returns a Float representing the distance."
    ^ ((x * x) + (y * y)) sqrt
```

Good comments explain:
- **What** the method does
- **What** it returns
- **What** the parameters mean (for methods with arguments)
- **Why** the method exists (if it's not obvious)

Don't comment obvious things:

**Bad:**
```smalltalk
x
    "Returns x"
    ^ x
```

**Better:**
```smalltalk
x
    "Return the x coordinate of this point"
    ^ x
```

## Method Visibility

In Smalltalk, all methods are public - any object can send any message to any other object. There's no `private` or `protected` keyword.

However, by convention, methods in a `private` protocol are considered internal and shouldn't be called from outside the class.

## Overriding Methods

Your class can override methods inherited from superclasses. For example, every object inherits `printString` from Object. When you define `printOn:`, you're customizing this behavior.

We'll explore this more in Chapter 15 on inheritance.

## Try This!

Practice writing methods:

1. **Add more methods to Point2D:**
   ```smalltalk
   "Add these methods:"
   - moveBy: deltaX and: deltaY  "Move the point"
   - isLeftOf: anotherPoint  "Test if this point is left of another"
   - asString  "Return a string representation"
   ```

2. **Create a Counter class:**
   ```smalltalk
   Object subclass: #Counter
       instanceVariableNames: 'value'
       ...
   ```

   Add methods:
   ```smalltalk
   initialize  "Start at 0"
   increment  "Add 1"
   decrement  "Subtract 1"
   reset  "Go back to 0"
   value  "Return current value"
   ```

3. **Create a Rectangle class:**
   ```smalltalk
   Object subclass: #Rectangle
       instanceVariableNames: 'width height'
       ...
   ```

   Add methods:
   ```smalltalk
   area  "width * height"
   perimeter  "2 * (width + height)"
   isSquare  "width = height"
   scale: factor  "Multiply width and height by factor"
   ```

4. **Create a Temperature class:**
   ```smalltalk
   Object subclass: #Temperature
       instanceVariableNames: 'celsius'
       ...
   ```

   Add methods:
   ```smalltalk
   celsius: degrees  "Set temperature in Celsius"
   celsius  "Get temperature in Celsius"
   fahrenheit  "Convert to Fahrenheit"
   kelvin  "Convert to Kelvin"
   isBoiling  "Is temperature >= 100°C?"
   isFreezing  "Is temperature <= 0°C?"
   ```

## Common Mistakes

### Forgetting the Caret

```smalltalk
distanceFromOrigin
    ((x * x) + (y * y)) sqrt  "Missing ^"
```

This method returns `self`, not the distance! Should be:

```smalltalk
distanceFromOrigin
    ^ ((x * x) + (y * y)) sqrt
```

### Confusing Instance and Class Side

If you define a method on the wrong side, it won't work as expected. Instance methods go on the instance side, class methods on the class side.

### Forgetting to Accept

After typing your method, always Accept (Ctrl+S or Cmd+S). Otherwise, your changes aren't saved!

### Direct Instance Variable Access from Outside

You can't do this:

```smalltalk
| point |
point := Point2D new.
point x := 10  "Error! Can't access x directly"
```

You must use methods:

```smalltalk
point x: 10  "Correct - uses the setter method"
```

## Looking Ahead

You now know how to:
- Define methods on your classes
- Create accessors and mutators
- Write methods that calculate, test, and transform
- Organize methods into protocols
- Initialize objects
- Create class methods for object creation

In Chapter 13, we'll dive deeper into **instance variables** - understanding how they work, when to use them, and how they give objects memory and state.

Then in Chapter 14, we'll unravel the mysteries of `self` and `super` - two special variables that are key to understanding how methods work.

Your classes are coming alive! You're not just using Smalltalk objects anymore - you're creating objects with their own behavior, and they interact just like built-in Smalltalk objects do. That's the power of object-oriented programming!

---

**Key Takeaways:**
- **Methods** define what objects can do (their behavior)
- Add methods using the System Browser on the instance side
- Use `^` to return values from methods
- **Accessor methods** read instance variables, **mutator methods** change them
- Method names use camelCase and should be descriptive
- Methods can have parameters (keyword messages end with colons)
- Organize methods into protocols for clarity
- The `initialize` method sets up new objects
- Class methods (on the class side) often create instances
- Always comment your methods to explain their purpose
- Accept (Ctrl+S/Cmd+S) to save your methods
- Good method names make code self-documenting

---

[Previous: Chapter 11 - Classes](chapter-11-classes.md) | [Next: Chapter 13 - Instance Variables](chapter-13-instance-variables.md)
