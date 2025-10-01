# Chapter 23: Protocols and Polymorphism

Welcome to Part VII: Intermediate Concepts! You've mastered the basics of Smalltalk programming and its powerful tools. Now we'll explore deeper concepts that make object-oriented programming elegant and powerful.

First up: **Protocols** and **Polymorphism** - two fundamental concepts that are at the heart of how Smalltalk (and all object-oriented systems) work. You've been using them all along; now you'll understand them deeply.

## What is a Protocol?

In Smalltalk, the word "protocol" has two related meanings:

### 1. Method Category (in the System Browser)

A **protocol** is a category for organizing related methods in a class:
- `accessing` - Getters and setters
- `testing` - Boolean queries
- `converting` - Type conversions
- `arithmetic` - Math operations

These are just organizational labels to help you find methods.

### 2. Interface Contract (the deeper meaning)

A **protocol** is a set of messages that an object understands - its interface to the world:

"Objects that understand these messages conform to this protocol."

This chapter focuses on the second, more important meaning.

## The Essential Idea

In Smalltalk, you don't care about an object's **class**. You care about what **messages** it understands.

### Traditional Object-Oriented Thinking:

```
"Is this object a Dog?"
if (object instanceof Dog) {
    object.bark();
}
```

You check the TYPE, then call methods.

### Smalltalk Thinking:

```smalltalk
"Does this object understand 'speak'?"
object speak
```

You just send the message. If the object understands it, great! If not, you get an error.

This is **duck typing**: "If it walks like a duck and quacks like a duck, it's a duck."

## What is Polymorphism?

**Polymorphism** means "many forms" - the ability of different objects to respond to the same message in their own way.

### Example:

```smalltalk
| shapes |
shapes := Array
    with: (Circle new radius: 5)
    with: (Rectangle new width: 10; height: 20)
    with: (Triangle new base: 8; height: 6).

shapes do: [ :shape |
    Transcript show: 'Area: '; show: shape area printString; cr ]
```

Each shape responds to `area`, but calculates it differently:
- Circle: π × r²
- Rectangle: width × height
- Triangle: (base × height) / 2

**Same message, different behavior.** That's polymorphism!

## Protocols Define Behavior

A protocol is an **implicit contract**:

"Objects conforming to this protocol understand these messages."

### The Collection Protocol

All collections understand:
- `add:` - Add an element
- `remove:` - Remove an element
- `size` - Number of elements
- `do:` - Iterate over elements
- `includes:` - Check membership

Whether it's an `Array`, `OrderedCollection`, `Set`, or `Dictionary`, they all conform to the Collection protocol.

### Why This Matters

You can write code that works with **any** collection:

```smalltalk
printAll: aCollection
    "Print all elements in any collection"
    aCollection do: [ :element |
        Transcript show: element printString; cr ]
```

This method works with:
- `Array`: `self printAll: #(1 2 3)`
- `OrderedCollection`: `self printAll: (OrderedCollection with: 'a' with: 'b')`
- `Set`: `self printAll: (Set new add: 1; add: 2; yourself)`
- Anything that understands `do:`!

## Formal vs Informal Protocols

### Informal Protocols

Most Smalltalk protocols are **informal** - not enforced by the language, just by convention.

"If you want to be a collection, implement `add:`, `remove:`, `size`, `do:`, etc."

There's no compiler check. You just implement the methods, and your object becomes part of that protocol.

### Example: The Comparable Protocol

Objects that can be compared implement:
- `=` - Equality
- `hash` - Hash code for hashing
- `<` - Less than
- `>` - Greater than
- `<=` - Less than or equal
- `>=` - Greater than or equal

Implement these, and your objects can be sorted, used in Sets, etc.!

### Example: The Printable Protocol

Objects that can be printed implement:
- `printString` - Return a string representation
- `printOn:` - Print to a stream

Implement these, and `Transcript show: yourObject` works!

## Designing Protocols

When designing classes, think in terms of protocols:

### Bad Design (thinking in types):

```smalltalk
Object subclass: #DogProcessor
    ...

process: aDog
    "Process a dog"
    aDog bark.
    aDog wagTail
```

This only works with Dogs!

### Good Design (thinking in protocols):

```smalltalk
Object subclass: #AnimalProcessor
    ...

process: anAnimal
    "Process any animal"
    anAnimal speak.
    anAnimal expressHappiness
```

This works with anything that understands `speak` and `expressHappiness`:
- Dogs: bark and wag tail
- Cats: meow and purr
- Birds: chirp and flutter wings

**Design for protocols, not types.**

## Common Smalltalk Protocols

Let's explore standard protocols:

### The Accessing Protocol

**Purpose**: Get and set values

**Methods**: `name`, `name:`, `age`, `age:`, etc.

**Example**:

```smalltalk
person name.  "Get name"
person name: 'Alice'.  "Set name"
```

### The Testing Protocol

**Purpose**: Boolean queries

**Methods**: `isEmpty`, `isNil`, `isNumber`, `isPrime`, etc.

**Naming**: Always start with `is` or `has`

**Example**:

```smalltalk
collection isEmpty.
number isPrime.
object isKindOf: String
```

### The Converting Protocol

**Purpose**: Convert to other types

**Methods**: `asString`, `asArray`, `asOrderedCollection`, `asInteger`, etc.

**Naming**: Always start with `as`

**Example**:

```smalltalk
42 asString.  "Returns '42'"
'42' asInteger.  "Returns 42"
#(1 2 3) asOrderedCollection
```

### The Comparing Protocol

**Purpose**: Compare objects

**Methods**: `=`, `~=`, `hash`, `<`, `>`, `<=`, `>=`

**Example**:

```smalltalk
5 = 5.  "true"
'hello' < 'world'.  "true (alphabetically)"
```

### The Enumerating Protocol

**Purpose**: Iterate over elements

**Methods**: `do:`, `collect:`, `select:`, `reject:`, `detect:`, `inject:into:`

**Example**:

```smalltalk
collection do: [ :each | Transcript show: each printString; cr ].
numbers collect: [ :n | n * 2 ].
numbers select: [ :n | n even ]
```

### The Arithmetic Protocol

**Purpose**: Math operations

**Methods**: `+`, `-`, `*`, `/`, `//`, `\\`, `sqrt`, `squared`, `abs`, etc.

**Example**:

```smalltalk
5 + 3.
10 sqrt.
-5 abs
```

### The Copying Protocol

**Purpose**: Duplicate objects

**Methods**: `copy`, `deepCopy`, `shallowCopy`, `postCopy`

**Example**:

```smalltalk
original := OrderedCollection with: 1 with: 2.
duplicate := original copy.
duplicate add: 3.
original  "Still #(1 2)"
duplicate  "Now #(1 2 3)"
```

## Implementing Protocols

To make your class conform to a protocol, implement the required methods:

### Example: Making a Custom Collection

```smalltalk
Object subclass: #MyCollection
    instanceVariableNames: 'items'
    classVariableNames: ''
    package: 'MyApp'
```

Implement the Collection protocol:

```smalltalk
initialize
    super initialize.
    items := OrderedCollection new

add: anObject
    items add: anObject.
    ^ anObject

remove: anObject
    items remove: anObject.
    ^ anObject

size
    ^ items size

do: aBlock
    items do: aBlock

includes: anObject
    ^ items includes: anObject
```

Now `MyCollection` acts like a collection! You can use it anywhere a collection is expected.

## Protocol Inheritance

Protocols often build on each other:

```
Object Protocol (everything)
  └─ Collection Protocol (collections)
      └─ SequenceableCollection Protocol (ordered collections)
          └─ OrderedCollection Protocol (dynamic ordered collections)
```

Each level adds more messages:
- **Object**: `=`, `hash`, `printString`, `copy`
- **Collection**: `add:`, `remove:`, `size`, `do:`, `includes:`
- **SequenceableCollection**: `at:`, `at:put:`, `first`, `last`, `indexOf:`
- **OrderedCollection**: `addFirst:`, `addLast:`, `removeFirst`, `removeLast`

An `OrderedCollection` understands **all** of these messages!

## Duck Typing in Action

Smalltalk uses **duck typing**: if an object responds to the right messages, it conforms to the protocol.

### Example: File-Like Objects

In many languages, you have a `File` interface. In Smalltalk, you have a "stream protocol":

Any object that understands:
- `nextPut:` - Write one element
- `nextPutAll:` - Write multiple elements
- `close` - Close the stream

can be used as a writable stream!

This includes:
- `FileStream` - Writing to files
- `String writeStream` - Building strings
- `SocketStream` - Writing to network sockets
- `StandardOutputStream` - Writing to console

**Same protocol, different implementations.**

```smalltalk
writeData: aStream
    "Write data to any stream"
    aStream nextPutAll: 'Hello, world!'.
    aStream close
```

This works with **any** stream!

## Polymorphism Examples

### Example 1: Shapes

```smalltalk
Object subclass: #Shape
    ...

area
    self subclassResponsibility

perimeter
    self subclassResponsibility
```

```smalltalk
Shape subclass: #Circle
    instanceVariableNames: 'radius'
    ...

area
    ^ Float pi * radius squared

perimeter
    ^ 2 * Float pi * radius
```

```smalltalk
Shape subclass: #Rectangle
    instanceVariableNames: 'width height'
    ...

area
    ^ width * height

perimeter
    ^ 2 * (width + height)
```

Usage:

```smalltalk
| shapes totalArea |
shapes := Array
    with: (Circle new radius: 5)
    with: (Rectangle new width: 10; height: 20)
    with: (Circle new radius: 3).

totalArea := 0.
shapes do: [ :shape | totalArea := totalArea + shape area ].
totalArea
```

Each shape calculates its area differently, but the code doesn't care!

### Example 2: Strategies

```smalltalk
Object subclass: #PaymentProcessor
    ...

processPayment: amount using: strategy
    strategy charge: amount
```

```smalltalk
Object subclass: #CreditCardPayment
    ...

charge: amount
    "Charge a credit card"
    Transcript show: 'Charging $', amount printString, ' to credit card'; cr
```

```smalltalk
Object subclass: #PayPalPayment
    ...

charge: amount
    "Charge via PayPal"
    Transcript show: 'Charging $', amount printString, ' via PayPal'; cr
```

```smalltalk
Object subclass: #CryptoPayment
    ...

charge: amount
    "Charge via cryptocurrency"
    Transcript show: 'Charging $', amount printString, ' in Bitcoin'; cr
```

Usage:

```smalltalk
processor := PaymentProcessor new.

processor processPayment: 100 using: CreditCardPayment new.
processor processPayment: 50 using: PayPalPayment new.
processor processPayment: 200 using: CryptoPayment new
```

Different payment methods, same protocol (`charge:`), seamless substitution!

## Protocols vs Interfaces (Other Languages)

### Java/C# Interfaces:

```java
interface Drawable {
    void draw();
}

class Circle implements Drawable {
    public void draw() { ... }
}
```

Explicit declaration: "I implement Drawable."

### Smalltalk Protocols:

```smalltalk
"No explicit declaration!"
"Just implement draw:"

draw
    "Draw the circle"
    ...
```

Implicit conformance: "If I implement `draw`, I'm drawable."

**Advantage**: More flexible. No need to declare conformance upfront.

**Disadvantage**: No compiler check. If you forget a method, you'll find out at runtime.

## Designing Good Protocols

### Principle 1: Cohesion

Keep related methods together:

**Good**:
- `Comparable Protocol`: `=`, `hash`, `<`, `>`

**Bad**:
- `MiscProtocol`: `equals`, `getName`, `calculateTax`, `serialize`

Group by purpose!

### Principle 2: Small Protocols

Smaller protocols are easier to implement:

**Good**:
- `Printable`: Just `printOn:`

**Bad**:
- `Everything`: Dozens of methods

### Principle 3: Composability

Protocols should compose well:

```
Printable + Comparable + Copyable = Rich Object
```

Don't create one giant protocol with everything.

### Principle 4: Meaningful Names

Use clear, descriptive names:

**Good**:
- `isEmpty` (testing)
- `asString` (converting)
- `add:` (modifying)

**Bad**:
- `check` (check what?)
- `convert` (convert to what?)
- `doIt` (do what?)

### Principle 5: Consistency

Follow conventions:
- Testing methods start with `is` or `has`
- Converting methods start with `as`
- Boolean methods return `true` or `false`
- Modifying methods often return the modified object or `self`

## The Null Object Pattern

Use polymorphism to eliminate nil checks:

### Without Null Object:

```smalltalk
process: account
    account ifNotNil: [
        account balance > 0 ifTrue: [
            self chargeAccount: account ] ]
```

Lots of nil checks!

### With Null Object:

```smalltalk
Object subclass: #NullAccount
    ...

balance
    ^ 0

charge: amount
    "Do nothing"
```

```smalltalk
process: account
    account balance > 0 ifTrue: [
        self chargeAccount: account ]
```

No nil checks! `NullAccount` responds to the same protocol as `Account`, but does nothing.

## Polymorphism and Testing

Polymorphism enables testing with mocks and stubs:

### Production Code:

```smalltalk
sendEmail: recipient subject: subject body: body
    emailService send: recipient subject: subject body: body
```

### Test Code:

```smalltalk
Object subclass: #MockEmailService
    instanceVariableNames: 'sentEmails'
    ...

send: recipient subject: subject body: body
    sentEmails add: (Dictionary new
        at: 'recipient' put: recipient;
        at: 'subject' put: subject;
        at: 'body' put: body;
        yourself)
```

The test uses `MockEmailService`, which conforms to the same protocol as the real `EmailService`, but just records calls instead of sending emails!

## Protocols in the Standard Library

Explore the standard library to see protocols in action:

### Number Protocol

All numbers understand:
- Arithmetic: `+`, `-`, `*`, `/`
- Comparisons: `<`, `>`, `=`
- Queries: `isZero`, `positive`, `negative`, `even`, `odd`
- Conversions: `asFloat`, `asInteger`, `asString`
- Math: `abs`, `sqrt`, `sin`, `cos`, `exp`, `ln`

### String Protocol

All strings understand:
- Accessing: `at:`, `size`
- Testing: `isEmpty`, `includes:`
- Converting: `asUppercase`, `asLowercase`, `asSymbol`
- Searching: `indexOf:`, `findString:`
- Modifying: `,` (concatenate), `copyReplaceAll:with:`

### Block Protocol

All blocks understand:
- Evaluation: `value`, `value:`, `value:value:`
- Control: `whileTrue:`, `whileFalse:`
- Exception: `on:do:`
- Timing: `ensure:`, `ifCurtailed:`

## Try This!

Practice with protocols and polymorphism:

1. **Implement a custom comparable class:**
   ```smalltalk
   Object subclass: #Person
       instanceVariableNames: 'name age'
       ...

   = other
       ^ self class = other class
           and: [ name = other name
           and: [ age = other age ] ]

   hash
       ^ name hash bitXor: age hash

   < other
       ^ age < other age  "Compare by age"
   ```

   Test:
   ```smalltalk
   alice := Person new name: 'Alice'; age: 30.
   bob := Person new name: 'Bob'; age: 25.
   alice = alice.  "true"
   alice < bob.  "false"
   bob < alice.  "true"
   ```

2. **Create polymorphic animals:**
   ```smalltalk
   Object subclass: #Animal
       ...
   speak
       self subclassResponsibility

   Animal subclass: #Dog
   speak
       ^ 'Woof!'

   Animal subclass: #Cat
   speak
       ^ 'Meow!'

   Animal subclass: #Cow
   speak
       ^ 'Moo!'
   ```

   Test:
   ```smalltalk
   animals := Array with: Dog new with: Cat new with: Cow new.
   animals do: [ :animal |
       Transcript show: animal speak; cr ]
   ```

3. **Implement a custom collection:**
   ```smalltalk
   Object subclass: #Stack
       instanceVariableNames: 'items'
       ...

   initialize
       items := OrderedCollection new

   push: anObject
       items addLast: anObject

   pop
       ^ items removeLast

   peek
       ^ items last

   size
       ^ items size

   isEmpty
       ^ items isEmpty

   do: aBlock
       items do: aBlock
   ```

   Now your Stack acts like a collection!

4. **Create payment strategies:**
   Implement the payment example from earlier with at least three different payment methods.

5. **Implement the Null Object pattern:**
   ```smalltalk
   Object subclass: #Customer
       instanceVariableNames: 'name email'
       ...

   Object subclass: #NullCustomer
       ...

   name
       ^ 'Guest'

   email
       ^ 'no-email@example.com'
   ```

   Use `NullCustomer` instead of `nil` in your code.

6. **Explore system protocols:**
   ```smalltalk
   "Find all methods in the 'comparing' protocol:"
   Object allSubclasses flatCollect: [ :class |
       (class organization protocolsForCategory: #comparing)
           ifEmpty: [ #() ]
           ifNotEmpty: [ :methods | methods ] ]
   ```

## Common Mistakes

### Checking Types Instead of Protocols

**Bad**:
```smalltalk
process: object
    object class = Array ifTrue: [ self processArray: object ].
    object class = OrderedCollection ifTrue: [ self processCollection: object ]
```

**Good**:
```smalltalk
process: aCollection
    aCollection do: [ :each | self processElement: each ]
```

Don't check types! Use polymorphism!

### Giant Protocols

Don't create protocols with 50 methods. Break them into smaller, focused protocols.

### Inconsistent Naming

Don't mix conventions. Testing methods should start with `is` or `has`, not sometimes `check` or `test`.

### Breaking Liskov Substitution

If a subclass can't fulfill the superclass's protocol, the hierarchy is wrong.

## The Power of Protocols

Protocols enable:
- **Flexibility**: Swap implementations easily
- **Testability**: Mock objects for testing
- **Reusability**: Code works with any conforming object
- **Extensibility**: Add new types without changing existing code
- **Simplicity**: No complex type hierarchies

## Looking Ahead

You now deeply understand protocols and polymorphism - the heart of object-oriented design! You know:
- Protocols define sets of messages objects understand
- Polymorphism lets different objects respond to the same message
- Smalltalk uses duck typing and informal protocols
- Design for protocols, not types
- Common protocols: accessing, testing, converting, comparing, enumerating
- Implementing protocols makes your objects integrate seamlessly

In Chapter 24, we'll explore **Error Handling** - how to signal, catch, and recover from errors gracefully using Smalltalk's exception system.

Then Chapter 25 covers **Testing Your Code** with SUnit, Smalltalk's testing framework.

Part VII is deepening your understanding of professional Smalltalk development!

---

**Key Takeaways:**
- **Protocol** = a set of messages an object understands
- **Polymorphism** = different objects responding to the same message differently
- Smalltalk uses **duck typing**: if it responds to the messages, it conforms
- **Design for protocols, not types**
- Common protocols: accessing, testing, converting, comparing, enumerating, copying
- Protocols are usually **informal** - no explicit declaration
- Implement protocol methods to make your objects integrate with the system
- **`self subclassResponsibility`** marks methods subclasses must implement
- Polymorphism enables flexibility, testability, and reusability
- The Null Object pattern uses polymorphism to eliminate nil checks
- Small, focused protocols are better than large, monolithic ones
- Consistent naming conventions make protocols discoverable
- Think "what messages should this object understand?" not "what class is it?"

---

[Previous: Chapter 22 - The Finder](chapter-22-finder.md) | [Next: Chapter 24 - Error Handling - When Things Go Wrong](chapter-24-error-handling.md)
