# Chapter 36: Design Patterns in Smalltalk

Welcome to Part X: Next Steps! You've mastered Smalltalk fundamentals and built real applications. Now let's explore **design patterns** - proven solutions to common programming problems.

Design patterns were actually **discovered** in Smalltalk! The "Gang of Four" book (Design Patterns: Elements of Reusable Object-Oriented Software, 1994) documented patterns that emerged from Smalltalk practice. Many patterns that require complex implementations in other languages are trivial or invisible in Smalltalk!

This chapter shows you classic design patterns and how Smalltalk's elegant features make them simple and natural.

## What Are Design Patterns?

**Design patterns** are reusable solutions to recurring design problems. They're not code you copy-paste, but rather approaches you adapt to your specific situation.

### Why Patterns Matter

1. **Common vocabulary** - "Use a Strategy pattern here"
2. **Proven solutions** - Tested by thousands of developers
3. **Better design** - More flexible, maintainable code
4. **Learning tool** - Understand expert thinking

### Pattern Categories

- **Creational** - Object creation (Factory, Singleton, Builder)
- **Structural** - Object composition (Composite, Decorator, Adapter)
- **Behavioral** - Object interaction (Strategy, Observer, Command)

## Patterns Native to Smalltalk

Some patterns are so natural in Smalltalk they're almost invisible!

### Strategy Pattern

**Problem**: Choose algorithm at runtime.

**Other languages**: Create interface, multiple implementations, dependency injection.

**Smalltalk**: Just use blocks!

```smalltalk
Object subclass: #Sorter
    instanceVariableNames: 'strategy'
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
initialize
    super initialize.
    strategy := [ :a :b | a <= b ]  "Default strategy"

strategy: aBlock
    "Set the sorting strategy"
    strategy := aBlock

sort: aCollection
    "Sort using the current strategy"
    ^ aCollection sorted: strategy
```

Usage:

```smalltalk
| sorter |
sorter := Sorter new.

"Sort ascending"
sorter strategy: [ :a :b | a <= b ].
sorter sort: #(3 1 4 1 5 9).  "-> #(1 1 3 4 5 9)"

"Sort descending"
sorter strategy: [ :a :b | a >= b ].
sorter sort: #(3 1 4 1 5 9).  "-> #(9 5 4 3 1 1)"

"Sort by string length"
sorter strategy: [ :a :b | a size <= b size ].
sorter sort: #('hello' 'hi' 'goodbye').  "-> #('hi' 'hello' 'goodbye')"
```

**In Smalltalk**: Blocks *are* the Strategy pattern!

### Command Pattern

**Problem**: Encapsulate requests as objects.

**Other languages**: Command interface, concrete command classes.

**Smalltalk**: Blocks again!

```smalltalk
Object subclass: #TextEditor
    instanceVariableNames: 'text undoStack'
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
initialize
    super initialize.
    text := ''.
    undoStack := OrderedCollection new

execute: command
    "Execute a command and save undo"
    | undo |
    undo := command value: self.
    undoStack add: undo

undo
    "Undo last command"
    undoStack ifEmpty: [ ^ self ].
    undoStack removeLast value: self

text
    ^ text

text: aString
    text := aString
```

Commands as blocks:

```smalltalk
| editor |
editor := TextEditor new.

"Insert command"
editor execute: [ :ed |
    | oldText |
    oldText := ed text.
    ed text: oldText, 'Hello'.
    "Return undo block"
    [ :e | e text: oldText ] ].

"Insert more"
editor execute: [ :ed |
    | oldText |
    oldText := ed text.
    ed text: oldText, ' World'.
    [ :e | e text: oldText ] ].

editor text.    "-> 'Hello World'"
editor undo.    "Undo 'World'"
editor text.    "-> 'Hello'"
editor undo.    "Undo 'Hello'"
editor text.    "-> ''"
```

**In Smalltalk**: Blocks with closures = Command pattern!

### Template Method Pattern

**Problem**: Define algorithm skeleton, let subclasses customize steps.

**Other languages**: Abstract method declarations.

**Smalltalk**: Natural with inheritance!

```smalltalk
Object subclass: #DataImporter
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
import: filePath
    "Template method - defines the algorithm"
    | data parsed validated |

    data := self readFile: filePath.
    parsed := self parse: data.
    validated := self validate: parsed.
    ^ self save: validated

readFile: filePath
    "Subclasses can override"
    ^ filePath asFileReference contents

parse: data
    "Abstract - subclasses must implement"
    self subclassResponsibility

validate: parsedData
    "Hook - subclasses can optionally override"
    ^ parsedData

save: validatedData
    "Abstract - subclasses must implement"
    self subclassResponsibility
```

Concrete implementation:

```smalltalk
DataImporter subclass: #CSVImporter
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
parse: data
    "Parse CSV format"
    ^ data lines collect: [ :line | line splitOn: $, ]

save: validatedData
    "Save to collection"
    Transcript show: 'Saving ', validatedData size asString, ' rows'; cr.
    ^ validatedData
```

**In Smalltalk**: Template Method is just normal inheritance!

### Iterator Pattern

**Problem**: Access collection elements sequentially without exposing representation.

**Other languages**: Iterator interface, concrete iterators.

**Smalltalk**: Built into collections!

```smalltalk
| collection |
collection := #(1 2 3 4 5).

"External iteration"
collection do: [ :each | Transcript show: each; cr ].

"Internal iteration with blocks"
collection select: [ :each | each even ].
collection collect: [ :each | each * 2 ].
collection detect: [ :each | each > 3 ].
```

**In Smalltalk**: Collections *are* iterators via blocks!

## Classic Design Patterns

Now let's implement classic patterns explicitly:

### Singleton Pattern

Ensure only one instance exists:

```smalltalk
Object subclass: #DatabaseConnection
    instanceVariableNames: 'connectionString'
    classVariableNames: 'UniqueInstance'
    package: 'Patterns'
```

```smalltalk
"Class side:"
uniqueInstance
    "Return the single instance"
    UniqueInstance ifNil: [ UniqueInstance := self new ].
    ^ UniqueInstance

new
    "Prevent direct instantiation"
    self error: 'Use uniqueInstance instead'

private_new
    "Private constructor"
    ^ super new

resetUniqueInstance
    "For testing"
    UniqueInstance := nil
```

```smalltalk
"Instance side:"
initialize
    super initialize.
    connectionString := 'localhost:5432'

connect
    Transcript show: 'Connected to ', connectionString; cr

query: sql
    Transcript show: 'Executing: ', sql; cr
```

Usage:

```smalltalk
| db1 db2 |
db1 := DatabaseConnection uniqueInstance.
db2 := DatabaseConnection uniqueInstance.

db1 == db2.  "-> true (same object)"
```

**Note**: Singletons are controversial! Consider dependency injection instead.

### Factory Method Pattern

Let subclasses decide which class to instantiate:

```smalltalk
Object subclass: #DocumentFactory
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
createDocument
    "Factory method - subclasses override"
    self subclassResponsibility

openDocument: filePath
    | document |
    document := self createDocument.
    document load: filePath.
    ^ document
```

Concrete factories:

```smalltalk
DocumentFactory subclass: #TextDocumentFactory
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
createDocument
    ^ TextDocument new
```

```smalltalk
DocumentFactory subclass: #PDFDocumentFactory
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
createDocument
    ^ PDFDocument new
```

Usage:

```smalltalk
| factory document |

factory := TextDocumentFactory new.
document := factory openDocument: 'report.txt'.

factory := PDFDocumentFactory new.
document := factory openDocument: 'report.pdf'.
```

### Builder Pattern

Construct complex objects step by step:

```smalltalk
Object subclass: #EmailBuilder
    instanceVariableNames: 'email'
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
initialize
    super initialize.
    email := Email new

from: address
    email from: address.
    ^ self

to: address
    email to: address.
    ^ self

subject: text
    email subject: text.
    ^ self

body: text
    email body: text.
    ^ self

addAttachment: file
    email addAttachment: file.
    ^ self

build
    ^ email
```

Usage (fluent interface):

```smalltalk
| email |
email := EmailBuilder new
    from: 'alice@example.com';
    to: 'bob@example.com';
    subject: 'Design Patterns in Smalltalk';
    body: 'Check out this great book!';
    addAttachment: 'book.pdf';
    build
```

**In Smalltalk**: Method cascades make this natural!

```smalltalk
| email |
email := Email new.
email
    from: 'alice@example.com';
    to: 'bob@example.com';
    subject: 'Design Patterns';
    body: 'Content here'
```

### Composite Pattern

Treat individual objects and compositions uniformly:

```smalltalk
Object subclass: #GraphicElement
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
draw
    "Abstract method"
    self subclassResponsibility

bounds
    self subclassResponsibility
```

Leaf element:

```smalltalk
GraphicElement subclass: #Circle
    instanceVariableNames: 'center radius'
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
center: aPoint radius: aNumber
    center := aPoint.
    radius := aNumber

draw
    Transcript show: 'Drawing circle at ', center asString; cr

bounds
    ^ Rectangle center: center extent: radius * 2 @ (radius * 2)
```

Composite element:

```smalltalk
GraphicElement subclass: #Group
    instanceVariableNames: 'children'
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
initialize
    super initialize.
    children := OrderedCollection new

add: aGraphicElement
    children add: aGraphicElement

remove: aGraphicElement
    children remove: aGraphicElement

draw
    "Draw all children"
    children do: [ :each | each draw ]

bounds
    "Union of all children bounds"
    | union |
    children ifEmpty: [ ^ 0@0 corner: 0@0 ].
    union := children first bounds.
    children allButFirst do: [ :each |
        union := union merge: each bounds ].
    ^ union
```

Usage:

```smalltalk
| circle1 circle2 group |

circle1 := Circle new center: 10@10 radius: 5.
circle2 := Circle new center: 30@30 radius: 8.

group := Group new.
group add: circle1.
group add: circle2.

"Treat group like a single element"
group draw.
group bounds.
```

### Decorator Pattern

Add responsibilities to objects dynamically:

```smalltalk
Object subclass: #Coffee
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
cost
    ^ 2.0

description
    ^ 'Coffee'
```

Decorators:

```smalltalk
Coffee subclass: #CoffeeDecorator
    instanceVariableNames: 'coffee'
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
coffee: aCoffee
    coffee := aCoffee

cost
    ^ coffee cost

description
    ^ coffee description
```

Concrete decorators:

```smalltalk
CoffeeDecorator subclass: #Milk
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
cost
    ^ super cost + 0.5

description
    ^ super description, ' + Milk'
```

```smalltalk
CoffeeDecorator subclass: #Sugar
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
cost
    ^ super cost + 0.2

description
    ^ super description, ' + Sugar'
```

Usage:

```smalltalk
| coffee |

"Plain coffee"
coffee := Coffee new.
coffee cost.         "-> 2.0"
coffee description.  "-> 'Coffee'"

"With milk"
coffee := Milk new coffee: Coffee new.
coffee cost.         "-> 2.5"
coffee description.  "-> 'Coffee + Milk'"

"With milk and sugar"
coffee := Sugar new coffee: (Milk new coffee: Coffee new).
coffee cost.         "-> 2.7"
coffee description.  "-> 'Coffee + Milk + Sugar'"
```

### Adapter Pattern

Convert interface of one class into another:

```smalltalk
Object subclass: #LegacyPrinter
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
printDocument: doc atQuality: quality
    Transcript show: 'Printing at quality ', quality asString; cr
```

Modern interface:

```smalltalk
Object subclass: #ModernPrinter
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
print: document
    self subclassResponsibility
```

Adapter:

```smalltalk
ModernPrinter subclass: #LegacyPrinterAdapter
    instanceVariableNames: 'legacyPrinter'
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
legacyPrinter: aPrinter
    legacyPrinter := aPrinter

print: document
    "Adapt modern interface to legacy interface"
    legacyPrinter printDocument: document atQuality: 100
```

Usage:

```smalltalk
| modern legacy adapter |

legacy := LegacyPrinter new.
adapter := LegacyPrinterAdapter new legacyPrinter: legacy.

"Use legacy printer through modern interface"
adapter print: 'My Document'
```

### Observer Pattern

Objects notify dependents of state changes:

```smalltalk
Object subclass: #Subject
    instanceVariableNames: 'observers'
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
initialize
    super initialize.
    observers := OrderedCollection new

addObserver: anObserver
    observers add: anObserver

removeObserver: anObserver
    observers remove: anObserver ifAbsent: [ ]

notifyObservers
    observers do: [ :each | each update: self ]
```

Concrete subject:

```smalltalk
Subject subclass: #WeatherStation
    instanceVariableNames: 'temperature humidity'
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
temperature: value
    temperature := value.
    self notifyObservers

humidity: value
    humidity := value.
    self notifyObservers

temperature
    ^ temperature

humidity
    ^ humidity
```

Observer:

```smalltalk
Object subclass: #WeatherDisplay
    instanceVariableNames: 'name'
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
name: aString
    name := aString

update: subject
    Transcript
        show: name, ' - Temp: ', subject temperature asString,
              ', Humidity: ', subject humidity asString; cr
```

Usage:

```smalltalk
| station display1 display2 |

station := WeatherStation new.

display1 := WeatherDisplay new name: 'Display 1'.
display2 := WeatherDisplay new name: 'Display 2'.

station addObserver: display1.
station addObserver: display2.

station temperature: 25.
"Both displays update automatically"

station humidity: 60.
"Both displays update again"
```

**In Smalltalk**: The `announcements` framework provides this built-in!

### Chain of Responsibility

Pass requests along a chain of handlers:

```smalltalk
Object subclass: #Handler
    instanceVariableNames: 'successor'
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
successor: aHandler
    successor := aHandler

handleRequest: request
    "Try to handle, otherwise pass to successor"
    (self canHandle: request)
        ifTrue: [ self handle: request ]
        ifFalse: [
            successor ifNotNil: [ successor handleRequest: request ] ]

canHandle: request
    self subclassResponsibility

handle: request
    self subclassResponsibility
```

Concrete handlers:

```smalltalk
Handler subclass: #ErrorHandler
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
canHandle: request
    ^ request level = #error

handle: request
    Transcript show: 'ERROR: ', request message; cr
```

```smalltalk
Handler subclass: #WarningHandler
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
canHandle: request
    ^ request level = #warning

handle: request
    Transcript show: 'WARNING: ', request message; cr
```

```smalltalk
Handler subclass: #InfoHandler
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
canHandle: request
    ^ request level = #info

handle: request
    Transcript show: 'INFO: ', request message; cr
```

Usage:

```smalltalk
| error warning info request |

"Build chain"
info := InfoHandler new.
warning := WarningHandler new successor: info.
error := ErrorHandler new successor: warning.

"Send requests"
request := LogRequest new level: #error; message: 'System failure'.
error handleRequest: request.

request := LogRequest new level: #info; message: 'User logged in'.
error handleRequest: request.
```

### Visitor Pattern

Separate algorithm from object structure:

```smalltalk
Object subclass: #ShapeVisitor
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
visitCircle: aCircle
    self subclassResponsibility

visitSquare: aSquare
    self subclassResponsibility
```

Elements:

```smalltalk
Object subclass: #Shape
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
accept: aVisitor
    self subclassResponsibility
```

```smalltalk
Shape subclass: #Circle
    instanceVariableNames: 'radius'
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
radius: aNumber
    radius := aNumber

accept: aVisitor
    ^ aVisitor visitCircle: self

radius
    ^ radius
```

Concrete visitor:

```smalltalk
ShapeVisitor subclass: #AreaCalculator
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
visitCircle: aCircle
    ^ Float pi * aCircle radius squared

visitSquare: aSquare
    ^ aSquare side squared
```

Usage:

```smalltalk
| shapes calculator |

shapes := OrderedCollection new
    add: (Circle new radius: 5);
    add: (Square new side: 4);
    yourself.

calculator := AreaCalculator new.

shapes do: [ :shape |
    | area |
    area := shape accept: calculator.
    Transcript show: 'Area: ', area asString; cr ]
```

## Smalltalk-Specific Patterns

Patterns unique to Smalltalk:

### Double Dispatch

Handle type combinations without conditionals:

```smalltalk
Object subclass: #Asteroid
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
collideWith: anObject
    "Double dispatch - let anObject decide"
    ^ anObject collideWithAsteroid: self

collideWithSpaceship: aSpaceship
    Transcript show: 'Asteroid hits spaceship!'; cr

collideWithAsteroid: anAsteroid
    Transcript show: 'Two asteroids collide!'; cr
```

```smalltalk
Object subclass: #Spaceship
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
collideWith: anObject
    ^ anObject collideWithSpaceship: self

collideWithSpaceship: aSpaceship
    Transcript show: 'Two spaceships collide!'; cr

collideWithAsteroid: anAsteroid
    Transcript show: 'Spaceship hits asteroid!'; cr
```

Usage:

```smalltalk
| asteroid spaceship |
asteroid := Asteroid new.
spaceship := Spaceship new.

asteroid collideWith: spaceship.   "-> Asteroid hits spaceship!"
spaceship collideWith: asteroid.   "-> Spaceship hits asteroid!"
asteroid collideWith: asteroid.    "-> Two asteroids collide!"
```

No conditionals, no type checking - pure polymorphism!

### Null Object Pattern

Replace nil checks with polymorphic object:

```smalltalk
Object subclass: #Customer
    instanceVariableNames: 'name discount'
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
name: aString
    name := aString

discount: aNumber
    discount := aNumber

name
    ^ name

discount
    ^ discount

isNull
    ^ false
```

Null object:

```smalltalk
Customer subclass: #NullCustomer
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
name
    ^ 'Guest'

discount
    ^ 0

isNull
    ^ true
```

Usage:

```smalltalk
| customer |

"Instead of:"
customer ifNotNil: [ customer discount ] ifNil: [ 0 ].

"Use null object:"
customer := customer ifNil: [ NullCustomer new ].
customer discount.  "Always works, no nil check needed"
```

### Method Object Pattern

Turn complex method into object:

Before (complex method):

```smalltalk
calculatePrice: items withDiscount: discountRate forCustomer: customer
    | subtotal tax shipping discount total |
    subtotal := items sum: #price.
    discount := subtotal * discountRate.
    tax := (subtotal - discount) * 0.08.
    shipping := customer isPremium ifTrue: [ 0 ] ifFalse: [ 5.99 ].
    total := subtotal - discount + tax + shipping.
    ^ total
```

After (method object):

```smalltalk
Object subclass: #PriceCalculator
    instanceVariableNames: 'items discountRate customer subtotal tax shipping discount'
    classVariableNames: ''
    package: 'Patterns'
```

```smalltalk
items: i discount: d customer: c
    items := i.
    discountRate := d.
    customer := c

calculate
    self calculateSubtotal.
    self calculateDiscount.
    self calculateTax.
    self calculateShipping.
    ^ self calculateTotal

calculateSubtotal
    subtotal := items sum: #price

calculateDiscount
    discount := subtotal * discountRate

calculateTax
    tax := (subtotal - discount) * 0.08

calculateShipping
    shipping := customer isPremium ifTrue: [ 0 ] ifFalse: [ 5.99 ]

calculateTotal
    ^ subtotal - discount + tax + shipping
```

Much more testable and maintainable!

## Anti-Patterns

Patterns to **avoid**:

### God Object

One class that does everything - violates single responsibility.

**Don't:**
```smalltalk
Application  "Handles UI, database, network, business logic, everything!"
```

**Do:**
```smalltalk
UIController
DatabaseRepository
NetworkService
BusinessLogic
```

### Primitive Obsession

Using primitives instead of objects:

**Don't:**
```smalltalk
email: 'alice@example.com'  "Just a string"
```

**Do:**
```smalltalk
email: (EmailAddress fromString: 'alice@example.com')
"Now has validation, formatting, etc."
```

### Anemic Domain Model

Objects with no behavior, just data:

**Don't:**
```smalltalk
account balance.
account balance: (account balance - amount).  "Logic outside object"
```

**Do:**
```smalltalk
account withdraw: amount  "Object handles its own logic"
```

## When to Use Patterns

**Use patterns when:**
- You recognize the problem they solve
- The pattern simplifies your design
- Your team understands the pattern

**Don't use patterns when:**
- You're forcing a pattern where it doesn't fit
- A simpler solution exists
- You're "pattern-happy" (over-engineering)

Remember: **Patterns are tools, not goals!**

## Try This!

Practice design patterns:

1. **Implement State Pattern**
   ```smalltalk
   "TCP connection with states: Closed, Listening, Connected"
   ```

2. **Build a Logger with Chain of Responsibility**
   ```smalltalk
   "Console logger → File logger → Remote logger"
   ```

3. **Create Flyweight Pattern**
   ```smalltalk
   "Share character glyphs to save memory"
   ```

4. **Memento Pattern for Undo**
   ```smalltalk
   "Save and restore object state"
   ```

5. **Proxy Pattern for Lazy Loading**
   ```smalltalk
   "Load expensive resources only when needed"
   ```

## What You Learned

Exploring design patterns, you've mastered:

1. **Pattern Fundamentals**
   - What patterns are and why they matter
   - Pattern categories
   - When to apply patterns

2. **Smalltalk's Natural Patterns**
   - Blocks implement Strategy and Command
   - Collections implement Iterator
   - Inheritance enables Template Method

3. **Classic Patterns**
   - Singleton, Factory, Builder
   - Composite, Decorator, Adapter
   - Observer, Chain of Responsibility
   - Visitor pattern

4. **Smalltalk-Specific Patterns**
   - Double dispatch
   - Null object
   - Method object

5. **Anti-Patterns**
   - What to avoid
   - Better alternatives

## Patterns in Smalltalk

Smalltalk makes patterns elegant because:
- **Blocks** replace many behavioral patterns
- **Dynamic typing** enables flexible composition
- **Reflectivity** supports meta-patterns
- **Simple syntax** reduces boilerplate
- **Live coding** lets you refactor patterns easily

Many GoF patterns are **invisible** in Smalltalk - they're just how you naturally program!

## Looking Ahead

You now understand design patterns in Smalltalk! You know:
- Classic design patterns and their implementation
- How Smalltalk's features simplify patterns
- Smalltalk-specific patterns
- When to use (and not use) patterns

In Chapter 37, we'll explore **Performance and Optimization** - making your Smalltalk code fast!

Then Chapter 38 covers **The Smalltalk Community** - connecting with fellow Smalltalkers!

Part X is guiding you toward Smalltalk mastery!

---

**Key Takeaways:**
- **Design patterns** are proven solutions to common problems
- Many patterns **originated** in Smalltalk
- **Blocks** implement Strategy and Command patterns naturally
- **Collections** embody Iterator pattern
- **Inheritance** enables Template Method
- Classic patterns: Singleton, Factory, Builder, Composite, Decorator
- Behavioral patterns: Observer, Chain of Responsibility, Visitor
- **Double dispatch** handles type combinations elegantly
- **Null Object** eliminates nil checks
- **Method Object** refactors complex methods
- Avoid **anti-patterns**: God Object, Primitive Obsession, Anemic Domain Model
- Patterns are **tools, not goals** - don't over-engineer
- Smalltalk's features make many patterns **trivial or invisible**
- Use patterns when they **simplify** your design
- Learn patterns to **communicate** with other developers

---

[Previous: Chapter 35 - Graphics and UI Basics](chapter-35-graphics-and-ui.md) | [Next: Chapter 37 - Performance and Optimization](chapter-37-performance.md)
