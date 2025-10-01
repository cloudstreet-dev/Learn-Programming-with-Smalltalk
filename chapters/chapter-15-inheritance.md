# Chapter 15: Inheritance - Standing on Shoulders

You've learned about classes, methods, instance variables, `self`, and `super`. Now it's time to explore one of the most powerful features of object-oriented programming: **inheritance**.

Inheritance is how classes build upon other classes, sharing and extending behavior. It's how we avoid duplicating code and create hierarchies of related concepts. It's the mechanism that lets you say "a Car is a kind of Vehicle" or "a SavingsAccount is a kind of BankAccount."

In this chapter, we'll explore inheritance in depth: what it is, how it works, when to use it, and how to design good class hierarchies.

## What is Inheritance?

**Inheritance** is a mechanism where one class (the **subclass** or **child class**) inherits behavior and state from another class (the **superclass** or **parent class**).

Think of it like family traits:
- You inherit characteristics from your parents
- But you're also your own unique person with your own traits
- You can do everything your parents can do (and more)
- You might do some things differently than they do

In Smalltalk:
- A subclass inherits all instance variables from its superclass
- A subclass inherits all methods from its superclass
- A subclass can add new instance variables
- A subclass can add new methods
- A subclass can override (replace) inherited methods

## Why Inheritance?

Inheritance solves several problems:

### 1. Code Reuse

Without inheritance:

```smalltalk
Object subclass: #Car
    instanceVariableNames: 'make model year color doors'
    ...

Object subclass: #Truck
    instanceVariableNames: 'make model year color bedSize'
    ...

Object subclass: #Motorcycle
    instanceVariableNames: 'make model year color hasSidecar'
    ...
```

Notice the duplication: `make`, `model`, `year`, `color` appear in all three!

With inheritance:

```smalltalk
Object subclass: #Vehicle
    instanceVariableNames: 'make model year color'
    ...

Vehicle subclass: #Car
    instanceVariableNames: 'doors'
    ...

Vehicle subclass: #Truck
    instanceVariableNames: 'bedSize'
    ...

Vehicle subclass: #Motorcycle
    instanceVariableNames: 'hasSidecar'
    ...
```

Now the common variables are defined once in Vehicle, and each specific vehicle type adds only what makes it unique.

### 2. Conceptual Modeling

Inheritance lets you model real-world relationships:

```
Animal
  ├─ Mammal
  │  ├─ Dog
  │  ├─ Cat
  │  └─ Human
  ├─ Bird
  │  ├─ Penguin
  │  └─ Eagle
  └─ Fish
     ├─ Salmon
     └─ Shark
```

This hierarchy captures the relationships: "A Dog is a Mammal is an Animal."

### 3. Polymorphism

You can treat objects uniformly through their common superclass:

```smalltalk
| animals |
animals := Array with: (Dog new) with: (Cat new) with: (Bird new).

animals do: [ :animal |
    animal speak.  "Each responds in its own way!"
    animal eat ]
```

All animals can speak and eat, but each does it differently.

## A Simple Inheritance Example

Let's build a simple hierarchy:

```smalltalk
Object subclass: #Vehicle
    instanceVariableNames: 'make model year'
    classVariableNames: ''
    package: 'MyFirstClasses'
```

Add methods to Vehicle:

```smalltalk
initialize
    "Initialize a new vehicle"
    super initialize.
    make := ''.
    model := ''.
    year := 0
```

```smalltalk
make
    ^ make
```

```smalltalk
make: aString
    make := aString
```

```smalltalk
model
    ^ model
```

```smalltalk
model: aString
    model := aString
```

```smalltalk
year
    ^ year
```

```smalltalk
year: aNumber
    year := aNumber
```

```smalltalk
description
    ^ year printString , ' ' , make , ' ' , model
```

Now create a Car subclass:

```smalltalk
Vehicle subclass: #Car
    instanceVariableNames: 'numberOfDoors'
    classVariableNames: ''
    package: 'MyFirstClasses'
```

Add methods to Car:

```smalltalk
initialize
    "Initialize a new car"
    super initialize.
    numberOfDoors := 4
```

```smalltalk
numberOfDoors
    ^ numberOfDoors
```

```smalltalk
numberOfDoors: aNumber
    numberOfDoors := aNumber
```

```smalltalk
description
    ^ super description , ', ' , numberOfDoors printString , ' doors'
```

Now try it:

```smalltalk
| car |
car := Car new.
car make: 'Toyota'.
car model: 'Camry'.
car year: 2023.
car numberOfDoors: 4.
car description  "Returns '2023 Toyota Camry, 4 doors'"
```

Notice:
- Car inherits `make`, `model`, `year` from Vehicle
- Car inherits `make:`, `model:`, `year:` methods from Vehicle
- Car adds its own `numberOfDoors` instance variable
- Car's `initialize` calls `super initialize` to set up inherited variables
- Car's `description` calls `super description` and extends it

## What Gets Inherited?

When you create a subclass, it inherits:

### Instance Variables

All instance variables from the superclass:

```smalltalk
Object subclass: #Animal
    instanceVariableNames: 'name age'
    ...

Animal subclass: #Dog
    instanceVariableNames: 'breed'
    ...
```

A Dog has:
- `name` (from Animal)
- `age` (from Animal)
- `breed` (its own)

### Methods

All methods from the superclass:

```smalltalk
"Animal methods:"
name
    ^ name

name: aString
    name := aString

speak
    ^ 'Some generic animal sound'
```

```smalltalk
"Dog methods:"
breed
    ^ breed

breed: aString
    breed := aString
```

A Dog understands:
- `name` and `name:` (inherited from Animal)
- `speak` (inherited from Animal)
- `breed` and `breed:` (its own)

## Method Overriding

A subclass can **override** (replace) methods from its superclass:

```smalltalk
Object subclass: #Animal
    instanceVariableNames: 'name'
    ...

speak
    ^ 'Some generic animal sound'
```

```smalltalk
Animal subclass: #Dog
    instanceVariableNames: ''
    ...

speak
    ^ 'Woof!'
```

```smalltalk
Animal subclass: #Cat
    instanceVariableNames: ''
    ...

speak
    ^ 'Meow!'
```

Now:

```smalltalk
Animal new speak  "Returns 'Some generic animal sound'"
Dog new speak     "Returns 'Woof!'"
Cat new speak     "Returns 'Meow!'"
```

Each subclass overrides `speak` with its own implementation.

## The Complete Method Lookup

When you send a message to an object, Smalltalk searches for the method:

```smalltalk
dog speak
```

1. Look in Dog for `speak`. Found it! Execute Dog>>speak.

```smalltalk
dog eat
```

1. Look in Dog for `eat`. Not found.
2. Look in Animal (Dog's superclass) for `eat`. Found it! Execute Animal>>eat.

```smalltalk
dog printString
```

1. Look in Dog for `printString`. Not found.
2. Look in Animal for `printString`. Not found.
3. Look in Object for `printString`. Found it! Execute Object>>printString.

The search continues up the hierarchy until the method is found or the top is reached.

## Extending vs Replacing

When overriding a method, you have two options:

### Completely Replace

```smalltalk
Object subclass: #Shape
    ...

area
    ^ 0  "Default: no area"
```

```smalltalk
Shape subclass: #Circle
    instanceVariableNames: 'radius'
    ...

area
    ^ Float pi * radius squared  "Completely replaces Shape's area"
```

### Extend with `super`

```smalltalk
Object subclass: #Vehicle
    ...

description
    ^ make , ' ' , model
```

```smalltalk
Vehicle subclass: #Car
    ...

description
    ^ super description , ' with ' , numberOfDoors printString , ' doors'
    "Extends Vehicle's description"
```

Using `super` lets you add to or wrap the superclass's behavior instead of completely replacing it.

## Multiple Levels of Inheritance

Inheritance can go multiple levels deep:

```smalltalk
Object
  └─ Vehicle
      └─ MotorVehicle
          ├─ Car
          │   ├─ Sedan
          │   └─ SUV
          └─ Truck
```

Each level inherits from the level above:

```smalltalk
Object subclass: #Vehicle
    instanceVariableNames: 'make model year'
    ...

Vehicle subclass: #MotorVehicle
    instanceVariableNames: 'engineSize fuelType'
    ...

MotorVehicle subclass: #Car
    instanceVariableNames: 'numberOfDoors'
    ...

Car subclass: #Sedan
    instanceVariableNames: 'trunkSize'
    ...
```

A Sedan has ALL these instance variables:
- `make`, `model`, `year` (from Vehicle)
- `engineSize`, `fuelType` (from MotorVehicle)
- `numberOfDoors` (from Car)
- `trunkSize` (its own)

And it inherits ALL methods from all levels!

## Inheritance for Specialization

Use inheritance to create specialized versions of general concepts:

```smalltalk
Object subclass: #BankAccount
    instanceVariableNames: 'balance accountNumber'
    ...

deposit: amount
    balance := balance + amount

withdraw: amount
    balance := balance - amount
```

```smalltalk
BankAccount subclass: #SavingsAccount
    instanceVariableNames: 'interestRate'
    ...

addInterest
    | interest |
    interest := balance * interestRate / 100.
    self deposit: interest
```

```smalltalk
BankAccount subclass: #CheckingAccount
    instanceVariableNames: 'overdraftLimit'
    ...

withdraw: amount
    "Override to allow overdraft"
    balance + overdraftLimit >= amount
        ifTrue: [ balance := balance - amount ]
        ifFalse: [ self error: 'Exceeds overdraft limit' ]
```

SavingsAccount adds `interestRate` and `addInterest`. CheckingAccount overrides `withdraw:` to allow overdrafts.

## Abstract Classes and Template Methods

Sometimes a superclass defines a structure but expects subclasses to fill in the details:

```smalltalk
Object subclass: #Game
    instanceVariableNames: 'players currentPlayer'
    ...

play
    "Template method - defines the game structure"
    self setup.
    [ self isOver ] whileFalse: [
        self takeTurn.
        self nextPlayer ].
    self announceWinner
```

```smalltalk
setup
    "Subclasses must implement this"
    self subclassResponsibility

takeTurn
    "Subclasses must implement this"
    self subclassResponsibility

isOver
    "Subclasses must implement this"
    self subclassResponsibility

nextPlayer
    currentPlayer := currentPlayer + 1.
    currentPlayer > players size ifTrue: [ currentPlayer := 1 ]

announceWinner
    Transcript show: 'Game over!'; cr
```

Now subclasses implement the specific details:

```smalltalk
Game subclass: #TicTacToe
    instanceVariableNames: 'board'
    ...

setup
    board := Array new: 9 withAll: nil

takeTurn
    "Get player input and make move"
    ...

isOver
    ^ self hasWinner or: [ self boardFull ]
```

The superclass (Game) provides the overall structure (`play`), and subclasses (TicTacToe) provide the specific behavior. This is called the **Template Method Pattern**.

## When to Use Inheritance

Inheritance is powerful, but not always the right choice. Use it when:

### 1. "Is-a" Relationship

The subclass truly IS a kind of the superclass:

**Good:**
- A Dog **is a** Mammal
- A Car **is a** Vehicle
- A SavingsAccount **is a** BankAccount

**Bad:**
- A Car **is NOT a** Engine (a Car has an Engine - use composition instead!)
- A Person **is NOT a** Address (a Person has an Address)

### 2. Sharing Behavior

Multiple classes share common behavior:

```smalltalk
Object subclass: #Shape
    ...

area
    self subclassResponsibility

perimeter
    self subclassResponsibility
```

All shapes have area and perimeter, but each calculates them differently.

### 3. Specialization

Creating more specific versions of general concepts:

```smalltalk
Employee
  ├─ FullTimeEmployee
  ├─ PartTimeEmployee
  └─ Contractor
```

Each type of employee shares common behavior (name, ID) but has specialized behavior (pay calculation).

## When NOT to Use Inheritance

Avoid inheritance when:

### 1. "Has-a" Relationship

Use **composition** instead:

**Bad:**
```smalltalk
Engine subclass: #Car
    "Wrong! A Car is not a kind of Engine!"
```

**Good:**
```smalltalk
Object subclass: #Car
    instanceVariableNames: 'engine'
    "Right! A Car has an Engine!"
```

### 2. Too Deep Hierarchies

Avoid deep inheritance trees (more than 4-5 levels). They become hard to understand and maintain:

**Bad:**
```
Object → Vehicle → MotorVehicle → PassengerVehicle → Car → FamilyCar → MiniVan → LuxuryMiniVan
```

**Better:** Use composition to reduce depth.

### 3. Forcing Unrelated Classes Together

Don't create artificial superclasses just to share a method:

**Bad:**
```smalltalk
Object subclass: #HasName
    instanceVariableNames: 'name'
    ...

HasName subclass: #Person
HasName subclass: #Country
HasName subclass: #Dog
```

**Better:** Use traits (covered later) or just duplicate the simple `name` accessor.

## Composition vs Inheritance

Sometimes composition (one object containing another) is better than inheritance:

### Inheritance Example:
```smalltalk
Vehicle subclass: #FlyingVehicle
    instanceVariableNames: 'altitude'
    ...

fly
    altitude := altitude + 100
```

But what about a flying car? It's both a Car and a FlyingVehicle. Smalltalk doesn't support multiple inheritance!

### Composition Example:
```smalltalk
Object subclass: #Car
    instanceVariableNames: 'engine flightSystem'
    ...

fly
    flightSystem fly

drive
    engine start
```

The Car contains both an Engine and a FlightSystem. It can do both!

**Rule of Thumb:** Prefer composition for "has-a" relationships, inheritance for "is-a" relationships.

## Designing Good Hierarchies

Follow these principles:

### 1. Liskov Substitution Principle

A subclass should be usable anywhere the superclass is expected:

```smalltalk
| vehicle |
vehicle := Car new.  "Car is a Vehicle"
vehicle description  "This should work!"
```

If you can't substitute a subclass for its superclass, your hierarchy is wrong.

### 2. Single Responsibility

Each class should have one clear responsibility:

**Bad:**
```smalltalk
Object subclass: #EmployeeAndPayroll
    instanceVariableNames: 'name salary taxRate benefits ...'
    "Too many responsibilities!"
```

**Good:**
```smalltalk
Object subclass: #Employee
    instanceVariableNames: 'name payroll'

Object subclass: #Payroll
    instanceVariableNames: 'salary taxRate benefits'
```

### 3. Keep Superclasses General

Superclasses should be general; subclasses should be specific:

```
Shape (general: area, perimeter)
  ├─ Circle (specific: radius, pi)
  ├─ Rectangle (specific: width, height)
  └─ Triangle (specific: three sides)
```

Don't put Circle-specific behavior in Shape!

### 4. Don't Repeat Yourself (DRY)

If multiple classes have the same code, move it to a superclass:

**Bad:**
```smalltalk
"In Circle:"
describe
    ^ 'A shape with area ' , self area printString

"In Rectangle:"
describe
    ^ 'A shape with area ' , self area printString

"In Triangle:"
describe
    ^ 'A shape with area ' , self area printString
```

**Good:**
```smalltalk
"In Shape (superclass):"
describe
    ^ 'A shape with area ' , self area printString

"All subclasses inherit it!"
```

## Practical Example: A Rich Hierarchy

Let's build a more complete example:

```smalltalk
Object subclass: #Employee
    instanceVariableNames: 'name employeeId hireDate'
    classVariableNames: ''
    package: 'MyFirstClasses'
```

```smalltalk
initialize
    super initialize.
    name := ''.
    employeeId := 0.
    hireDate := Date today
```

```smalltalk
name
    ^ name
```

```smalltalk
name: aString
    name := aString
```

```smalltalk
employeeId
    ^ employeeId
```

```smalltalk
employeeId: aNumber
    employeeId := aNumber
```

```smalltalk
yearsOfService
    ^ (Date today - hireDate) days / 365.25
```

```smalltalk
calculatePay
    "Subclasses must implement this"
    self subclassResponsibility
```

Now create subclasses:

```smalltalk
Employee subclass: #SalariedEmployee
    instanceVariableNames: 'annualSalary'
    classVariableNames: ''
    package: 'MyFirstClasses'
```

```smalltalk
initialize
    super initialize.
    annualSalary := 0
```

```smalltalk
annualSalary
    ^ annualSalary
```

```smalltalk
annualSalary: aNumber
    annualSalary := aNumber
```

```smalltalk
calculatePay
    ^ annualSalary / 12  "Monthly pay"
```

```smalltalk
Employee subclass: #HourlyEmployee
    instanceVariableNames: 'hourlyRate hoursWorked'
    classVariableNames: ''
    package: 'MyFirstClasses'
```

```smalltalk
initialize
    super initialize.
    hourlyRate := 0.
    hoursWorked := 0
```

```smalltalk
hourlyRate
    ^ hourlyRate
```

```smalltalk
hourlyRate: aNumber
    hourlyRate := aNumber
```

```smalltalk
hoursWorked
    ^ hoursWorked
```

```smalltalk
hoursWorked: aNumber
    hoursWorked := aNumber
```

```smalltalk
calculatePay
    | regularPay overtimePay |
    regularPay := (hoursWorked min: 40) * hourlyRate.
    overtimePay := (hoursWorked max: 0) - 40 max: 0) * hourlyRate * 1.5.
    ^ regularPay + overtimePay
```

```smalltalk
resetHours
    hoursWorked := 0
```

Now you can use them polymorphically:

```smalltalk
| employees totalPayroll |
employees := OrderedCollection new.

employees add: (SalariedEmployee new
    name: 'Alice Smith';
    employeeId: 1001;
    annualSalary: 72000;
    yourself).

employees add: (HourlyEmployee new
    name: 'Bob Jones';
    employeeId: 1002;
    hourlyRate: 25;
    hoursWorked: 45;
    yourself).

totalPayroll := 0.
employees do: [ :employee |
    | pay |
    pay := employee calculatePay.
    totalPayroll := totalPayroll + pay.
    Transcript show: employee name; show: ': $'; show: pay printString; cr ].

Transcript show: 'Total payroll: $'; show: totalPayroll printString; cr
```

This works beautifully! Each employee calculates pay differently, but you can treat them all uniformly through their common superclass.

## Inheritance and Instance Variables

Subclasses can access inherited instance variables directly:

```smalltalk
Object subclass: #Person
    instanceVariableNames: 'firstName lastName'
    ...

fullName
    ^ firstName , ' ' , lastName
```

```smalltalk
Person subclass: #Employee
    instanceVariableNames: 'employeeId'
    ...

badge
    "Can access firstName and lastName directly!"
    ^ 'Employee: ' , firstName , ' ' , lastName , ' (#' , employeeId printString , ')'
```

The Employee can use `firstName` and `lastName` as if they were its own variables.

## Protected vs Private

In Smalltalk, there's no true "private" or "protected" - all methods can be called by anyone, and all instance variables can be accessed by any method in the class or its subclasses.

However, by convention:
- **Public methods** are documented and intended for external use
- **Private methods** (by convention, prefixed with `pvt` or in "private" protocol) are internal implementation details
- **Instance variables** are always accessed through methods when possible

## The Root: Object

Every class ultimately inherits from Object. Object provides fundamental behavior:

- `new` - Create an instance
- `initialize` - Initialize an instance
- `class` - Return the receiver's class
- `isKindOf:` - Check class membership
- `respondsTo:` - Check if an object understands a message
- `printString` - Convert to string representation
- `=` - Check equality
- `hash` - Generate hash code
- `copy` - Create a copy
- `yourself` - Return self (useful for cascades)
- `error:` - Signal an error
- `subclassResponsibility` - Mark methods subclasses must implement

And hundreds more! Browse Object in the System Browser to see everything.

## ProtoObject: Beyond Object

There's actually one class above Object: **ProtoObject**. It's the absolute root.

```
ProtoObject
  └─ Object
      └─ (everything else)
```

ProtoObject has minimal behavior - just enough to be an object. Object adds all the commonly-needed behavior.

Most of the time, you'll inherit from Object. ProtoObject is used for special cases like proxies and minimal objects.

## Viewing Hierarchies

In the System Browser, you can view class hierarchies:

1. Select a class
2. Right-click and choose "Browse Hierarchy" or similar
3. See a tree view of the class and its subclasses

Or in code:

```smalltalk
Collection allSubclasses  "Returns all subclasses of Collection"
```

```smalltalk
Array allSuperclasses  "Returns all superclasses of Array"
```

```smalltalk
Collection withAllSubclasses size  "How many collection classes are there?"
```

## Try This!

Practice with inheritance:

1. **Create an Animal hierarchy:**
   ```smalltalk
   Object subclass: #Animal
       instanceVariableNames: 'name age'
       ...

   initialize
       super initialize.
       name := 'Unknown'.
       age := 0

   speak
       self subclassResponsibility

   eat
       ^ name , ' is eating'
   ```

   Then create Dog, Cat, Bird subclasses, each with their own `speak` implementation.

2. **Create a Shape hierarchy:**
   ```smalltalk
   Object subclass: #Shape
       instanceVariableNames: 'color'
       ...

   area
       self subclassResponsibility

   perimeter
       self subclassResponsibility

   describe
       ^ 'A ' , color , ' shape with area ' , self area printString
   ```

   Then create Circle, Rectangle, Triangle subclasses with proper calculations.

3. **Create account types:**
   ```smalltalk
   Object subclass: #BankAccount
       instanceVariableNames: 'balance'
       ...

   deposit: amount
       balance := balance + amount

   withdraw: amount
       balance := balance - amount
   ```

   Then create:
   - SavingsAccount (adds interest rate)
   - CheckingAccount (allows overdraft)
   - Test with an OrderedCollection of mixed accounts

4. **Explore Smalltalk's hierarchy:**
   ```smalltalk
   Collection allSubclasses.
   Number allSubclasses.
   Exception allSubclasses
   ```

   Pick a class and trace its hierarchy:
   ```smalltalk
   OrderedCollection allSuperclasses
   ```

5. **Create a game hierarchy:**
   ```smalltalk
   Object subclass: #Game
       instanceVariableNames: 'players winner'
       ...

   play
       self setup.
       [ self isOver ] whileFalse: [ self takeTurn ].
       self announceWinner
   ```

   Implement Chess, Checkers, or TicTacToe as subclasses.

## Common Mistakes

### Forgetting `super initialize`

```smalltalk
initialize
    balance := 0  "Missing super initialize!"
```

Should be:

```smalltalk
initialize
    super initialize.
    balance := 0
```

### Deep Hierarchies

```
Object → A → B → C → D → E → F → G
```

Too deep! Aim for 3-4 levels maximum.

### Inheritance for Code Reuse Only

Don't inherit just to reuse code if there's no "is-a" relationship:

**Bad:**
```smalltalk
ArrayList subclass: #Person
    "Wrong! Person is NOT a kind of ArrayList!"
```

**Good:**
```smalltalk
Object subclass: #Person
    instanceVariableNames: 'friends'  "Person HAS a list, doesn't IS a list"
```

### Not Calling Super When Needed

```smalltalk
Vehicle subclass: #Car
    ...

description
    ^ 'A car with ' , numberOfDoors printString , ' doors'
    "Missing super description! Lost make/model/year info!"
```

Should be:

```smalltalk
description
    ^ super description , ', ' , numberOfDoors printString , ' doors'
```

### Overriding to Do Nothing

```smalltalk
fly
    "Do nothing - this vehicle can't fly"
```

If a subclass can't meaningfully implement a method, maybe it shouldn't inherit from that superclass!

## Advanced: Method Lookup in Detail

Let's trace a complex example:

```smalltalk
Object subclass: #A
    ...

foo
    ^ 'A-foo'

bar
    ^ self foo

baz
    ^ 'A-baz'
```

```smalltalk
A subclass: #B
    ...

foo
    ^ 'B-foo'

qux
    ^ super bar , ' / ' , self baz
```

```smalltalk
B subclass: #C
    ...

baz
    ^ 'C-baz'
```

Now:

```smalltalk
| c |
c := C new.
c qux
```

Trace it:

1. `c qux` - Look for `qux` in C. Not found. Look in B. Found! Execute B>>qux with self = c
2. B>>qux executes: `^ super bar , ' / ' , self baz`
3. `super bar` - Look for `bar` in A (superclass of where super appears). Found A>>bar. Execute with self = c
4. A>>bar executes: `^ self foo`
5. `self foo` - Look for `foo` in C (self's class). Not found. Look in B. Found B>>foo. Execute with self = c
6. B>>foo returns 'B-foo'
7. Back to A>>bar, returns 'B-foo'
8. Back to B>>qux: `'B-foo' , ' / ' , self baz`
9. `self baz` - Look for `baz` in C. Found! Execute C>>baz with self = c
10. C>>baz returns 'C-baz'
11. B>>qux returns 'B-foo / C-baz'

Complex, but follows consistent rules!

## Inheritance Best Practices

1. **Keep hierarchies shallow** - 3-4 levels max
2. **Use inheritance for "is-a"** - Use composition for "has-a"
3. **Always call `super initialize`** - In every initialize method
4. **Don't break Liskov Substitution** - Subclasses must be substitutable
5. **Put common behavior in superclass** - Don't repeat yourself
6. **Use abstract methods** - `self subclassResponsibility` for required overrides
7. **Document your hierarchy** - Add class comments explaining the design
8. **Favor composition over inheritance** - When in doubt, use composition
9. **One responsibility per class** - Don't create god classes
10. **Test polymorphism** - Use collections of mixed types

## Looking Ahead

You now understand inheritance - how classes build on other classes, how method lookup works through hierarchies, and how to design good class structures.

You've completed Part IV (Creating Your Own Objects)! You can now:
- Create classes with instance variables
- Add methods (accessors, mutators, computed values)
- Use `self` and `super` correctly
- Build inheritance hierarchies

In Part V, we'll explore the **Image** - Smalltalk's persistent object memory. You'll learn:
- What the image file is and how it works
- The changes file and recovery
- The sources file and system code
- How to save and share your work

Then in Part VI, we'll dive deep into Smalltalk's development tools: the System Browser, Inspector, Debugger, and more.

You're now writing real object-oriented programs! The foundation is solid. Everything from here builds on what you've learned.

---

**Key Takeaways:**
- **Inheritance** lets one class (subclass) inherit from another (superclass)
- Subclasses inherit all instance variables and methods
- Subclasses can add new variables and methods
- Subclasses can **override** methods to change behavior
- Use `super` to call the superclass's version of a method
- **Method lookup**: Start in receiver's class, go up hierarchy until found
- Use inheritance for "is-a" relationships, composition for "has-a"
- Keep hierarchies shallow (3-4 levels maximum)
- **Template Method Pattern**: Superclass defines structure, subclasses fill in details
- `self subclassResponsibility` marks methods subclasses must implement
- **Liskov Substitution Principle**: Subclasses must be substitutable for superclasses
- Avoid deep hierarchies and artificial relationships
- Every class ultimately inherits from Object (or ProtoObject)
- Inheritance enables polymorphism - treating different objects uniformly
- Always call `super initialize` in initialize methods

---

[Previous: Chapter 14 - self and super](chapter-14-self-and-super.md) | [Next: Chapter 16 - Understanding the Image](chapter-16-understanding-the-image.md)
