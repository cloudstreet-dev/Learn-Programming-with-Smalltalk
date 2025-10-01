# Appendix D: Glossary

A comprehensive reference of Smalltalk-specific terminology and concepts. All terms are defined in beginner-friendly language with cross-references to relevant chapters.

## A

**Abstract Class**
A class designed to be subclassed but not instantiated directly. It defines common behavior for its subclasses but leaves some methods unimplemented. Example: `Collection` is abstract; you use `Array` or `Set` instead. See Chapter 15.

**Accessor**
A method that provides access to an instance variable. There are two types: getters (return the value) and setters (change the value). Example: `name` and `name:`. See Chapter 12.

**Alan Kay**
Principal inventor of Smalltalk and the concept of object-oriented programming. Visionary behind the "everything is an object" philosophy. Winner of the Turing Award. See Chapter 1.

**Argument**
A value passed to a method or block when it's executed. Also called a "parameter." Example: in `3 + 4`, the `4` is an argument. See Chapters 4, 9.

**Array**
A fixed-size, ordered collection that can hold any objects. Created with `#( )` literal syntax or `Array new:`. Example: `#(1 2 3)`. See Chapter 6.

**Assignment**
The act of storing a value in a variable using `:=`. Example: `x := 42`. The `:=` is the assignment operator. See Chapter 7.

**Automatic Testing**
See **Unit Testing**.

## B

**Bag**
A collection that holds objects with duplicates allowed, optimized for counting occurrences. Like a multiset in mathematics. See Chapter 6.

**Baseline**
A Metacello configuration that defines package dependencies and load order for a project. Used for managing complex multi-package projects. See Chapter 26.

**Binary Message**
A message using an operator symbol like `+`, `-`, `*`, `/`, `<`, `>`, `=`. Always takes exactly one argument. Example: `3 + 4`. See Chapter 4.

**Block**
A chunk of code enclosed in square brackets `[ ]` that can be stored, passed around, and executed later. Also called a **closure**. Example: `[ 2 + 2 ]`. See Chapter 9.

**Block Argument**
A parameter passed to a block when it executes. Declared with colons. Example: `[ :x | x * 2 ]` - here `:x` is the block argument. See Chapter 9.

**Boolean**
An object representing true or false. In Smalltalk, `true` and `false` are actual objects (instances of `True` and `False`). See Chapter 8.

**Browser**
See **System Browser**.

## C

**Cascade**
A way to send multiple messages to the same receiver using semicolons `;`. Example: `transcript show: 'Hello'; cr; show: 'World'`. Avoids repeating the receiver. See Chapter 4.

**Changes File**
A file (`.changes`) that logs every change you make to the code in an image. A complete history that allows recovery after crashes. See Chapter 17.

**Class**
A blueprint or template for creating objects. Defines the structure (instance variables) and behavior (methods) of its instances. Example: `Point`, `String`, `Array`. See Chapter 11.

**Class-side Method**
A method defined on the class itself rather than on instances. Used for creating instances or accessing class-level information. Example: `Date today`. See Chapter 11.

**Closure**
Another name for a **block**. Called "closure" because it closes over (captures) variables from its surrounding scope. See Chapter 9.

**Cog VM**
The modern, high-performance virtual machine used by Pharo, Squeak, and Cuis. Features JIT compilation. Developed at OpenSmalltalk project. See Chapter 2.

**Collection**
A general term for objects that hold multiple other objects. Includes `Array`, `OrderedCollection`, `Set`, `Dictionary`, `Bag`. See Chapter 6.

**Comment**
Explanatory text enclosed in double quotes `" "`. Ignored by the interpreter. Example: `"This is a comment"`. See Chapter 3.

**Compiler**
The part of Smalltalk that translates source code into bytecode. Accessible as an object - you can customize it! See Chapter 16.

**Cuis Smalltalk**
A minimalist, elegant Smalltalk implementation focused on simplicity and understandability. Created by Juan Vuletich. See Chapter 30.

## D

**Debugger**
An interactive tool for examining and fixing code while it's running. One of Smalltalk's most powerful features - you can modify code in a paused program! See Chapter 21.

**Dictionary**
A collection that maps keys to values. Like a phone book or hash map. Created with `Dictionary new`. Example: `dict at: 'name' put: 'Alice'`. See Chapter 6.

**Do It**
Execute selected code in the current context. Keyboard shortcut: `Ctrl+D` (or `Cmd+D` on Mac). Fundamental way to run code in Smalltalk. See Chapter 3.

**Dolphin Smalltalk**
A Windows-only Smalltalk implementation with excellent Windows integration. Open source. See Chapter 30.

**Double Dispatch**
A design pattern using two polymorphic calls to achieve behavior based on two objects' types. Common in Smalltalk for operations like arithmetic with mixed types. See Chapter 23.

**Duck Typing**
"If it walks like a duck and quacks like a duck, it's a duck." In Smalltalk, objects are compatible if they respond to the needed messages, regardless of their class. See Chapter 23.

**Dynamic Typing**
Variables don't have declared types; any variable can hold any object. Types are checked at runtime, not compile time. Fundamental to Smalltalk's flexibility. See Chapter 7.

## E

**Encapsulation**
Hiding an object's internal state and requiring interaction through methods. Instance variables are private; access through accessor methods. See Chapter 13.

**ESUG**
European Smalltalk User Group. Organizes the main annual Smalltalk conference. Active community, great conference videos on YouTube. See Chapter 38.

**Exception**
An object representing an error or unusual condition. Can be signaled, caught, handled, resumed, or retried. Example: `Error`, `ZeroDivide`. See Chapter 24.

**Explorer**
A tool for navigating object structures, especially nested collections. Like a file browser but for objects. See Chapter 20.

## F

**Factory Method**
A design pattern where a class-side method creates and returns instances. Example: `Color red`, `Date today`. See Chapter 36.

**False**
The class of the single `false` object. A singleton. Responds to `ifTrue:ifFalse:` by executing the false branch. See Chapter 8.

**File-in**
Loading code from a file into the image. Historical term from early Smalltalk days. Modern Smalltalk uses Tonel/Git. See Chapter 17.

**File-out**
Saving code from the image to a file. Historical term. Replaced by modern version control with Tonel. See Chapter 17.

**Finder**
See **Method Finder** or **Spotter**.

**Fork**
Create a new process (lightweight thread) that runs concurrently. Example: `[ expensive computation ] fork`. See Chapter 37.

## G

**Garbage Collection**
Automatic memory management. The system finds and reclaims objects no longer referenced. You never manually free memory in Smalltalk. See Chapter 16.

**Getter**
An accessor method that returns an instance variable's value. Example: `person name` calls the `name` getter. See Chapter 12.

**Glamorous Toolkit (GT)**
A revolutionary Smalltalk environment focused on "moldable development" - customizing tools for your domain. Created by Tudor Gîrba. See Chapter 29.

**Global Variable**
A variable accessible from anywhere in the system. Starts with a capital letter. Example: `Transcript`, `Smalltalk`. Use sparingly! See Chapter 7.

**GNU Smalltalk**
A command-line Smalltalk implementation following GNU project conventions. File-based rather than image-based. See Chapter 30.

## H

**Halo**
In Morphic (Squeak/Pharo UI framework), colored handles around a morph allowing you to resize, move, rotate, etc. Opened with Shift+Click. See Chapter 35.

**Heap**
The memory area where objects are allocated. Managed automatically by garbage collection. See Chapter 16.

**Hierarchy Browser**
A tool showing the class inheritance hierarchy visually as a tree. Useful for understanding relationships between classes. See Chapter 19.

## I

**Iceberg**
Pharo's Git integration tool. Manages code repositories, commits, branches, and remote synchronization. See Chapter 18.

**Image**
A snapshot of all objects in the Smalltalk system - your code, tools, open windows, everything. Saved as a `.image` file. The heart of Smalltalk development. See Chapter 16.

**Implementors**
A tool that finds all methods with a given name across the entire system. Example: find all implementations of `+`. Keyboard: `Ctrl+M`. See Chapter 22.

**Inheritance**
When a class inherits behavior from a parent class (superclass). The subclass gets all methods and instance variables of the superclass. Example: `String` inherits from `Collection`. See Chapter 15.

**Initialize**
A conventional method name for setting up a new object's initial state. Automatically called after `new`. See Chapter 13.

**Inspect It**
Execute code and open an inspector on the result. Keyboard: `Ctrl+I` (or `Cmd+I`). Essential for exploring objects. See Chapter 3.

**Inspector**
A tool for examining an object's instance variables and evaluating expressions in its context. Like X-ray vision for objects. See Chapter 20.

**Instance**
A specific object created from a class. Example: `'hello'` is an instance of `String`, `42` is an instance of `SmallInteger`. See Chapter 11.

**Instance-side Method**
A normal method that operates on instances of a class. Contrasts with class-side methods. Example: `size` is instance-side. See Chapter 12.

**Instance Variable**
A variable that belongs to an object, holding part of its state. Private to the object. Example: a `Person` might have `name` and `age` instance variables. See Chapter 13.

**Interval**
A collection representing a range of numbers. Created with `to:` or `to:by:`. Example: `1 to: 10` represents numbers from 1 to 10. See Chapter 10.

**Iterate**
To repeat an operation for each element in a collection. Example: `#(1 2 3) do: [ :each | ... ]`. See Chapter 10.

## J

**JIT Compiler**
Just-In-Time compiler. Part of Cog VM that compiles frequently-executed bytecode into native machine code for speed. See Chapter 2.

## K

**Keyword Message**
A message with one or more colons in its name. Example: `at:put:`, `ifTrue:ifFalse:`. Each keyword takes an argument. See Chapter 4.

**KM (Keymapping)**
Pharo's keyboard shortcut system. Accessible via `KMRepository`. Can be customized. See Appendix B.

## L

**Lazy Initialization**
A pattern where an instance variable is created only when first accessed. Saves memory and time. Example: `cache ifNil: [ cache := Dictionary new ]`. See Chapter 13.

**Lepiter**
Glamorous Toolkit's notebook system for moldable documentation and development. Combines text, code, and custom views. See Chapter 29.

**Literal**
A value written directly in code. Examples: `42` (number), `'hello'` (string), `#(1 2 3)` (array), `#symbol` (symbol). See Chapter 5.

**Live Coding**
Modifying and debugging code while the program is running. No compile-run-crash cycle. Fundamental to Smalltalk. See Chapter 16.

**Live Object**
An actual object in memory, not a representation or data structure describing an object. In Smalltalk, you work directly with live objects. See Chapter 20.

## M

**Message**
A request sent to an object to perform some behavior. The fundamental mechanism of Smalltalk. Example: `3 + 4` sends message `+` to object `3`. See Chapter 4.

**Message Browser**
A tool showing all messages sent or referenced in selected code. Useful for understanding dependencies. See Chapter 19.

**Message Passing**
The paradigm where computation happens by objects sending messages to each other. Smalltalk is pure message passing. See Chapter 4.

**Message Send**
See **Message**.

**Metacello**
A package management system for Smalltalk. Handles dependencies, versions, and complex project configurations. See Chapter 26.

**Metaclass**
The class of a class. Every class is an instance of its metaclass. Where class-side methods live. Advanced topic! See Chapter 11.

**Method**
A named piece of code belonging to a class that defines behavior. Called when the corresponding message is sent. See Chapter 12.

**Method Finder**
A tool that finds methods by example behavior. Example: give it `'hello'` and `'HELLO'`, it suggests `asUppercase`. See Chapter 22.

**Method Lookup**
The process of finding which method to execute when a message is sent. Searches the receiver's class, then superclasses, up to `Object`. See Chapter 14.

**Monticello**
An older package management system for Smalltalk. Replaced by Tonel/Git in modern Pharo. See Chapter 17.

**Morph**
A graphical object in the Morphic UI framework. Everything visual is a morph. See Chapter 35.

**Morphic**
The graphics and UI framework used by Pharo and Squeak. Everything is a composable graphical object (morph). See Chapter 35.

## N

**Nautilus**
The older System Browser in Pharo versions before Pharo 7. Replaced by Calypso. See Chapter 19.

**Nil**
The object representing "no object" or "nothing." The only instance of class `UndefinedObject`. Default value of uninitialized variables. See Chapter 7.

**Nesting**
Placing one structure inside another. Example: blocks within blocks, conditionals within loops. See Chapters 8-10.

## O

**Object**
An entity that combines data (instance variables) and behavior (methods). Everything in Smalltalk is an object. See Chapter 4.

**OrderedCollection**
A dynamically-sized, ordered collection. Can grow and shrink. Like `ArrayList` in Java or `list` in Python. See Chapter 6.

**Override**
To define a method in a subclass that already exists in a superclass. The subclass version is used for instances of the subclass. See Chapter 15.

## P

**Package**
A unit of code organization. A collection of related classes and extensions. Example: `Collections-Strings`, `Kernel-Objects`. See Chapter 26.

**PARC (Xerox PARC)**
Palo Alto Research Center. Where Smalltalk was invented in the 1970s. Also created GUIs, ethernet, laser printing, and more. See Chapter 1.

**Pharo**
A modern, open-source Smalltalk implementation. Clean, actively developed, great tools. The recommended Smalltalk for most users. See Chapter 27.

**PharoLauncher**
An application for managing multiple Pharo images and VMs. Simplifies installing and organizing different Pharo versions. See Chapter 27.

**Playground**
The modern name for Workspace in Pharo. A place to write and execute code snippets. See Chapter 3.

**Polymorphism**
When different classes respond to the same message in different ways. Example: `size` works on strings, arrays, sets, etc. See Chapter 23.

**Print It**
Execute code and print the result after the selection. Keyboard: `Ctrl+P` (or `Cmd+P`). See Chapter 3.

**Print String**
The textual representation of an object. Returned by the `printString` method. What you see in inspectors and print-it results. See Chapter 5.

**Process**
A lightweight thread of execution. Smalltalk supports concurrent processes. Created with `fork`. See Chapter 37.

**Protocol**
A group of related methods in a class. Used for organization, not enforcement. Example: `accessing`, `testing`, `private`. See Chapter 12.

**Protocol (Duck Typing)**
An informal interface - a set of messages an object understands. If an object responds to the right messages, it conforms to the protocol. See Chapter 23.

## R

**Receiver**
The object that receives a message. In `3 + 4`, `3` is the receiver. In `person name`, `person` is the receiver. See Chapter 4.

**Refactoring**
Changing code structure without changing behavior. Smalltalk has excellent refactoring tools built in. See Chapter 19.

**Reflection**
The ability of a program to examine and modify itself at runtime. Smalltalk is highly reflective - everything is accessible. See Chapter 16.

**Return**
To exit a method and provide a result. Uses the caret `^`. Example: `^ 42`. Methods without explicit return automatically return `self`. See Chapter 12.

## S

**Selector**
The name of a message/method. Example: `size`, `at:`, `ifTrue:ifFalse:`. Can be represented as a symbol like `#size`. See Chapter 4.

**Self**
A pseudo-variable referring to the receiver of the current message. "This object." Example: `self name` calls the `name` method on this object. See Chapter 14.

**Senders**
A tool that finds all places where a particular message is sent. Example: find everywhere that sends `+`. Keyboard: `Ctrl+N`. See Chapter 22.

**Setter**
An accessor method that changes an instance variable's value. By convention, ends with colon. Example: `name:`. See Chapter 12.

**Set**
An unordered collection with no duplicates. Like mathematical sets. Example: `Set with: 1 with: 2 with: 2` contains only `1` and `2`. See Chapter 6.

**Singleton**
A design pattern where a class has exactly one instance. Example: `true`, `false`, and `nil` are singletons. See Chapter 36.

**Smalltalk**
1. The programming language. 2. The global object representing the system (capital S). Example: `Smalltalk globals` accesses global variables. See Chapter 1.

**SmallInteger**
A class representing small integers that fit in a machine word. Highly optimized. Usually transparent to users. See Chapter 5.

**Source Code**
The text form of programs. In Smalltalk, stored in the changes file and sources file. See Chapter 17.

**Sources File**
A file (`.sources`) containing the source code for the system classes. Shared among images of the same version. See Chapter 17.

**Spec 2**
Pharo's modern UI framework. Declarative, reusable, flexible. Used for building tools and applications. See Chapter 31.

**Spotter**
Pharo's universal search tool. Opened with `Shift+Enter`. Searches classes, methods, files, and more. Powerful and fast. See Chapter 22.

**Squeak**
A Smalltalk implementation focused on multimedia and education. Very portable, stable, great for teaching. See Chapter 28.

**Stack**
1. The call stack - the chain of method calls currently executing. Visible in the debugger. 2. A data structure (LIFO). See Chapters 21, 36.

**String**
A sequence of characters. Created with single quotes. Example: `'Hello, World!'`. Strings are objects! See Chapter 5.

**Subclass**
A class that inherits from another class (its superclass). Example: `String` is a subclass of `Collection`. See Chapter 15.

**Super**
A pseudo-variable that sends messages to the superclass's implementation. Used when overriding methods. Example: `super initialize`. See Chapter 14.

**Superclass**
The parent class in an inheritance hierarchy. Example: `Collection` is the superclass of `Array`, `Set`, etc. See Chapter 15.

**Symbol**
An immutable, unique string used as an identifier. Created with `#`. Example: `#name`. Only one `#name` symbol exists in memory. See Chapter 5.

**System Browser**
The primary tool for browsing and editing classes and methods. Four or five pane layout. Heart of Smalltalk development. See Chapter 19.

**SUnit**
Smalltalk's unit testing framework. The original xUnit framework that inspired JUnit, NUnit, etc. See Chapter 25.

## T

**TDD (Test-Driven Development)**
A development methodology: write test first (red), make it pass (green), improve code (refactor). Invented by Kent Beck in Smalltalk! See Chapter 25.

**Template Method**
A design pattern where a superclass defines an algorithm's structure, and subclasses fill in specific steps. Common in Smalltalk. See Chapter 36.

**Temporary Variable**
A variable that exists only within a method or block. Declared with pipes `| |`. Example: `| temp |`. See Chapter 7.

**TestCase**
The superclass for all test classes in SUnit. Your tests inherit from this. Provides assertion methods. See Chapter 25.

**Tonel**
A text-based file format for Smalltalk code. One file per class. Designed for version control (Git). Used by Iceberg. See Chapter 18.

**Transcript**
A system-wide output window for displaying messages. Like console output. Example: `Transcript show: 'Hello'`. See Chapter 3.

**True**
The class of the single `true` object. A singleton. Responds to `ifTrue:ifFalse:` by executing the true branch. See Chapter 8.

## U

**UndefinedObject**
The class of `nil`. Has only one instance. See Chapter 7.

**Unary Message**
A message with no arguments. Just a name. Example: `'hello' size`, `42 factorial`. See Chapter 4.

**Unicode**
Character encoding supporting all writing systems. Smalltalk strings support Unicode. See Chapter 5.

**Unit Test**
A small test that verifies one specific behavior. Written with SUnit framework. See Chapter 25.

## V

**VA Smalltalk**
A commercial Smalltalk implementation by Instantiations. Strong in enterprise environments, especially IBM mainframe integration. See Chapter 30.

**Variable**
A named container for an object. Types: temporary, instance, class, global. See Chapter 7.

**Virtual Machine (VM)**
The engine that runs Smalltalk bytecode. Handles memory, graphics, OS interaction. Example: Cog VM. See Chapter 2.

**VisualWorks**
A commercial Smalltalk implementation by Cincom. Highly portable, mature, widely used in industry. See Chapter 30.

## W

**Workspace**
A text area for writing and executing code snippets. Called **Playground** in modern Pharo. See Chapter 3.

## X

**Xerox PARC**
See **PARC**.

## Z

**ZeroConf**
A one-line installation script for Pharo. Downloads and sets up everything automatically. See Appendix A.

## Common Abbreviations

- **GT** - Glamorous Toolkit
- **VM** - Virtual Machine
- **UI** - User Interface
- **API** - Application Programming Interface
- **ESUG** - European Smalltalk User Group
- **TDD** - Test-Driven Development
- **REPL** - Read-Eval-Print Loop (though Smalltalk is much more!)
- **OOP** - Object-Oriented Programming
- **JIT** - Just-In-Time (compiler)

## Special Characters and Syntax

- `:=` - Assignment operator
- `^` - Return operator
- `|` - Variable declaration pipes
- `[ ]` - Block delimiters
- `#( )` - Array literal
- `#` - Symbol prefix
- `"` - Comment delimiters
- `'` - String delimiters
- `;` - Cascade operator
- `:` - Keyword argument separator
- `.` - Statement separator (period)

## Keyboard Shortcut Abbreviations

- `Ctrl` - Control key (Windows/Linux)
- `Cmd` - Command key (macOS)
- `Alt` - Alt/Option key
- `Shift` - Shift key

See Appendix B for complete keyboard shortcut reference.

## Resources

**Where to look up terms:**

1. **System itself**: Select any term and `Ctrl+B` (Browse it)
2. **Pharo By Example**: Free book with clear explanations
3. **Discord**: Ask the community at https://discord.gg/QewZMZa
4. **This book**: Check the table of contents for relevant chapters

## Tips for Using This Glossary

**Cross-references**: When you see "See Chapter X", that chapter covers the topic in depth.

**Bold terms**: Indicate the main entry. Synonyms point to main entries.

**Examples**: Most entries include code examples showing usage.

**Progressive disclosure**: Start with basic entries (Object, Message, Class) and build up to advanced concepts (Metaclass, Reflection).

**Context matters**: Some terms have multiple meanings (like Protocol or Process). Context makes it clear.

## Contributing

Found a term missing or unclear? This glossary should grow with the community's needs. Suggest additions via:
- Discord discussions
- GitHub issues
- Pull requests

## Quick Concept Map

```
Object
  ├─ has Instance Variables
  ├─ responds to Messages
  ├─ created from Class
  └─ lives in Image

Class
  ├─ defines Instance Variables
  ├─ defines Methods
  ├─ has Superclass (Inheritance)
  └─ creates Instances

Message
  ├─ Unary (no args)
  ├─ Binary (operators)
  └─ Keyword (named args)

Method
  ├─ has Selector (name)
  ├─ has Parameters
  ├─ contains Statements
  └─ returns Object

Image
  ├─ contains all Objects
  ├─ paired with Changes file
  ├─ run by VM
  └─ saved as snapshot
```

---

[Previous: Appendix C - Useful Code Snippets](appendix-c-snippets.md) | [Next: Appendix E - Further Reading](appendix-e-further-reading.md)
