# Chapter 22: The Finder - Discovering Code

You've mastered the System Browser for navigating code, the Inspector for examining objects, and the Debugger for fixing problems. Now you'll learn about tools for **discovering** code: finding methods, searching for implementations, and quickly jumping to any part of the system.

In a large codebase with thousands of classes and tens of thousands of methods, how do you find what you're looking for? The Finder (and in modern Pharo, **Spotter**) makes this effortless.

## The Challenge: Finding Code

Traditional programming:
```
Open file explorer
Browse through directories
Grep for text strings
Hope you find the right file
Open it in an editor
Search within the file
```

Smalltalk:
```
Press Shift+Enter
Type a few characters
See instant results
Jump directly to the code
```

This chapter covers the tools that make code discovery instantaneous.

## Spotter - The Universal Search

**Spotter** (in Pharo) is a fast, fuzzy-matching search tool for finding anything in the system. Think of it like Spotlight (macOS) or Windows Search, but for code.

### Opening Spotter

Press **Shift+Enter** anywhere in Pharo.

A search window appears at the top center of the screen.

### Basic Usage

Start typing:

**Type:** `OrderedC`

Spotter shows:
- **Classes**: `OrderedCollection`, `OrderedCollectionTest`, etc.
- **Packages**: Packages containing "OrderedC"
- **Methods**: Methods with "OrderedC" in their name

Select a result and press Enter to jump to it!

### Fuzzy Matching

Spotter uses fuzzy matching. You don't need exact spelling:

- **Type:** `orc` → finds `OrderedCollection`
- **Type:** `dict` → finds `Dictionary`
- **Type:** `tstcas` → finds `TestCase`

It matches on:
- Initials: `OC` → `OrderedCollection`
- Substrings: `ered` → `OrderedCollection`
- Patterns: `O*C` → `OrderedCollection`

### Categories

Spotter organizes results into categories:

- **Classes** - Find class definitions
- **Packages** - Find packages
- **Implementors** - Find methods by name
- **Senders** - Find methods that call a method
- **Methods** - Find methods by content
- **Help Topics** - Find documentation
- **File System** - Find files on disk
- **... and more!**

You can filter by category or browse all results.

### Navigating Results

- **Type** to filter
- **Arrow keys** to move up/down
- **Enter** to select and open
- **Tab** to switch categories
- **Esc** to close

### Example: Find a Class

Open Spotter (`Shift+Enter`).

Type: `String`

Results:
- `String` (class)
- `StringTest` (class)
- `WideString` (class)
- Methods with "String" in the name

Click on `String` (or press Enter when selected).

A System Browser opens on the String class!

### Example: Find a Method

Type: `add:`

Results show all implementors of `add:`:
- `OrderedCollection>>add:`
- `Set>>add:`
- `Dictionary>>add:`
- `Array>>add:` (wait, does Array have add:? Check!)

Select `OrderedCollection>>add:` and press Enter.

The System Browser opens showing that method!

### Example: Find Senders

Type: `add:` then switch to the **Senders** category (Tab or click).

Results show all methods that **call** `add:`:
- `SomeClass>>populateCollection`
- `AnotherClass>>appendItem`
- ...

Select one to see how it's used.

### Example: Find by Content

Type: `sqrt` in the **Methods** category.

Spotter searches method **bodies** for "sqrt". Find all methods that use square root!

## The Method Finder

The **Method Finder** is a tool for discovering methods by **example**.

"I want to do X to Y and get Z. What method does that?"

### Opening Method Finder

World menu → `Tools` → `Method Finder`

Or evaluate:

```smalltalk
MethodFinder open
```

### Find by Example

The Method Finder window has input fields:

**Receiver**: The object
**Arguments**: Arguments to pass
**Result**: Expected result

Example: "How do I uppercase a string?"

- **Receiver**: `'hello'`
- **Arguments**: (leave empty)
- **Result**: `'HELLO'`

Click `Search`.

Results show:
- `String>>asUppercase`
- `String>>uppercaseFirst` (no, not the same)

Found it! `asUppercase` is the method you want.

### More Examples

**"How do I remove an element from a collection?"**

- **Receiver**: `#(1 2 3 4 5)`
- **Arguments**: `3`
- **Result**: `#(1 2 4 5)`

Results:
- `SequenceableCollection>>copyWithout:`

**"How do I round a number to 2 decimal places?"**

- **Receiver**: `3.14159`
- **Arguments**: `2`
- **Result**: `3.14`

Results:
- `Float>>roundTo:` (almost, but that rounds to nearest 0.01)
- `Float>>truncateTo:` (also close)

Actually, try:
```smalltalk
(3.14159 * 100) rounded / 100  "Returns 3.14"
```

Method Finder helps you discover the right approach!

### Why This is Powerful

You don't need to **know** method names. You just describe what you want, and Method Finder suggests options.

## Finder - The Original Search Tool

Before Spotter, Pharo had the **Finder** tool. It's still available and useful:

World menu → `Tools` → `Finder`

The Finder has tabs:

### Classes Tab

Search for classes by name:
- Full name: `OrderedCollection`
- Partial: `*Collection*`
- Pattern: `Ordered*`

### Methods Tab

Search for methods by:
- **Selector**: Method name (e.g., `add:`)
- **Source**: Code content (e.g., `sqrt`)
- **Pragmas**: Methods with special annotations

### Examples Tab

Similar to Method Finder - find methods by example.

### Packages Tab

Find packages by name.

## Finding Implementors and Senders

From anywhere (System Browser, Playground, etc.):

### Find Implementors

1. Select a method name (e.g., `add:`)
2. Right-click → `Implementors of 'add:'`

Or keyboard shortcut: **Ctrl+M I** (or **Cmd+M I** on macOS)

A list opens showing ALL classes that implement `add:`.

### Find Senders

1. Select a method name
2. Right-click → `Senders of 'add:'`

Or: **Ctrl+M S** (or **Cmd+M S**)

A list opens showing all methods that **call** `add:`.

### Finding from Code

You don't need to select text. In the System Browser, just click on a method name in the Method pane, then:

- Right-click → `Implementors`
- Right-click → `Senders`

## Finding References

### Class References

Find all places a class is mentioned:

1. Right-click on a class name → `References to Class`

Shows everywhere that class is used: as a type, in method bodies, etc.

### Variable References

Find all uses of an instance variable:

1. In a method, right-click on an instance variable → `Inst var refs`

Shows all methods that read that variable.

For assignments:

- Right-click → `Inst var assignments`

Shows all methods that write to that variable.

## Finding by Keyboard Shortcuts

Pharo is keyboard-friendly:

| Shortcut | Action |
|----------|--------|
| **Shift+Enter** | Open Spotter |
| **Ctrl+O N** | Navigate to class |
| **Ctrl+O B** | Open new System Browser |
| **Ctrl+M I** | Implementors of selected method |
| **Ctrl+M S** | Senders of selected method |
| **Ctrl+M R** | References to selected class/variable |
| **Ctrl+F** | Find in current context |

(On macOS, use **Cmd** instead of **Ctrl**.)

## Browsing Class Hierarchies

### View Hierarchy

Right-click on a class → `Browse Hierarchy`

See the entire inheritance tree, with the selected class highlighted.

### View Subclasses

Right-click → `Browse Subclasses`

See all classes that inherit from this class.

### View Superclasses

The class definition shows the superclass. Click on it to navigate up the hierarchy.

## Searching Method Source

### System Browser Search

In the System Browser:

Right-click in the Code pane → `Find...` or `Search` → `Method source with it`

Enter search text: `sqrt`

A list opens with all methods containing "sqrt" in their body.

### From Spotter

Open Spotter (`Shift+Enter`).

Switch to the **Methods** category.

Type: `sqrt`

Results show methods containing "sqrt".

## Browsing by Protocol

In the System Browser:

1. Select a class
2. Look at the Protocol pane (method categories)
3. Browse by category: `accessing`, `testing`, `comparing`, etc.

This helps discover methods logically grouped by purpose.

## Searching for Pragmas

**Pragmas** are special annotations in methods:

```smalltalk
myMethod
    <pragma: #example>
    ...
```

To find methods with specific pragmas:

In the Finder → `Methods` tab → Search by `Pragma`.

Example: Find all test methods:

```smalltalk
<test>
```

## Spotter Tips and Tricks

### Tip 1: Use Abbreviations

Type just the initials:
- `OC` → `OrderedCollection`
- `DF` → `DateAndTime`

### Tip 2: Navigate Categories

Press `Tab` to cycle through categories. Each category shows different results.

### Tip 3: Preview

Some results have previews. Hover or select to see a preview of the class or method.

### Tip 4: Recent Items

Spotter often shows recent items first. If you just worked on a class, it'll be near the top!

### Tip 5: Dive Deeper

After selecting a result, you can immediately open Spotter again (`Shift+Enter`) to continue exploring related code.

## Discovering Patterns

### "Who else does this?"

Found a useful method? Find implementors to see how different classes solve the same problem:

```smalltalk
Collection>>select:
```

Find implementors. See implementations in `OrderedCollection`, `Set`, `Dictionary`, `Array`, etc.

Compare approaches and learn patterns!

### "How is this used?"

Found a method? Find senders to see real-world usage:

```smalltalk
OrderedCollection>>addAll:
```

Find senders. See how other code uses this method. Learn by example!

### "What can I do with this object?"

Select a class in the System Browser. Browse its protocols to see all available methods. Discover capabilities you didn't know existed!

## Finding Help and Documentation

### Class Comments

In the System Browser, select a class and click the `Comments` button.

Read the class comment - often contains usage examples!

### Help Topics

In Spotter, switch to **Help Topics** category.

Type: `Collections`

Find documentation and tutorials on collections.

### Method Comments

In the System Browser, read method comments (the string at the start of a method):

```smalltalk
select: aBlock
    "Evaluate aBlock with each element as argument.
    Return a new collection with elements for which aBlock evaluates to true."
    ...
```

## Exploring System Classes

Want to learn Smalltalk? Explore system classes:

### Collections

```
Collection
├─ SequenceableCollection
│  ├─ OrderedCollection
│  ├─ Array
│  ├─ String
│  └─ Interval
├─ Set
├─ Dictionary
└─ Bag
```

Browse each class. Read comments. Look at methods. See how they're implemented!

### Numbers

```
Number
├─ Integer
├─ Float
├─ Fraction
└─ ...
```

### Exceptions

```
Exception
├─ Error
│  ├─ MessageNotUnderstood
│  ├─ ZeroDivide
│  └─ ...
└─ Notification
```

Use Spotter and the Browser to explore these hierarchies!

## Practical Examples

### Example 1: "How do I sort a collection?"

Open Spotter. Type: `sort`

Results:
- `SortedCollection` (a class)
- `SequenceableCollection>>sorted` (a method!)
- `Collection>>sort:` (another method)

Click on `sorted`:

```smalltalk
sorted
    "Return a new collection sorted"
    ^ self asArray sort
```

Try it:

```smalltalk
#(3 1 4 1 5 9 2 6) sorted  "Returns #(1 1 2 3 4 5 6 9)"
```

Found it!

### Example 2: "How do I iterate with an index?"

Method Finder:

- **Receiver**: `#('a' 'b' 'c')`
- **Arguments**: `[ :element :index | ... ]` (describe what you want)
- **Result**: (not sure)

Actually, browse Collection. Look in the `enumerating` protocol:

- `do:` - simple iteration
- `collect:` - transform
- `select:` - filter
- `doWithIndex:` - iterate with index! (Aha!)

```smalltalk
#('a' 'b' 'c') doWithIndex: [ :element :index |
    Transcript show: index printString, ': ', element; cr ]
```

Output:
```
1: a
2: b
3: c
```

### Example 3: "Who calls this method?"

You're refactoring `MyClass>>oldMethod`. Before deleting it, check if anyone calls it:

Right-click on `oldMethod` → `Senders`

If the list is empty, it's safe to delete!

If there are senders, refactor them first.

## Try This!

Practice discovering code:

1. **Use Spotter to find classes:**
   ```
   Shift+Enter → Type "Float"
   Shift+Enter → Type "Test"
   Shift+Enter → Type "HTTP"
   ```

2. **Find all implementors of common methods:**
   ```
   Find implementors of: size
   Find implementors of: printString
   Find implementors of: =
   ```

   See how different classes implement these!

3. **Find senders of key methods:**
   ```
   Find senders of: at:put:
   Find senders of: ifTrue:ifFalse:
   Find senders of: new
   ```

   Understand usage patterns!

4. **Use Method Finder:**
   ```
   Receiver: 'hello world'
   Arguments: (none)
   Result: 'Hello World'
   ```

   Find the method! (Hint: It's not asUppercase...)

   Actually:
   ```smalltalk
   'hello world' capitalized  "Returns 'Hello World'"
   ```

5. **Search for interesting code:**
   ```
   Spotter → Methods → "fibonacci"
   Spotter → Methods → "prime"
   Spotter → Methods → "quicksort"
   ```

   Explore algorithm implementations!

6. **Browse a class you're curious about:**
   ```
   Spotter → "Date" → Enter
   Read class comment
   Browse protocols: accessing, arithmetic, comparing
   See what Date can do!
   ```

7. **Find where a class is used:**
   ```
   Right-click on OrderedCollection → References to Class
   See everywhere it's used
   ```

8. **Find all tests for a class:**
   ```
   Spotter → "OrderedCollectionTest"
   Browse test methods
   Learn from tests!
   ```

## Common Workflows

### Learning the System

1. Wonder: "How do I do X?"
2. Method Finder or Spotter: Find candidates
3. Browse implementations: See how it works
4. Try in Playground: Test it out
5. Find senders: See real-world usage

### Refactoring

1. Find senders of method you're changing
2. Understand current usage
3. Make changes
4. Update all senders if needed

### Understanding Bugs

1. Error occurs in method X
2. Find senders of X: Who calls it?
3. Find implementors: How do similar methods work?
4. Trace through code: Follow the call chain
5. Understand and fix

### Discovering APIs

1. Have an object of a certain class
2. Browse that class in the System Browser
3. Look through protocols
4. Discover methods you didn't know existed
5. Read comments and try them

## Beyond Basic Finding

### Custom Searches

You can write custom searches in Smalltalk!

```smalltalk
"Find all methods longer than 50 lines:"
| longMethods |
longMethods := OrderedCollection new.
Object allSubclasses do: [ :class |
    class methods do: [ :method |
        method linesOfCode > 50 ifTrue: [
            longMethods add: class name , '>>' , method selector ] ] ].
longMethods
```

### Query the System

Smalltalk is reflective. You can query it:

```smalltalk
"How many classes are there?"
Object allSubclasses size

"How many methods?"
Object allSubclasses sum: [ :class | class methods size ]

"Classes without tests:"
Object allSubclasses select: [ :class |
    (Smalltalk classNamed: class name , 'Test') isNil ]
```

### Build Your Own Finder

Advanced users can extend Spotter or create custom search tools!

## Keyboard Mastery

Master these shortcuts for blazing-fast navigation:

- **Shift+Enter** - Spotter (universal search)
- **Ctrl+O B** - New System Browser
- **Ctrl+O N** - Navigate to class
- **Ctrl+M I** - Implementors
- **Ctrl+M S** - Senders
- **Ctrl+M R** - References
- **Ctrl+F** - Find in current context

Combine with typing to filter, and you'll navigate thousands of methods effortlessly!

## The Philosophy

The Finder and Spotter embody key Smalltalk principles:

### Discoverability

You don't need to memorize everything. The tools help you discover what's available.

### Transparency

Everything is searchable. No hidden APIs, no secret methods.

### Interactivity

Search is instant. Results appear as you type. Navigation is seamless.

### Empowerment

The tools empower exploration. Try things! Search! Discover!

## Looking Ahead

You now understand the tools for discovering code! You can:
- Use Spotter for universal search
- Find implementors and senders instantly
- Use Method Finder to discover methods by example
- Navigate hierarchies and protocols
- Search method source
- Explore the system freely

This completes Part VI (Tools of the Trade)! You've mastered:
- System Browser (Chapter 19) - navigating and editing code
- Inspector/Explorer (Chapter 20) - examining objects
- Debugger (Chapter 21) - fixing bugs and developing interactively
- Finder/Spotter (Chapter 22) - discovering code

In Part VII (Intermediate Concepts), we'll explore:
- Chapter 23: Protocols and Polymorphism
- Chapter 24: Error Handling
- Chapter 25: Testing Your Code
- Chapter 26: Packages and Code Organization

These chapters build on your foundation to make you a proficient Smalltalk developer!

---

**Key Takeaways:**
- **Spotter** (`Shift+Enter`) is the universal search tool
- Find classes, methods, senders, implementors instantly
- **Fuzzy matching** works on abbreviations and substrings
- **Method Finder** discovers methods by example input/output
- **Implementors** shows all classes implementing a method
- **Senders** shows all methods calling a method
- **References** finds all uses of classes or variables
- Search method source with Spotter or System Browser
- Browse hierarchies to understand class relationships
- Keyboard shortcuts enable lightning-fast navigation
- Use finding tools to learn the system and discover patterns
- The Finder and Spotter make large codebases navigable
- Everything is searchable and discoverable
- Exploration and discovery are core to the Smalltalk workflow

---

[Previous: Chapter 21 - The Debugger](chapter-21-debugger.md) | [Next: Chapter 23 - Protocols and Polymorphism](chapter-23-protocols-and-polymorphism.md)
