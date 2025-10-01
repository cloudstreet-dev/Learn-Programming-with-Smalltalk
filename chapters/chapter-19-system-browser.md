# Chapter 19: The System Browser - Your Code Navigator

Welcome to Part VI: Tools of the Trade! You've learned the fundamentals of Smalltalk programming and how to manage code with version control. Now we'll explore the powerful tools that make Smalltalk development so productive and enjoyable.

First up: the **System Browser** - your primary tool for navigating, reading, writing, and understanding code. It's not just an editor; it's a sophisticated code navigation system designed around Smalltalk's object-oriented structure.

By the end of this chapter, you'll be navigating codebases like a pro, finding methods instantly, and understanding how the System Browser makes Smalltalk development faster than traditional text-based editors.

## What is the System Browser?

The **System Browser** (often just called "the Browser") is a tool for exploring and editing classes and methods. It's structured around Smalltalk's organization:

- **Packages** - Groups of related classes
- **Classes** - Blueprints for objects
- **Protocols** - Categories of related methods
- **Methods** - Individual pieces of behavior

The Browser shows you this hierarchy clearly, making it easy to navigate large codebases.

## Opening the System Browser

Launch the Browser:
- **World menu** → `System Browser`
- **Or press** `Ctrl+O` `B` (Windows/Linux) or `Cmd+O` `B` (macOS)

A window opens with four panes horizontally across the top, and a large code pane at the bottom.

## The Five Panes

The System Browser has five distinct areas:

### 1. Package Pane (Far Left)

Lists all packages (groups of classes):
- `Collections`
- `Kernel`
- `Morphic`
- `MyFirstClasses` (your packages!)
- ...

Packages organize classes by functionality or project.

### 2. Class Pane (Second from Left)

Lists all classes in the selected package:
- `OrderedCollection`
- `Set`
- `Dictionary`
- `Array`
- ...

Shows the classes alphabetically.

### 3. Protocol Pane (Third)

Lists all protocols (method categories) for the selected class:
- `accessing`
- `adding`
- `testing`
- `comparing`
- `converting`
- ...

Protocols group related methods for easy browsing.

### 4. Method Pane (Fourth)

Lists all methods in the selected protocol:
- `at:`
- `at:put:`
- `size`
- `isEmpty`
- ...

Shows method names (selectors).

### 5. Code Pane (Bottom)

Displays the actual source code of the selected method, or the class definition if no method is selected.

This is where you read and edit code!

## Basic Navigation

Let's explore a class step-by-step:

### Step 1: Select a Package

Click `Collections` in the Package pane.

The Class pane updates to show all classes in the Collections package: `Array`, `Bag`, `Dictionary`, `OrderedCollection`, `Set`, etc.

### Step 2: Select a Class

Click `OrderedCollection` in the Class pane.

The Protocol pane updates to show method categories: `accessing`, `adding`, `removing`, `enumerating`, `testing`, etc.

The Code pane shows the class definition:

```smalltalk
SequenceableCollection subclass: #OrderedCollection
    instanceVariableNames: 'array firstIndex lastIndex'
    classVariableNames: ''
    package: 'Collections-Sequenceable'
```

You can see:
- OrderedCollection is a subclass of SequenceableCollection
- It has three instance variables: `array`, `firstIndex`, `lastIndex`
- It's in the `Collections-Sequenceable` package

### Step 3: Select a Protocol

Click `adding` in the Protocol pane.

The Method pane updates to show methods in that category: `add:`, `addAll:`, `addFirst:`, `addLast:`, etc.

### Step 4: Select a Method

Click `add:` in the Method pane.

The Code pane shows the method source:

```smalltalk
add: newObject
    "Append newObject to the end of the receiver"
    ^ self addLast: newObject
```

Now you can read the code!

## Understanding What You See

When viewing a class:

```smalltalk
Object subclass: #Point2D
    instanceVariableNames: 'x y'
    classVariableNames: ''
    package: 'MyFirstClasses'
```

This tells you:
- **Superclass**: Point2D inherits from Object
- **Instance variables**: Each Point2D has `x` and `y`
- **Class variables**: None in this case
- **Package**: Found in MyFirstClasses

When viewing a method:

```smalltalk
distanceFromOrigin
    "Calculate distance from (0,0)"
    ^ (x squared + y squared) sqrt
```

You see:
- **Method name**: `distanceFromOrigin`
- **Comment**: "Calculate distance from (0,0)"
- **Body**: The actual code

## Creating a Class

Let's create a class using the Browser:

### Step 1: Select a Package

If you don't have a package:
1. Right-click in the Package pane
2. Choose `New package`
3. Name it: `MyApp`
4. Press Enter

Now click on `MyApp`.

### Step 2: View the Class Template

With no class selected, the Code pane shows a template:

```smalltalk
Object subclass: #NameOfSubclass
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'MyApp'
```

### Step 3: Modify the Template

Change it to:

```smalltalk
Object subclass: #Counter
    instanceVariableNames: 'count'
    classVariableNames: ''
    package: 'MyApp'
```

### Step 4: Accept (Save)

- **Right-click** in the Code pane → `Accept` (or `Ctrl+S` / `Cmd+S`)

Pharo compiles your class! `Counter` appears in the Class pane.

## Adding a Method

Now let's add methods to Counter:

### Step 1: Select the Class

Click `Counter` in the Class pane.

### Step 2: Create a Protocol

Right-click in the Protocol pane → `New protocol` → Enter `accessing`.

Or just start typing a method; Pharo will ask which protocol to use.

### Step 3: View the Method Template

With no method selected, the Code pane shows:

```smalltalk
messageSelectorAndArgumentNames
    "comment stating purpose of message"

    | temporary variable names |
    statements
```

### Step 4: Write Your Method

Replace the template with:

```smalltalk
initialize
    super initialize.
    count := 0
```

### Step 5: Accept (Save)

Right-click → `Accept` (or `Ctrl+S` / `Cmd+S`).

Pharo asks which protocol. Choose `initialization` or type a new name.

The method is now compiled and appears in the Method pane!

### Step 6: Add More Methods

Add these methods the same way:

```smalltalk
count
    ^ count
```

```smalltalk
increment
    count := count + 1
```

```smalltalk
decrement
    count := count - 1
```

```smalltalk
reset
    count := 0
```

Now your Counter class has five methods!

## Navigating Quickly

### Finding a Class

Don't scroll through hundreds of classes! Type to filter:

1. Click in the Class pane
2. Start typing: `Ord`
3. The list filters to classes starting with "Ord": `OrderedCollection`, `OrderedDictionary`, etc.

### Finding a Method

Same with methods:

1. Click in the Method pane
2. Start typing: `add`
3. Methods filter to those starting with "add"

### Jump to Any Class

Even faster:

- **Press** `Ctrl+F` (or `Cmd+F`) in the Browser
- **Or** `Ctrl+O` `N` globally
- Type the class name: `OrderedCollection`
- Press Enter

The Browser jumps to that class instantly!

### Jump to Any Method

- **Press** `Ctrl+M` in the Browser (or use Spotter: `Shift+Enter`)
- Type the method name: `add:`
- See all classes that implement `add:`
- Select one to jump to it

## The Class Side

Remember: classes have two sides:

### Instance Side

Methods that instances understand:

```smalltalk
counter := Counter new.
counter increment.  "Instance method"
```

These are what you see by default in the Browser.

### Class Side

Methods that the class itself understands:

```smalltalk
Counter reset.  "Class method"
```

To view class-side methods:

1. Click the `Class side` button (usually near the top or a tab)
2. The Browser switches to show class methods
3. Click `Instance side` to switch back

Example: Add a class-side method to Counter:

1. Select Counter class
2. Click `Class side`
3. Add method:
   ```smalltalk
   withCount: aNumber
       ^ self new count: aNumber; yourself
   ```
4. Accept

Now you can create counters with an initial value:

```smalltalk
Counter withCount: 10
```

## Protocols (Method Categories)

Protocols organize methods by purpose. Common protocols:

### Instance Side Protocols:

- **accessing** - Getters/setters for instance variables
- **testing** - Boolean queries (`isEmpty`, `isValid`, etc.)
- **comparing** - `=`, `hash`, `<`, `>`, etc.
- **converting** - `asString`, `asArray`, etc.
- **printing** - `printOn:`, `printString`
- **initialization** - `initialize`
- **copying** - `copy`, `postCopy`
- **private** - Internal methods not for external use

### Class Side Protocols:

- **instance creation** - Factory methods (`new`, `with:`, etc.)
- **class initialization** - `initialize` for the class

You can create your own protocols! Name them descriptively: `arithmetic`, `validation`, `calculations`, etc.

## Viewing the Hierarchy

Want to see where a class fits in the hierarchy?

### View Superclasses

Right-click on a class → `Browse Hierarchy`

See the entire inheritance chain:

```
ProtoObject
  └─ Object
      └─ Collection
          └─ SequenceableCollection
              └─ OrderedCollection
```

### View Subclasses

Right-click on a class → `Browse Subclasses`

See all classes that inherit from it.

### Flat View

By default, the Browser shows a flat list of classes. To see the hierarchy as a tree:

- Some browsers have a "Hierarchy" view mode
- Or use the Hierarchy Browser (separate tool)

## Finding Implementors and Senders

Two critical navigation features:

### Implementors

**Implementors** are all classes that implement a specific method.

Example: Who implements `add:`?

1. Click on any `add:` method
2. Right-click → `Implementors of 'add:'` (or press `Ctrl+M` `I`)
3. A list opens showing ALL classes with an `add:` method: `OrderedCollection`, `Set`, `Dictionary`, `Array`, etc.

This shows you all the different implementations!

### Senders

**Senders** are all methods that send a specific message.

Example: Who calls `add:`?

1. Click on any `add:` method
2. Right-click → `Senders of 'add:'` (or press `Ctrl+M` `S`)
3. A list opens showing ALL methods that call `add:`

This shows you who uses this method!

### Why This Matters

In traditional editors, finding who calls a method requires text search - slow and inaccurate (can't distinguish `add:` from `addAll:` or `add` in a comment).

In Smalltalk, the Browser knows the code structure. It can instantly find exact matches!

## References

Find all references to a class:

1. Right-click on a class name
2. Choose `References to Class`
3. See everywhere that class is mentioned

Or for instance variables:

1. Right-click in the Code pane on a variable name
2. Choose `Inst var refs` or `Inst var assignments`
3. See everywhere that variable is read or written

## Refactoring

The System Browser has refactoring tools:

### Rename

Rename a method safely:

1. Right-click on the method → `Rename` (or `Ctrl+R` `R`)
2. Enter new name
3. Pharo renames it AND updates all senders!

No broken references!

### Extract Method

Select code, right-click → `Extract method`

Pharo creates a new method and replaces your selection with a call to it.

### More Refactorings

- **Add Parameter** - Add a parameter to a method, updating all senders
- **Remove Parameter** - Remove a parameter
- **Inline Method** - Replace method calls with the method body
- **Extract to Instance Variable** - Turn a temporary variable into an instance variable
- **Push Up/Pull Down** - Move methods up/down the hierarchy

These keep your code clean and safe!

## Comments

### Class Comments

To add a class comment:

1. Select your class
2. Click the `Comments` button or tab
3. Type your comment:
   ```
   Counter is a simple counting object that maintains a count and allows increment, decrement, and reset operations.

   Example usage:
       counter := Counter new.
       counter increment.
       counter count  "Returns 1"
   ```
4. Accept (`Ctrl+S` / `Cmd+S`)

Now anyone browsing your class can read the documentation!

### Method Comments

Methods can have comments too:

```smalltalk
increment
    "Increase the count by 1"
    count := count + 1
```

The first string in a method is treated as a comment.

## Versions

View previous versions of a method:

1. Right-click on a method → `Versions` or `Browse versions`
2. See all historical versions from the Changes file
3. Click on any version to view it
4. Compare versions
5. Revert to an older version if needed

This uses the Changes file (Chapter 17) behind the scenes!

## Searching Within Methods

Find methods containing specific text:

1. Right-click in the Code pane → `Search` → `Method source with it`
2. Enter search text: `sqrt`
3. See all methods containing "sqrt"

Or use Spotter (`Shift+Enter`) and search globally!

## Keyboard Shortcuts

The Browser is keyboard-friendly:

- **Ctrl+S / Cmd+S** - Accept (save)
- **Ctrl+O B** - Open new browser
- **Ctrl+F** - Find class
- **Ctrl+M I** - Implementors
- **Ctrl+M S** - Senders
- **Ctrl+M R** - References
- **Ctrl+R R** - Rename
- **Tab** - Move between panes
- **Type to filter** - In any list pane

Learn these and you'll fly through code!

## Multiple Browsers

You can open multiple System Browsers:

- One browser for your code
- Another for system classes you're studying
- A third for a different package

They're independent and don't interfere with each other.

## Customization

Pharo lets you customize the Browser:

- **Themes** - Change colors
- **Fonts** - Change font size and family
- **Layout** - Rearrange panes
- **Shortcuts** - Customize key bindings

Explore: World menu → `Settings` → `System Browser`

## Practical Example: Exploring a Class

Let's fully explore the `OrderedCollection` class:

### Step 1: Open Browser

`Ctrl+O` `B`

### Step 2: Find the Class

Type `OrderedCollection` (or browse to it)

### Step 3: Read the Class Comment

Click `Comments` button. Read what an OrderedCollection is.

### Step 4: See the Definition

Click `Definition` (or just select the class). See:

```smalltalk
SequenceableCollection subclass: #OrderedCollection
    instanceVariableNames: 'array firstIndex lastIndex'
    ...
```

It has three instance variables. Interesting! It's not just an array - it's more complex.

### Step 5: Browse Methods

Click on protocols:
- `accessing` - How to get elements
- `adding` - How to add elements
- `removing` - How to remove elements
- `enumerating` - How to iterate

Look at methods:
- `add:` - Adds to the end
- `addFirst:` - Adds to the beginning
- `at:` - Gets element at index
- `size` - Returns number of elements

### Step 6: Study an Implementation

Click on `addLast:`:

```smalltalk
addLast: newObject
    "Add newObject to the end of the receiver. Answer newObject"
    lastIndex = array size ifTrue: [ self makeRoomAtLast ].
    lastIndex := lastIndex + 1.
    array at: lastIndex put: newObject.
    ^ newObject
```

Aha! It uses an internal array and tracks the last index. When the array is full, it calls `makeRoomAtLast` to expand.

### Step 7: Find Senders

Right-click on `addLast:` → `Senders`

See everywhere in the system that calls `addLast:`.

### Step 8: Find Implementors

Right-click → `Implementors of 'addLast:'`

See other classes that implement `addLast:` (like `LinkedList`).

### Step 9: Explore Superclass

Click `SequenceableCollection` (the superclass).

See what methods are inherited.

Now you deeply understand OrderedCollection!

## Best Practices

### 1. Organize with Protocols

Group related methods into protocols. Don't dump everything into one protocol.

### 2. Write Class Comments

Document what your class does, its responsibilities, and example usage.

### 3. Write Method Comments

Add comments to complex methods. Explain the "why", not the "what".

### 4. Use Meaningful Names

Method names should be self-explanatory. `calculateMonthlyPayment` is better than `calc`.

### 5. Keep Methods Short

If a method is longer than one screen, consider extracting parts into helper methods.

### 6. Browse Before Writing

Before implementing something, browse the system. Maybe it already exists!

### 7. Study System Classes

The best way to learn good Smalltalk style is to read system classes. Browse `Collection`, `Stream`, `Object`, etc.

## Common Workflows

### Implementing a New Feature

1. Create/select class
2. Add methods in appropriate protocols
3. Test in Playground or with unit tests
4. Refactor as needed

### Understanding Existing Code

1. Find the class
2. Read class comment
3. Browse methods by protocol
4. Find implementors and senders to understand usage
5. Trace through method calls

### Debugging

1. Find where the error occurred (from the stack trace)
2. Browse to that method
3. Understand what it does
4. Find senders to see how it's called
5. Fix the method

### Refactoring

1. Select code to refactor
2. Use Browser's refactoring tools
3. Rename, extract, inline as needed
4. Let Pharo update all references automatically

## Advanced Features

### Testing from the Browser

Some browsers integrate with testing:

1. Right-click on a class → `Run tests`
2. Tests run and results appear

### Filtering Methods

Show only methods matching criteria:

- Instance side only
- Class side only
- Methods beginning with...
- Methods containing...

### Method Pane Icons

Icons in the Method pane indicate:
- **Red dot** - Method overrides superclass
- **Green arrow** - Method is new in this class
- **Triangle** - Method is abstract (calls `subclassResponsibility`)

(Icon specifics vary by Smalltalk version.)

### Syntax Highlighting

The Code pane highlights:
- **Keywords** in bold or color
- **Strings** in a different color
- **Comments** in italic or grey
- **Self/super** highlighted

### Auto-Completion

Start typing and press Tab or Ctrl+Space:
- Method names auto-complete
- Variable names auto-complete
- Templates inserted

## Try This!

Practice with the System Browser:

1. **Explore a system class:**
   - Open Browser
   - Navigate to `Dictionary`
   - Read the class comment
   - Browse `at:put:`, `at:`, `keys`, `values`
   - Find implementors of `at:`
   - Find senders of `at:put:`

2. **Create a Book class:**
   ```smalltalk
   Object subclass: #Book
       instanceVariableNames: 'title author isbn pages'
       ...
   ```

   Add methods:
   - `initialize`
   - `title`, `title:`
   - `author`, `author:`
   - `isbn`, `isbn:`
   - `pages`, `pages:`
   - `description` (returns a formatted string)

3. **Add a class-side factory method:**
   ```smalltalk
   "Class side:"
   title: aTitle author: anAuthor
       ^ self new
           title: aTitle;
           author: anAuthor;
           yourself
   ```

4. **Test your class:**
   ```smalltalk
   book := Book title: 'Learn Programming with Smalltalk' author: 'You'.
   book description
   ```

5. **Refactor:**
   - Rename the `description` method to `summary`
   - Use the Browser's refactoring tools
   - Watch it update automatically!

6. **Find who uses your class:**
   - Right-click on Book → `References to Class`
   - See everywhere it's mentioned

7. **Browse the hierarchy:**
   - Right-click on OrderedCollection → `Browse Hierarchy`
   - Explore the inheritance tree

8. **Compare versions:**
   - Modify a method several times
   - Right-click → `Versions`
   - See all versions
   - Revert to an older one

## Common Mistakes

### Forgetting to Accept

You edited code but didn't press `Ctrl+S` / `Cmd+S`. The changes weren't saved!

Always accept your changes.

### Editing the Wrong Method

You meant to edit `YourClass>>foo` but edited `SomeOtherClass>>foo`.

Double-check which class is selected before editing.

### Breaking Syntax

Syntax error? The Browser won't accept the method. Fix the error and try again.

### Lost in the Code

Opened too many browsers? Close unused ones to reduce clutter.

## Looking Ahead

You now understand the System Browser - your primary code navigation and editing tool! You can:
- Browse packages, classes, protocols, and methods
- Create classes and methods
- Find implementors and senders
- Navigate quickly with shortcuts
- Refactor code safely
- View versions and comments

In Chapter 20, we'll explore the **Inspector and Explorer** - tools for examining live objects and their state. These tools let you interact with running objects, modify their values, and understand their structure.

Then in Chapter 21, you'll meet the **Debugger** - possibly the most revolutionary tool in Smalltalk, letting you fix bugs while the program is running!

Part VI is all about the tools that make Smalltalk development a joy. Master these tools and you'll be incredibly productive!

---

**Key Takeaways:**
- The **System Browser** is your primary code navigation tool
- Five panes: Package, Class, Protocol, Method, Code
- Navigate by clicking through the panes
- Type to filter in any list pane
- **Accept** (Ctrl+S / Cmd+S) saves changes
- **Implementors** shows all classes implementing a method
- **Senders** shows all methods calling a method
- Use **Instance side** and **Class side** buttons to switch
- **Protocols** organize methods by category
- **Refactoring tools** rename, extract, and restructure safely
- **Versions** show historical changes from the Changes file
- Multiple browsers can be open simultaneously
- Keyboard shortcuts speed up navigation dramatically
- Browse system classes to learn good Smalltalk style
- The Browser is structured around Smalltalk's organization
- Understanding the hierarchy helps understand the code

---

[Previous: Chapter 18 - Version Control for Smalltalkers](chapter-18-version-control.md) | [Next: Chapter 20 - The Inspector and Explorer](chapter-20-inspector-and-explorer.md)
