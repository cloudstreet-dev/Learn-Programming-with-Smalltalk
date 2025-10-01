# Chapter 16: Understanding the Image

Now that you've learned the fundamentals of object-oriented programming in Smalltalk, it's time to understand one of Smalltalk's most unique and powerful features: **the Image**.

The Image is what makes Smalltalk different from almost every other programming environment. It's a snapshot of living, breathing objects - your entire development environment, frozen in time and ready to resume exactly where you left off.

Understanding the Image is crucial to mastering Smalltalk. It fundamentally changes how you think about saving, loading, and working with code.

## What is the Image?

The **Image** is a binary file containing a complete snapshot of all objects in memory at a specific point in time.

Think of it like:
- **Hibernation for your computer** - Everything stops exactly where it is and resumes later
- **A save game** - Your entire game state saved to continue later
- **A photograph** - Capturing a moment in time that you can return to

In the Image:
- Every object you created
- Every class you defined
- Every method you wrote
- Every variable value
- Every open window and tool
- The entire Smalltalk system itself

**Everything** is in the Image.

## The Three Files

When you run Smalltalk, you actually work with three files:

### 1. The Virtual Machine (VM)

**File**: `Pharo.exe` (Windows), `Pharo.app` (macOS), `pharo` (Linux)

This is the executable - the program that runs Smalltalk. It's written in C and provides:
- Memory management
- Garbage collection
- Just-in-time compilation
- OS integration (files, network, graphics)
- Image loading and saving

The VM doesn't contain your code - it's just the engine. You rarely modify or think about the VM.

### 2. The Image

**File**: `Pharo.image` (or similar)

This is the snapshot of all objects. It's a binary file, typically 40-100 MB (or larger), containing:
- All class definitions
- All method bytecode
- All objects in memory
- Tool windows and their state
- Your code and data

**This is your working environment.**

### 3. The Changes File

**File**: `Pharo.changes` (or similar)

This is a text file recording every change you make, in chronological order:
- Every method you define or modify
- Every class you create or change
- Timestamps for each change

Think of it as a transaction log or undo buffer.

We'll explore the changes file in depth in Chapter 17.

## How the Image Works

### Starting Smalltalk

When you double-click the Pharo application or run it:

1. The **VM** (virtual machine) starts
2. The VM looks for a `.image` file
3. The VM loads the image into memory
4. All objects come to life!
5. You see the Smalltalk environment exactly as it was when last saved

It's like waking up from sleep - everything is exactly where you left it.

### Working in Smalltalk

As you work:
- You create objects in memory
- You define classes and methods
- You open tools and windows
- You execute code in the Playground

All of this happens **in memory**, in the Image.

### Saving the Image

When you save the Image:

1. All objects in memory are written to the `.image` file
2. The current state is completely captured
3. The `.changes` file is also updated with recent changes

**Note**: In Pharo and most modern Smalltalks, the Image is automatically saved periodically or when you quit.

### Quitting and Resuming

When you quit Smalltalk:
- The Image is saved (if auto-save is enabled)
- The VM shuts down
- All memory is freed

When you start again:
- The VM loads the saved Image
- Everything resumes exactly as it was
- Your open windows, your code, your objects - all back!

## A Concrete Example

Let's trace what happens:

**Monday morning:**
```smalltalk
"Open Pharo, see a fresh image"
| counter |
counter := 0.
```

You create a Counter class with a class variable `GlobalCount`:

```smalltalk
Object subclass: #Counter
    instanceVariableNames: 'count'
    classVariableNames: 'GlobalCount'
    package: 'MyFirstClasses'
```

In the Playground:

```smalltalk
Counter GlobalCount: 100.
Counter GlobalCount  "Returns 100"
```

You quit Pharo. The Image is saved.

**Tuesday morning:**
```smalltalk
"Start Pharo again"
Counter GlobalCount  "Still returns 100!"
```

The Counter class exists, with `GlobalCount` still set to 100. Nothing was "loaded" from source files - the objects never left memory (from their perspective). They were just frozen and then thawed.

## The Image Contains EVERYTHING

Let me emphasize: **everything** is in the Image.

Open a System Browser:

```smalltalk
"Press Ctrl+O B (or Cmd+O B)"
```

Browse to the String class. Look at the `asUppercase` method. You see the code, right?

**That code is in the Image.** The String class is an object, stored in the Image. Its methods are objects, stored in the Image.

Open an Inspector:

```smalltalk
'hello' inspect
```

That Inspector window is an object, stored in the Image. When you save, the Inspector is saved. When you load, it comes back!

## Image-Based Development

This is fundamentally different from traditional programming:

### Traditional Programming:
1. Write code in text files
2. Save files to disk
3. Compile files to executable
4. Run executable
5. Executable starts fresh each time

### Image-Based Programming:
1. Modify living objects in memory
2. Test immediately (they're already running!)
3. Save the entire world to disk
4. Resume the entire world later
5. Environment continues from where it left off

## Benefits of the Image

### 1. Instant Persistence

Everything you create automatically persists:

```smalltalk
| myData |
myData := OrderedCollection new.
10 timesRepeat: [ myData add: Dictionary new ].
```

If you save the image (or it auto-saves), `myData` and all those dictionaries are preserved!

(Well, almost - temporary variables in the Playground are lost, but global variables and class variables persist.)

### 2. No "Loading" Code

There's no `import`, `require`, `#include`, or `using`. All classes are always available because they're all in memory:

```smalltalk
OrderedCollection new  "Always available"
String new  "Always available"
MyCustomClass new  "Available if you defined it"
```

### 3. Incremental Development

Change one method, test immediately. No recompile, no restart, no waiting:

```smalltalk
"Edit a method in System Browser"
"Click to save"
"Test it immediately"
"Edit again"
"Test again"
```

All in seconds!

### 4. Live Debugging

When you hit a debugger, you're debugging the actual running Image. You can:
- Modify methods while debugging
- Continue execution with the new method
- Change variable values
- Resume or restart

The program never stops - you're modifying a living system!

### 5. Time Travel

Because the changes file records every change, you can:
- Recover deleted methods
- See what a class looked like yesterday
- Undo changes you made
- Review your development history

We'll cover this in Chapter 17.

## Working with Images

### Creating a New Image

Start with a fresh image:

1. Download Pharo from pharo.org
2. Extract the zip file
3. You get `Pharo.exe` (VM) and `Pharo.image` (fresh image)
4. Double-click Pharo.exe
5. A new window opens with a clean Smalltalk environment

### Saving Your Image

In Pharo:

- **Save**: World menu → `Save` (or the image saves automatically periodically)
- **Save As**: World menu → `Save as...` → Enter new name

The `.image` file and its corresponding `.changes` file are saved.

### Loading an Image

Just double-click a `.image` file, or drag it onto the VM executable.

### Multiple Images

You can have multiple images for different projects:

```
projects/
  project-a/
    project-a.image
    project-a.changes
  project-b/
    project-b.image
    project-b.changes
  experiments/
    experiments.image
    experiments.changes
```

Each image is completely independent - different code, different state, different everything.

### Sharing Images

You can share an image with someone:

1. Copy the `.image` file (and optionally the `.changes` file)
2. Send it to them
3. They open it with their VM
4. They see exactly what you saw!

**Warning**: Images are binary and platform-independent, but:
- They're large (40-100+ MB)
- They contain everything (including experimental code, open windows, etc.)
- Better to share code via version control (Chapter 18)

### Image Size

Images grow over time as you:
- Create more classes
- Define more methods
- Create more objects
- Open more tools

Typical sizes:
- **Fresh Pharo image**: 40-50 MB
- **Worked-in image**: 50-80 MB
- **Large project**: 80-150 MB
- **Image with data**: Can be gigabytes!

## What's NOT in the Image

A few things are not in the image:

### External Files

Files on disk are not in the image. If you read a file:

```smalltalk
'/Users/me/data.txt' asFileReference contents
```

The file path is in the image, but the actual file is not. If you move the image to another computer without the file, it won't be found.

### Network Connections

If you open a network connection, it's not persisted:

```smalltalk
| socket |
socket := SocketStream openConnectionToHost: 'example.com' port: 80.
```

After saving and loading, that connection is closed.

### OS Resources

OS-level resources (file handles, window handles, etc.) are not persisted. Smalltalk handles this transparently - resources are recreated when needed.

### Playground Temporary Variables

Temporary variables in the Playground don't persist:

```smalltalk
| temp |
temp := 42.
"Save and quit"
"Start again"
temp  "Error - temp doesn't exist"
```

Use Workspace variables or class variables for persistent data.

## The Image is Your Source

In traditional systems, source code is in text files. In Smalltalk, **the Image is your source**.

All class definitions and method code are in the Image. When you browse code in the System Browser, you're viewing objects from the Image.

### Where's the Text?

"But wait," you might ask, "where are the text files with my code?"

**There aren't any** (by default).

Your code lives as objects in the Image:
- Classes are objects
- Methods are objects containing bytecode
- The System Browser shows you those objects

### Decompiling

When you view a method in the browser, Smalltalk decompiles the bytecode back into source code:

1. The method object contains bytecode
2. The System Browser asks: "What's your source code?"
3. The method decompiles itself back to text
4. You see readable code

Usually, the original source is preserved (in the sources or changes file), but the Image could reconstruct it if needed.

## The Image as a Database

Think of the Image as an object database:

- Classes are records
- Methods are fields
- Objects are data
- It's all persisted together

You can treat it as a simple database:

```smalltalk
Object subclass: #Customer
    instanceVariableNames: 'name email'
    classVariableNames: 'AllCustomers'
    package: 'MyApp'
```

```smalltalk
"Class side:"
initialize
    AllCustomers := OrderedCollection new

add: aCustomer
    AllCustomers add: aCustomer

all
    ^ AllCustomers
```

```smalltalk
"Instance side:"
name: aName email: anEmail
    name := aName.
    email := anEmail.
    self class add: self.
    ^ self
```

Usage:

```smalltalk
Customer name: 'Alice' email: 'alice@example.com'.
Customer name: 'Bob' email: 'bob@example.com'.
Customer all  "Returns all customers"
```

When you save the Image, all customers are saved too!

**Note**: For large datasets or multi-user apps, use a real database. But for small apps or prototypes, the Image works great.

## Image Corruption

Since the Image is binary, it can become corrupted if:
- Pharo crashes during save
- Disk runs out of space
- File system error

**Prevention**:
- Keep backups of your image
- Use version control for code (Chapter 18)
- Pharo auto-saves periodically
- The changes file can recover code (Chapter 17)

**Recovery**:
If an image is corrupted:
1. Start with a fresh image
2. Recover code from the changes file (Chapter 17)
3. Or restore from a backup

## Cleaning Up the Image

Over time, images accumulate:
- Experimental code
- Unused classes
- Open windows
- Old objects in memory

### Manual Cleanup

```smalltalk
"Remove a class:"
MyClass removeFromSystem

"Close all windows except the World:"
World submorphs do: [ :each | each delete ]

"Run garbage collection:"
Smalltalk garbageCollect
```

### Starting Fresh

If your image is cluttered:

1. Export your code to a Git repository (Chapter 18)
2. Start with a fresh image
3. Load your code from Git
4. Clean slate!

## Advanced: Image Customization

You can customize your image:

### Startup Actions

Have code run every time the image loads:

```smalltalk
"Class side of a class:"
initialize
    SessionManager default registerSystemClassNamed: self name

startUp: resuming
    resuming ifTrue: [
        Transcript show: 'Hello! The image has started.'; cr ]
```

### Default Windows

Position windows where you want them, then save. They'll be there next time!

### Preferences

Change settings (fonts, colors, etc.) and save. Your preferences persist.

### Tools and Extensions

Install tools (from Iceberg, the Pharo Catalog, etc.). They're added to the Image and persist.

## Comparing to Other Systems

### Traditional (C, Java, Python):
```
source.c → compile → executable
                       ↓
                   run (fresh start every time)
```

### Image-Based (Smalltalk):
```
living objects in memory ←→ save to image file
                           ←→ load from image file
          ↓
      modify objects
          ↓
    (repeat forever)
```

### Modern Hybrid (Rails, Django):
```
source.rb → run server
              ↓
          database (persistent data)
          code (reloaded on change)
```

Smalltalk is closest to the hybrid, but the Image combines both code and data persistence.

## Philosophical Implications

The Image fundamentally changes programming:

### No "Build" Step

There's no separation between "writing code" and "running code". You're always running. You modify the running system.

### No "Restart"

There's no "restart the app to see changes". Changes are immediate. The app is always running.

### Continuous Evolution

Your image evolves continuously. It's never "finished" - it's always in progress, always alive.

### Living Documentation

The Image is self-documenting. The code is right there, readable, modifiable. You're always one click away from seeing how something works.

## Practical Tips

### 1. Save Often (or Enable Auto-Save)

In Pharo, auto-save is usually enabled. But you can manually save before risky operations:

```smalltalk
"World menu → Save"
```

### 2. Make Backup Images

Before major changes:

```
cp my-project.image my-project-backup.image
cp my-project.changes my-project-backup.changes
```

Or use the "Save as..." feature.

### 3. Keep Images Small

Don't load unnecessary packages. Don't accumulate dead code. Start fresh periodically.

### 4. One Image per Project

Don't mix projects in one image. Use separate images for separate concerns.

### 5. Use Git for Code

Export code to Git repositories (Chapter 18). Images are for working; Git is for sharing and versioning.

### 6. Don't Share Raw Images

Share code via Git, not by emailing images. Images are too large and contain too much junk.

### 7. Experiment Freely

Because you can always recover from the changes file or revert to a backup, experiment freely! Try things! Break things! Learn!

## Common Mistakes

### Losing Work by Not Saving

If Pharo crashes and you didn't save recently, you lose work. Enable auto-save!

### Sharing Images Instead of Code

Don't email a 100 MB image. Use Git!

### Mixing Projects in One Image

Keep separate images for separate projects. Avoids confusion and bloat.

### Not Backing Up

Make backups before major changes or experiments.

### Treating the Image Like Source Files

The Image is not "the source code" in the traditional sense. Use version control (Chapter 18) to manage code properly.

## Try This!

Explore the Image:

1. **Create some persistent data:**
   ```smalltalk
   Object subclass: #Settings
       instanceVariableNames: ''
       classVariableNames: 'Username Theme'
       package: 'MyApp'
   ```

   Set values:
   ```smalltalk
   Settings classVarNamed: #Username put: 'Alice'.
   Settings classVarNamed: #Theme put: 'Dark'.
   ```

   Save the image (World menu → Save or wait for auto-save).

   Quit Pharo and restart.

   Check the values:
   ```smalltalk
   Settings classVarNamed: #Username.  "Still 'Alice'!"
   Settings classVarNamed: #Theme.  "Still 'Dark'!"
   ```

2. **Experiment with image size:**
   ```smalltalk
   | filePath |
   filePath := Smalltalk imageDirectory / Smalltalk imageName.
   filePath size / 1024 / 1024  "Size in megabytes"
   ```

3. **Open windows and save:**
   Open several System Browsers, Inspectors, and Playgrounds. Position them where you like. Save the image. Quit and restart. Your windows are back!

4. **Create a backup:**
   World menu → `Save as...` → Enter "my-backup.image"

   Now you have two images. Open each one - they're independent!

5. **Explore system classes:**
   ```smalltalk
   Smalltalk allClasses size  "How many classes in the image?"
   ```

   ```smalltalk
   Smalltalk allClasses select: [ :cls | cls name beginsWith: 'Test' ]
   "All test classes"
   ```

   ```smalltalk
   String allSelectors size  "How many methods does String have?"
   ```

6. **Force garbage collection:**
   ```smalltalk
   | before after |
   before := Smalltalk vm totalFreeSpace.
   Smalltalk garbageCollect.
   after := Smalltalk vm totalFreeSpace.
   (after - before) / 1024 / 1024  "Memory freed in MB"
   ```

## Looking Ahead

You now understand the Image - Smalltalk's persistent object memory. You know:
- The Image is a snapshot of all objects
- It's loaded by the VM
- Everything you create lives in the Image
- Saving the Image persists everything
- Each project can have its own Image

In Chapter 17, we'll explore the **Changes File** - the chronological log of all your modifications. You'll learn:
- How changes are recorded
- How to recover lost code
- How to browse history
- How to undo mistakes

Then in Chapter 18, we'll cover **Version Control for Smalltalkers** - how to use Git with Smalltalk to share code, collaborate, and manage versions properly.

Part V (The Image, Changes, and Sources) demystifies Smalltalk's unique persistence model. Once you understand these three files, you'll be comfortable managing your Smalltalk projects!

---

**Key Takeaways:**
- The **Image** is a binary snapshot of all objects in memory
- The **VM** (virtual machine) loads and runs the Image
- The **Changes file** logs all modifications chronologically
- Image-based development means modifying living objects
- Everything persists: classes, methods, objects, open windows
- No separate "compile" or "run" steps - always running
- Save the Image to preserve your work (often auto-saved)
- Each project should have its own Image
- Images are large; use Git for sharing code, not raw images
- The Image is your development environment and object database
- Traditional source files don't exist (by default)
- You can have multiple independent images
- Back up images before risky operations
- The Image enables instant persistence and incremental development
- Image corruption is rare but recoverable from changes file

---

[Previous: Chapter 15 - Inheritance](chapter-15-inheritance.md) | [Next: Chapter 17 - Changes and Sources Files](chapter-17-changes-and-sources-files.md)
