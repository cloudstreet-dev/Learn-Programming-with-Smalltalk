# Chapter 17: Changes and Sources Files

In Chapter 16, you learned about the Image - Smalltalk's persistent object memory. But the Image is only one piece of the puzzle. In this chapter, we'll explore the other two crucial files: **Changes** and **Sources**.

These files work together to give you a complete history of your development, enable recovery from crashes, and let you see the source code for every class and method in the system.

Understanding these files transforms you from a user of Smalltalk into someone who can confidently manage, recover, and explore Smalltalk systems.

## The Three-File System Revisited

Recall the three files:

1. **Image** (.image) - Binary snapshot of all objects
2. **Changes** (.changes) - Text log of all your modifications
3. **Sources** (.sources) - Text archive of system source code

Together, they form a complete system:
- **Image** = current state
- **Changes** = your development history
- **Sources** = original system code

## What is the Changes File?

The **Changes file** is a text file that records every modification you make to the system, in chronological order, with timestamps.

Think of it like:
- **A journal** - Recording what you did and when
- **An undo buffer** - You can go back and see previous versions
- **A backup** - If the Image corrupts, you can recover your code
- **An audit log** - Every change is recorded, nothing is lost

### What Gets Recorded?

The Changes file records:
- Every method you define or modify
- Every class you create or change
- Every method you delete
- Timestamps for each change
- Even failed attempts (syntax errors)!

### Example Changes File Entry

Here's what an entry looks like:

```
!Point2D methodsFor: 'accessing' stamp: 'Alice 1/15/2024 10:23:45'!
x
	"Return the x coordinate"
	^ x! !

!Point2D methodsFor: 'accessing' stamp: 'Alice 1/15/2024 10:24:12'!
y
	"Return the y coordinate"
	^ y! !

!Point2D methodsFor: 'arithmetic' stamp: 'Alice 1/15/2024 10:25:03'!
distanceFromOrigin
	"Calculate distance from (0,0)"
	^ (x squared + y squared) sqrt! !
```

Each entry shows:
- The class and protocol (category)
- The timestamp and developer name
- The complete method source code

This is **human-readable text**!

## What is the Sources File?

The **Sources file** is a text file containing the source code for all system classes and methods that come with Smalltalk.

Think of it like:
- **The system library source** - All the built-in classes
- **Read-only** - You don't modify it; it's the original system code
- **Shared** - Multiple images can share the same sources file

### System vs User Code

- **System code** (String, Array, OrderedCollection, etc.) → Sources file
- **Your code** (your classes and methods) → Changes file

When you browse the `String>>asUppercase` method, Smalltalk reads from the Sources file. When you browse your own `Point2D>>distanceFromOrigin` method, it reads from the Changes file.

## How They Work Together

### Browsing Code

When you open a System Browser and view a method:

1. The Image contains the method object (with bytecode)
2. The method object knows where its source code is stored
3. For system methods: Read from the Sources file at a specific position
4. For your methods: Read from the Changes file at a specific position
5. Display the source code to you

### Modifying Code

When you modify a method:

1. You edit code in the System Browser
2. You save (accept) the method
3. The new version is compiled into bytecode
4. The bytecode is stored in the Image
5. The source code is appended to the Changes file
6. The method object records where in the Changes file its source is

### Multiple Versions

The Changes file is **append-only**. Old versions remain:

```
!Counter methodsFor: 'accessing' stamp: 'Alice 1/15/2024 09:00'!
increment
	count := count + 1! !

!Counter methodsFor: 'accessing' stamp: 'Alice 1/15/2024 11:30'!
increment
	count := count + 1.
	^ self! !

!Counter methodsFor: 'accessing' stamp: 'Alice 1/15/2024 14:45'!
increment
	count := count + 1.
	Transcript show: 'Incremented to ', count printString; cr.
	^ self! !
```

Three versions of `increment`, all preserved!

The Image only points to the **latest** version, but you can browse earlier versions through the changes file.

## Browsing the Changes File

You can view and search the Changes file directly:

### Opening the Changes File

In Pharo, there are several ways:

1. **World menu** → `Tools` → `Change Sorter`
2. Or evaluate:
   ```smalltalk
   ChangeSorter open
   ```

The Change Sorter shows all changes in chronological order.

### Searching for Changes

Find all changes to a specific method:

```smalltalk
"Right-click in System Browser on a method → Versions"
```

This shows all versions of that method from the Changes file:

```
3:45 PM - Made it return self
11:30 AM - Simplified implementation
9:00 AM - Initial version
```

You can view any version, compare versions, and even revert to an older version!

### Browsing Recent Changes

See what you did recently:

```smalltalk
"World menu → Tools → Recent Changes"
```

Shows your recent modifications with timestamps.

### Filtering Changes

The Change Sorter lets you:
- Filter by date range
- Filter by class
- Filter by author
- Search for specific text

## Recovering Lost Code

This is where the Changes file shines: **recovering from disasters**.

### Scenario 1: Accidental Deletion

You accidentally delete a method:

```smalltalk
"Oops, deleted Counter>>increment by mistake!"
```

**Recovery:**

1. Right-click on Counter class → `Versions of method` → `increment`
2. Select the latest version before deletion
3. Click `Revert` or `File in`
4. The method is back!

The Changes file still has the source code, even though you deleted the method from the Image.

### Scenario 2: Image Corruption

Your Image file becomes corrupted and won't load.

**Recovery:**

1. Start with a fresh Image
2. Open the Change Sorter
3. Point it to your old Changes file
4. Filter to show only your classes
5. File in all your changes
6. Your code is restored!

This is why the Changes file is so valuable - it's an independent backup of your code.

### Scenario 3: Want to Undo Many Changes

You've been experimenting and made a mess. Want to go back to this morning?

1. Open the Change Sorter
2. Filter to changes after 9:00 AM today
3. Review what changed
4. Selectively revert changes
5. Or: revert to a saved image from this morning (if you have a backup)

## Practical Examples

### Example 1: View Method History

Let's trace a method's evolution:

```smalltalk
"Create a method:"
Object subclass: #Calculator
    instanceVariableNames: 'result'
    ...

add: number
    result := result + number
```

Save it. Note the time: 10:00 AM.

Later, modify it:

```smalltalk
add: number
    result := result + number.
    ^ result
```

Save it. Time: 10:30 AM.

Modify again:

```smalltalk
add: number
    "Add a number to the result"
    result := result + number.
    ^ result
```

Save it. Time: 11:00 AM.

Now view history:
1. Right-click on `add:` method in Browser
2. Choose `Versions` or `Browse Versions`
3. See all three versions with timestamps

You can click on each to view the code, compare them, or revert.

### Example 2: Recover After Crash

Imagine you've been working for hours. Pharo crashes before auto-save. Disaster!

But wait - the Changes file has everything:

1. Start Pharo with a fresh or old Image
2. Tools → Change Sorter
3. The Changes file contains all your work!
4. File in your changes since the last good save
5. Your code is restored

**Note**: You might lose some non-code data (global variables, objects you created in Playground), but your class definitions and methods are safe!

### Example 3: See Who Changed What

In a team, multiple developers work on the same codebase:

```smalltalk
"Look at a method:"
!String methodsFor: 'converting' stamp: 'Bob 1/15/2024 14:20'!
asUppercase
    "New implementation by Bob"
    ...
```

The stamp shows Bob changed this method on January 15 at 2:20 PM.

You can track who made what changes and when.

## The Sources File in Detail

The Sources file is simpler - it's a read-only archive of system code.

### Finding the Sources File

It's typically named `PharoVXX.sources` (where XX is the version number, like `PharoV110.sources`).

It's usually in the same directory as the Image, or in a shared location.

### When is it Used?

Every time you browse a system method (like `String>>asUppercase`), Smalltalk reads from the Sources file.

### Can You Edit It?

No, and you shouldn't! It's the original source code. Your changes go in the Changes file.

### Multiple Images, One Sources

Several images can share the same Sources file:

```
pharo-v10.sources (shared)

projects/
  project-a/
    project-a.image
    project-a.changes
  project-b/
    project-b.image
    project-b.changes
```

Both images reference the same Sources file, saving disk space.

## File Format Details

### Changes File Format

The Changes file is plain text with a specific format:

```
!ClassName methodsFor: 'protocol' stamp: 'author date time'!
methodName
    "Comment"
    methodBody! !
```

- `!ClassName methodsFor: 'protocol'` - Declares class and category
- `stamp: 'author date time'` - Timestamp
- `methodBody` - The actual code
- `! !` - End marker

You can read and edit this file with any text editor! (But be careful - it's easy to break the format.)

### Sources File Format

Same format as Changes, but for system code, and it's indexed for fast lookup.

## Practical Uses

### 1. Code Review

Review what changed today:

```smalltalk
"Tools → Recent Changes"
"Filter by date"
```

Great for end-of-day review or preparing commit messages.

### 2. Archeology

Discover why a method was written a certain way:

```smalltalk
"Browse versions"
"Read old comments and implementations"
```

Understand the evolution of the code.

### 3. Blame/Attribution

Find out who wrote a method:

```smalltalk
"Look at the stamp in the method definition"
```

### 4. Undo Tree

Unlike a simple undo buffer, the Changes file is a complete history. You can:
- See all attempts, including dead ends
- Compare different approaches
- Cherry-pick good ideas from old versions

### 5. Backup

Regular backups of the Changes file protect your code:

```bash
cp my-project.changes my-project.changes.backup
```

Even if the Image corrupts, your code is safe.

## Advanced Topics

### Compressing Changes Files

Changes files grow over time. Compressing them removes old obsolete entries:

In Pharo:
```smalltalk
"World menu → System → Save and Clean Up"
```

This removes old method versions that are no longer useful, keeping the file manageable.

### Splitting Changes Files

For very large projects, you might split the Changes file:

1. Export code to Git (Chapter 18)
2. Start with a fresh Image
3. Load code from Git
4. New Changes file starts fresh

### Changes Files and Version Control

When using Git (Chapter 18):
- **Don't commit** the Changes file to Git
- **Don't commit** the Image file to Git
- **Do commit** your code (exported as .st or using Iceberg)

The Changes file is personal history; Git is team history.

## Common Scenarios

### "I Deleted My Class!"

No problem:

1. Tools → Change Sorter
2. Find the class definition
3. File it in again

Or:

1. Browse any method from that class in the Changes file
2. File in the methods
3. Recreate the class

### "Pharo Crashed and I Lost Hours of Work!"

Not lost:

1. Start Pharo
2. Tools → Change Sorter
3. File in changes since your last save
4. Your methods are back

(But objects created in the Playground might be lost - use class variables or instance variables for persistent data.)

### "I Want to See What I Did Yesterday"

Easy:

1. Tools → Recent Changes
2. Filter by date: yesterday
3. Review all changes

### "Someone Changed My Method!"

Check the stamp:

```smalltalk
!MyClass methodsFor: 'accessing' stamp: 'Bob 1/16/2024 09:15'!
```

Bob changed it on January 16 at 9:15 AM. View versions to see what changed.

## Limitations

### 1. Not a Full VCS

The Changes file is not a replacement for Git or other version control:
- No branches
- No merging
- No collaboration features
- Hard to compare versions across the team

Use Git for real version control (Chapter 18).

### 2. Grows Forever

Unless you compress it, the Changes file keeps growing. It's append-only, so old cruft accumulates.

### 3. Manual Exploration

Browsing the Changes file manually is tedious for large histories. Git log and diff tools are much better for analysis.

### 4. Personal History Only

The Changes file reflects one developer's work on one Image. It doesn't integrate team changes.

## Best Practices

### 1. Keep the Changes File

Don't delete it! It's your safety net.

### 2. Backup Regularly

```bash
cp my-project.changes my-project.changes.$(date +%Y%m%d)
```

### 3. Use Git for Team Code

Changes file = personal history. Git = team history.

### 4. Review Changes Before Committing

Use Recent Changes to review what you did, then commit to Git:

```smalltalk
"Tools → Recent Changes"
"Review what changed today"
"Then commit to Git via Iceberg"
```

### 5. Compress Periodically

If your Changes file is > 100 MB, consider compressing or starting fresh:

```smalltalk
"Save and Clean Up from World menu"
```

Or start with a fresh Image and load code from Git.

### 6. Don't Edit Manually

Don't open the Changes file in a text editor and edit it. Use Smalltalk tools! Manual editing can corrupt the file.

## Try This!

Explore the Changes and Sources files:

1. **Create and modify a method several times:**
   ```smalltalk
   Object subclass: #Greeter
       instanceVariableNames: ''
       ...

   greet
       ^ 'Hello'
   ```

   Save. Wait a minute. Modify:

   ```smalltalk
   greet
       ^ 'Hello, World'
   ```

   Save. Wait. Modify again:

   ```smalltalk
   greet
       ^ 'Hello, World!'
   ```

   Save. Now view versions (right-click on method → Versions). See all three!

2. **Open the Change Sorter:**
   ```smalltalk
   ChangeSorter open
   ```

   Browse recent changes. Find your `greet` method entries.

3. **Inspect a method's source pointer:**
   ```smalltalk
   (Greeter >> #greet) sourcePointer
   ```

   This is the byte offset in the Changes file where the source code is stored.

4. **View system code from Sources:**
   Browse `String>>asUppercase` in the System Browser. This code comes from the Sources file.

5. **Simulate recovery:**
   Create a class:
   ```smalltalk
   Object subclass: #TestRecovery
       instanceVariableNames: 'data'
       ...

   data
       ^ data

   data: anObject
       data := anObject
   ```

   Save. Now delete the class:
   ```smalltalk
   TestRecovery removeFromSystem
   ```

   Gone! Now recover it:
   - Tools → Change Sorter
   - Find TestRecovery entries
   - File them in
   - Class is back!

6. **Check Changes file size:**
   ```smalltalk
   | changesFile |
   changesFile := Smalltalk changesName asFileReference.
   changesFile size / 1024 / 1024  "Size in MB"
   ```

7. **See who wrote a system method:**
   Browse `Collection>>do:` and look at the stamp. Likely someone like "nice" or "marcus" (system developers).

## The Philosophy

The Changes and Sources files embody Smalltalk's philosophy:

### Nothing is Ever Lost

Every keystroke is recorded. You can always go back. Mistakes are recoverable.

### Transparency

You can see the source code for everything - even system classes. No hidden binaries, no proprietary libraries. It's all text, all readable.

### History Matters

Code evolves. Seeing how it evolved helps understand why it is the way it is.

### Live System, Safe Experimentation

Because everything is recorded, you can experiment freely. Try things! Break things! You can always recover.

## Looking Ahead

You now understand the Changes and Sources files - Smalltalk's chronological logs of code. You know:
- The Changes file records all your modifications
- The Sources file contains system code
- You can browse versions, recover code, and see who changed what
- The Changes file is your safety net

In Chapter 18, we'll explore **Version Control for Smalltalkers** - how to use Git with Smalltalk for proper team collaboration, branching, merging, and code sharing. The Changes file is great for personal history, but Git is essential for professional development.

Then in Part VI (Tools of the Trade), we'll dive deep into Smalltalk's development tools: the System Browser, Inspector, Debugger, and Finder.

You're now comfortable with Smalltalk's unique three-file architecture. This knowledge gives you confidence to work safely, experiment freely, and recover from mistakes!

---

**Key Takeaways:**
- The **Changes file** is an append-only text log of all modifications
- Every method definition, class change, and deletion is recorded
- Timestamps and author stamps track who changed what and when
- The **Sources file** contains read-only system source code
- Multiple versions of methods coexist in the Changes file
- You can browse versions, compare, and revert using System Browser
- The **Change Sorter** tool lets you explore the Changes file
- The Changes file enables recovery from corruption, crashes, or mistakes
- Changes file = personal history; Git = team history
- Don't commit Changes or Image files to Git
- The Changes file is human-readable text
- Method objects in the Image point to positions in Changes/Sources files
- Regular backups of the Changes file protect your code
- Compress the Changes file periodically to remove cruft
- Use Recent Changes to review your work before committing to Git

---

[Previous: Chapter 16 - Understanding the Image](chapter-16-understanding-the-image.md) | [Next: Chapter 18 - Version Control for Smalltalkers](chapter-18-version-control.md)
