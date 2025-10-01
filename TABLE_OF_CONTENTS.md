# Table of Contents

## Learn Programming with Smalltalk

---

## Part I: Welcome to Programming

### [Chapter 1: What is Programming (And Why Smalltalk?)](chapters/chapter-01-what-is-programming.md)
Introduction to programming concepts, why Smalltalk is uniquely suited for learning, brief history, and what makes it special.

### [Chapter 2: Your First Steps - Installing and Exploring](chapters/chapter-02-your-first-steps.md)
Detailed walkthrough of installing Pharo, Squeak, and Glamorous Toolkit; understanding what an "image" is; first look at the environment.

### [Chapter 3: The Workspace - Your Programming Playground](chapters/chapter-03-the-workspace.md)
Using workspaces/playgrounds to experiment; your first lines of code; the "Do it," "Print it," and "Inspect it" paradigm.

---

## Part II: Thinking in Objects

### [Chapter 4: Everything is an Object (No, Really!)](chapters/chapter-04-everything-is-an-object.md)
Objects, messages, and methods; the fundamental mantra; sending your first messages.

### [Chapter 5: Numbers, Strings, and Basic Types](chapters/chapter-05-numbers-strings-and-basic-types.md)
Working with numbers (integers, floats); strings and characters; booleans; nil; everything is an object!

### [Chapter 6: Collections - Organizing Your Data](chapters/chapter-06-collections.md)
Arrays, OrderedCollections, Sets, Dictionaries; the power of collection protocols.

### [Chapter 7: Variables - Naming Your Objects](chapters/chapter-07-variables.md)
Temporary variables, instance variables (preview), class variables (preview); the assignment operator.

---

## Part III: Making Decisions and Repeating Actions

### [Chapter 8: Conditionals - Making Choices](chapters/chapter-08-conditionals.md)
ifTrue:, ifFalse:, ifTrue:ifFalse:; how Smalltalk implements control flow with objects and blocks.

### [Chapter 9: Blocks - Code You Can Hold](chapters/chapter-09-blocks.md)
Understanding blocks (closures); storing code as objects; the power of first-class functions.

### [Chapter 10: Loops and Iteration](chapters/chapter-10-loops-and-iteration.md)
timesRepeat:, to:do:, whileTrue:, whileFalse:; collection iteration (do:, collect:, select:, reject:, inject:into:).

---

## Part IV: Creating Your Own Objects

### [Chapter 11: Classes - The Blueprint](chapters/chapter-11-classes.md)
What are classes?; creating your first class; the System Browser; understanding instance vs. class.

### [Chapter 12: Methods - Teaching Objects New Tricks](chapters/chapter-12-methods.md)
Defining methods; method protocols; the method browser; return values.

### [Chapter 13: Instance Variables - Giving Objects Memory](chapters/chapter-13-instance-variables.md)
Adding state to your objects; accessors and mutators; encapsulation.

### [Chapter 14: The Mystery of 'self' and 'super'](chapters/chapter-14-self-and-super.md)
Understanding self; message sending to self; what super means; the lookup chain.

### [Chapter 15: Inheritance - Standing on Shoulders](chapters/chapter-15-inheritance.md)
Creating subclasses; overriding methods; when to inherit; the class hierarchy.

---

## Part V: The Image, Changes, and Sources

### [Chapter 16: Understanding the Image](chapters/chapter-16-understanding-the-image.md)
What the image file contains; saving and loading; why it's revolutionary; managing multiple images.

### [Chapter 17: Changes and Sources Files](chapters/chapter-17-changes-and-sources.md)
The changes file as your safety net; the sources file; recovering lost code; understanding the three-file system.

### [Chapter 18: Version Control for Smalltalkers](chapters/chapter-18-version-control.md)
Git with Smalltalk; Monticello; Iceberg in Pharo; best practices; moving beyond the image.

---

## Part VI: Tools of the Trade

### [Chapter 19: The System Browser - Your Code Navigator](chapters/chapter-19-the-system-browser.md)
Deep dive into browsing classes; finding methods; refactoring tools; keyboard shortcuts.

### [Chapter 20: The Inspector and Explorer](chapters/chapter-20-the-inspector-and-explorer.md)
Inspecting objects live; modifying running objects; exploring object graphs; debugging in the inspector.

### [Chapter 21: The Debugger - Your New Best Friend](chapters/chapter-21-the-debugger.md)
Why Smalltalk's debugger is magical; fixing bugs while the program runs; stepping through code; the stack.

### [Chapter 22: The Finder - Discovering Code](chapters/chapter-22-the-finder.md)
Finding implementors, senders; method searches; example searches; learning from existing code.

---

## Part VII: Intermediate Concepts

### [Chapter 23: Protocols and Polymorphism](chapters/chapter-23-protocols-and-polymorphism.md)
Duck typing in Smalltalk; designing with protocols; the power of polymorphism.

### [Chapter 24: Error Handling - When Things Go Wrong](chapters/chapter-24-error-handling.md)
Exceptions in Smalltalk; on:do:; ensure:; creating custom exceptions.

### [Chapter 25: Testing Your Code](chapters/chapter-25-testing-your-code.md)
SUnit testing framework; writing tests first; test-driven development; running test suites.

### [Chapter 26: Packages and Code Organization](chapters/chapter-26-packages-and-code-organization.md)
Organizing code into packages; dependencies; loading code; baseline configurations.

---

## Part VIII: Exploring the Smalltalk Variations

### [Chapter 27: Pharo - The Modern Smalltalk](chapters/chapter-27-pharo.md)
Pharo's unique features; the community; key libraries (Seaside, Roassal, etc.); when to choose Pharo.

### [Chapter 28: Squeak - Multimedia and Education](chapters/chapter-28-squeak.md)
Squeak's heritage; Morphic UI; multimedia capabilities; Etoys and learning environments.

### [Chapter 29: Glamorous Toolkit - Moldable Development](chapters/chapter-29-glamorous-toolkit.md)
GT's revolutionary approach; custom inspectors; moldable development; exploring data.

### [Chapter 30: Other Smalltalks Worth Knowing](chapters/chapter-30-other-smalltalks.md)
Brief overview of Cuis, GNU Smalltalk, VA Smalltalk, and the broader family.

---

## Part IX: Building Real Things

### [Chapter 31: Project 1 - A Todo List Manager](chapters/chapter-31-project-todo-list.md)
Putting it all together; building a simple application; managing state; collections in practice.

### [Chapter 32: Project 2 - A Text Adventure Game](chapters/chapter-32-project-text-adventure.md)
Objects modeling game entities; polymorphism in action; designing a small game engine.

### [Chapter 33: Project 3 - A Simple Web Server](chapters/chapter-33-project-web-server.md)
Understanding Zinc HTTP; serving web pages; handling requests; introduction to web development.

### [Chapter 34: Working with Files and Streams](chapters/chapter-34-files-and-streams.md)
File I/O; reading and writing files; streams; binary vs. text; FileSystem API.

### [Chapter 35: Graphics and UI Basics](chapters/chapter-35-graphics-and-ui.md)
Introduction to Morphic; creating simple UIs; event handling; making things visual.

---

## Part X: Next Steps

### [Chapter 36: Design Patterns in Smalltalk](chapters/chapter-36-design-patterns.md)
Classic patterns that originated in Smalltalk; MVC; Observer; Strategy; when to use them.

### [Chapter 37: Performance and Optimization](chapters/chapter-37-performance-and-optimization.md)
Profiling code; common performance pitfalls; when to optimize; benchmarking.

### [Chapter 38: The Smalltalk Community](chapters/chapter-38-the-smalltalk-community.md)
Where to get help; Discord, Slack, mailing lists; contributing to open source; conferences.

### [Chapter 39: Beyond Smalltalk - Taking Your Skills Further](chapters/chapter-39-beyond-smalltalk.md)
How Smalltalk concepts transfer to other languages; what to learn next; career possibilities.

### [Chapter 40: Your Smalltalk Journey](chapters/chapter-40-your-smalltalk-journey.md)
Recap; resources for continued learning; parting wisdom; the joy of live programming.

---

## Appendices

### [Appendix A: Installation Guide Details](appendices/appendix-a-installation-guide.md)
Platform-specific installation instructions; troubleshooting common issues.

### [Appendix B: Keyboard Shortcuts Reference](appendices/appendix-b-keyboard-shortcuts.md)
Essential shortcuts for Pharo, Squeak, and Glamorous Toolkit.

### [Appendix C: Useful Code Snippets](appendices/appendix-c-code-snippets.md)
Common patterns and idioms for quick reference.

### [Appendix D: Glossary](appendices/appendix-d-glossary.md)
Definitions of Smalltalk-specific terms and concepts.

### [Appendix E: Further Reading](appendices/appendix-e-further-reading.md)
Books, papers, websites, and online resources for continued learning.

---

[Back to Main README](README.md)
