# Chapter 2: Your First Steps - Installing and Exploring

It's time to get your hands dirty! In this chapter, we'll install Smalltalk on your computer, start it up for the first time, and take a tour of what you see. By the end of this chapter, you'll have a working Smalltalk environment and understand the basic layout.

## Choosing Your Smalltalk

As mentioned in Chapter 1, we'll be working primarily with three modern Smalltalk environments:

- **Pharo** - Modern, clean, and what we'll use most
- **Squeak** - Stable, portable, and educational
- **Glamorous Toolkit (GT)** - Cutting-edge and innovative

For your first installation, I recommend starting with **Pharo**. It has excellent documentation, a welcoming community, and is the most widely used for professional development. Once you're comfortable with Pharo, you can explore Squeak and GT to see how they differ.

Don't worry - the fundamental concepts are the same across all three. Code you write in one will mostly work in the others (with minor differences). We're learning Smalltalk, not just one specific version of it.

## Installing Pharo

Pharo is available for Windows, macOS, and Linux. The installation process is refreshingly simple compared to many programming environments.

### Windows Installation

1. **Visit the Pharo website**: Go to https://pharo.org/download

2. **Download the installer**: Click on the Windows download button. This will download a file called something like `Pharo64-12-win.zip` (the numbers might be different - that's just the version).

3. **Extract the ZIP file**: Right-click the downloaded file and choose "Extract All". Pick a location you can easily find, like `C:\Pharo` or a folder in your Documents.

4. **Run Pharo**: Navigate to the extracted folder and double-click `Pharo.exe`. You might see a security warning saying "Windows protected your PC" - this is normal for downloaded applications. Click "More info" and then "Run anyway".

5. **Success!** Pharo should start up. You'll see a window with what might look like a lot of stuff. Don't worry - we'll explain it all shortly.

### macOS Installation

1. **Visit the Pharo website**: Go to https://pharo.org/download

2. **Download the installer**: Click on the macOS download button. This will download a file like `Pharo64-12-mac.zip`.

3. **Extract the ZIP file**: Double-click the downloaded file to extract it. You might want to move the resulting Pharo folder to your Applications folder, but it can run from anywhere.

4. **Run Pharo**: Double-click the Pharo application. On first run, you might see a message saying "Pharo cannot be opened because it is from an unidentified developer". This is normal. Here's how to proceed:
   - Right-click (or Control-click) the Pharo application
   - Choose "Open" from the menu
   - Click "Open" in the dialog that appears
   - Pharo will start, and you won't need to do this again

5. **Success!** Pharo is now running on your Mac.

### Linux Installation

Linux users have a few options. Here's the simplest:

1. **Visit the Pharo website**: Go to https://pharo.org/download

2. **Download the installer**: Click on the Linux download button to get `Pharo64-12-linux.zip`.

3. **Extract the ZIP file**: Open a terminal and navigate to where you downloaded the file, then:
   ```bash
   unzip Pharo64-12-linux.zip -d ~/pharo
   cd ~/pharo
   ```

4. **Make the files executable**:
   ```bash
   chmod +x pharo pharo-vm/pharo
   ```

5. **Run Pharo**:
   ```bash
   ./pharo
   ```

6. **Success!** Pharo should launch in all its glory.

**Alternative for Linux**: Many Linux distributions have Pharo in their package repositories. On Ubuntu/Debian:
```bash
sudo apt install pharo
```

However, package managers sometimes have older versions, so downloading directly from pharo.org ensures you get the latest version.

## What Just Happened? Understanding the Files

When you extracted Pharo, you got several files. Let's understand what they are:

### The Image File (`.image`)

This is the star of the show. Remember in Chapter 1 when we talked about Smalltalk's "image" system? This file IS that image. It contains:
- All of Smalltalk's core code (classes, methods, everything)
- All your code (once you write some)
- All the development tools
- The state of every object in the system

Think of it as a snapshot of a living, breathing Smalltalk environment frozen in time. When you start Pharo, it brings this snapshot to life.

The image file is typically named something like `Pharo12.image`.

### The Changes File (`.changes`)

This file is your safety net. It records every single change you make in the image - every method you define, every class you create, every modification you make. It's like a journal of everything that's happened.

If something goes wrong, you can recover your work from the changes file. We'll explore this more in Chapter 17.

The changes file is typically named something like `Pharo12.changes` and sits right next to your image file.

### The Sources File (`.sources`)

This file contains the source code for the core Smalltalk system - the code that was written by the Pharo developers themselves. Your image references this file when you want to look at how built-in classes work.

You usually don't interact with this file directly; the image uses it behind the scenes.

The sources file is typically named something like `PharoV130.sources`.

### The Virtual Machine (VM)

The VM is the actual program that runs Smalltalk. On Windows, this is `Pharo.exe`. On macOS, it's the Pharo application. On Linux, it's the `pharo` executable.

The VM reads the image file and executes it. It's the interpreter that turns Smalltalk code into machine code your computer can run.

**Important Concept**: Separating the VM from the image is powerful. You can:
- Update your VM without losing your work
- Share your image with others who can run it with their VM
- Have multiple images with different projects
- Copy an image to back it up or move it to another computer

### The Three-File System

So you have:
1. **The VM** - The engine that runs Smalltalk
2. **The Image** - Your living Smalltalk environment
3. **The Changes** - Your safety net and history

(Plus the Sources file, which is more of a reference library.)

This might seem unusual if you're used to programming languages where your code is just text files, but it's one of Smalltalk's superpowers. We'll appreciate why as we go along.

## Your First Look at Pharo

When Pharo starts, you'll see a window that might look overwhelming at first. Let's take a tour.

### The World

The background - that large gray area - is called **The World**. It's the container for everything else. Think of it as your desktop within Smalltalk.

You can right-click (or control-click on macOS) anywhere in the World to bring up the **World Menu**. This is your main navigation hub. We'll explore this menu in detail soon.

### The Welcome Window

When Pharo first starts, you'll probably see a welcome window with news, tips, and links. You can close this - it appears by default on startup, but you can disable it if you want.

### The Taskbar

At the top (or sometimes bottom, depending on your settings) is a taskbar. It shows:
- The **Pharo logo** - Click it for system menu
- **Open windows** - Tabs for different tools you have open
- **Clock** - The current time
- **World button** - Another way to access the World Menu

### Is That All?

If you don't see many windows, that's normal! Smalltalk doesn't bombard you with stuff on startup. The environment is there, ready for you to use, but it stays out of your way until you need it.

Think of it like a workshop with tools hanging on the walls. You don't take all the tools off the walls at once - you grab what you need when you need it.

## Opening Your First Tool: The Playground

Let's open something interactive. The **Playground** (sometimes called a Workspace in other Smalltalks) is where you'll experiment with code and try things out.

### Opening the Playground

There are several ways to open a Playground:

**Method 1: World Menu**
1. Right-click anywhere in the World (the gray background)
2. Navigate to `Playground` in the menu that appears
3. Click it

**Method 2: Keyboard Shortcut**
- Press `Ctrl+O` then `W` (on Windows/Linux)
- Press `Cmd+O` then `W` (on macOS)

The keyboard shortcut means: "Open something" (`Ctrl+O` or `Cmd+O`), specifically a "Workspace/Playground" (`W`).

### What You See

A Playground window appears! It's a simple white text area where you can type code. Think of it as a scratch pad for programming - a place to experiment, test ideas, and see what happens.

At the bottom of the Playground, you'll see some small icons. These are action buttons we'll use constantly:
- **Do it** (or a "play" icon) - Executes your code
- **Print it** (or a "P" icon) - Executes your code and shows the result
- **Inspect it** (or an "i" icon) - Executes your code and opens an inspector to explore the result
- **Publish** - Saves your code snippet for later

Don't worry about memorizing these yet. We'll use them extensively in the next chapter.

## Your First Smalltalk Code

Let's type something! In the Playground, type this:

```smalltalk
2 + 3
```

That's it. Just those three characters: the number 2, a plus sign, and the number 3.

Now, select that text with your mouse (highlight it). With it selected, right-click and choose **"Print it"** from the menu that appears.

### What Happened?

After the `3`, you should now see: `2 + 3 . 5`

The `. 5` is Smalltalk telling you "the result of this expression is 5". Congratulations! You just ran your first Smalltalk code!

### What Really Happened?

Here's something that will blow your mind: what you actually did was send a **message** to an object.

- `2` is an object (the number two)
- `+` is a message you sent to that object
- `3` is an argument (a parameter) to that message
- `5` is the result that came back

In Smalltalk, everything is objects sending messages to each other. You just told the object `2` to add `3` to itself, and it responded with `5`.

We'll explore this in much more depth in Chapter 4, but I wanted you to see it in action right away.

## Try a Few More

Let's experiment a bit more. Type each of these in your Playground, select them, and "Print it" to see what happens:

```smalltalk
10 * 4
```

This should show: `10 * 4 . 40`

You sent the `*` (multiply) message to 10, with 4 as the argument.

```smalltalk
'Hello, Smalltalk!'
```

This should show: `'Hello, Smalltalk!' . 'Hello, Smalltalk!'`

This is a string (text) object. When you print it, it just shows itself.

```smalltalk
'Hello, Smalltalk!' size
```

This should show: `'Hello, Smalltalk!' size . 17`

You sent the `size` message to the string, asking how many characters it contains. The answer is 17.

```smalltalk
Date today
```

This should show something like: `Date today . 1 October 2025`

You sent the `today` message to the Date class, asking for today's date.

### The Magic of Experimentation

Notice how easy this is? Type something, select it, print it, see the result. No compilation step, no building, no waiting. This immediate feedback is one of Smalltalk's greatest strengths for learning.

You can't break anything in the Playground. Experiment freely! Try variations. What happens if you do `3 + 2` instead of `2 + 3`? What about `'Hello' , ' World'` (that's a comma, not a period)? Try it!

## Understanding the Image Concept

Remember how we talked about the image file? Let's understand what that really means.

### Everything Lives in the Image

When you started Pharo, you didn't just start a program. You brought an entire environment to life. Everything you see - every window, every tool, every piece of code - is an object living in memory. The image file is just all of those objects frozen on disk.

### Saving Your Work

Type something in your Playground - anything at all. Now, let's save the image.

Go to the World Menu (right-click the background) and choose `Save`. You might see a brief flash or progress indicator. That's it - the entire state of your Smalltalk environment has been saved.

### Testing the Save

Now close Pharo completely. Just close the window or quit the application.

Start Pharo again. When it comes back up, open a Playground (if one isn't already open).

**The Playground should still have your text in it!** (If you had it open when you saved.)

This is the power of the image. Your environment isn't just saved when you explicitly save files - the entire state can be preserved and restored.

### Multiple Images

Here's something cool: you can have multiple images for different projects.

1. Go to the folder where Pharo is installed
2. Find the `.image` file (probably `Pharo12.image`)
3. Make a copy of it (along with its corresponding `.changes` file)
4. Rename the copy to something like `MyProject.image` (and `MyProject.changes`)

Now you have two separate Smalltalk environments! You can work on different projects without them interfering with each other. Each image is its own universe.

To open a specific image:
- **Windows**: Drag the `.image` file onto `Pharo.exe`
- **macOS/Linux**: From terminal: `./pharo MyProject.image`
- **Any platform**: Right-click the image file and choose "Open With" Pharo

## A Quick Tour of Other Tools

We're not going to dive deep into the tools yet (that's what Part VI is for), but let's take a quick peek at what's available.

### The System Browser

From the World Menu, choose `System Browser`. This opens the tool you'll use to browse and edit classes and methods. It might look complex, but don't worry - we'll cover it thoroughly when we start creating our own classes in Chapter 11.

For now, just notice: it shows you all the code in the system. You can browse through classes, read their methods, and see how everything works. Close it for now.

### The Iceberg (Git Integration)

From the World Menu, you might see `Iceberg`. This is Pharo's Git integration tool for version control. We'll cover it in Chapter 18. For now, just know it exists.

### Settings

From the World Menu, choose `Settings`. This is where you can customize Pharo's appearance, fonts, keyboard shortcuts, and behavior. Feel free to explore, but the defaults are generally good for learning.

Particularly useful settings:
- **Appearance** - Change themes, fonts, colors
- **Desktop** - Adjust window behavior
- **Code Completion** - Configure autocomplete options

## Common Beginner Questions

### "Where are my files?"

In most programming environments, you work with files: `.py` for Python, `.js` for JavaScript, etc. Smalltalk doesn't work that way (at least, not primarily).

Your code lives in the image as objects. You interact with it through tools, not by editing text files. This feels weird at first but becomes natural quickly.

That said, you *can* export code to files (for sharing or version control), and we'll cover that in Chapter 18.

### "How do I save my code?"

When you save the image (via the World Menu or `Ctrl+S` / `Cmd+S`), everything is saved: all your code, all your objects, the state of your tools, everything.

For sharing code with others or backing up specific packages, we'll learn about version control tools in Chapter 18.

### "What if I mess something up?"

That's what the `.changes` file is for! Every modification is recorded. If something goes wrong, you can recover. We'll cover recovery techniques in Chapter 17.

Also, before experimenting with something you're unsure about, just make a copy of your image file. Then you can experiment freely, knowing you have a backup.

### "Why does Pharo look different from screenshots I see online?"

Pharo updates regularly, and different versions have different default themes and layouts. Also, people customize their environments. Don't worry about making yours look exactly like someone else's - focus on understanding the concepts.

### "Can I use my favorite text editor?"

You *can* edit Smalltalk code in external text editors and load it in, but you'd be missing out on the magic. Smalltalk's tools are designed to work with living objects, not just text. Give the built-in tools a real try before deciding you want something else.

## Installing Squeak (Optional)

If you're curious about Squeak, here's how to install it:

1. Visit https://squeak.org/downloads/
2. Download the installer for your operating system
3. Run the installer (it's straightforward on all platforms)
4. Launch Squeak

Squeak looks a bit different from Pharo - it has a more colorful, playful appearance by default. The concepts are the same, though. In Squeak, the Playground is called the Workspace.

## Installing Glamorous Toolkit (Optional)

For Glamorous Toolkit:

1. Visit https://gtoolkit.com/download/
2. Download for your operating system
3. Extract and run (similar to Pharo)

GT looks quite different - it's more modern and minimalist. It has a strong focus on documentation and custom visualizations. We'll explore GT's unique features in Chapter 29.

## Troubleshooting

### Pharo won't start

- **Check your system**: Pharo requires 64-bit operating systems. Very old computers might not be supported.
- **Graphics drivers**: Make sure your graphics drivers are up to date, especially on Linux.
- **Permissions**: Make sure the Pharo executable has permission to run.

### Everything is too small/too large

- Go to Settings → Appearance → Desktop Scaling
- Adjust the scale factor until comfortable

### Pharo is slow

- Close unnecessary windows and tools
- Make sure you have at least 2GB of available RAM
- On older computers, consider using an older version of Pharo (like Pharo 9 or 10 instead of the latest)

### I can't find the World Menu

- Try right-clicking in different areas of the gray background
- On macOS, make sure you're using a two-finger click or Control+click
- You can also click the Pharo logo or world icon in the taskbar

## Your Setup is Complete!

You now have Smalltalk installed and running. You've seen:
- How Smalltalk is organized (VM + Image + Changes)
- The basic layout of the Pharo environment
- How to open a Playground
- How to run your first code
- How the image system preserves your work

This is your programming home for the rest of this book. Get comfortable here.

## Try This!

Before moving on to Chapter 3, spend some time just exploring:

1. **Open and close windows** - Get comfortable with the interface. Notice how windows can be resized, moved, and closed.

2. **Experiment in the Playground** - Try different expressions:
   - `10 + 20 * 3` (does it give 90 or 70? Why?)
   - `15 / 3` (division)
   - `'your name here' reversed` (replace with your actual name!)
   - `Time now` (what time is it?)

3. **Save and reload** - Save your image, close Pharo, restart it. Verify that your Playground content is still there.

4. **Customize something** - Go to Settings and change the theme or font. Make Pharo feel like yours.

5. **Break something** - Seriously! Type something nonsensical in the Playground and try to print it. See what error message you get. You can't break anything permanent - just close any error windows and keep going.

Getting comfortable with the environment is important. Smalltalk is different from other programming setups, and that's okay. Different is good!

## Coming Up Next

In Chapter 3, we'll dive deep into the Playground (Workspace) and really learn how to use it. You'll discover the "Do it", "Print it", and "Inspect it" commands that make Smalltalk so interactive. You'll write more code, see more results, and start to get a feel for how Smalltalk thinks.

The real programming begins next!

---

**Key Takeaways:**
- Smalltalk uses a three-part system: VM (the engine), Image (your live environment), and Changes (your history)
- The Image is a snapshot of all objects in memory - your entire environment frozen in time
- Installing Pharo, Squeak, or GT is straightforward across all platforms
- The Playground is your experimentation space
- Saving the image saves everything - all code, all state, all tools
- You can have multiple images for different projects
- The environment feels different from file-based programming, and that's intentional

---

[Previous: Chapter 1 - What is Programming](chapter-01-what-is-programming.md) | [Next: Chapter 3 - The Workspace](chapter-03-the-workspace.md)
