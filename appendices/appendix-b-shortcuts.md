# Appendix B: Keyboard Shortcuts Reference

Quick reference for keyboard shortcuts in Pharo, Squeak, and other Smalltalk environments. Master these shortcuts to code faster!

## Notation

- `Cmd` = Command key (macOS)
- `Ctrl` = Control key (Windows/Linux)
- `Alt` = Alt key (Windows/Linux) or Option (macOS)
- `Shift` = Shift key
- `+` = Press keys together
- `,` = Press keys in sequence

**macOS**: Use `Cmd` where `Ctrl` is shown
**Windows/Linux**: Use `Ctrl`

## Universal Shortcuts (All Smalltalks)

### Code Execution

| Shortcut | Action |
|----------|--------|
| `Ctrl+D` | **Do it** - Execute selected code |
| `Ctrl+P` | **Print it** - Execute and print result |
| `Ctrl+I` | **Inspect it** - Execute and inspect result |
| `Ctrl+G` | **Debug it** - Execute in debugger |

### Text Editing

| Shortcut | Action |
|----------|--------|
| `Ctrl+X` | Cut |
| `Ctrl+C` | Copy |
| `Ctrl+V` | Paste |
| `Ctrl+Z` | Undo |
| `Ctrl+Shift+Z` | Redo |
| `Ctrl+A` | Select all |
| `Ctrl+F` | Find |
| `Ctrl+H` | Replace |

### Navigation

| Shortcut | Action |
|----------|--------|
| `Ctrl+Home` | Go to beginning |
| `Ctrl+End` | Go to end |
| `Ctrl+←` | Word left |
| `Ctrl+→` | Word right |

## Pharo-Specific Shortcuts

### Opening Tools

| Shortcut | Action |
|----------|--------|
| `Ctrl+O, B` | Open **System Browser** |
| `Ctrl+O, W` | Open **Workspace/Playground** |
| `Ctrl+O, T` | Open **Test Runner** |
| `Ctrl+O, C` | Open **Transcript** |
| `Ctrl+O, I` | Open **Iceberg** (Git) |
| `Ctrl+O, P` | Open **Process Browser** |
| `Shift+Enter` | Open **Spotter** (search) |
| `Ctrl+O, F` | Open **File Browser** |

### Browser Navigation

| Shortcut | Action |
|----------|--------|
| `Ctrl+B` | Browse it (selected symbol) |
| `Ctrl+M` | **Implementors** of... |
| `Ctrl+N` | **Senders** of... |
| `Alt+N` | Browse **references** to... |
| `Ctrl+W` | **Close** window/pane |
| `Alt+.` | Browse **method** |
| `Alt+,` | Browse **class hierarchy** |

### Code Completion

| Shortcut | Action |
|----------|--------|
| `Ctrl+Space` | **Auto-completion** menu |
| `Tab` | Accept completion |
| `Esc` | Cancel completion |

### Code Formatting

| Shortcut | Action |
|----------|--------|
| `Ctrl+Shift+F` | **Format** (pretty print) code |
| `Ctrl+T` | **Insert template** |

### Debugging

| Shortcut | Action |
|----------|--------|
| `F10` | **Step over** |
| `F11` | **Step into** |
| `Shift+F11` | **Step out** |
| `F5` | **Resume** execution |
| `Ctrl+Shift+R` | **Restart** method |

### Refactoring

| Shortcut | Action |
|----------|--------|
| `Ctrl+Shift+R` | **Rename** |
| `Ctrl+Shift+M` | **Extract method** |
| `Ctrl+Shift+V` | **Extract temp** |
| `Ctrl+Shift+I` | **Inline** |
| `Ctrl+Shift+P` | **Push down** method |
| `Ctrl+Shift+U` | **Push up** method |

### Window Management

| Shortcut | Action |
|----------|--------|
| `Ctrl+Shift+W` | Close **all** windows |
| `Ctrl+L` | Toggle **full screen** |
| `Alt+Tab` | Switch windows |

## Squeak-Specific Shortcuts

### Morphic World

| Shortcut | Action |
|----------|--------|
| `Alt+Click` | **World menu** |
| `Shift+Click` | Open **halo** (morph handles) |
| `Ctrl+Click` | **Context menu** |

### Tools

| Shortcut | Action |
|----------|--------|
| `Ctrl+O, B` | Open **Browser** |
| `Ctrl+O, W` | Open **Workspace** |
| `Ctrl+O, T` | Open **Transcript** |
| `Ctrl+O, C` | Open **Change Sorter** |
| `Ctrl+O, F` | Open **File List** |

### Navigation (Squeak Browser)

| Shortcut | Action |
|----------|--------|
| `Ctrl+M` | Browse **implementors** |
| `Ctrl+N` | Browse **senders** |
| `Ctrl+B` | Browse it |
| `Ctrl+D` | Do it |
| `Ctrl+P` | Print it |
| `Ctrl+I` | Inspect it |

## Glamorous Toolkit Shortcuts

### Navigation

| Shortcut | Action |
|----------|--------|
| `Ctrl+Shift+O` | Open **Spotter** |
| `Ctrl+Shift+G` | **Go to** class |
| `Ctrl+Shift+F` | **Search** in methods |

### Inspector

| Shortcut | Action |
|----------|--------|
| `Ctrl+I` | Inspect it |
| `Ctrl+G` | Dive in |
| `Alt+←` | Navigate back |
| `Alt+→` | Navigate forward |

### Coder

| Shortcut | Action |
|----------|--------|
| `Ctrl+S` | **Save** method |
| `Ctrl+B` | Browse references |
| `Ctrl+Shift+R` | Rename |

## Text Selection Shortcuts

### All Smalltalks

| Shortcut | Action |
|----------|--------|
| `Shift+←` | Select left |
| `Shift+→` | Select right |
| `Shift+↑` | Select line up |
| `Shift+↓` | Select line down |
| `Ctrl+Shift+←` | Select word left |
| `Ctrl+Shift+→` | Select word right |
| `Ctrl+Shift+Home` | Select to beginning |
| `Ctrl+Shift+End` | Select to end |
| `Ctrl+L` | Select line |

## Code Templates (Pharo)

Type keyword, press `Tab`:

| Template | Expands to |
|----------|------------|
| `do` | `do: [ :each │ ]` |
| `sel` | `select: [ :each │ ]` |
| `col` | `collect: [ :each │ ]` |
| `rej` | `reject: [ :each │ ]` |
| `det` | `detect: [ :each │ ] ifNone: [ ]` |
| `inj` | `inject: into: [ :sum :each │ ]` |
| `if` | `ifTrue: [ ] ifFalse: [ ]` |
| `ifn` | `ifNotNil: [ :value │ ]` |
| `wh` | `whileTrue: [ ]` |
| `to` | `to:do: [ :i │ ]` |

## Spotter (Pharo)

Once Spotter is open (`Shift+Enter`):

| Shortcut | Action |
|----------|--------|
| `#` | Search **classes** |
| `>` | Search **implementors** |
| `@` | Search **senders** |
| `/` | Search **files** |
| `?` | **Help** |
| `Enter` | Select result |
| `Ctrl+Enter` | Open in background |
| `Esc` | Close Spotter |
| `↑/↓` | Navigate results |
| `Ctrl+↑/↓` | Navigate categories |

## Inspector Shortcuts (Pharo)

| Shortcut | Action |
|----------|--------|
| `Ctrl+I` | **Inspect** field |
| `Ctrl+B` | **Browse** class |
| `Ctrl+D` | **Do it** in context |
| `Ctrl+P` | **Print it** in context |
| `Tab` | Switch panes |

## Debugger Shortcuts (Pharo)

| Shortcut | Action |
|----------|--------|
| `F10` | **Over** - Step over |
| `F11` | **Into** - Step into |
| `Shift+F11` | **Out** - Step out |
| `F5` | **Proceed** - Continue |
| `Ctrl+Shift+R` | **Restart** - Restart method |
| `Ctrl+T` | **Through** - Run to cursor |
| `Ctrl+W` | **Close** debugger |

## System Browser Shortcuts (Pharo)

| Shortcut | Action |
|----------|--------|
| `Ctrl+S` | **Save** method |
| `Ctrl+F` | **Find** in code |
| `Ctrl+Shift+F` | **Format** code |
| `Ctrl+M` | Browse **implementors** |
| `Ctrl+N` | Browse **senders** |
| `Ctrl+B` | **Browse** selected |
| `Alt+C` | **Comment** selection |
| `Alt+U` | **Uncomment** selection |
| `Tab` | Switch focus between panes |

## Custom Shortcuts

### Setting Custom Shortcuts (Pharo)

1. Open **Settings Browser**:
   ```smalltalk
   SettingBrowser open
   ```

2. Navigate to **Keymapping**
3. Customize shortcuts
4. Save

### Example: Add Custom Shortcut

```smalltalk
"In your startup code:"
KMRepository default
    addKeymapCategory: 'My Custom Shortcuts'
    with: [ :map |
        map addShortcut: PharoShortcuts systemBrowserShortcut
            action: [ Smalltalk tools browser open ] ].
```

## Productivity Tips

### Most Used Shortcuts

Master these first:

1. `Ctrl+D` - **Do it**
2. `Ctrl+P` - **Print it**
3. `Ctrl+I` - **Inspect it**
4. `Shift+Enter` - **Spotter** (Pharo)
5. `Ctrl+O, B` - **Browser**
6. `Ctrl+O, W` - **Workspace**
7. `Ctrl+M` - **Implementors**
8. `Ctrl+N` - **Senders**

### Speed Coding

```
1. Type code
2. Ctrl+D to test
3. Ctrl+S to save
4. Ctrl+M to browse
5. Repeat!
```

### Three-Finger Salute

For any selection:
- `Ctrl+D` - Does it work?
- `Ctrl+P` - What does it return?
- `Ctrl+I` - Let me inspect that...

## Mouse Shortcuts

### Pharo

| Mouse Action | Effect |
|--------------|--------|
| **Single click** | Select |
| **Double click** | Select word |
| **Triple click** | Select line |
| **Click + drag** | Select text |
| **Middle click** | **Browse** (if over symbol) |
| **Ctrl+Click** | Context menu |
| **Shift+Click** | Extend selection |

### Squeak

| Mouse Action | Effect |
|--------------|--------|
| **Alt+Click** | World menu |
| **Shift+Click** | Halo (morph handles) |
| **Ctrl+Click** | Context menu |

## Special Characters

### Pharo

| Shortcut | Character | Name |
|----------|-----------|------|
| `Alt+L` | `←` | Left arrow (assignment in old Smalltalks) |
| `Alt+R` | `→` | Right arrow |
| `Alt+U` | `↑` | Up arrow (return in old Smalltalks) |
| `Alt+Shift+/` | `÷` | Division |
| `Alt+X` | `≠` | Not equal |
| `Alt+<` | `≤` | Less than or equal |
| `Alt+>` | `≥` | Greater than or equal |

**Note**: Modern Smalltalk uses `:=` for assignment and `^` for return.

## Workspace/Playground Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl+D` | Execute selection (or line) |
| `Ctrl+P` | Execute and print result |
| `Ctrl+I` | Execute and inspect result |
| `Ctrl+Shift+P` | Print to Transcript |
| `Ctrl+S` | Save workspace contents |
| `Ctrl+F` | Find in workspace |

## Transcript Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl+K` | **Clear** transcript |
| `Ctrl+S` | **Save** transcript to file |

## File Browser Shortcuts

| Shortcut | Action |
|----------|--------|
| `Enter` | Open selected file |
| `Delete` | Delete selected file |
| `F2` | Rename file |
| `Ctrl+N` | New file |
| `Ctrl+F` | Find files |

## Version Control (Iceberg) Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl+O, I` | Open **Iceberg** |
| `Ctrl+K` | **Commit** |
| `Ctrl+P` | **Push** |
| `Ctrl+U` | **Pull** |
| `Ctrl+D` | Show **diff** |

## Emergency Shortcuts

### All Smalltalks

| Shortcut | Action |
|----------|--------|
| `Alt+.` | **Interrupt** (emergency stop) |
| `Ctrl+Shift+Q` | **Emergency evaluator** |
| `Alt+Shift+W` | Close **all** debuggers |

### Force Quit

If Smalltalk hangs:
- **macOS**: `Cmd+Alt+Esc` → Force Quit
- **Windows**: `Ctrl+Alt+Del` → Task Manager
- **Linux**: `xkill` or `killall pharo`

## Customizing Shortcuts

### View All Shortcuts (Pharo)

```smalltalk
"Open keymapping browser:"
KMRepository default inspect.

"List all shortcuts:"
KMRepository default allShortcuts.
```

### Change a Shortcut

```smalltalk
"Example: Change 'Do it' to Ctrl+E:"
KMRepository default
    at: #DoIt
    put: (KMKeymap shortcut: $e control).
```

## Platform Differences

### macOS vs Windows/Linux

| Action | macOS | Windows/Linux |
|--------|-------|---------------|
| Execute | `Cmd+D` | `Ctrl+D` |
| Print | `Cmd+P` | `Ctrl+P` |
| Inspect | `Cmd+I` | `Ctrl+I` |
| Copy | `Cmd+C` | `Ctrl+C` |
| Paste | `Cmd+V` | `Ctrl+V` |

**General rule**: Replace `Ctrl` with `Cmd` on macOS.

## Quick Reference Card

Print this for your desk:

```
═══════════════════════════════════════
    PHARO ESSENTIAL SHORTCUTS
═══════════════════════════════════════
Execute:
  Ctrl+D  Do it
  Ctrl+P  Print it
  Ctrl+I  Inspect it
  Ctrl+G  Debug it

Tools:
  Shift+Enter    Spotter
  Ctrl+O, B      Browser
  Ctrl+O, W      Workspace

Browse:
  Ctrl+M  Implementors
  Ctrl+N  Senders
  Ctrl+B  Browse it

Debug:
  F10     Step over
  F11     Step into
  F5      Resume

Code:
  Ctrl+Space     Auto-complete
  Ctrl+Shift+F   Format
  Ctrl+S         Save

Emergency:
  Alt+.   Interrupt!
═══════════════════════════════════════
```

## Learn More

**Practice shortcuts daily!** Muscle memory takes time but pays off.

**Discover shortcuts:**
```smalltalk
"See all shortcuts:"
KMRepository default shortcuts.
```

**Ask the community:**
- Discord: "What's your favorite shortcut?"
- Many power users share tips!

---

[Previous: Appendix A - Installation Guide Details](appendix-a-installation.md) | [Next: Appendix C - Useful Code Snippets](appendix-c-snippets.md)
