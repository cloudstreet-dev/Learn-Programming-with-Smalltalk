# Chapter 31: Project 1 - A Todo List Manager

Welcome to Part IX: Building Real Things! You've learned Smalltalk fundamentals, explored the development environment, and surveyed different implementations. Now it's time to **build complete applications** from scratch.

In this chapter, we'll create a fully functional **Todo List Manager**. You'll apply everything you've learned: objects, collections, UI components, persistence, testing, and more. This is real-world Smalltalk development!

By the end, you'll have a working application you can actually use to manage your tasks!

## What We're Building

A **Todo List Manager** with:

- **Create tasks** - Add new todo items
- **Mark complete** - Check off finished tasks
- **Edit tasks** - Modify descriptions
- **Delete tasks** - Remove tasks
- **Filter views** - Show all, active, or completed
- **Persistence** - Save and load tasks
- **Priority levels** - Mark tasks as high/medium/low priority
- **Due dates** - Set deadlines for tasks
- **Categories** - Organize tasks by category

We'll build this step-by-step, starting simple and adding features!

## Project Structure

We'll organize our code in packages:

```
TodoApp-Core
├─ TodoItem (model)
├─ TodoList (model)
└─ TodoManager (controller)

TodoApp-UI
├─ TodoListPresenter (main window)
├─ TodoItemPresenter (item display)
└─ TodoEditorPresenter (add/edit dialog)

TodoApp-Persistence
└─ TodoStore (save/load)

TodoApp-Tests
├─ TodoItemTest
├─ TodoListTest
└─ TodoManagerTest
```

## Step 1: Create the Packages

First, set up our project structure:

```smalltalk
"Create packages:"
RPackageOrganizer default createPackageNamed: 'TodoApp-Core'.
RPackageOrganizer default createPackageNamed: 'TodoApp-UI'.
RPackageOrganizer default createPackageNamed: 'TodoApp-Persistence'.
RPackageOrganizer default createPackageNamed: 'TodoApp-Tests'
```

Now we're ready to build!

## Step 2: The TodoItem Model

A **TodoItem** represents a single task.

Open the System Browser and create:

```smalltalk
Object subclass: #TodoItem
    instanceVariableNames: 'description completed priority dueDate category createdAt'
    classVariableNames: ''
    package: 'TodoApp-Core'
```

### Instance Variables

- `description` - The task text
- `completed` - Boolean, whether it's done
- `priority` - #high, #medium, or #low
- `dueDate` - Date or nil
- `category` - String category name
- `createdAt` - Timestamp when created

### Initialize Method

```smalltalk
initialize
    super initialize.
    description := ''.
    completed := false.
    priority := #medium.
    dueDate := nil.
    category := 'General'.
    createdAt := DateAndTime now
```

Every new TodoItem starts incomplete, medium priority, no due date.

### Accessors

```smalltalk
description
    ^ description

description: aString
    description := aString

completed
    ^ completed

priority
    ^ priority

priority: aSymbol
    "aSymbol should be #high, #medium, or #low"
    priority := aSymbol

dueDate
    ^ dueDate

dueDate: aDate
    dueDate := aDate

category
    ^ category

category: aString
    category := aString

createdAt
    ^ createdAt
```

### Behavior Methods

```smalltalk
markComplete
    "Mark this task as completed"
    completed := true

markIncomplete
    "Mark this task as not completed"
    completed := false

toggleComplete
    "Toggle completion status"
    completed := completed not

isComplete
    "Answer whether this task is completed"
    ^ completed

isPending
    "Answer whether this task is not completed"
    ^ completed not

isOverdue
    "Answer whether this task is past its due date"
    dueDate ifNil: [ ^ false ].
    ^ dueDate < Date today

isHighPriority
    ^ priority = #high

isMediumPriority
    ^ priority = #medium

isLowPriority
    ^ priority = #low
```

### Display Methods

```smalltalk
printOn: aStream
    "Display a readable representation"
    aStream nextPutAll: description.
    completed ifTrue: [ aStream nextPutAll: ' ✓' ]

displayString
    "String for UI display"
    | status |
    status := completed
        ifTrue: [ '[✓] ' ]
        ifFalse: [ '[ ] ' ].
    ^ status, description

fullDescription
    "Detailed description with all info"
    ^ String streamContents: [ :stream |
        stream
            nextPutAll: self displayString; cr;
            nextPutAll: 'Priority: ', priority asString; cr;
            nextPutAll: 'Category: ', category; cr.
        dueDate ifNotNil: [
            stream nextPutAll: 'Due: ', dueDate asString; cr ].
        stream nextPutAll: 'Created: ', createdAt asString ]
```

### Convenience Class Method

```smalltalk
"Class side (click 'Class' button in browser):"
description: aString
    "Create a new TodoItem with the given description"
    ^ self new
        description: aString;
        yourself
```

Now we can create items easily:

```smalltalk
TodoItem description: 'Learn Smalltalk'
```

## Step 3: The TodoList Model

A **TodoList** manages a collection of TodoItems.

```smalltalk
Object subclass: #TodoList
    instanceVariableNames: 'items name'
    classVariableNames: ''
    package: 'TodoApp-Core'
```

### Initialize

```smalltalk
initialize
    super initialize.
    items := OrderedCollection new.
    name := 'My Tasks'

name
    ^ name

name: aString
    name := aString
```

### Adding and Removing Items

```smalltalk
addItem: aTodoItem
    "Add a todo item to the list"
    items add: aTodoItem

removeItem: aTodoItem
    "Remove a todo item from the list"
    items remove: aTodoItem ifAbsent: [ ]

addDescription: aString
    "Convenience: add a new item with the given description"
    | item |
    item := TodoItem description: aString.
    self addItem: item.
    ^ item

clear
    "Remove all items"
    items removeAll
```

### Accessing Items

```smalltalk
items
    "Answer all items"
    ^ items

size
    "Answer the number of items"
    ^ items size

isEmpty
    "Answer whether the list is empty"
    ^ items isEmpty

at: index
    "Answer the item at the given index"
    ^ items at: index

includes: aTodoItem
    "Answer whether the list includes this item"
    ^ items includes: aTodoItem
```

### Filtering Methods

```smalltalk
completedItems
    "Answer all completed items"
    ^ items select: [ :item | item isComplete ]

pendingItems
    "Answer all pending (incomplete) items"
    ^ items select: [ :item | item isPending ]

itemsWithPriority: aPriority
    "Answer items with the given priority (#high, #medium, #low)"
    ^ items select: [ :item | item priority = aPriority ]

highPriorityItems
    ^ self itemsWithPriority: #high

itemsInCategory: aCategory
    "Answer items in the given category"
    ^ items select: [ :item | item category = aCategory ]

overdueItems
    "Answer items that are past their due date"
    ^ items select: [ :item | item isOverdue ]

categories
    "Answer all unique categories"
    ^ (items collect: [ :item | item category ]) asSet
```

### Statistics

```smalltalk
completedCount
    "Answer the number of completed items"
    ^ self completedItems size

pendingCount
    "Answer the number of pending items"
    ^ self pendingItems size

percentComplete
    "Answer the percentage of completed items (0-100)"
    self isEmpty ifTrue: [ ^ 0 ].
    ^ (self completedCount / self size * 100) rounded
```

### Sorting

```smalltalk
sortByPriority
    "Sort items by priority (high, medium, low)"
    | priorityOrder |
    priorityOrder := { #high -> 1. #medium -> 2. #low -> 3 } asDictionary.
    items := items sorted: [ :a :b |
        (priorityOrder at: a priority) < (priorityOrder at: b priority) ]

sortByDueDate
    "Sort items by due date (nil dates go last)"
    items := items sorted: [ :a :b |
        a dueDate ifNil: [ false ] ifNotNil: [
            b dueDate ifNil: [ true ] ifNotNil: [
                a dueDate < b dueDate ] ] ]

sortByCreatedDate
    "Sort items by creation date"
    items := items sorted: [ :a :b | a createdAt < b createdAt ]
```

### Printing

```smalltalk
printOn: aStream
    aStream
        nextPutAll: name;
        nextPutAll: ' (';
        print: self size;
        nextPutAll: ' items)'
```

## Step 4: Write Tests

Before building the UI, let's test our models!

```smalltalk
TestCase subclass: #TodoItemTest
    instanceVariableNames: 'item'
    classVariableNames: ''
    package: 'TodoApp-Tests'
```

### TodoItem Tests

```smalltalk
setUp
    item := TodoItem description: 'Test task'

testCreation
    self assert: item description equals: 'Test task'.
    self deny: item isComplete.
    self assert: item priority equals: #medium

testCompletion
    self assert: item isPending.
    item markComplete.
    self assert: item isComplete.
    item markIncomplete.
    self assert: item isPending

testToggle
    self assert: item isPending.
    item toggleComplete.
    self assert: item isComplete.
    item toggleComplete.
    self assert: item isPending

testPriority
    item priority: #high.
    self assert: item isHighPriority.
    self deny: item isMediumPriority

testOverdue
    self deny: item isOverdue.
    item dueDate: Date yesterday.
    self assert: item isOverdue.
    item dueDate: Date tomorrow.
    self deny: item isOverdue

testDisplayString
    | display |
    display := item displayString.
    self assert: (display includesSubstring: 'Test task').
    self assert: (display beginsWith: '[ ]').
    item markComplete.
    self assert: (item displayString beginsWith: '[✓]')
```

### TodoList Tests

```smalltalk
TestCase subclass: #TodoListTest
    instanceVariableNames: 'list'
    classVariableNames: ''
    package: 'TodoApp-Tests'
```

```smalltalk
setUp
    list := TodoList new

testAddingItems
    self assert: list isEmpty.
    list addDescription: 'Task 1'.
    self assert: list size equals: 1.
    self deny: list isEmpty

testRemovingItems
    | item |
    item := list addDescription: 'Task 1'.
    self assert: list size equals: 1.
    list removeItem: item.
    self assert: list isEmpty

testFiltering
    list addDescription: 'Task 1'.
    (list addDescription: 'Task 2') markComplete.
    list addDescription: 'Task 3'.

    self assert: list size equals: 3.
    self assert: list completedItems size equals: 1.
    self assert: list pendingItems size equals: 2

testStatistics
    list addDescription: 'Task 1'.
    (list addDescription: 'Task 2') markComplete.

    self assert: list completedCount equals: 1.
    self assert: list pendingCount equals: 1.
    self assert: list percentComplete equals: 50

testCategories
    (list addDescription: 'Work task') category: 'Work'.
    (list addDescription: 'Home task') category: 'Home'.
    (list addDescription: 'Another work task') category: 'Work'.

    self assert: list categories size equals: 2.
    self assert: (list categories includes: 'Work').
    self assert: (list itemsInCategory: 'Work') size equals: 2

testPriorityFiltering
    (list addDescription: 'High priority') priority: #high.
    (list addDescription: 'Low priority') priority: #low.
    list addDescription: 'Medium priority'.

    self assert: list highPriorityItems size equals: 1.
    self assert: (list itemsWithPriority: #low) size equals: 1
```

Run the tests:

```smalltalk
TodoItemTest suite run.
TodoListTest suite run
```

All green? Great! Our models work correctly.

## Step 5: Persistence

Let's save and load our todo lists!

```smalltalk
Object subclass: #TodoStore
    instanceVariableNames: ''
    classVariableNames: 'DefaultFilePath'
    package: 'TodoApp-Persistence'
```

### Class-Side Setup

```smalltalk
"Class side:"
initialize
    "Set the default file path"
    DefaultFilePath := FileLocator home / 'todos.ston'

defaultFilePath
    ^ DefaultFilePath

defaultFilePath: aFileReference
    DefaultFilePath := aFileReference
```

```smalltalk
"Execute to set up:"
TodoStore initialize
```

### Saving

We'll use **STON** (Smalltalk Object Notation) - a human-readable format:

```smalltalk
"Class side:"
save: aTodoList
    "Save the todo list to the default file"
    self save: aTodoList to: self defaultFilePath

save: aTodoList to: aFileReference
    "Save the todo list to the specified file"
    aFileReference ensureDelete.
    aFileReference writeStreamDo: [ :stream |
        STON put: aTodoList onStream: stream ]
```

### Loading

```smalltalk
"Class side:"
load
    "Load the todo list from the default file"
    ^ self loadFrom: self defaultFilePath

loadFrom: aFileReference
    "Load the todo list from the specified file"
    aFileReference exists ifFalse: [
        ^ TodoList new ].
    ^ aFileReference readStreamDo: [ :stream |
        STON fromStream: stream ]

exists
    "Answer whether a saved file exists"
    ^ self defaultFilePath exists
```

### Testing Persistence

```smalltalk
"Try it:"
| list loaded |

"Create and populate a list:"
list := TodoList new.
list name: 'My Important Tasks'.
list addDescription: 'Finish Smalltalk book'.
(list addDescription: 'Buy groceries') markComplete.

"Save it:"
TodoStore save: list.

"Load it back:"
loaded := TodoStore load.

"Check:"
loaded name.           "-> 'My Important Tasks'"
loaded size.           "-> 2"
loaded completedCount. "-> 1"
```

Perfect! Our tasks persist across sessions.

## Step 6: The User Interface

Now for the UI! We'll use **Spec 2** - Pharo's UI framework.

### TodoListPresenter

The main window:

```smalltalk
SpPresenter subclass: #TodoListPresenter
    instanceVariableNames: 'todoList itemsTable addButton deleteButton toggleButton filterDropdown statisticsLabel'
    classVariableNames: ''
    package: 'TodoApp-UI'
```

### Initialize

```smalltalk
initialize
    super initialize.
    todoList := TodoList new

setModelBeforeInitialization: aTodoList
    todoList := aTodoList
```

### Layout

```smalltalk
defaultLayout
    ^ SpBoxLayout newTopToBottom
        add: (SpBoxLayout newLeftToRight
            add: statisticsLabel;
            add: filterDropdown width: 150;
            yourself)
        expand: false;
        add: itemsTable;
        add: (SpBoxLayout newLeftToRight
            add: addButton;
            add: toggleButton;
            add: deleteButton;
            yourself)
        expand: false;
        yourself
```

### Initialize Presenters

```smalltalk
initializePresenters
    "Create the table"
    itemsTable := self newTable.
    itemsTable
        addColumn: (SpCheckBoxTableColumn new
            width: 30;
            evaluated: [ :item | item isComplete ];
            onActivation: [ :item | item toggleComplete. self updateView ];
            onDeactivation: [ :item | item toggleComplete. self updateView ];
            yourself);
        addColumn: (SpStringTableColumn new
            title: 'Description';
            evaluated: [ :item | item description ];
            yourself);
        addColumn: (SpStringTableColumn new
            title: 'Priority';
            width: 80;
            evaluated: [ :item | item priority asString ];
            yourself);
        addColumn: (SpStringTableColumn new
            title: 'Category';
            width: 100;
            evaluated: [ :item | item category ];
            yourself);
        addColumn: (SpStringTableColumn new
            title: 'Due Date';
            width: 100;
            evaluated: [ :item |
                item dueDate
                    ifNil: [ '' ]
                    ifNotNil: [ :date | date asString ] ];
            yourself).

    "Create buttons"
    addButton := self newButton
        label: 'Add Task';
        action: [ self addTask ];
        yourself.

    toggleButton := self newButton
        label: 'Toggle Complete';
        action: [ self toggleSelectedTask ];
        yourself.

    deleteButton := self newButton
        label: 'Delete';
        action: [ self deleteSelectedTask ];
        yourself.

    "Create filter dropdown"
    filterDropdown := self newDropList
        items: #('All' 'Active' 'Completed' 'High Priority' 'Overdue');
        selectIndex: 1;
        whenSelectedItemChangedDo: [ self updateView ];
        yourself.

    "Create statistics label"
    statisticsLabel := self newLabel.

    self updateView
```

### Actions

```smalltalk
addTask
    "Open dialog to add a new task"
    | description |
    description := self request: 'Task description:'.
    description ifNil: [ ^ self ].
    description ifEmpty: [ ^ self ].

    todoList addDescription: description.
    self updateView.
    self saveList

deleteSelectedTask
    "Delete the currently selected task"
    | selected |
    selected := itemsTable selection selectedItem.
    selected ifNil: [ ^ self ].

    (self confirm: 'Delete this task?') ifFalse: [ ^ self ].

    todoList removeItem: selected.
    self updateView.
    self saveList

toggleSelectedTask
    "Toggle completion of the selected task"
    | selected |
    selected := itemsTable selection selectedItem.
    selected ifNil: [ ^ self ].

    selected toggleComplete.
    self updateView.
    self saveList

updateView
    "Refresh the display"
    | filteredItems |

    "Apply filter"
    filteredItems := self currentFilter.

    "Update table"
    itemsTable items: filteredItems.

    "Update statistics"
    statisticsLabel label: self statisticsString

currentFilter
    "Answer the items for the current filter"
    | filter |
    filter := filterDropdown selectedItem.
    filter = 'All' ifTrue: [ ^ todoList items ].
    filter = 'Active' ifTrue: [ ^ todoList pendingItems ].
    filter = 'Completed' ifTrue: [ ^ todoList completedItems ].
    filter = 'High Priority' ifTrue: [ ^ todoList highPriorityItems ].
    filter = 'Overdue' ifTrue: [ ^ todoList overdueItems ].
    ^ todoList items

statisticsString
    ^ String streamContents: [ :s |
        s
            print: todoList size;
            nextPutAll: ' tasks  •  ';
            print: todoList completedCount;
            nextPutAll: ' completed  •  ';
            print: todoList percentComplete;
            nextPutAll: '% done' ]

saveList
    "Save the todo list to disk"
    [ TodoStore save: todoList ]
        on: Error
        do: [ :ex | self inform: 'Could not save: ', ex messageText ]
```

### Window Configuration

```smalltalk
initializeWindow: aWindowPresenter
    super initializeWindow: aWindowPresenter.
    aWindowPresenter
        title: 'Todo List Manager';
        initialExtent: 800@600;
        whenClosedDo: [ self saveList ]
```

### Class-Side Convenience Methods

```smalltalk
"Class side:"
open
    "Open a new todo list"
    ^ self new open

openWithList: aTodoList
    "Open with an existing list"
    ^ self on: aTodoList

on: aTodoList
    ^ self new
        setModelBeforeInitialization: aTodoList;
        open

openSaved
    "Open the saved todo list"
    ^ self openWithList: TodoStore load
```

## Step 7: Launch the Application!

Now let's run it:

```smalltalk
TodoListPresenter openSaved
```

🎉 You have a working Todo List Manager!

Try it:
1. Click **Add Task** to create tasks
2. Check boxes to mark tasks complete
3. Select a task and click **Toggle Complete**
4. Use the filter dropdown to view different subsets
5. Click **Delete** to remove tasks

When you close the window, your tasks are saved automatically!

## Step 8: Enhancements

Let's add more features!

### Advanced Task Editor

Create a better dialog for adding/editing tasks:

```smalltalk
SpPresenter subclass: #TodoEditorPresenter
    instanceVariableNames: 'item descriptionInput priorityDropdown categoryInput dueDateInput'
    classVariableNames: ''
    package: 'TodoApp-UI'
```

```smalltalk
setModelBeforeInitialization: aTodoItem
    item := aTodoItem

initializePresenters
    descriptionInput := self newText
        placeholder: 'Task description...';
        text: (item ifNil: [ '' ] ifNotNil: [ item description ]);
        yourself.

    priorityDropdown := self newDropList
        items: #(#high #medium #low);
        selectItem: (item ifNil: [ #medium ] ifNotNil: [ item priority ]);
        yourself.

    categoryInput := self newTextInput
        placeholder: 'Category';
        text: (item ifNil: [ 'General' ] ifNotNil: [ item category ]);
        yourself.

    dueDateInput := self newTextInput
        placeholder: 'YYYY-MM-DD';
        text: (item ifNil: [ '' ] ifNotNil: [
            item dueDate ifNil: [ '' ] ifNotNil: [ :d | d asString ] ]);
        yourself

defaultLayout
    ^ SpBoxLayout newTopToBottom
        add: 'Description:' expand: false;
        add: descriptionInput height: 100;
        add: 'Priority:' expand: false;
        add: priorityDropdown expand: false;
        add: 'Category:' expand: false;
        add: categoryInput expand: false;
        add: 'Due Date:' expand: false;
        add: dueDateInput expand: false;
        yourself

accept
    "Apply changes to the item"
    item description: descriptionInput text.
    item priority: priorityDropdown selectedItem.
    item category: categoryInput text.

    dueDateInput text ifNotEmpty: [
        [ item dueDate: dueDateInput text asDate ]
            on: Error
            do: [ self inform: 'Invalid date format' ] ].

    ^ item

"Class side:"
editItem: aTodoItem
    "Open editor for an existing item"
    ^ self on: aTodoItem

on: aTodoItem
    ^ self new
        setModelBeforeInitialization: aTodoItem;
        openDialog

createNew
    "Open editor for a new item"
    ^ self on: TodoItem new
```

### Update TodoListPresenter to Use Editor

Modify the `addTask` method:

```smalltalk
addTask
    "Open dialog to add a new task"
    | editor item |
    editor := TodoEditorPresenter createNew.
    editor
        okAction: [
            item := editor accept.
            todoList addItem: item.
            self updateView.
            self saveList ];
        open
```

Add an edit action:

```smalltalk
editSelectedTask
    "Edit the selected task"
    | selected editor |
    selected := itemsTable selection selectedItem.
    selected ifNil: [ ^ self ].

    editor := TodoEditorPresenter editItem: selected.
    editor
        okAction: [
            editor accept.
            self updateView.
            self saveList ];
        open
```

Add an edit button to the layout:

```smalltalk
"In initializePresenters, add:"
editButton := self newButton
    label: 'Edit';
    action: [ self editSelectedTask ];
    yourself.

"And add to the button layout:"
add: editButton;
```

## Step 9: Context Menus

Add right-click actions:

```smalltalk
initializePresenters
    "... existing code ..."

    "Add context menu to table"
    itemsTable contextMenu: [ self itemContextMenu ]

itemContextMenu
    ^ self newMenu
        addItem: [ :item |
            item
                name: 'Edit...';
                action: [ self editSelectedTask ] ];
        addItem: [ :item |
            item
                name: 'Toggle Complete';
                action: [ self toggleSelectedTask ] ];
        addItem: [ :item |
            item
                name: 'Delete';
                action: [ self deleteSelectedTask ] ];
        yourself
```

Now right-click items for quick actions!

## Step 10: Keyboard Shortcuts

Make the app keyboard-friendly:

```smalltalk
initializeWindow: aWindowPresenter
    super initializeWindow: aWindowPresenter.
    aWindowPresenter
        title: 'Todo List Manager';
        initialExtent: 800@600;
        whenClosedDo: [ self saveList ].

    "Add keyboard shortcuts"
    aWindowPresenter whenOpenedDo: [
        self bindKeys ]

bindKeys
    self bindKeyCombination: $n command
        toAction: [ self addTask ].

    self bindKeyCombination: $d command
        toAction: [ self deleteSelectedTask ].

    self bindKeyCombination: Character space
        toAction: [ self toggleSelectedTask ]
```

Now:
- **Cmd/Ctrl+N** adds a task
- **Cmd/Ctrl+D** deletes selected task
- **Space** toggles completion

## Complete Feature List

Our Todo List Manager now has:

✅ Add, edit, delete tasks
✅ Mark tasks complete/incomplete
✅ Priority levels (high, medium, low)
✅ Categories
✅ Due dates
✅ Filter views (all, active, completed, high priority, overdue)
✅ Statistics display
✅ Automatic saving/loading
✅ Context menus
✅ Keyboard shortcuts
✅ Clean, organized code
✅ Comprehensive tests

## Try This!

Enhance the application:

1. **Search Feature**
   Add a search box to filter by description:
   ```smalltalk
   itemsMatching: searchString
       ^ items select: [ :item |
           item description includesSubstring: searchString caseSensitive: false ]
   ```

2. **Sort Options**
   Add a sort dropdown with options:
   - By priority
   - By due date
   - By creation date
   - By completion status

3. **Multiple Lists**
   Create multiple named lists:
   ```smalltalk
   TodoListManager new
       addList: (TodoList new name: 'Work'; yourself);
       addList: (TodoList new name: 'Personal'; yourself)
   ```

4. **Export to Text**
   Generate a text file of all tasks:
   ```smalltalk
   exportAsText
       ^ String streamContents: [ :stream |
           todoList items do: [ :item |
               stream
                   nextPutAll: item displayString; cr ] ]
   ```

5. **Recurring Tasks**
   Add support for tasks that repeat daily/weekly:
   ```smalltalk
   TodoItem >> recurrence: aSymbol
       "aSymbol is #daily, #weekly, #monthly, or nil"
   ```

6. **Subtasks**
   Allow tasks to have subtasks:
   ```smalltalk
   TodoItem >> subtasks
       ^ subtasks ifNil: [ subtasks := OrderedCollection new ]
   ```

7. **Dark Mode**
   Add a theme toggle:
   ```smalltalk
   toggleTheme
       Smalltalk ui theme: (
           Smalltalk ui theme isDark
               ifTrue: [ Smalltalk ui theme light ]
               ifFalse: [ Smalltalk ui theme dark ] )
   ```

8. **Notifications**
   Show system notifications for overdue tasks:
   ```smalltalk
   checkOverdueTasks
       self overdueItems do: [ :item |
           Notification signal: item description, ' is overdue!' ]
   ```

## Architecture Review

Our application demonstrates:

### Model-View-Presenter (MVP)

- **Model** (TodoItem, TodoList): Business logic, no UI
- **View** (Spec UI): Display only
- **Presenter** (TodoListPresenter): Coordinates model and view

### Separation of Concerns

- **Core**: Domain models
- **UI**: Presentation logic
- **Persistence**: Storage
- **Tests**: Verification

### Best Practices

- **Single Responsibility**: Each class has one job
- **Encapsulation**: Internal state is private
- **Testability**: Models testable without UI
- **Persistence**: Separate from business logic
- **User Experience**: Keyboard shortcuts, context menus

## What You Learned

Building this application, you practiced:

1. **Object-Oriented Design**
   - Creating classes with clear responsibilities
   - Using collections effectively
   - Implementing queries and commands

2. **Testing**
   - Writing comprehensive tests
   - Testing models independently
   - TDD workflow

3. **UI Development**
   - Spec 2 framework
   - Layouts and presenters
   - Event handling

4. **Persistence**
   - STON serialization
   - File I/O
   - Error handling

5. **Package Organization**
   - Logical code structure
   - Dependency management
   - Clean architecture

6. **Real-World Development**
   - Building complete applications
   - User experience design
   - Iteration and enhancement

## The Power of Live Coding

Notice how you built this:
- **No compile-restart cycle** - Added features while running
- **Immediate feedback** - Test changes instantly
- **Interactive development** - Inspect objects live
- **Debugger-driven development** - Fix issues on the fly

This is Smalltalk's superpower!

## Looking Ahead

You've built a complete, real application! You now understand:
- End-to-end application development
- Model-View-Presenter architecture
- Testing strategies
- Persistence
- User interface design
- Code organization

In Chapter 32, we'll build a **Text Adventure Game** - exploring different aspects of Smalltalk: parsing, state machines, storytelling, and more!

Then Chapter 33 creates a **Simple Web Server** - HTTP, routing, and serving web applications!

Part IX shows you Smalltalk's versatility - from desktop apps to games to web servers!

---

**Key Takeaways:**
- Built a complete **Todo List Manager** from scratch
- Applied **Model-View-Presenter** architecture
- Created clean separation: Core, UI, Persistence, Tests
- Used **Spec 2** for user interface
- Implemented **STON** for persistence
- Added **keyboard shortcuts** and **context menus**
- Wrote **comprehensive tests** for models
- Organized code in **logical packages**
- Demonstrated **live coding** benefits
- Created a maintainable, extensible application
- Used collections, blocks, and OOP effectively
- Implemented filtering, sorting, and statistics
- Built professional-quality software in Smalltalk
- Showed real-world development workflow
- Proved Smalltalk's power for rapid application development

---

[Previous: Chapter 30 - Other Smalltalks Worth Knowing](chapter-30-other-smalltalks.md) | [Next: Chapter 32 - Project 2: A Text Adventure Game](chapter-32-project-text-adventure.md)
