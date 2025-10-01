# Chapter 26: Packages and Code Organization

You've learned to write classes, methods, and tests. But as projects grow, organization becomes crucial. How do you structure a codebase with hundreds of classes? How do you separate concerns? How do you manage dependencies?

Smalltalk uses **packages** to organize code. Packages group related classes, manage dependencies, and make projects maintainable and navigable.

In this chapter, you'll learn to organize code professionally, structure larger projects, and manage dependencies effectively.

## What Are Packages?

A **package** is a named collection of related classes. Think of it as a folder or module:

```
MyApp (package)
├─ Customer (class)
├─ Order (class)
├─ Product (class)
└─ Invoice (class)
```

All related classes in one package!

### Why Packages?

Without packages:
- All classes in one giant list
- Hard to find things
- No clear organization
- Difficult to understand project structure

With packages:
- Related classes grouped together
- Clear project structure
- Easy navigation
- Logical separation of concerns

## Creating Packages

### In the System Browser

1. Right-click in the Package pane
2. Choose `New package`
3. Enter name: `MyApp`
4. Press Enter

Or evaluate:

```smalltalk
RPackageOrganizer default createPackageNamed: 'MyApp'
```

### Package Naming Conventions

**Good names:**
- `MyApp` - The main application package
- `MyApp-Core` - Core functionality
- `MyApp-UI` - User interface
- `MyApp-Tests` - Tests
- `MyApp-Examples` - Examples and demos

**Naming patterns:**
- Use hyphens to separate parts: `Package-Subpackage`
- Keep names descriptive
- Group related functionality

## Organizing Code into Packages

### Small Project (Single Package)

For tiny projects:

```
TodoApp
├─ TodoItem
├─ TodoList
└─ TodoManager
```

One package is enough!

### Medium Project (Multiple Packages)

For larger projects, separate concerns:

```
MyApp
├─ MyApp-Core
│  ├─ Domain models
│  └─ Business logic
├─ MyApp-UI
│  └─ User interface
├─ MyApp-Persistence
│  └─ Database access
├─ MyApp-Network
│  └─ HTTP/API code
└─ MyApp-Tests
   └─ All tests
```

Each package has a clear responsibility!

### Large Project (Many Packages)

For complex systems:

```
MyApp
├─ MyApp-Core-Model
├─ MyApp-Core-Services
├─ MyApp-UI-Components
├─ MyApp-UI-Views
├─ MyApp-Persistence-Database
├─ MyApp-Persistence-Cache
├─ MyApp-Network-HTTP
├─ MyApp-Network-WebSockets
├─ MyApp-Tests-Unit
├─ MyApp-Tests-Integration
└─ MyApp-Examples
```

Highly modular!

## Package Dependencies

Packages can depend on other packages:

```
MyApp-UI
  ↓ depends on
MyApp-Core
```

The UI package uses classes from the Core package.

### Viewing Dependencies

In Pharo, you can visualize dependencies:

```smalltalk
(RPackageOrganizer default packageNamed: 'MyApp-UI') dependencies
```

Or use tools like **Moose** for dependency analysis.

### Managing Dependencies

Keep dependencies clear and minimal:

**Good:**
```
MyApp-UI → MyApp-Core
MyApp-Tests → MyApp-Core
```

Clean, one-way dependencies!

**Bad:**
```
MyApp-UI ↔ MyApp-Core
```

Circular dependencies! Avoid these!

## Layered Architecture

A common pattern is layered architecture:

```
Presentation Layer (UI)
    ↓
Business Logic Layer (Core)
    ↓
Data Access Layer (Persistence)
```

Each layer depends only on the layer below:

```
MyApp-UI
  ↓
MyApp-Core
  ↓
MyApp-Persistence
```

**Rules:**
- UI can use Core, but Core can't use UI
- Core can use Persistence, but Persistence can't use Core
- Keep dependencies one-way!

## Separation of Concerns

Organize packages by concern:

### By Feature

```
MyApp-Users
MyApp-Orders
MyApp-Products
MyApp-Billing
```

Each package represents a feature/domain.

### By Layer

```
MyApp-UI
MyApp-Domain
MyApp-Infrastructure
```

Each package represents a technical layer.

### Hybrid

```
MyApp-Users-Domain
MyApp-Users-UI
MyApp-Orders-Domain
MyApp-Orders-UI
```

Combine both approaches!

### Which to Choose?

- **Small projects**: Single package or simple split (Core + UI + Tests)
- **Medium projects**: By layer or by feature
- **Large projects**: Hybrid (features × layers)

## Test Packages

Tests go in separate packages:

```
MyApp-Core
MyApp-Core-Tests

MyApp-UI
MyApp-UI-Tests
```

**Why separate?**
- Don't ship tests with production code
- Clear separation
- Can exclude tests from code analysis

**Naming convention:** Append `-Tests` to the package name.

## Tag Organization Within Packages

Within a package, you can use **tags** to further organize classes:

```
MyApp-Core
├─ Model (tag)
│  ├─ User
│  ├─ Order
│  └─ Product
└─ Services (tag)
   ├─ OrderService
   └─ PaymentService
```

Tags are like sub-packages, but lighter weight.

### Creating Tags

1. In the System Browser, select a package
2. Right-click on a class
3. Choose `Move to tag...`
4. Enter tag name: `Model`

Or set the tag in the class definition:

```smalltalk
Object subclass: #User
    instanceVariableNames: 'name email'
    classVariableNames: ''
    package: 'MyApp-Core-Model'
```

The part after the hyphen (`Model`) becomes the tag!

## Package Manifests

A **manifest** is a class that describes a package:

```smalltalk
PackageManifest subclass: #MyAppManifest
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'MyApp'
```

Manifests can:
- Document the package
- Declare dependencies
- Configure code critics
- Provide package metadata

### Example Manifest

```smalltalk
description
    ^ 'MyApp is a todo list application with rich features.'

version
    ^ '1.0.0'

author
    ^ 'Your Name'

license
    ^ 'MIT'
```

## Practical Example: E-Commerce System

Let's organize a realistic project:

### Structure

```
ECommerce-Core-Model
├─ Customer
├─ Product
├─ Order
├─ OrderItem
└─ ShoppingCart

ECommerce-Core-Services
├─ OrderService
├─ PaymentService
├─ InventoryService
└─ NotificationService

ECommerce-UI-Components
├─ ProductCard
├─ ShoppingCartView
├─ CheckoutForm
└─ OrderHistoryView

ECommerce-Persistence
├─ CustomerRepository
├─ ProductRepository
└─ OrderRepository

ECommerce-Tests-Unit
├─ CustomerTest
├─ OrderTest
└─ ShoppingCartTest

ECommerce-Tests-Integration
├─ OrderServiceTest
└─ PaymentServiceTest
```

### Dependencies

```
ECommerce-UI-Components
  ↓
ECommerce-Core-Services
  ↓
ECommerce-Core-Model
  ↓
ECommerce-Persistence
```

Tests depend on everything they test!

### Benefits

- Clear structure
- Easy to find classes
- Testable in isolation
- Can deploy only what's needed

## Loading and Unloading Packages

### Load a Package

In Pharo, use Iceberg or Metacello (more on this in Chapter 27).

```smalltalk
Metacello new
    baseline: 'MyApp';
    repository: 'github://username/MyApp:main';
    load
```

### Unload a Package

```smalltalk
(RPackageOrganizer default packageNamed: 'MyApp') removeFromSystem
```

**Warning**: This removes all classes in the package!

## Baseline: Declaring Dependencies

A **baseline** is a configuration that declares package dependencies:

```smalltalk
BaselineOf subclass: #BaselineOfMyApp
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'BaselineOfMyApp'
```

```smalltalk
baseline: spec
    <baseline>
    spec for: #common do: [
        "Packages"
        spec
            package: 'MyApp-Core';
            package: 'MyApp-UI' with: [ spec requires: #('MyApp-Core') ];
            package: 'MyApp-Tests' with: [ spec requires: #('MyApp-Core' 'MyApp-UI') ].

        "Groups"
        spec
            group: 'default' with: #('MyApp-Core' 'MyApp-UI');
            group: 'tests' with: #('MyApp-Tests');
            group: 'all' with: #('default' 'tests') ]
```

This declares:
- `MyApp-UI` depends on `MyApp-Core`
- `MyApp-Tests` depends on both
- Groups for loading subsets

### Loading with Baseline

```smalltalk
Metacello new
    baseline: 'MyApp';
    repository: 'github://username/MyApp:main';
    load: 'default'
```

Loads `MyApp-Core` and `MyApp-UI` (but not tests).

```smalltalk
load: 'tests'
```

Loads tests too!

## Best Practices

### 1. One Responsibility per Package

Each package should have a clear, single responsibility:

**Good:**
- `MyApp-Authentication` - Only authentication
- `MyApp-Reporting` - Only reporting

**Bad:**
- `MyApp-Stuff` - Random unrelated classes

### 2. Minimize Dependencies

Fewer dependencies = simpler system.

**Good:**
```
A → B → C
```

Linear dependencies.

**Bad:**
```
A ↔ B ↔ C ↔ D
```

Tangled web of dependencies!

### 3. Avoid Circular Dependencies

Never create circular dependencies:

**Bad:**
```
MyApp-UI → MyApp-Core
MyApp-Core → MyApp-UI
```

Refactor to break the cycle!

### 4. Depend on Abstractions

Higher-level packages should depend on abstractions, not concrete implementations:

```
MyApp-UI
  ↓
MyApp-Core-Interfaces (abstract)
  ↑
MyApp-Core-Implementation (concrete)
```

### 5. Separate Tests

Always put tests in separate packages:

```
MyApp
MyApp-Tests
```

### 6. Document Packages

Add package comments (via manifests) explaining purpose and structure.

### 7. Use Consistent Naming

Stick to a naming convention:
- `ProjectName-Feature`
- `ProjectName-Feature-Tests`

### 8. Keep Packages Cohesive

Classes in a package should be highly related. If a class doesn't fit, it probably belongs elsewhere.

## Package Anti-Patterns

### The God Package

**Anti-pattern:** One package with all classes.

```
MyApp
├─ Customer
├─ OrderService
├─ PaymentGateway
├─ UIComponent
├─ DatabaseConnection
└─ ... (100 more classes)
```

**Problem:** No organization, hard to navigate.

**Fix:** Split into logical packages!

### The Over-Engineered Hierarchy

**Anti-pattern:** Too many tiny packages.

```
MyApp-Core-Model-Domain-Entities-User
```

**Problem:** Overly complex, hard to understand.

**Fix:** Keep it simple!

### Circular Dependencies

**Anti-pattern:** Packages depending on each other.

```
A → B → C → A
```

**Problem:** Can't load independently, tightly coupled.

**Fix:** Refactor to break cycles!

### Leaky Abstractions

**Anti-pattern:** Implementation details exposed across package boundaries.

**Fix:** Define clear interfaces, hide implementations.

## Navigating Packages

### In the System Browser

Packages appear in the leftmost pane. Click to see classes in that package.

### Finding Packages

```smalltalk
RPackageOrganizer default packages
```

Returns all packages.

```smalltalk
RPackageOrganizer default packageNamed: 'MyApp-Core'
```

Gets a specific package.

### Package Contents

```smalltalk
(RPackageOrganizer default packageNamed: 'MyApp-Core') definedClasses
```

Lists all classes in the package.

## Refactoring Package Structure

As projects evolve, reorganize packages:

### Moving Classes

1. In System Browser, right-click on class
2. Choose `Move to package...`
3. Select destination package

Or programmatically:

```smalltalk
User package: (RPackageOrganizer default packageNamed: 'MyApp-Core-Model')
```

### Renaming Packages

1. Right-click on package
2. Choose `Rename...`
3. Enter new name

Or:

```smalltalk
(RPackageOrganizer default packageNamed: 'OldName') renameTo: 'NewName'
```

### Splitting Packages

Extract a subset of classes into a new package:

1. Create new package: `MyApp-NewFeature`
2. Move relevant classes to it
3. Update dependencies

## Try This!

Practice organizing code:

1. **Create a multi-package project:**
   ```smalltalk
   "Create packages:"
   RPackageOrganizer default createPackageNamed: 'Library-Core'.
   RPackageOrganizer default createPackageNamed: 'Library-UI'.
   RPackageOrganizer default createPackageNamed: 'Library-Tests'.

   "Add classes to each package"
   ```

2. **Organize a todo app:**
   ```
   TodoApp-Core
   ├─ TodoItem
   ├─ TodoList
   └─ TodoManager

   TodoApp-UI
   ├─ TodoItemView
   └─ TodoListView

   TodoApp-Tests
   ├─ TodoItemTest
   └─ TodoListTest
   ```

3. **Create a baseline:**
   ```smalltalk
   BaselineOf subclass: #BaselineOfTodoApp
       package: 'BaselineOfTodoApp'

   baseline: spec
       <baseline>
       spec for: #common do: [
           spec
               package: 'TodoApp-Core';
               package: 'TodoApp-UI' with: [ spec requires: #('TodoApp-Core') ];
               package: 'TodoApp-Tests' with: [ spec requires: #('TodoApp-Core' 'TodoApp-UI') ] ]
   ```

4. **Visualize dependencies:**
   Explore your package structure. Which packages depend on which?

5. **Refactor a messy project:**
   Take a project with all classes in one package. Split it into logical packages.

6. **Study system packages:**
   ```smalltalk
   RPackageOrganizer default packages
   ```

   Explore how Pharo itself is organized!

## Real-World Example: Seaside Web Framework

Seaside (a web framework) has excellent package organization:

```
Seaside-Core
├─ Core classes

Seaside-Canvas
├─ HTML rendering

Seaside-Session
├─ Session management

Seaside-Component
├─ Component framework

Seaside-Tests-Core
Seaside-Tests-Canvas
...
```

Each package has a clear responsibility. Dependencies are well-managed.

Study Seaside's structure to learn professional organization!

## The Big Picture

Package organization is about:
- **Clarity** - Easy to understand project structure
- **Maintainability** - Easy to modify and extend
- **Testability** - Easy to test in isolation
- **Reusability** - Can reuse packages in other projects
- **Collaboration** - Team members know where things go

Good organization scales. Bad organization creates chaos.

## Looking Ahead

You now understand package organization! You can:
- Create packages for logical grouping
- Structure projects with multiple packages
- Manage dependencies between packages
- Use baselines to declare dependencies
- Organize tests separately
- Refactor package structures as projects evolve

This completes Part VII (Intermediate Concepts)! You've learned:
- Protocols and polymorphism (Chapter 23)
- Error handling (Chapter 24)
- Testing with SUnit (Chapter 25)
- Code organization with packages (Chapter 26)

In Part VIII (Exploring the Smalltalk Variations), we'll explore different Smalltalk implementations:
- Chapter 27: Pharo - The Modern Smalltalk
- Chapter 28: Squeak - Multimedia and Education
- Chapter 29: Glamorous Toolkit - Moldable Development
- Chapter 30: Other Smalltalks Worth Knowing

Each Smalltalk has unique features and strengths. You'll learn which to use for different purposes!

---

**Key Takeaways:**
- **Packages** group related classes for organization
- Create packages in System Browser or programmatically
- **Naming convention**: `ProjectName-Feature` or `ProjectName-Feature-Tests`
- **Separate concerns**: Different packages for different responsibilities
- **Layered architecture**: UI → Core → Persistence (one-way dependencies)
- **Tests** go in separate packages ending with `-Tests`
- **Tags** provide sub-organization within packages
- **Baselines** declare package dependencies formally
- **Avoid circular dependencies** - keep dependencies one-way
- **Package manifests** document and configure packages
- Move classes between packages as needed during refactoring
- Study system packages and open-source projects to learn good organization
- Good organization = clarity, maintainability, testability, reusability
- Scale organization to project size: simple for small, modular for large

---

[Previous: Chapter 25 - Testing Your Code](chapter-25-testing.md) | [Next: Chapter 27 - Pharo - The Modern Smalltalk](chapter-27-pharo.md)
