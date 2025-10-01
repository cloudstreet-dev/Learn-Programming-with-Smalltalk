# Chapter 18: Version Control for Smalltalkers

You've learned about the Image, Changes, and Sources files - Smalltalk's unique three-file system. Now it's time to learn how to integrate with modern version control systems, especially **Git**.

While the Changes file provides personal history, Git provides team collaboration, branching, merging, and professional code management. Mastering Git with Smalltalk is essential for real-world development.

In this chapter, we'll explore how Smalltalk works with Git, focusing on **Iceberg** - Pharo's integrated Git client.

## Why Version Control?

The Changes file is great for personal history, but it has limitations:

### Changes File Limitations:
- **No branching** - Can't work on multiple features simultaneously
- **No merging** - Can't combine changes from multiple developers
- **No remote collaboration** - Hard to share with team
- **Image-bound** - Tied to one Image file
- **Binary Image** - Can't diff or merge Images

### Git Solves These Problems:
- **Branching** - Work on features independently
- **Merging** - Combine work from multiple developers
- **Remote repos** - GitHub, GitLab, Bitbucket for sharing
- **Text-based** - Source code as text files
- **Standard tool** - Used across the industry
- **History** - Complete project history
- **Collaboration** - Pull requests, code review, CI/CD

## The Challenge: Image vs Files

Traditional programming:
```
Write code in .c files
├─ main.c
├─ utils.c
└─ lib.c

Commit files to Git
```

Smalltalk:
```
Write code in living Image
├─ All classes in memory
├─ All methods as objects
└─ No separate files

Need to export to text files for Git!
```

This is the challenge: How do we bridge the gap between Smalltalk's Image-based system and Git's file-based system?

## Introducing Iceberg

**Iceberg** is Pharo's integrated Git client. It:
- Exports Smalltalk code to text files (Tonel format)
- Commits those files to Git
- Loads code from Git back into the Image
- Manages branches, merges, and remotes
- All from within Pharo!

Iceberg is already included in Pharo. You don't need to install anything.

## Opening Iceberg

Launch Iceberg:
- **World menu** → `Iceberg` (usually in the toolbar)
- **Or press** `Ctrl+O` `I` (or `Cmd+O` `I` on macOS)

The Iceberg window opens, showing:
- List of repositories (initially empty)
- Buttons to add, clone, or create repositories

## Your First Git Repository with Iceberg

Let's create a Git repository for your code:

### Step 1: Create a Package

First, create a package with some code:

1. Open System Browser (`Ctrl+O` `B`)
2. Create a package: `MyApp`
3. Create a class:
   ```smalltalk
   Object subclass: #Greeter
       instanceVariableNames: 'name'
       classVariableNames: ''
       package: 'MyApp'
   ```
4. Add methods:
   ```smalltalk
   name
       ^ name

   name: aString
       name := aString

   greet
       ^ 'Hello, ' , name , '!'
   ```

### Step 2: Create a Git Repository

In Iceberg:

1. Click `New repository` (or `+` button)
2. Choose `New repository`
3. Fill in:
   - **Project name**: MyApp
   - **Location**: Where to store the repo (e.g., `~/projects/MyApp`)
4. Click `Create repository`

Iceberg creates:
- A Git repository at that location
- A `src/` directory for code
- Exports your package to `src/MyApp/`

### Step 3: Initial Commit

Iceberg shows your repository with uncommitted changes:

1. Click on the repository to open it
2. You'll see uncommitted changes (the `Greeter` class)
3. Click `Commit` button
4. Enter a commit message: "Initial commit: Add Greeter class"
5. Click `Commit`

Your code is now committed to Git!

### Step 4: View on Disk

Open the repository directory in your file explorer. You'll see:

```
MyApp/
  .git/             (Git metadata)
  src/
    MyApp/
      Greeter.class.st
      package.st
  .gitignore
  .project
```

The `Greeter.class.st` file contains your class definition and methods as text!

Open it in a text editor:

```smalltalk
Class {
    #name : #Greeter,
    #superclass : #Object,
    #instVars : [
        'name'
    ],
    #category : #MyApp
}

{ #category : #accessing }
Greeter >> greet [
    ^ 'Hello, ' , name , '!'
]

{ #category : #accessing }
Greeter >> name [
    ^ name
]

{ #category : #accessing }
Greeter >> name: aString [
    name := aString
]
```

This is the **Tonel format** - human-readable Smalltalk code in text files. Perfect for Git!

## Working with Iceberg

### Making Changes

Modify the `greet` method:

```smalltalk
greet
    ^ 'Hello, ' , name , '! Welcome to Smalltalk.'
```

In Iceberg:

1. Your repository shows "uncommitted changes"
2. Click to view changes
3. You see the diff (what changed)
4. Enter a commit message: "Update greeting message"
5. Click `Commit`

### Viewing History

In Iceberg:

1. Click on your repository
2. Click `History` button
3. You see all commits with messages, timestamps, and authors

### Checking Status

Iceberg shows the status of each repository:
- **Green checkmark** - Clean, no uncommitted changes
- **Orange dot** - Uncommitted changes
- **Red X** - Conflicts or errors

## Connecting to GitHub

To share your code, connect to GitHub (or GitLab, Bitbucket, etc.):

### Step 1: Create a GitHub Repository

1. Go to github.com
2. Create a new repository: `MyApp`
3. Don't initialize with README (we already have code)
4. Copy the repository URL: `https://github.com/yourusername/MyApp.git`

### Step 2: Add Remote in Iceberg

In Iceberg:

1. Click on your repository
2. Right-click → `Repository` → `Add remote`
3. Name: `origin`
4. URL: `https://github.com/yourusername/MyApp.git`
5. Click `OK`

### Step 3: Push to GitHub

1. Click `Push` button
2. Enter GitHub credentials (or use SSH keys)
3. Your code is pushed to GitHub!

Now anyone can see your code on GitHub!

### Step 4: Pull Changes

If someone else makes changes on GitHub:

1. Click `Pull` button in Iceberg
2. Changes are downloaded and merged into your Image
3. Resolve any conflicts if needed

## Cloning a Repository

To work on someone else's project:

### In Iceberg:

1. Click `Clone repository`
2. Enter the repository URL (e.g., from GitHub)
3. Choose a local directory
4. Click `Clone`
5. Iceberg clones the repo and loads the code into your Image

Now you have their code loaded and can work on it!

## Branching and Merging

Git's branching is powerful for managing features:

### Create a Branch

In Iceberg:

1. Click on your repository
2. Right-click → `Branches` → `New branch`
3. Name: `feature-polite-greeting`
4. Click `Create`

You're now on the new branch!

### Make Changes

Modify the code:

```smalltalk
greet
    ^ 'Good day, ' , name , '! How are you?'
```

Commit:
```
"Commit message: Make greeting more polite"
```

### Switch Branches

In Iceberg:

1. Right-click → `Branches`
2. Select `main` (or `master`)
3. Click `Checkout`

Your Image updates to the `main` branch code! The greeting reverts to the old version because you switched branches.

### Merge

To merge your feature branch into main:

1. Switch to `main` branch
2. Right-click → `Branches` → `Merge`
3. Select `feature-polite-greeting`
4. Click `Merge`

The changes from the feature branch are now in main!

## Tonel Format

Iceberg uses the **Tonel format** for storing code as text:

### One File per Class

```
src/
  MyApp/
    Greeter.class.st
    Calculator.class.st
    package.st
```

Each class is a separate file, making diffs clear and merge conflicts rare.

### Human-Readable

The .st files are readable Smalltalk code, not binary. You can:
- View them in GitHub
- Diff them easily
- Merge changes
- Read code without Pharo

### Chunk Format Alternative

Older Smalltalk projects use **Chunk format** (.st files with `!` delimiters). Tonel is newer and better for Git.

## Best Practices

### 1. One Package per Repository

```
MyApp/
  src/
    MyApp/
      (all classes for MyApp)
```

Don't mix unrelated packages in one repo.

### 2. Commit Often

Make small, focused commits:
- "Add User class"
- "Implement login method"
- "Fix validation bug"

Not:
- "Lots of changes" (too vague)

### 3. Write Good Commit Messages

**Good:**
```
Add email validation to User class

- Validate format using regex
- Reject empty emails
- Add tests for edge cases
```

**Bad:**
```
"stuff"
"fixed it"
"asdf"
```

### 4. Use Branches for Features

```
main
  ├─ feature-user-auth
  ├─ feature-reports
  └─ bugfix-login-error
```

Keep `main` stable. Work on branches. Merge when ready.

### 5. Don't Commit the Image or Changes File

The `.gitignore` file should include:

```
*.image
*.changes
*.sources
```

Only commit the `src/` directory with code!

### 6. Pull Before Push

Before pushing:
1. Pull latest changes from remote
2. Resolve any conflicts
3. Test that everything works
4. Then push

### 7. Use Pull Requests for Teams

On GitHub/GitLab:
1. Push your feature branch
2. Create a pull request
3. Team reviews your code
4. Address feedback
5. Merge when approved

This enables code review and quality control.

## Handling Merge Conflicts

Sometimes two people change the same code:

### Alice's Changes (on main):
```smalltalk
greet
    ^ 'Hello, ' , name
```

### Bob's Changes (on feature-greeting):
```smalltalk
greet
    ^ 'Hi, ' , name
```

### Merging Creates a Conflict!

Iceberg detects the conflict and shows both versions:

```smalltalk
greet
<<<<<<< HEAD
    ^ 'Hello, ' , name
=======
    ^ 'Hi, ' , name
>>>>>>> feature-greeting
```

### Resolving:

1. Iceberg shows the conflict
2. Choose Alice's version, Bob's version, or write a new one
3. Mark the conflict as resolved
4. Commit the merge

Example resolution:

```smalltalk
greet
    ^ 'Hello, ' , name , '!'  "Combined: Alice's 'Hello' + polite punctuation"
```

## Working with Teams

### Workflow:

1. **Clone the repository**
   ```
   Iceberg → Clone → Enter repo URL
   ```

2. **Create a feature branch**
   ```
   Iceberg → New branch → feature-add-user-profile
   ```

3. **Make changes, commit**
   ```
   Write code → Commit → "Add user profile page"
   ```

4. **Push your branch**
   ```
   Iceberg → Push
   ```

5. **Create pull request on GitHub**
   ```
   GitHub → Create PR from feature-add-user-profile to main
   ```

6. **Team reviews**
   ```
   Team comments on your code
   ```

7. **Address feedback**
   ```
   Make changes → Commit → Push
   ```

8. **Merge when approved**
   ```
   GitHub → Merge PR
   ```

9. **Pull latest main**
   ```
   Iceberg → Switch to main → Pull
   ```

10. **Repeat!**

## Advanced: Multiple Packages

For larger projects, organize code into multiple packages:

```
MyApp/
  src/
    MyApp-Core/
      (domain models, core logic)
    MyApp-UI/
      (user interface)
    MyApp-Tests/
      (unit tests)
```

Each package is a directory in `src/`.

All managed by one repository in Iceberg.

## SSH Keys for Authentication

Instead of entering passwords, use SSH keys:

### Generate SSH Key:
```bash
ssh-keygen -t rsa -b 4096 -C "your_email@example.com"
```

### Add to GitHub:
1. Copy `~/.ssh/id_rsa.pub` contents
2. GitHub → Settings → SSH Keys → Add New
3. Paste the key

### Use SSH URL in Iceberg:
```
git@github.com:yourusername/MyApp.git
```

Now you can push/pull without passwords!

## Common Issues

### "Detached HEAD"

If Iceberg says "detached HEAD":
1. Create a new branch from this point
2. Or switch to an existing branch

### "Merge Conflicts"

If conflicts occur:
1. Iceberg shows the conflicts
2. Resolve each one
3. Commit the resolution

### "Repository Dirty"

If Iceberg shows changes you didn't make:
- Iceberg detected code changes in the Image not yet committed
- Review changes, commit them, or discard

### "Cannot Push"

If push fails:
1. Pull latest changes first
2. Resolve conflicts
3. Then push

## Migrating Old Code

If you have old Smalltalk code not in Git:

### Step 1: File Out

Export code:
```smalltalk
"Right-click package → Export → Fileout"
```

### Step 2: Create Git Repo

In Iceberg, create a new repository.

### Step 3: File In

Import code:
```smalltalk
"Right-click repository → Import → File in"
```

### Step 4: Commit

Commit the imported code.

Now it's in Git!

## Exploring GitHub Projects

Many Smalltalk projects are on GitHub:

- **Seaside** (web framework)
- **Magritte** (meta-model framework)
- **Voyage** (object persistence)
- **Roassal** (visualization)
- **Pillar** (documentation tool)

Clone them in Iceberg to explore and learn!

## CI/CD with Smalltalk

You can automate testing with GitHub Actions:

### .github/workflows/test.yml:
```yaml
name: Tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Run tests
        run: |
          wget -O- get.pharo.org/64/stable | bash
          ./pharo Pharo.image test --junit-xml-output "MyApp.*"
```

This runs your tests automatically on every commit!

## Try This!

Practice with Iceberg and Git:

1. **Create a project:**
   ```smalltalk
   "Create package: TodoApp"
   "Create class: TodoItem"
   "Create class: TodoList"
   "Add methods"
   ```

2. **Initialize Git:**
   ```
   Iceberg → New repository → TodoApp
   ```

3. **Make initial commit:**
   ```
   Iceberg → Commit → "Initial commit"
   ```

4. **Create a GitHub repository:**
   ```
   GitHub → New repo → TodoApp
   ```

5. **Push to GitHub:**
   ```
   Iceberg → Add remote → origin → github URL
   Iceberg → Push
   ```

6. **Create a feature branch:**
   ```
   Iceberg → New branch → feature-due-dates
   ```

7. **Add due date functionality:**
   ```smalltalk
   "Add dueDate instance variable to TodoItem"
   "Add methods: dueDate, dueDate:, isOverdue"
   ```

8. **Commit and push:**
   ```
   Iceberg → Commit → "Add due date support"
   Iceberg → Push
   ```

9. **Merge to main:**
   ```
   Iceberg → Switch to main
   Iceberg → Merge → feature-due-dates
   Iceberg → Push
   ```

10. **Clone a public project:**
    ```
    Iceberg → Clone → https://github.com/pharo-project/pharo-counter
    ```

    Explore someone else's code!

## Iceberg Tips

### View Diffs

Before committing, view exactly what changed:
1. Click on uncommitted changes
2. See side-by-side diff of old vs new code
3. Review carefully before committing

### Repair Repository

If Iceberg loses track:
1. Right-click repository → Repair
2. Follow the repair wizard
3. Usually fixes issues automatically

### Multiple Remotes

You can have multiple remotes:
- `origin` → Your fork on GitHub
- `upstream` → Original project

Useful for contributing to open-source projects.

### Fetch vs Pull

- **Fetch** - Download changes but don't merge
- **Pull** - Download and merge

Fetch is safer; you can review before merging.

## The Bigger Picture

Git integration transforms Smalltalk from a personal playground into a professional platform:

### Before Git:
- Share images via email (yuck!)
- No collaboration
- No history tracking
- Hard to contribute to projects

### With Git:
- Share code via GitHub
- Team collaboration with branches and PRs
- Complete history
- Easy contribution to open source

## Looking Ahead

You now understand how to use Git with Smalltalk via Iceberg! You can:
- Create repositories
- Commit and push code
- Clone projects
- Use branches and merge
- Collaborate with teams

This completes Part V (The Image, Changes, and Sources)! You now understand Smalltalk's unique three-file system and how to integrate with modern version control.

In Part VI (Tools of the Trade), we'll explore Smalltalk's development tools in depth:
- Chapter 19: System Browser (navigating code)
- Chapter 20: Inspector and Explorer (examining objects)
- Chapter 21: Debugger (fixing bugs interactively)
- Chapter 22: Finder (discovering code)

These tools make Smalltalk development incredibly productive and enjoyable. You're going to love them!

---

**Key Takeaways:**
- **Iceberg** is Pharo's integrated Git client
- Exports Smalltalk code to text files (Tonel format)
- **Tonel format** is human-readable, one class per file
- Create repositories with `New repository` in Iceberg
- Commit changes with clear, descriptive messages
- Push to GitHub/GitLab for sharing and backup
- Clone repositories to work on others' projects
- Use branches for features, merge when ready
- Don't commit `.image` or `.changes` files
- Pull before pushing to avoid conflicts
- Resolve merge conflicts in Iceberg
- Use SSH keys for authentication
- Pull requests enable code review
- Git enables team collaboration and professional development
- The Changes file = personal history; Git = team history

---

[Previous: Chapter 17 - Changes and Sources Files](chapter-17-changes-and-sources-files.md) | [Next: Chapter 19 - The System Browser](chapter-19-system-browser.md)
