# Chapter 25: Testing Your Code

Professional developers test their code. Not by manually clicking through the application (though that has its place), but by writing **automated tests** - code that verifies other code works correctly.

Smalltalk pioneered automated testing with **SUnit**, the first xUnit testing framework. SUnit inspired JUnit (Java), NUnit (.NET), PyTest (Python), and countless others. It's simple, elegant, and powerful.

In this chapter, you'll learn to write tests that give you confidence your code works, catch regressions early, and enable fearless refactoring.

## Why Test?

### Manual Testing is Tedious

Without automated tests:
1. Make a change
2. Manually test feature A
3. Manually test feature B
4. Manually test feature C
5. Did you remember to test everything?
6. Make another change
7. Repeat all tests again

Exhausting and error-prone!

### Automated Testing is Fast

With automated tests:
1. Make a change
2. Run all tests (takes seconds)
3. Green? Great! Red? Fix it.
4. Make another change
5. Run all tests again (still takes seconds)

Fast, reliable, comprehensive!

### Tests Provide Confidence

Tests prove your code works. They catch bugs before users do. They let you refactor fearlessly.

### Tests Are Documentation

Tests show how code is meant to be used. They're examples that always stay up-to-date because they must pass!

## Introducing SUnit

**SUnit** is Smalltalk's testing framework. It's built into Pharo and other Smalltalks.

Key concepts:
- **TestCase** - A class containing tests
- **Test method** - A method that verifies behavior
- **Assertions** - Statements that verify expectations
- **Test runner** - Tool that runs tests and reports results

## Your First Test

Let's write a test for a simple class:

### Step 1: Create a Class to Test

```smalltalk
Object subclass: #Calculator
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'MyApp'
```

```smalltalk
add: a to: b
    ^ a + b

subtract: a from: b
    ^ a - b

multiply: a by: b
    ^ a * b
```

### Step 2: Create a Test Class

Test classes subclass `TestCase`:

```smalltalk
TestCase subclass: #CalculatorTest
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'MyApp-Tests'
```

**Naming convention**: Test classes end with `Test`.

**Package convention**: Tests go in a separate package, often `Package-Tests`.

### Step 3: Write Test Methods

Test methods start with `test`:

```smalltalk
testAddition
    | calc result |
    calc := Calculator new.
    result := calc add: 2 to: 3.
    self assert: result equals: 5
```

```smalltalk
testSubtraction
    | calc result |
    calc := Calculator new.
    result := calc subtract: 5 from: 10.
    self assert: result equals: 5
```

```smalltalk
testMultiplication
    | calc result |
    calc := Calculator new.
    result := calc multiply: 3 by: 4.
    self assert: result equals: 12
```

### Step 4: Run the Tests

In the System Browser:
1. Right-click on `CalculatorTest` class
2. Choose `Run tests` (or click the green circle icon)

Or evaluate:

```smalltalk
CalculatorTest run
```

**Result**: Green! All tests pass!

## Assertions

Assertions verify expectations. If an assertion fails, the test fails.

### Common Assertions:

#### assert:

```smalltalk
self assert: (5 > 3).
self assert: collection isEmpty
```

Verifies the expression is `true`.

#### assert:equals:

```smalltalk
self assert: result equals: 42.
self assert: name equals: 'Alice'
```

Verifies two values are equal (using `=`).

#### deny:

```smalltalk
self deny: (5 < 3).
self deny: collection isEmpty
```

Verifies the expression is `false`.

#### assert:identicalTo:

```smalltalk
self assert: obj identicalTo: sameObj
```

Verifies two variables refer to the same object (using `==`).

#### should:raise:

```smalltalk
self should: [ 1 / 0 ] raise: ZeroDivide
```

Verifies a block signals a specific exception.

#### shouldnt:raise:

```smalltalk
self shouldnt: [ 1 / 2 ] raise: ZeroDivide
```

Verifies a block doesn't signal an exception.

## Test Structure

A good test has three parts (Arrange-Act-Assert):

### 1. Arrange - Set up

```smalltalk
testAddition
    | calc |
    calc := Calculator new.  "Arrange"
```

Create objects and set up initial state.

### 2. Act - Execute

```smalltalk
    result := calc add: 2 to: 3.  "Act"
```

Call the method you're testing.

### 3. Assert - Verify

```smalltalk
    self assert: result equals: 5  "Assert"
```

Verify the result is correct.

## setUp and tearDown

If multiple tests need the same setup, use `setUp`:

```smalltalk
TestCase subclass: #CalculatorTest
    instanceVariableNames: 'calculator'
    ...
```

```smalltalk
setUp
    "Run before each test"
    super setUp.
    calculator := Calculator new
```

```smalltalk
testAddition
    | result |
    result := calculator add: 2 to: 3.
    self assert: result equals: 5
```

```smalltalk
testSubtraction
    | result |
    result := calculator subtract: 5 from: 10.
    self assert: result equals: 5
```

Now `calculator` is created fresh before each test!

### tearDown

Clean up after tests:

```smalltalk
tearDown
    "Run after each test"
    calculator := nil.
    super tearDown
```

Use `tearDown` to close files, disconnect from databases, etc.

## Testing Collections

```smalltalk
Object subclass: #Stack
    instanceVariableNames: 'items'
    ...

initialize
    super initialize.
    items := OrderedCollection new

push: anObject
    items addLast: anObject

pop
    ^ items removeLast

peek
    ^ items last

size
    ^ items size

isEmpty
    ^ items isEmpty
```

Tests:

```smalltalk
TestCase subclass: #StackTest
    instanceVariableNames: 'stack'
    ...

setUp
    stack := Stack new

testPushAndPop
    stack push: 1.
    stack push: 2.
    self assert: stack pop equals: 2.
    self assert: stack pop equals: 1

testPeek
    stack push: 42.
    self assert: stack peek equals: 42.
    self assert: stack size equals: 1  "Peek doesn't remove"

testEmptyStack
    self assert: stack isEmpty.
    stack push: 1.
    self deny: stack isEmpty

testPopEmptyStack
    self should: [ stack pop ] raise: Error
```

## Testing Edge Cases

Good tests cover edge cases:

```smalltalk
testEmptyCollection
    self assert: collection isEmpty.
    self assert: collection size equals: 0

testSingleElement
    collection add: 'item'.
    self assert: collection size equals: 1.
    self assert: (collection includes: 'item')

testManyElements
    1 to: 1000 do: [ :i | collection add: i ].
    self assert: collection size equals: 1000

testNilElements
    collection add: nil.
    self assert: (collection includes: nil)

testDuplicates
    collection add: 'x'; add: 'x'; add: 'x'.
    "Test how your collection handles duplicates"
```

## Testing Error Conditions

Verify your code handles errors properly:

```smalltalk
testDivisionByZero
    | calc |
    calc := Calculator new.
    self should: [ calc divide: 10 by: 0 ] raise: ZeroDivide

testNegativeAge
    | person |
    person := Person new.
    self should: [ person age: -5 ] raise: InvalidArgumentError

testFileNotFound
    self
        should: [ '/nonexistent/file.txt' asFileReference contents ]
        raise: FileDoesNotExistException
```

## Test Organization

### Protocols

Organize tests into protocols:
- `tests - arithmetic` - Math-related tests
- `tests - collections` - Collection-related tests
- `tests - error handling` - Exception tests
- `tests - edge cases` - Boundary tests

### One Concept per Test

**Bad:**
```smalltalk
testEverything
    self assert: calc add: 2 to: 3 equals: 5.
    self assert: calc subtract: 5 from: 10 equals: 5.
    self assert: calc multiply: 3 by: 4 equals: 12.
    "... 50 more assertions ..."
```

**Good:**
```smalltalk
testAddition
    self assert: (calc add: 2 to: 3) equals: 5

testSubtraction
    self assert: (calc subtract: 5 from: 10) equals: 5

testMultiplication
    self assert: (calc multiply: 3 by: 4) equals: 12
```

Each test focuses on one thing!

### Descriptive Names

Test names should describe what they test:

**Good names:**
- `testAdditionWithPositiveNumbers`
- `testDivisionByZero`
- `testEmptyCollectionSize`
- `testUserLoginWithValidCredentials`

**Bad names:**
- `test1`
- `testStuff`
- `testCalculator`

## Running Tests

### Run One Test

```smalltalk
CalculatorTest run: #testAddition
```

Or: Right-click on the test method → `Run test`

### Run All Tests in a Class

```smalltalk
CalculatorTest run
```

Or: Right-click on the test class → `Run tests`

### Run All Tests in a Package

```smalltalk
(TestSuite new
    addPackage: 'MyApp-Tests';
    yourself) run
```

Or: Right-click on the package → `Run tests`

### Test Runner Tool

Open the Test Runner:
- World menu → `Test Runner`
- Or: `Ctrl+O` `U`

The Test Runner shows:
- All test classes
- Run buttons
- Results (green/red/yellow)
- Failed assertions

Select tests and click `Run Selected` to run them!

## Test-Driven Development (TDD)

TDD is a workflow where you write tests **before** code:

### The TDD Cycle:

1. **Red** - Write a failing test
2. **Green** - Write minimal code to make it pass
3. **Refactor** - Clean up code
4. **Repeat**

### Example:

#### Step 1: Red - Write the Test

```smalltalk
testCapitalize
    | text result |
    text := 'hello world'.
    result := text capitalized.
    self assert: result equals: 'Hello World'
```

Run it. It fails! (Method doesn't exist yet.)

#### Step 2: Green - Implement

In the Debugger or System Browser, implement `capitalized`:

```smalltalk
capitalized
    "Capitalize each word"
    ^ (self substrings: ' ')
        collect: [ :word | word asUppercase ]
        thenJoin: ' '
```

Wait, that uppercases everything. Fix it:

```smalltalk
capitalized
    "Capitalize each word"
    ^ (self substrings: ' ')
        collect: [ :word |
            word isEmpty
                ifTrue: [ word ]
                ifFalse: [ word first asUppercase asString , word allButFirst ] ]
        thenJoin: ' '
```

Run the test. Green!

#### Step 3: Refactor

The code works, but could be cleaner. Refactor if needed.

#### Step 4: Repeat

Write the next test!

## Testing Private Methods

Should you test private methods? Opinions vary:

### Option 1: Don't Test Private Methods

Test only the public API. Private methods are tested indirectly through public methods.

**Advantage**: Tests are decoupled from implementation details.

### Option 2: Test Private Methods

Make private methods testable by calling them in tests.

**Advantage**: More thorough coverage.

**Disadvantage**: Tests coupled to implementation.

### Pragmatic Approach:

Test complex private methods directly. Test simple private methods indirectly through public methods.

## Mocking and Stubbing

Sometimes tests need fake objects (mocks) to avoid dependencies:

### Without Mocking:

```smalltalk
testSendEmail
    | emailer |
    emailer := EmailService new.
    emailer sendEmail: 'test@example.com' subject: 'Test' body: 'Hello'.
    "Actually sends an email! Slow and has side effects!"
```

### With Mocking:

```smalltalk
Object subclass: #MockEmailService
    instanceVariableNames: 'sentEmails'
    ...

initialize
    sentEmails := OrderedCollection new

sendEmail: to subject: subject body: body
    sentEmails add: (Dictionary new
        at: #to put: to;
        at: #subject put: subject;
        at: #body put: body;
        yourself)

sentEmails
    ^ sentEmails
```

```smalltalk
testSendEmail
    | emailer |
    emailer := MockEmailService new.
    emailer sendEmail: 'test@example.com' subject: 'Test' body: 'Hello'.
    self assert: emailer sentEmails size equals: 1.
    self assert: (emailer sentEmails first at: #to) equals: 'test@example.com'
```

No actual email sent! Fast and no side effects.

## Test Coverage

**Test coverage** measures what percentage of code is executed by tests.

Pharo has coverage tools. Run:

```smalltalk
(TestCoverage new
    test: CalculatorTest;
    run) report
```

This shows which methods are tested and which aren't.

Aim for high coverage (80-100%), but don't obsess. Coverage doesn't guarantee correctness!

## Continuous Testing

Some Smalltalks support **continuous testing** - tests run automatically as you code:

- Edit a method
- Tests re-run automatically
- Instant feedback!

Check your Smalltalk's documentation for details.

## Testing Best Practices

### 1. Fast Tests

Tests should run quickly. Slow tests won't be run often.

### 2. Independent Tests

Each test should be independent. Don't rely on test execution order.

### 3. Repeatable Tests

Tests should pass consistently. No randomness or timing issues.

### 4. Clear Failure Messages

When a test fails, the message should clearly indicate what went wrong:

```smalltalk
self assert: result equals: expected
    description: 'Expected ', expected printString, ' but got ', result printString
```

### 5. Test One Thing

Each test verifies one behavior. If it fails, you know exactly what broke.

### 6. No Logic in Tests

Tests should be simple. No loops, conditionals, or complex logic. Just setup, execute, assert.

### 7. Keep Tests Clean

Test code is real code. Keep it clean, refactor it, maintain it.

## Common Testing Mistakes

### Testing Implementation, Not Behavior

**Bad:**
```smalltalk
testInternalState
    calculator calculate: 5.
    self assert: calculator internalBuffer equals: #(5 0 0)  "Testing internals"
```

**Good:**
```smalltalk
testCalculation
    result := calculator calculate: 5.
    self assert: result equals: 5  "Testing behavior"
```

Test what the code **does**, not how it does it.

### Brittle Tests

Tests that break when you refactor code (even though behavior doesn't change) are brittle.

### Too Many Assertions

**Bad:**
```smalltalk
testEverything
    self assert: this.
    self assert: that.
    self assert: other.
    "... 20 more assertions ..."
```

If this fails, which assertion failed? Hard to tell!

**Good:** One focused test per concept.

### No Edge Case Testing

Don't just test the happy path! Test:
- Empty collections
- nil values
- Negative numbers
- Boundary values

## Practical Example: Testing a TodoList

```smalltalk
Object subclass: #TodoList
    instanceVariableNames: 'items'
    ...

initialize
    super initialize.
    items := OrderedCollection new

addItem: anItem
    items add: anItem

removeItem: anItem
    items remove: anItem

allItems
    ^ items copy

completedItems
    ^ items select: [ :item | item isCompleted ]

pendingItems
    ^ items reject: [ :item | item isCompleted ]
```

Tests:

```smalltalk
TestCase subclass: #TodoListTest
    instanceVariableNames: 'list item1 item2'
    ...

setUp
    list := TodoList new.
    item1 := TodoItem new description: 'Task 1'.
    item2 := TodoItem new description: 'Task 2'

testAddItem
    list addItem: item1.
    self assert: list allItems size equals: 1.
    self assert: (list allItems includes: item1)

testRemoveItem
    list addItem: item1.
    list removeItem: item1.
    self assert: list allItems isEmpty

testCompletedItems
    item1 markCompleted.
    list addItem: item1; addItem: item2.
    self assert: list completedItems size equals: 1.
    self assert: (list completedItems includes: item1)

testPendingItems
    item1 markCompleted.
    list addItem: item1; addItem: item2.
    self assert: list pendingItems size equals: 1.
    self assert: (list pendingItems includes: item2)

testEmptyList
    self assert: list allItems isEmpty.
    self assert: list completedItems isEmpty.
    self assert: list pendingItems isEmpty
```

## Try This!

Practice testing:

1. **Write tests for a Stack class:**
   ```smalltalk
   TestCase subclass: #StackTest
       ...

   testPushAndPop
   testPeek
   testEmptyStack
   testPopEmptyStack
   testSize
   ```

2. **Write tests for a BankAccount:**
   ```smalltalk
   TestCase subclass: #BankAccountTest
       ...

   testDeposit
   testWithdraw
   testOverdraft
   testBalance
   testNegativeDeposit
   ```

3. **Test error conditions:**
   ```smalltalk
   testInvalidAge
       | person |
       person := Person new.
       self should: [ person age: -5 ] raise: InvalidArgumentError

   testDivideByZero
       self should: [ calc divide: 10 by: 0 ] raise: ZeroDivide
   ```

4. **Practice TDD:**
   - Write a failing test for a `reverse` method on String
   - Implement it to make the test pass
   - Refactor
   - Write more tests for edge cases

5. **Test a collection:**
   Write comprehensive tests for a custom collection class:
   - Empty collection
   - Single element
   - Many elements
   - Duplicates
   - nil elements
   - Adding, removing, querying

6. **Run the tests:**
   ```smalltalk
   YourTestClass run
   ```

   Watch them turn green!

## The Value of Tests

Tests provide:
- **Confidence** - Know your code works
- **Documentation** - Show how code is used
- **Regression prevention** - Catch bugs early
- **Refactoring safety** - Change code fearlessly
- **Design feedback** - Hard-to-test code is often poorly designed

Tests are an investment that pays off!

## Looking Ahead

You now understand testing with SUnit! You can:
- Write test classes and test methods
- Use assertions to verify behavior
- Set up and tear down test fixtures
- Test edge cases and error conditions
- Run tests and interpret results
- Practice test-driven development

In Chapter 26, we'll explore **Packages and Code Organization** - how to structure larger projects with multiple packages, classes, and dependencies.

Then in Part VIII (Chapters 27-30), we'll explore different Smalltalk implementations: Pharo, Squeak, Glamorous Toolkit, and others. Each has unique strengths!

Part VII has equipped you with professional development skills: protocols, error handling, and testing. You're ready for serious Smalltalk development!

---

**Key Takeaways:**
- **SUnit** is Smalltalk's testing framework
- Test classes subclass `TestCase`
- Test methods start with `test`
- **Assertions** verify expectations: `assert:`, `assert:equals:`, `deny:`, `should:raise:`
- **setUp** runs before each test; **tearDown** runs after
- Tests should be fast, independent, and repeatable
- **TDD cycle**: Red (failing test) → Green (make it pass) → Refactor
- Test one thing per test method
- Test edge cases and error conditions
- Use descriptive test names
- **Mocking** replaces real dependencies with test doubles
- Run tests frequently to catch bugs early
- Tests are documentation that never goes out of date
- High test coverage gives confidence but doesn't guarantee correctness

---

[Previous: Chapter 24 - Error Handling](chapter-24-error-handling.md) | [Next: Chapter 26 - Packages and Code Organization](chapter-26-packages.md)
