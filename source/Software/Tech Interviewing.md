---
title: Notes on Tech Interviewing
published: 2011-06-27
---

Unstructured notes on preparing for technical programming interviews and improving one’s programming skills.

* [Practising Programming](#practising-programming)
* [Programming Interviews Preparation](#programming-interviews-preparation)
* [Java Concurrency](#java-concurrency)
* [C# in Depth](#c-in-depth)
* [Java Core Libraries Source Code](#java-core-libraries-source-code)
* [Continuations](#continuations)

## Practicing Programming
### Resources
* [Teach Yourself Programming in Ten Years](http://norvig.com/21-days.html)
* [Steve Yegge’s post ‘Practicing Programming’](https://sites.google.com/site/steveyegge2/practicing-programming)

### Practice Drill #1
Write your resume. List all your relevant skills, then note
the ones that will still be needed in 100 years. Give yourself a 1-10 rating
in each skill.

### Practice Drill #2
Make a list of programmers who you admire.
Try to include some you work with, since you'll be borrowing them for some
drills. Make one or two notes about things they seem to do well — things you
wish you were better at.

### Practice Drill #3
Go to Wikipedia's entry for [computer science], scroll down to the "Prominent
pioneers in computer science" section, pick a person from the list, and read
about them. Follow any links from there that you think look interesting.

If you run out of people to read about, there's also a list of [computer
scientists]. Spending a few hours a week in Wikipedia is a fun, easy way to get
a feel for our field and related fields. Make sure you pay attention to the
the history and the people. Makes it more fun.

[computer science]: http://en.wikipedia.org/wiki/Computer_science
[computer scientists]: http://en.wikipedia.org/wiki/List_of_computer_scientists

### Practice Drill #4
Read through someone else's code for 20 minutes. For this drill, alternate
between reading great code and reading bad code; they're both instructive. If
you're not sure of the difference, ask a programmer you respect to show you
examples of each. Show the code you read to someone else, and see what they
think of it.

### Practice Drill #5
Make a list of your 10 favorite programming tools: the ones you feel you use
the most, the ones you almost couldn't live without. Spend an hour reading the
docs for one of the tools in your list, chosen at random. In that hour, try
learn some new feature of the tool that you weren't aware of, or figure out
some new way to use the tool.

### Practice Drill #6
Pick something you're good at that has nothing to do with programming. Think
about how the professionals or great masters of that discipline do their
practice. What can you learn from them that you can apply to programming?

### Practice Drill #11
Find a buddy for trading practice questions. Ask each other programming
questions, alternating weeks. Spend 10 or 15 minutes working on the problem,
and 10 or 15 minutes discussing it (finished or not.)

## Programming Interviews Preparation
### General Tech Interview Guidance
* [Hacking a Google Interview](http://courses.csail.mit.edu/iap/interview/)
* [Get that job at Google](http://steve-yegge.blogspot.com/2008/03/get-that-job-at-google.html)
* [Five Essential Phone Screen Question](http://steve.yegge.googlepages.com/five-essential-phone-screen-questions)
* [i has 1337 code](http://www.ihas1337code.com/)

### Algorithms
* [Cormen Book](http://mitpress.mit.edu/algorithms/)
* [Algorithm Design Manual](http://www.amazon.com/dp/0387948600)

### Programming
* [Bit Twiddling Hacks](http://www.cs.utk.edu/~vose/c-stuff/bithacks.html)
* [JDK Source Code](http://hg.openjdk.java.net/)

### Systems and Networking
* [18-845: Internet Services,Carnegie Mellon University, Spring 2007](http://www.ece.cmu.edu/~ece845/)
* [Java Concurrency in Practise by Brian Goetz](http://www.javaconcurrencyinpractice.com/)
* [Computer Systems: A Programmer's Perspective, Second Edition (CS:APP2e)](http://csapp.cs.cmu.edu/)

## Java Concurrency
### Resources
* [Java Concurrency in Practise by Brian Goetz](http://www.javaconcurrencyinpractice.com/)

### Notes : Chapter 5
* _Concurrency_ is different from _Synchronization_
* Java Standard Library provides concurrent collections like
  `ConcurrentHashMap`, `ConcurrentHashTable` and `CopyOnWriteArrayList`.
* `ConcurrentHashMap` supports lock striping (no idea what that means) to provide more
  fine grained access than synchronization, with support for multiple readers and limited writers.
* A thread can be in any of the `BLOCKED`, `WAITING`, `TIMED_WAITING` or `RUNNABLE` states. Only in the `RUNNABLE` state is
  available for scheduling.
* Synchronizers
  - Latches, e.g. `CountDownLatch`
  - `FutureTask`
  - `Semaphore`
  - Barriers, e.g. `CyclicBarrier`. Latches are for waiting for _events_; barriers are for waiting for _other threads_.
* An interesting memoizer example, using `FutureTask` in a `ConcurrentHashMap` to store computations.

~~~~~~~~~~
public class Memoizer<A, V> implements Computable<A, V> {
    private final ConcurrentMap<A, Future<V>> cache
        = new ConcurrentHashMap<A, Future<V>>();
    private final Computable<A, V> c;

    public Memoizer(Computable<A, V> c) { this.c = c; }

    public V compute(final A arg) throws InterruptedException {
        while (true) {
            Future<V> f = cache.get(arg);
            if (f == null) {
                Callable<V> eval = new Callable<V>() {
                    public V call() throws InterruptedException {
                        return c.compute(arg);
                    }
                };
                FutureTask<V> ft = new FutureTask<V>(eval);
                f = cache.putIfAbsent(arg, ft);
                if (f == null) { f = ft; ft.run(); }
            }
            try {
                return f.get();
            } catch (CancellationException e) {
                cache.remove(arg, f);
            } catch (ExecutionException e) {
                throw launderThrowable(e.getCause());
            }
        }
    }
}
~~~~~~~~~~

### Notes : Chapter 6
* `Executor` separates execution policy from the task submission
* `Executor.newFixedThreadPool` factory method used a thread pool of fixed size
  to run tasks
* `ExecutorService` extends Executor to provide lifecycle management
* `ScheduledThreadPoolExecutor` can manage execution of deferred or periodic tasks

> The Executor framework uses `Runnable` as its basic task representation.
> `Runnable` is a fairly limiting abstraction; `run` cannot return a value or throw
> checked exceptions, although it can have side effects such as writing to a log
> file or placing a result in a shared data structure.

> Many tasks are effectively deferred computations executing a database query,
> fetching a resource over the network, or computing a complicated function. For
> these types of tasks, Callable is a better abstraction: it expects that the
> main entry point, call, will return a value and anticipates that it might
> throw an exception.Executors includes several utility methods for wrapping
> other types of tasks, including `Runnable` and `java.security.PrivilegedAction`,
> with a `Callable`.

## C# in Depth
### Resources
* [C# in depth by John Skeet](http://www.manning.com/skeet/)
* [Covariance and contravariance](http://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science)#Java)
* [Eric Lippert's Blog Posts on Variance](http://blogs.msdn.com/ericlippert/archive/tags/Covariance+and+Contravariance/default.aspx)

### Generics
Type constraints on C# generics :

* Reference type constraints : `struct RefSample<T> where T : class`
* Value type constraints : `class ValSample<T> where T : struct`
* Constructor type constraints : `public T CreateInstance<T>() where T : new() { return new T(); }`
* Derivation type constraints : `class Sample<T> where T:IComparable<T>`
* _Combination of constraints_ : `class Sample<T> where T : class, Stream, new()`

Generics covariance and contravariance

Covariance :

~~~~~
List<Animal>animals=newList<Cat>();
animals.Add(new Animal());
~~~~~

Contravariance :

~~~~~
IComparer<IShape> areaComparer = new AreaComparer();
List<Circle> circles = new List<Circle>();
circles.Add(new Circle(20));
circles.Add(new Circle(10));
circles.Sort(areaComparer);
~~~~~

Now suppose you have two methods, one which takes a Giraffe and one which takes an Animal:

~~~
void Foo(Giraffe g) {}
void Bar(Animal a) {}
~~~

and a delegate to a void-returning function that takes a Mammal:

~~~
Action<Mammal> action1 = Foo; // illegal
Action<Mammal> action2 = Bar; // legal
~~~

Why is the first assignment illegal? Because the caller of action1 can pass a
Tiger, but Foo cannot take a Tiger, only a Giraffe! The second assignment is
legal because Bar can take any Animal.

In our previous example we preserved the direction of the assignability:
Giraffe is smaller than Animal, so a method which returns a Giraffe is smaller
than a delegate which returns an Animal. In this example we are reversing the
direction of the assignability: Mammal is smaller than Animal, so a method
which takes an Animal is smaller than a delegate which takes a Mammal. Because
the direction is reversed, method group to delegate conversions are
_contravariant in their argument types_.

## Java Core Libraries Source Code
### Resources
* [http://hg.openjdk.java.net/](http://hg.openjdk.java.net/)

### Collections
Interesting things found in JDK 7 source code

* `shuffle` and `rotate` method
* `subList` method for `List` class
* `rotate2` method in `Collections` class uses an interesting algorithms to
  reverse the list, by reversing two sublists around a pivot and then reversing
  the entire list
* `reverseOrder` method in `Collections` returns a comparator that compares items
  in reverse
* `frequency` and `disjoint` methods in `Collections` class
* TreeMap (which is in turn used by TreeSet) uses a Red-Black binary tree
  implementation to provide log n insertion, deletion and lookup characteristics

## Continuations
### Resources
* [Wikipedia](http://en.wikipedia.org/wiki/Continuation)
* [Continuations made simple](http://www.ps.uni-saarland.de/~duchier/python/continuations.html)
* [Continuations for Curmudgeons](http://www.intertwingly.net/blog/2005/04/13/Continuations-for-Curmudgeons)
* [Chapter 13 : Jumps](http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-15.html#node_chap_13)

### Notes
It actually makes a lot of sense to understand the way function calls are
implemented in C, using stack frames etc before diving into continuations. It
makes the explanations a lot more clearer.
