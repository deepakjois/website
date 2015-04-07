---
published: 2015-04-06
lead: Notes from <a href="https://www.coursera.org/course/cryptography">Cryptography</a>, a <abbr title="Massive Open Online Course">MOOC</abbr> on Coursera
---

## Week 7: Number Theory
* Number theory is needed for public-key cryptography
* Basic treatment here, for more details refer to Cat Lyndel textbook.
* *Algorithmic* Number Theory
    - ‘easy’: can be solved in polynomial time
    - ‘hard’: can be solved in exponential time

#### Algorithmic Number Theory
* Efficient computations
    - Integer addition/subtraction/multipllication/division with remainder
    - Modular addition/subtraction/multiplication/reduction
    - Modular Exponentiation?
    
### Exponentiation

* Compute a^b^?
    - \\(a^b  = O(b \\cdot \\|a\\|)\\)
    - Just writing down the answer takes exponential time

* Compute [a^b^ mod N]
    - Size of the answer < \\(\\|N\\|\\)
    - Do not compute a^b^ and then reduce modulo N…

#### Modular Exponentiation Take 1:
~~~~
exp(a,b,N) {
  // assume b > 0
  ans = 1;
  for (i=1, i ≤ b; i++)
    ans = [ans * a mod N]
  return ans;
}
~~~~
Running time: *O(b)* (So polynomial time in the *magnitude* of *b*, not the *length* of *b*.

#### Modular Exponentiation Take 2:
* Assume b=2^k^ for simplicity
    - 2^k^ multiplications vs. k squarings. k=O(len(b))

~~~~
exp(a, b, N) {
  // assume b ≥ 0
  x=a, t=1;
  while (b>0) {
    if (b odd)
      t = [t * x mod N], b = b -1;
    x = [x^2 mod N], b = b/2; }
  return t; }   
}
~~~~

Overall running time is polynomial in length of 