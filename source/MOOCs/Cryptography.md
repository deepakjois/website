---
title: Cryptography
published: 2015-04-06
lead: Notes from <a href="https://www.coursera.org/course/cryptography">Cryptography</a>, a <abbr title="Massive Open Online Course">MOOC</abbr> on Coursera
---

## Week 4: Message Integrity

* Secrecy and Integrity
    - Orthogonal Concerns
    * Encryption does not (in general) provide any integrity (_malleability_)
* Threat model = “Adaptive chosen-message attack”
* Security Goal = “Existential Unforgeability”
* Replay attacks are still possible
* CBC-MAC
    * deterministic (no IV)
    * Online final value is output
* Hash functions: Maps arbitrary length inputs to a short, fixed length “digest”
    - Keyed functions
    - Unkeyed functions
* Types of hash functions
    - MD5: 128-bit, insecure
    - SHA-1: 160-bit, known “theoritical” weaknesses
    * SHA-2: 256-bit (same family as SHA-1)
    * SHA-3/Keccak: 224, 256, 384 and 512-bit variants
* H-MAC (Hash and MAC)

## Week 5: Number Theory
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
    - $a^b  = O(b \cdot \|a\|)$
    - Just writing down the answer takes exponential time

* Compute \[a^b^ mod N\]
    - Size of the answer < $\|N\|$
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

Overall running time is polynomial in length of $\|a\|$,$\|b\|$,$\|N\|$

### Groups

* Provides a way about reasoning about objects that share the same mathematical structure

* An abelian group is a set G and a binary  operation $\circ$ defined on G such that:
    - There is an identity e $\in$ G such that e$\circ$g=g for g$\in$G
    - Every g$\in$G has an inverse h$\in$G such that h$\circ$g=e
    - (**Associativity**) For all f,g,h $\in$ G
    - (**Commutativity**) For all g,h $\in$ G, g $\circ$ h = h $\circ$ g
* The order of a finite group G is the number of elements in G

Example:

* $\mathbb{Z} = \{0, …, N-1\}$ under addition modulo N
    - Identity is 0
    - Inverse of a is \[-a mod N]
    - Associativity, commutativity is obvious
    * Order is N

#### Modular Inverses

* Say *b* is invertible modulo N if there is an element, b^-1^, such that b $\circ$ b^-1^ = 1 mod N
    - If such b^-1^ exists, it is unique modulo N
* Define “division by b” = “multiplication by b^-1^”
    - only defined when b is invertible modulo N
* Which elements are invertible?
    - Theorem: b is invertible modulo N iff gcd(b,N) = 1 (Invertibility can be determined efficiently)
* If p is prime, then 1,2,3,…,p-1 are all invertible modulo p
* If N=pq for p,q distinct primes, then the invertible elements are the integers from 1 to N-1 that are *not* multiples of p or q

Example:

* $\mathbb{Z}$^\*^~N~ = invertible elements between 1 and N-1 under multiplication modulo N
    - Closure not obvious, but can be shown
    * Identity is 1
    * Inverse of a is \[a^-1^ mod N]
    * Associativity, commutativity obvious
    * $\phi(n)$ = the number of invertible elements modulo N
        * = |{a $\in$ {1,…,N-1} : gcd(a,N) = 1}|
        * = the order of $\mathbb{Z}$^\*^~N~
    * If N is prime, then $\phi(N)$ = N-1
    * If N = pq, p and q are distinct primes, $\phi(N)$ = ?
        * = |{1, …, N-1}| - |{multiples of p}| - |{multiples of q}|
        * N-1-(q-1)-(p-1) = (p-1)(q-1)

#### Fermat’s little theorem

* Let G be a finite group of order m. Then for any g$\in$G, it holds that g^m^=1
    * In $\mathbb{Z}$~N~: For all a $\in$ $\mathbb{Z}$~N~, we have N $\circ$ a = 0 mod N
    * In  $\mathbb{Z}$^\*^~N~: For all a $\in$ $\mathbb{Z}$^\*^~N~, we have a^$\phi(N)$^ = 1 mod N


* Corollary: Let G be a finite group of order m. Then for g$\in$G and integer x, it holds that g^x^ = g^[x mod m]^
    - Proof: Let x = qm+r. Then g^x^=g^qm+r^=(g^m^)^q^g^r^=g^r^
* Another Corollary: Let G be a finite group of order m. For any integer e, define f~e~(g)=g^e^. If gcd(e,m)=1, then f~e~ is a permutation. Moreover, if d = e^-1^ mod m then f~d~ is the inverse of f~e~
    - Proof: The first part follows from the second. And f~d~(f~e~(g)) = (g^e^)^d^ = g^ed^=g^[ed mod m]^ = g^1^ = g

#### Factoring
* Hard problem
* Given x $\circ$ y, hard to find x and y
* It’s not hard to factor _all_ numbers
* Harder to factor 2-equal length primes

#### RSA Problem
* Informally: given N and e, hard to compute the e-th root of a uniform element y $\in$ $\mathbb{Z}$^\*^~N~
* GenRSA
    - Generate uniform n-bit primes p,q
    * Set N := pq
    * Choose arbitrary e with gcd(e,$\phi(N)$) = 1
    * Compute d := \[e^-1^ mod $\phi(N)$\]
    * Output (N,e,d)

#### Cyclic Groups
* Diffie Helman problems
* Elliptic Curves

## Week 6: Public Key Cryptography

* Problems with Private Key Crypto
    - Key Distribution Problem
    - Key Management Problem
    - Lack of support for “Open Systems”
        - Two users who have no prior relationship, but want to communicate securely
* New approaches were needed
    * [New Directions in Cryptography](http://www-ee.stanford.edu/~hellman/publications/24.pdf) by Whitfied Diffie and Martin E. Hellman, published in 1975
    * Some problems exhibit _assymetry_ – easy to convert, hard to invert
    * Use assymetry to agree on a shared secret key using public channel
* _Authenticated Key Exchange_
    - Parties know each others identities
* Public Key Distribution
    - One implicit assumption might be that parties are able to obtain _correct_ copies of each other’s public keys
    * Will revisit this assumption next week
* Why study private key crypto (if public key crypto can do the job for us)
    * Private key crypto is more suitable for certain applications, e.g. disk encryption
    * Public key crypto is SLOW! (2–3 orders of magnitude)

## Week 7: Digital Signatures and PKI

### Digital Signatures

* Digital Signatures
    - Provide integrity in the public key setting
    * e.g. used to distribute software patches
* Comparision to MACs
    - Public verifiability: “Anyone” can verify (only a holder of the key can verify a MAC tag)
    * Transferability: Can forward a signture to someone else
    * Non-repudiation
        * Signer cannot (easily) deny using a signature
        - Crucial for legal applications
        - Judge can verify signature using public copy of it
        * MAC cannot provide this functionality
* Hash-and-sign paradigm
    - Analogous to hybrid encryption: The _functionality_ of digital signatures and the _asymptomatic cost_ of a symmetric key operation
    - Used extensively in practise
* Identification schemes
    * Extremely important as a building block for digital signatures
    * Not suitable for remote authentication
    * e.g. Schnorr identification scheme
    * DSS (Digital Signature Standard)
        - NIST standard for digital signatures
            - DSA, based on discrete logarithm problem in subgroups of $\mathbb{Z}$^\*^~p~
            * ECDSA, based on elliptic curve groups

### Public Key Infrastructure
* Use signatures for secure key distribution
* Assume a trusted party with a public key known to everyone
    - CA = Certificate authority
    - Public Key *pk~CA~*
* Alice asks the CA to sign the _binding_ (Alice, pk)
    - Cert ~CA→Alice~ = Sign(Alice,pk) with *pk~CA~* (CA Must certify Alice’s identity out of band)
* Bob obtains Alice, pk and Cert ~CA→Alice~ and verifies it. Bob is then assured that pk is Alice’s public key
    * As long as the CA is trustworthy
* Chicken-and-egg problem - how does Bob get public key of CA
    - “Roots of trust” model
        - Only need to obtain a small number of CA public keys, so need to ensure secure distribution for only those
        * Distribute as part of web browser, OS etc.
    * “Web of trust” model
        * Obtain public keys from my friends in person
            - “key-signing parties”
        * Obtain “certificates” on my public key from my friends
        * Public key repositiries
            * e.g. MIT PGP key server
* PKI in practise
    - Proliferation of root CAs (hence difficult to manage) (see news item [Google Chrome will banish Chinese certificate authority for breach of trust](http://arstechnica.com/security/2015/04/google-chrome-will-banish-chinese-certificate-authority-for-breach-of-trust/))
    * Revocation
    * Other issues…

### SSL/TLS (Putting it all together)

* Standards
    - Secure Sockets Layer (Netscape, mid 90s)
    - Transport Layer Security
        - TLS 1.0 (1999)
        - TLS 1.2 (2008, current)
        - TLS 1.3 (draft)
* Protocol (two phases)
    - Handshake protocol: Establish a shared key between two entities
        * <img src="/images/ssl_handshake.jpg" class="img-responsive" alt="SSL Handshake"/>
    * Record-layer protocol: Use the shared key for secure communication
        * Parties now share k~c~, k~c~\', k~s~, k~s~\'
        * Client uses k~c~, k~c~\' (for encryption and authentication respectively)
        * Server uses  k~s~, k~s~\'
        * Separate keys for client and server prevent reflection attacks
        * Sequence numbers used to prevent reflection attacks

