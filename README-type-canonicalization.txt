# Type canonicalization and multi-threading opportunity

This document introduces the concepts and the associated vocabulary
related to type canonicalization used in the Libabigail code base.

## Principle: what is a canonical type?

### Forewords about the Libabigail type system

One major activity of the Libabigail code base is to compare types to
see if they are equal.  In essence, this comparison is a member-wise
comparison where every single member of one type is compared to the
counterpart member of the other type.  This type of comparison is
called "structural comparison".  It determines if two types are
structurally equal or different.

By default, the Libabigail comparison engine considers that two types
with different names are different.  It follows that typedef or
aliased types with different names are considered different even if
their underlying types are the same.  After comparison, there are
change categorization passes that analyze the resulting changes and
determine if a change is harmful as far as ABI compatibility is
concerned.  Thus, if two types are aliases of one another, the pass
considers the change as harmless.  Subsequent change reporting passes
can suppress the harmless changes.  This implies that we can safely
assume that two types with different names are indeed different.

Please also note that we always consider pointers to types.  T is
always a pointer to an object representing a type.  A structural
comparison of T and T' is a structural comparison of the type objects
pointed-to by the pointers T and T'.

### Canonical types

Let's thus consider all the types in the Libabigail type system and
let's consider that they are sorted by their textual pretty
representation.

The first encountered type T becomes its own canonical type, noted
C(T).  In other words, for the first encountered type T, we can write
that C(T) == T.  Said otherwise, if all types are sorted by their
pretty representation, the first type T of a given pretty
representation is its own canonical type.

Then, if the subsequent homonym type T' (i.e, with the same pretty
representation) structurally equals C(T), then, C(T') structurally
equals C(T).  That also means that C(T') structurally equals T,
because C(T) structurally equals T.

Furthermore, if T'' -- a type homonym to T -- is structurally
different from C(T) and is structurally different from all other
canonical types, then C(T'') equals T''; said otherwise, T'' is its
own canonical type.

For the record, type_base::get_canonical_type_for is where the crux of
type canonicalization happens in the AIR (Abigail Intermediate
Representation) code.

## Why having the concept of canonical type at all?

Answer: To compare types fast.

Let's understand why.

### Canonical type concept: first insight

Once the canonical type of each single type of the system has been
determined, then comparing two random types T and T' amounts to
comparing their canonical types C(T) and C(T').  In other words,
structural identity is equivalent to canonical identity.

### Canonical type concept: second deeper insight

Comparing C(T) and C(T') amounts to comparing their pointer value.
Said otherwise, unlike with structural identity, canonical identity is
equivalent to pointer identity.

Let's think about the consequences of this.

Essentially, comparing two random types T and T' means comparing them
member-wise; this is also called structural comparison.  At best, the
complexity of the structural comparison can be O(N), with N being the
average size of the types.

However, if comparing T and T' amounts to comparing the values of C(T)
against C(T'), then this brings the complexity of the comparison to
O(1).  The speed benefit of type canonical type comparison is huge.

The cost of this canonical type comparison however is the (initial)
cost of the structural comparisons needed to determine the canonical
types of all the types of the system.

## Type space partitioning and classes of equivalence

The set of types sharing the same pretty representation is called an
homonym types group.

An homonym group can be further segmented in several classes of
equivalence.  A class of equivalence would be the set of types that
share the same canonical type, inside a given homonym group.

The set of all homonym groups constitutes a partition.

The canonicalization of types of a given homonym group can take place
independently from the canonicalization of the types of the other
homonym groups.  It follows that type canonicalization of a given
homonym group can be done in its own separate thread.

## Homonym type groups and parallel computing of canonical types

As each homonym type group is disjoint from all other ones, the
canonical type of a given homonym group can be determined
independently from the types of all other homonym group.  Canonical
types of each homonym type groups can thus be determined in parallel.

## Resulting high level algorithm to compute canonical types of the system

Please note that this is a high level algorithm.  It thus does
abstraction of the possible needs for synchronization requirements.
The aim is to identify the strong parallel nature of the problem at
hand and to leverage that to minimize the synchronization.  There
could be some minimal synchronization needs that are intentionally
left out of the scope of this section.

The algorithm for parallel canonical type computation leverages the
independence of homonym type groups:

1. **Partitioning Phase**

Group all types by their textual pretty representation, creating
disjoint homonym type groups.

2. **Parallel Canonicalization**

For each homonym group (can be done in parallel across groups):
   - Take the first type T in the group; set C(T) = T (it becomes its
     own canonical type)
   - Maintain a list of discovered canonical types for this group,
     initially containing just T
   - For each subsequent type T' in the group:
     - Perform structural comparison of T' against each canonical type
       in the list
     - If T' structurally equals some canonical type C, set C(T') = C
     - Otherwise, T' is structurally distinct from all existing
       canonical types; set C(T') = T' and add T' to the canonical
       types list

3. **Completion**

Once all groups are processed, every type in the system has a
canonical type, and type comparison reduces to O(1) pointer
comparison.

The key insight enabling parallelism is that structural comparison
within one homonym group never requires examining types from another
group, as types with different pretty representations cannot be
structurally equal.

