## Appendix B: Compiler Intrinsics

The Pint compiler supports a list of intrinsics that perform various low level operations that are
mostly useful for building libraries. Intrinsics are generally target-specific. They give library
authors access to VM-specific instructions while preserving type safety. Below is a list of all
available compiler intrinsics for the Essential VM:

---

```pint
__address_of(name: string) -> b256
```

**Description:** Returns the content hash of predicate named `name` in the same contract. The name
must be the full absolute path to the predicate, such as `::Foo`, and cannot be the name of the
predicate it's used in.

---

```pint
__predicate_at(pathway: int) -> { b256, b256 }
```

**Description:** Returns the full address of predicate at pathway `<pathway>`. The pathway of a
predicate is the index of the solution data currently being used to check the predicate. The full
address contains both the content hash of the contract to which the predicate belongs and the
content hash of the predicate itself

---

```pint
__recover_secp256k1(data_hash: b256, sig: { b256, b256, int }) -> { b256, int }
```

**Description:** Recover the public key from a secp256k1 signature.

---

```pint
__sha256(data: _) -> b256
```

**Description:** Returns a SHA 256 hash from the specified data.

---

```pint
__size_of(data: _) -> int
```

**Description:** Returns the size, in words, of an expression. This is often the same as the size of
the type of the expression (e.g. 1 word for `int`, 4 words for `b256`, 3 words for `{int, int,
bool}`, and so on). However, it can also be different, namely for expressions that can be `nil` such
as storage accesses and paths to state variables.

---

```pint
__this_address() -> b256
```

**Description:** Returns the content hash of this predicate.

---

```pint
__this_contract_address() -> b256
```

**Description:** Returns the content hash of the contract that this predicate belongs to.

---

```pint
__this_pathway() -> int
```

**Description:** Returns the "pathway" of this predicate. The pathway of a predicate is the index of
the solution data currently being used to check the predicate.

<!---
Uncomment when we officially support storage vectors

```pint
__vec_len(vec: _[]) -> int
```

**Description:** Returns the length of a storage vector.
-->

---

```pint
__verify_ed25519(data: _, sig: { b256, b256 }, pub_key: b256) -> bool
```

**Description:** Validate an Ed25519 signature against a public key.

---
