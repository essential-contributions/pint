## Appendix B: Compiler Intrinsics

The Pint compiler supports a list of intrinsics that perform various low level operations that are
mostly useful for building libraries. Intrinsics are generally target-specific. They give library
authors access to VM-specific instructions while preserving type safety. Below is a list of all
available compiler intrinsics for the Essential VM:

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

---

```pint
__sha256(data: <any>) -> b256
```

**Description:** Returns a SHA 256 hash from the specified data.

---

```pint
__state_len(data: <state>) -> int
```

**Description:** Returns the length of a state variable. the argument `data` must be a state
variable or a "next state" expression but can have any type.

---

```pint
__vec_len(<storage access>) -> int
```

**Description:** Returns the length of a storage vector.

---

```pint
__verify_ed25519(data: <any>, sig: { b256, b256 }, pub_key: b256) -> bool
```

**Description:** Validate an Ed25519 signature against a public key.

---

```pint
__recover_secp256k1(data_hash: b256, sig: { b256, b256, int }) -> { b256, int }
```

**Description:** Recover the public key from a secp256k1 signature.
