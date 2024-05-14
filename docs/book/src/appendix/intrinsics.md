## Appendix B: Compiler Intrinsics

The Pint compiler supports a list of intrinsics that perform various low level operations that are
mostly useful for building libraries. Intrinsics are generally target-specific. They give library
authors access to VM-specific instructions while preserving type safety. Below is a list of all
available compiler intrinsics for the Essential VM:

---

```pint
__mut_keys_len() -> int
```

**Description:** Returns the number of mutable keys being proposed for mutation.

---

```pint
__mut_keys_contains(key: int[<n>]) -> bool
```

**Description:** Returns a `bool` indicating whether the mutable keys being proposed contain `key`.
`<n>` is any positive integer.

---

```pint
__this_address() -> b256
```

**Description:** Returns the content hash of this intent.

---

```pint
__this_set_address() -> b256
```

**Description:** Returns the content hash of the set of intents that this intent belongs to.

---

```pint
__this_pathway() -> int
```

**Description:** Returns the "pathway" of this intent. The pathway of an intent is the index of the
solution data currently being used to check the intent.

---

```pint
__sha256(data: <any>) -> b256
```

**Description:** Returns a SHA 256 hash from the specified data.

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
