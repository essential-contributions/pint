## Appendix C: Application Binary Interface (ABI)

The Application Binary Interface (ABI) is a condensed representation of a smart contract that
exposes enough information about the contract to allow external contexts to interact with it. The
ABI does not contain any contract _logic_ but only its public data such as its storage variables and
its predicates. The ABI is serialized in JSON format, making it both human readable and easily
parsable by relevant tools.

> **Note** This particular ABI specification is mostly relevant for the EssentialVM. Other virtual
> machines may have different architectures, requiring a completely different ABI format.

In this chapter, we will cover the following:

1. The ABI specification.
2. How to construct solutions using the ABI.
