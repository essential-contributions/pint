# Storage

Most useful Pint contracts require some sort of _persistent storage_ that represent _state_. After
all, a blockchain is a decentralized distributed database and contracts are a way to enforce rules
on how "entries" in this database are allowed to change. Therefore, having the ability to express
those database entries using variables and dynamic containers is quite useful. For that reason, Pint
offers a way to declare and access a variety of storage types.

In this chapter, we will cover the following:

- How to declare and access storage variables with statically-sized types.
- How to declare and access storage variables with dynamically-sized types.
- How to access storage variables that belong to an external contract.
