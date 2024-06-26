+++
title = "XenAPI Basics"
weight = 10
+++

This document contains a description of the Xen Management API – an interface for
remotely configuring and controlling virtualised guests running on a
Xen-enabled host.

The XenAPI is presented here as a set of Remote Procedure Calls, with a wire
format based upon [XML-RPC](http://xmlrpc.scripting.com).
No specific language bindings are prescribed,
although examples will be given in the python programming language.

Although we adopt some terminology from object-oriented programming,
future client language bindings may or may not be object oriented.
The API reference uses the terminology _classes_ and _objects_.
For our purposes a _class_ is simply a hierarchical namespace;
an _object_ is an instance of a class with its fields set to
specific values. Objects are persistent and exist on the server-side.
Clients may obtain opaque references to these server-side objects and then
access their fields via get/set RPCs.

For each class we specify a list of fields along with their _types_ and _qualifiers_.  A
qualifier is one of:

- _RO/runtime_: the field is Read
Only. Furthermore, its value is automatically computed at runtime.
For example: current CPU load and disk IO throughput.
- _RO/constructor_: the field must be manually set
when a new object is created, but is then Read Only for
the duration of the object's life.
For example, the maximum memory addressable by a guest is set 
before the guest boots.
- _RW_: the field is Read/Write. For example, the name of a VM.

Types
-----

The following types are used to specify methods and fields in the API Reference:

- `string`: Text strings.
- `int`: 64-bit integers.
- `float`: IEEE double-precision floating-point numbers.
- `bool`: Boolean.
- `datetime`: Date and timestamp.
- `c ref`: Reference to an object of class `c`.
- `t set`: Arbitrary-length set of values of type `t`.
- `(k → v) map`: Mapping from values of type `k` to values of type `v`.
- `e enum`: Enumeration type with name `e`. Enums are defined in the API Reference together with classes that use them.

Note that there are a number of cases where `ref`s are _doubly
linked_ – e.g. a VM has a field called `VIFs` of type
`VIF ref set`; this field lists
the network interfaces attached to a particular VM. Similarly, the VIF
class has a field called `VM` of type `VM ref` which references the VM to which the interface is connected.
These two fields are _bound together_, in the sense that
creating a new VIF causes the `VIFs` field of the corresponding
VM object to be updated automatically.

The API reference explicitly lists the fields that are
bound together in this way. It also contains a diagram that shows
relationships between classes. In this diagram an edge signifies the
existance of a pair of fields that are bound together, using standard
crows-foot notation to signify the type of relationship (e.g.
one-many, many-many).

RPCs associated with fields
---------------------------

Each field, `f`, has an RPC accessor associated with it
that returns `f`'s value:

- `get_f (r)`: Takes a `ref`, `r`, that refers to an object and returns the value of `f`.

Each field, `f`, with attribute `RW` and whose outermost type is `set` has the following
additional RPCs associated with it:

- `add_f (r, v)`: Adds a new element `v` to the set. Since sets cannot contain duplicate values this operation has no action in the case
that `v` was already in the set.

- `remove_f (r, v)`: Removes element `v` from the set.

Each field, `f`, with attribute RW and whose outermost type is `map` has the following
additional RPCs associated with it:

- `add_to_f (r, k, v)`: Adds new pair `(k → v)` to the mapping stored in `f` in object `r`. Attempting to add a new pair for duplicate
key, `k`, fails with an `MAP_DUPLICATE_KEY` error.
- `remove_from_f (r, k)`: Removes the pair with key `k` from the mapping stored in `f` in object `r`.

Each field whose outermost type is neither `set` nor `map`, 
but whose attribute is RW has an RPC accessor associated with it
that sets its value:

- `set_f (r, v)`: Sets field `f` on object `r` to value `v`.

RPCs associated with classes
----------------------------

- Most classes have a _constructor_ RPC named `create` that
takes as parameters all fields marked RW and
RO/constructor. The result of this RPC is that a new _persistent_ object is
created on the server-side with the specified field values.
- Each class has a `get_by_uuid (uuid)` RPC that returns the object
of that class that has the specified UUID.
- Each class that has a `name_label` field has a `get_by_name_label (name_label)` RPC that returns a set of objects of that
class that have the specified `name_label`.
- Most classes have a `destroy (r)` RPC that explicitly deletes the persistent object specified by `r` from the system.  This is a
non-cascading delete – if the object being removed is referenced by another
object then the `destroy` call will fail.

Additional RPCs
---------------

As well as the RPCs enumerated above, most classes have additional RPCs
associated with them. For example, the VM class has RPCs for cloning,
suspending, starting etc. Such additional RPCs are described explicitly
in the API reference.
