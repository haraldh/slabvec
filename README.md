# slabvec

Array of `T` with dynamic allocations of `N` entries for slabs of size `M`

The array will dynamically allocate slabs of size `M` to extend its internal
linked list, which will extend the capacity by `N`.

Currently nightly until `#![feature(min_const_generics)]` hits stable with `1.51`.
