# Sparse Set

Sparse Sets are useful when you have a lot of potential keys but only a small part of them is used. Sparse sets can deal with many keys while providing fast iteration over the values within the set.

There are 4 variants:
- `Boxed`: The standard version which can deal with arbitrary Haskell values.
- `Storable`: Can only deal with Storable values.
- `Unboxed`: Can only deal with Unbox(vector package) values.
- `NoComponent`: Contains no values, only the keys.
