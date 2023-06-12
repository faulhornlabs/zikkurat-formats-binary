
Handling common ZK file formats
===============================

Parsing and writing commonly used ZK file formats:

- `.r1cs` files, describing R1CS circuits used by `circom`, `snarkjs` and others
- `.wtns` files, used by `circom` and `snarkjs` to encode the witness
- `.sym` files used by `circom` and `snarks` to describe mapping of names to witness indices
- `.ptau` files used by `snarkjs`, `phase2-bn254` and others to encode the results of the "powers of tau" trusted ceremony


iden3 binary containers
-----------------------

The formats `.r1cs`, `.wtns` and `.ptau` share a common generic structure.
This format is very simple: There is a global header, and then several sections.
The content of the sections is format-dependent.

The `.r1cs` format is specified here: https://github.com/iden3/r1csfile/blob/master/doc/r1cs_bin_format.md