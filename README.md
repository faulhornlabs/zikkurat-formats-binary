
Handling common binary ZK file formats
======================================

Parsing (and later maybe also writing) commonly used binary ZK file formats:

- `.ptau` files used by `snarkjs`, `phase2-bn254` and others to encode the results of the "powers of tau" trusted ceremony
- `.r1cs` files, describing R1CS circuits used by `circom`, `snarkjs` and others
- `.wtns` files, used by `circom` and `snarkjs` to encode the witness
- `.sym` files used by `circom` and `snarks` to describe mapping of names to witness indices

Note: the JSON-based formats are in a separate library, to avoid the unnecessary
`aeson` dependency hell. 


iden3 binary containers
-----------------------

The formats `.r1cs`, `.wtns` and `.ptau` share a common generic structure.
This format is very simple: There is a global header, and then several sections.
The actual content of the sections is format-dependent.

The `.r1cs` format is specified here: https://github.com/iden3/r1csfile/blob/master/doc/r1cs_bin_format.md

