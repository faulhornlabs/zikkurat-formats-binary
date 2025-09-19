
Handling common binary ZK file formats
======================================

Parsing (and later maybe also writing) commonly used binary ZK file formats:

- `.ptau` files used by `snarkjs`, `phase2-bn254` and others to encode the results of the "powers of tau" trusted ceremony
- `.r1cs` files, describing R1CS circuits used by `circom`, `snarkjs` and others
- `.wtns` files, used by `circom` and `snarkjs` to encode the witness
- `.zkey` files, the prover key used by `snarkjs` or `rapidsnark`
- `.ltau` files, our version of `.ptau` containing the Lagrange basis form of a KZG setup

Note: the JSON- and text-based formats are in a separate library, to avoid the unnecessary
`aeson` dependency hell. 


iden3 binary containers
-----------------------

The formats `.r1cs`, `.wtns`, `.zkey` and `.ptau` share a common generic structure.
This format is very simple: There is a global header, and then several sections.
The actual content and meaning of the sections is format-dependent.

The `.r1cs` format is specified here: https://github.com/iden3/r1csfile/blob/master/doc/r1cs_bin_format.md

The particular file formats are also documented in the relevant source files in this repo.


TODO
----

- [ ] factor out the FlatArray types from `zikkurat-algebra`, and use that for array formats (?)
- [x] export the witness into `.wtns` 
- [x] export constraints into `.r1cs` 
- [ ] export the proving key into `.zkey`
