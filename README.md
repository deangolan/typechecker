Small dependently typed core language based on Martin-Löf Type Theory.

#### Implemented
- Π-types
- Universes
- Lambda abstraction and application
- De Bruijn indices
- β-reduction and normalization
- Parser with lexical scoping
- Typechecking

#### Notes

This is an experimental implementation of a minimal dependently typed core language.
Future work includes extending toward homotopy type theory.

The long-term goal is to experiment with features from HoTT such as higher inductive
types. For example, I would like to support definitions like:

```
Set (A : *) :=
    | Nil
    | Cons A (Set A)
    | Collapse : (x : A) -> Set (Cons x (Cons x Nil)) = Set (Cons x Nil)
    | Reorder : (x : A) -> (y : A) -> Set (Cons x (Cons y Nil)) = (Set (Cons y (Cons x Nil)))
```
