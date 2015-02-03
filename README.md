# elm-diagrams

An Elm library for creating diagrams, inspired by Brent Yorgey's [Diagrams](http://projects.haskell.org/diagrams/) library for Haskell.

Support for

- Layout: Position diagrams next to, above, and atop each other
- Picking: Given a diagram and a point, find what was clicked on
- Getting coordinates: Given a _tag path_, find what coordinates it was placed at in a diagram.
- Getting width & height of diagrams
- Rendering with Elm's `Graphics.Collage`

Under construction; many bugs and TODOs. File them under GitHub Issues.

### Examples

There are a few examples under `examples/`. To run them, run `elm-reactor` in the root directory of the repository (not the `examples` directory — if you run it there, the Reactor will not find the library code in `Diagrams.elm`).