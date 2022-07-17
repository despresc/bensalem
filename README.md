# Scriba - markup language, publishing, and library tools

This repository will eventually contain a larger suite of tools for publishing
and cataloguing documents, focusing primarily on historical scientific and other
technical documents. At the moment it contains a parser for a native markup
language for the eventual larger scriba system.

This project exists because of my frustrations with existing markup languages,
document management systems, and archives. While there is undoubtedly a level of
irreducible complexity in this field, I feel that a new system based on modern
programming principles can still ease the pain of writing and maintaining
collections of documents and other archival materials.

## The system

The goal of the system is to be able to store, catalogue, and distribute a wide
range of documents and associated archival materials. Such a system requires a
number of components:

- A markup language that can be used to write new digital editions of documents
- A compiler that can produce good-looking documents in a range of formats
- A packaging system that can simplify document distribution, re-use, and
  archiving.
- A library and publishing system that can be used to view, explore, and
  maintain collections of documents.

## The markup language

The scope of the markup language itself, its capabilities and precise semantics,
is not yet fully determined. However, it will ideally meet certain desiderata:

- Expressiveness. The markup language should be able to capture the meaning of a
  wide range of historical documents.
- Extensibility. The markup language should be easily extended with new
  constructs by users to fill in gaps in any future language core or standard
  library.
- Reusability. The markup language should be programmable, in some sense, and
  the new language constructs and functions should be easily reusaable through a
  module and build system.
- Durability. The markup language should be readable without special tools,
  documents written in it should be easily integrated into archival systems.
- Beauty. The markup language source itself should be easy to read and write,
  and it should be easy to produce beautiful documents with it.

A simple document (with suggestive but presently undefined element names) is
given here as illustration of the syntax, taken from *An essay towards solving a
problem in the doctrine of chances* by the Rev. Thomas Bayes:

```
#mainMatter

&problem
  \physPage{376} \emph{Given} the number of times in which an unknown event has
  happened and failed: \emph{Required} the chance that the probability of its
  happening in a single trial lies somewhere between any two degrees of
  probability that can be named.

##section
  [title = Section I]

&definition
  &olist
    &li
      Several events are \emph{inconsistent}, when if one of them
      happens, none of the rest can.
    &li
      Two events are \emph{contrary} when one, or other of them must;
      and both together cannot happen.
    &li
      An event is said to \emph{fail}, when it cannot happen; or,
      which comes to the same thing, when its contrary has happened.
    &li
      An event is said to be determined when it has either happened
      or failed.
    &li
      The \emph{probability of any event} is the ratio between the
      value at which an expectation depending on the happening of the
      event ought to be computed, and the value of the thing expected
      upon \reg{it's}{its} happening.
    &li
      By \emph{chance} I mean the same as probability.
    &li
      Events are independent when the happening of any one of them
      does neither increase nor abate the probability of the rest.
```

## Why this and not other things?

I initially envisioned a very particular purpose for this project: to develop
tools that would allow me to create new digital editions of documents, link them
together, explore their connections, and have them be rendered to both HTML (in
a live web view) and TeX (for eventual printing and binding). After having
thought about the problem, I came to the conclusion that keeping the solution
tailored narrowly to the individual documents that I wanted to digitize would
result in significant rigidity in the ultimate design: better instead to create
a larger, more flexible, yet still coherent system that would accomplish the
same goals. I haven't yet seen something that accomplishes exactly what I want:

- Markdown as a markup language is not suited to extensibility - it works well
  as a simple, lightweight language that compiles transparently to HTML, but
  suffers when more complex semantic constructs are shoehorned into it.
- XML as a markup language has a heavy, unwieldy syntax. Its main use would be
  for the ecosystem of standards and tools around it, but I feel that a new
  system based on modern programming principles (with features like modules,
  standard packaging and distribution, an improved type system) can improve
  significantly in this area.
- Asciidoc and reStructuredText suffer from a combination of the problems of
  Markdown and XML.
- TeX is an excellent typesetting engine and is also highly extensible, but is a
  rather poor programming language. It is of course also geared toward fixed,
  paginated output, and its semantic constructs are precariously built on top of
  macros over its typesetting primitives. Past efforts to improve and extend TeX
  (LuaTeX improving the underlying programming language, formats like OMDoc
  attempting to improve the semantics in some areas, various converters from TeX
  to other output formats) still seem inadequate for my goals.

Many of the existing formats still have their merits, and, while they all have
deficiencies that I believe can be fixed with a new design, their features can
inform those of Scriba. In particular, TeX, XML formats like those maintained by
the [Text Encoding Initiative](https://tei-c.org/), and various library and
archival standards are all rich sources for the ultimate semantics of the markup
language and the workings of the library system itself.

If the design is ever completely firmed up and the tools are finished (perhaps
unlikely, since this is only something I work on for fun in my spare time, but
possible) then the result could be a fine replacement for many different XML
formats and standards, LaTeX and ConTeXt, a number of different lightweight
markup languages, and various library and archive systems. It would not really
replace TeX (and the packages that extend it) as a typesetting engine, nor would
it replace simple languages like Markdown that are well-suited to their narrow
domains.
