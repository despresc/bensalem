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
      By {emph|chance} I mean the same as probability.
    &li
      Events are independent when the happening of any one of them
      does neither increase nor abate the probability of the rest.
```
