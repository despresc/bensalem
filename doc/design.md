Some rough notes on the eventual structure of the system.

# Goals

Develop:

- A durable, flexible, plain-text language that can represent complex historical
  scientific documents
- A packaging format for different types of archival items
- A reproducible build system that can render packaged items to different
  supported formats (paginated or reflowable, offline or online)
- A content-addressed archive and a package manager/item resolver to distribute
  items
- An online library to view collections, and other library analysis and
  exploration tools
- All of the above being as distributed as possible in design

Some things that should be supported in the eventual system:

- Completely digitize, say, Gauss's Disquisitiones Arithmeticae.
  - Represent and view different scans of the book
    - Note edits to scanned images, substitutions for bad scan images (so have
      both scan runs and "cleaned up" versions)
    - Create different navigation structures for the scans (logical divisions,
      individual pages)
    - Record sources, circumstances of scanning
  - Make a new digital edition of the second Latin edition
    - Define sufficiently rich semantic markup elements to capture the whole
      work
    - Include links to the original scanned pages
    - Link names, books, articles mentioned in the digital edition to items
      representing and explaining what they are and in turn linking to related
      items (works by mentioned authors, etc.)
    - Support editorial enhancements and explanations, like glosses of the
      mathematical Latin used
    - Create different navigational aids for the book (indices, links to
      original pages, logical table of contents) and its segmentation into
      individual web pages
  - Gather together different editions of the book into one or more records in a
    library - the scans, different editions of the book, perhaps descriptions of
    physical copies of the book, are all manifestations of a single abstract
    book.
- Gather common element definitions into modules, namespaces, to be distributed
  and used by many different documents
- Modify, override the metadata of documents in a registry

# Rough design sketch

Native markup documents:

- Define higher level semantic elements and how they are compiled to a
  lower-level core language (that then gets compiled to the various recognized
  output formats). These elements can have different properties that influence
  different compiler passes (numbering, gathering of content, placement of
  generated content).
- Refer (link) to other documents and items. Linking is fairly free of
  restrictions (notably, can form cycles).
- Import other documents and items. This can be internal, convenience importing
  (splitting a large document into individual files - a source code concept) or
  external importing (e.g., embedding bits of another document as a quotation).

Items:

- Contain metadata related to the circumstances of their creation: who created
  them, when, where, that sort of thing. Standard library data.
- Describe their own capabilities and requirements: how they ought to be
  displayed online, how to navigate them, how they may be transformed, what
  binary files (e.g., pictures and graphics) they needs to use.
- Represent different atomic archival entities and combinations of these:
  images, documents in particular markup languages, sequences of items (these
  being sub-items of a main item), entities without content (to represent
  non-digital things like people or places).

Registries:

- Maintain a mapping from names to the actual descriptions of items. In the URI
  sense, they could be considered an authority, with the names being the first
  component of a path, and sub-items of that named item being addressed with
  extensions to that path.
- May need to have some notion of foreign registries in order for item importing
  to work.
- Can be specified by items (somehow - by cryptographic key? hash of a registry
  state? web address? address in some kind of blockchain?) in order to guide
  resolution of names inside themselves. This is possible in a number of
  languages (e.g., Rust), though the main registries of those languages
  generally forbid packages in the registries themselves from containing any
  references to foreign registries, the possibility of specifying alternate
  registries being an option for private vendoring. The possibility of mixed
  registry items may complicate certain things; we will need to see how much of
  this complexity arises in the design of the system itself (too much is bad,
  may require abandonment of the concept) and how much arises in the
  administration of individual libraries/registries (less bad, can be dealt with
  simply by banning mixed-registry items).

# A brief discussion of the scriba markup syntax

Subject to change. The syntax takes some inspiration from TeX. A document is a
list of nodes, which are either plain text strings or elements. Elements can
take one of three forms:

1. The inline form starts with a tag like `\elt`. These have an optional
   attribute map enclosed in `[]` brackets, and an optional sequence of braced
   arguments enclosed in `{}` braces.
2. The indent form starts with a tag like `&elt`. These have optional attributes
   and arguments, then a "content" argument that must be indented more than the
   opening tag and continues until either content is reached with the same or
   lower indentation, or until an enclosing brace scope ends.
3. The level form starts with a tag like `#elt` or `##elt` at the beginning of a
   line. These have optional attributes and braced arguments, then a "content"
   argument that continues until either an element of a not-higher level (the
   same or fewer `#` symbols) is reached or an enclosing scope ends.

There are line comments that look like `\%comment text` and that remove all
subsequent text up to (but not including) the next newline from the document.
There are verbatim spans that look like `` \`verbatim content`/ `` that disable
the processing of (most) syntactically-relevant characters inside them. Finally,
there are escape sequences for some syntactically-relevant characters that may
be used to include these directly in the text.

Examples of the syntax:

```
#levelElement
  [attribute = something,
   anotherAttribute = {something else},
   mapAttribute = [x=y]
  ]

The escape sequences are the pairs: \\, \{, \}, \[, \], \&, \#
These stand for those characters without the initial backslash.

Inline elements look like \elt[attr1=thing]{arg1}{arg2}. If an element has no arguments
or attributes it can be written like \elt[] or \elt{}.

&indentElement
  {argument to indent element}
  {another argument}
  Final "content" argument that
     continues until the indentation drops back to the level
   of the indentElement tag.

   &anotherIndentElement
     This one is nested within the enclosing indentElement
   This text is included in indentElement, not anotherIndentElement.

##anotherLevelElement

This entire level element is contained within levelElement, and this sentence is 
contained in the content of anotherLevelElement.

#finalLevelElement
{argument to final level element}

Note that finalLevelElement ends the content of the initial levelElement.

Observe that forms can be nested if desired, as in

\elt{an argument
     &otherelt
       with an indent element inside of it
       #lvl
       and even a level element within that}

Verbatim spans appear like this:

\`Verbatim content that can include characters like & and \ directly without escaping them.
To include a backtick character, a double-bactick sequence `` can be used.`/

These can always be translated into a form without escape sequences, and the following
paragraph produces the same text as the verbatim span above:

Verbatim content that can include characters like \& and \\ directly without escaping them.
To include a backtick character, a double-bactick sequence ` can be used.
```

Presently the different forms have little semantic relevance; they exist to
structure document source code in an aesthetically pleasing way, and to control
certain aspects of whitespace handling. The rules around whitespace and
indentation are a little complex, but are intended to produce sensible results
while still allowing for precise whitespace handling when necessary. In more
detail:

- Whitespace here means either the single space ASCII character (one or more of
  these is "line space") or a line ending. We don't presently force a particular
  line ending sequence, but we likely will in future; for now, we take whatever
  the Haskell `text` library gives us as source text, which in practice means
  that any of the sequences `\n\r`, `\n`, and `\r` (or indeed the end of input)
  will end a line.
- Line space at the beginning of a line is indentation.
- A line that consists entirely of line space and then a line ending (or end of
  input) is a blank line.
- The content of indent elements, level elements, and braced groups all form an
  *indentation scope*; the common indentation is stripped from each line of
  these lines.
- A single initial and terminal blank line is stripped from any braced group, if
  they exist. This is slightly subtle, since the initial stripped whitespace
  will look like `<left brace><line space><line end>`, while the terminal
  stripped whitespace will look like `<line end><line space><right brace>`.
- All initial and terminal whitespace is stripped from the content of indent
  elements and level elements.
- The content of verbatim spans does not form its own indentation scope; a
  single initial and terminal blank line are stripped from these as with braced
  groups, but otherwise indentation is stripped according to the enclosing
  indentation scope, if one exists.


Examples of equivalent forms due to the whitespace handling rules:

```
#levelElt
  content
etc.

means the same thing as

#levelElt
    content
  etc.

---------

{content}
means the same thing as
{
content
}
and
{content
}

---------

&elt
  x
     y
    z
means the same thing as
&elt
      x
         y
        z
```
