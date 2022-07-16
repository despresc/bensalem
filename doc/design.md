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
- Refer (link) and import other documents and items. The distinction is
  important - mere linking is freer (can form cycles) and is sort of analogous
  to dynamic linking in programs, while importing is stricter 

Items:

- Contain metadata related to the circumstances of their creation: who created
  them, when, where, that sort of thing. Standard library data.
- Describe their own capabilities and requirements: how they ought to be
  displayed online, how to navigate them, how they may be transformed, what
  binary files (e.g., pictures and graphics) they needs to use.
- Represent different atomic archival entities and combinations of these:
  images, documents in particular markup languages, sequences of items (these
  being sub-items of a main item).

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
