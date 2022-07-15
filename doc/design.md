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

Some things that should be supported in the eventual system:

- I should be able to completely digitize, say, Gauss's Disquisitiones
  Arithmeticae. 
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
