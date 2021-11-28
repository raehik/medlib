  * Status display: better column sizing behaviour
  * When checking transcoded file hashes, also check for FFmpeg argument
    equality (have we changed the quality we're transcoding at?)
  * b3sum is blazingly fast to the point where IO is the clear bottleneck, not
    CPU. Wow lol. (It turns out it segments the file in a threaded manner! So
    freaking efficient!!)
    * So, shift jobs around. Sucks looooool
    * Shifted -- but write this down somewhere.
  * Use xxHash instead of b3sum, it's even faster
  * Consider a sample-based hasher
  * Redo transcoding interface
    * Pass a `Map Text Text` map of tag-value pairs
    * All tags are auto-prefixed with `Medlib`
    * Always save a hash and the size
    * Maybe remove the CLI for now
