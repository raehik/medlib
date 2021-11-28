  * Status display: better column sizing behaviour
  * When checking transcoded file hashes, also check for FFmpeg argument
    equality (have we changed the quality we're transcoding at?)
  * Status display: consider: `CPU X | transcode | file` and just manually space
    verbs lol. Easy, pretty nice.
  * b3sum is blazingly fast to the point where IO is the clear bottleneck, not
    CPU. Wow lol. (It turns out it segments the file in a threaded manner! So
    freaking efficient!!)
    * So, shift jobs around. Sucks looooool
    * Shifted -- but write this down somewhere.
  * Use xxHash instead of b3sum, it's even faster
  * Use a sample-based hasher
  * Check files via metadata (bytesize) instead of hash -- zoom.
