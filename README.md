# medlib
A set of tools for managing an audio media library.

Intended use is for efficient conversion of a large library storing tracks in
lossless format with various metadata and extras, to a "portable" version which
may transcode and skip extras to reduce size. (I store my music in such a
fashion, and a portable library makes sense since I use an MP3 player a lot.

## Usage
`stack install`

Then use as a CLI program: try `medlib --help` for starters.

## Design
### Library mapper
Efficiently map a library to another library via filepath matching.

Proudly overengineered. Two sized worker pools run concurrently to maximise
resource usage without bottlenecking.

Some jobs when mapping a library are IO-bound (e.g. direct copying), while
others are CPU-bound (e.g. transcoding). An IO job will try to use as much IO
as it can -- but a CPU job is limited to one core, unless it can run itself
parallel. Transcoding with FFmpeg is a slow single-core operation, so we get
great potential speedup by executing jobs concurrently.

BLAKE3's `b3sum` appears to have great parallelism, so adding concurrency
doesn't save us anything there, though does possibly square the number of
threads. Wonder if I could expand the pool concept? Wonder what existing
libraries and code similar to mine do that the face of trying to apply
concurrency to mixed parallelism.

## License
Provided under the MIT license. Please see `LICENSE` for the full license text.
