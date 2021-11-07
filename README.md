# medlib
## Design
### Library mapper
Efficiently map a library to another library via filepath matching.

Mapping is done concurrently. For efficiency, matching occurs at two points:

  * Library traversal: skip traversing certain directories
  * Action scheduling: select action to perform depending on filepath

We don't build action management in, it's bring your own `IO ()`. Actions should
always write to a unique path, else you might silently clobber things with no
order guarantees.

#### TODO: Fancy display
Show with a fancy updating display (see concurrent-output package):

  * Processed/Total files to copy: (current copy)
  * Processed/Total files to convert: (current copy)

#### TODO: Job failures, thread exceptions
If a thread throws an exception, it should propagate to the thread scheduler
which kills all other threads.

#### Silly: Overly clever target directory structure handling
```haskell
data FS f a = FS (f (Map a (FS f a)))
type FS' = FS TVar Text
```

An `FS` holds a `TVar` map of a single path to another `FS`. We essentially
build a simplified effectful filesystem listing (directories only). It enables
us to use tons of smaller maps and have some concurrent access, instead of one
huge map (potential slowdown) with repeated paths (waste of memory) and less
concurrent access. Realistically, though, operations are all `O(log n)`, it's
extra kilobytes at the most, and we can still read concurrently all the same,
it's just writes that need locking. So it's not really worth considering.
