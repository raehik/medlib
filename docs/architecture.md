# Architecture
## By data
  * (?) Appears I should shove it all into a `MonadReader CMap`, where `CMap`
    includes libroot, libdestroot, a pool size function.

## By thread
### Logger
Puts things on stdout etc. TODO it's not actually a logger, it's essentially the
main thread

  * Delegator polls when a pool has been assigned a job, and when the library
    has been fully traversed.
  * Pool schedulers poll when a worker completes a job, and when all jobs are
    complete (and the job queue is closed).
