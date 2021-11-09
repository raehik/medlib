# Architecture
## Runtime
  * One status thread. Keeps track of mapping operation status via a single
    'TQueue'. Other threads send it detailed messages, and it updates its
    state accordingly. Once it tracks that all jobs have finished, it exits.
  * One traverser thread. Traverses the source directory and queues jobs.
  * X pool manager threads. Continually schedules jobs and updates status
    thread, until its job queue is closed and all jobs are complete, at which
    point it tells the status thread so and exits.
  * Dynamic number of worker threads. *Maybe* needs to report some status, but
    preferably handled by pool manager instead.
  * One logger thread. Status thread child, runtime config determines what
    actually runs. Status thread always sends the same info, logger decides what
    to do with it. Allows us to do fancy updating stuff along with regular
    scrolling logs without writing separate logging.

Heavily concurrent. The idea is to place any potentially blocking operation into
its own thread. Consider the logger: it's a separate thread primarily so that
terminal I/O only occurs in one stream (=> no need to lock). But also, it may
*want* to be blocking. Like, it may want to update only once per second.

## Data
I decide to pass library files around as `(FP, FP)` where the first
position is the directory (`""` -> root), and the second is the filename, purely
because my traverser doesn't use the filename (so no need to combine them there)
and I see it simplifying some actions and code.

I could go a step further and handle all library paths as `([FP], FP)`,
indicating crumbs and a file.

  * (?) Appears I should shove it all into a `MonadReader CMap`, where `CMap`
    includes libroot, libdestroot, a pool size function.
