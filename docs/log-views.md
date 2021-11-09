In order to handle all of these:

  * workers need an `Either -> IO ()`

## View 1: Log
Still some bits that change due to the concurrent nature.

```
CPU | 1/? | tmp/main/track1.flac | checking hash
CPU | 2/? | tmp/main/track2.flac | transcoding
IO  | 1/? | tmp/main/track3.ogg | copying
```

Only show the total pool jobs when we know it's final (when all jobs are
queued).

```
CPU | 1/5 | tmp/main/track1.flac | checking hash
CPU | 2/5 | tmp/main/track2.flac | transcoding
IO  | 1/3 | tmp/main/track3.ogg  | copying
```

## View 2: Dynamic region
Show every worker slot.

```
CPU 1 | tmp/main/track1.flac | checking hash
CPU 2 | tmp/main/track2.flac | transcoding
CPU 3 | [nothing to do]
IO  1 | tmp/main/track3.ogg | copying

CPU 0/?
IO  0/?
```

```
CPU 1 | tmp/main/track1.flac | checking hash
CPU 2 | tmp/main/track2.flac | transcoding
IO  1 | tmp/main/track3.ogg | copying

CPU 0/2
IO  0/1
```

## View 3: Small dynamic region
Problem: Detailed information for every worker is busy.
Solution: Do a cute worker status display.

```
CPU xxxxxxxxxx
IO  x
```
