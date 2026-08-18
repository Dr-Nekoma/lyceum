-module(lyceum_cluster_backoff).
-moduledoc """
Capped exponential backoff with full jitter, for retry loops.

Extracted so the policy is stated once. `world_migrations` grew the
same doubling-with-a-ceiling logic independently, and every layer
boundary added from here on needs it again: a node retrying its peers,
a worker waiting on a database, a session waiting on a logic node.

## Why jitter

Plain capped exponential backoff is the wrong shape for a cluster.
When a service node restarts, every frontend and logic node receives
`nodedown` in the same instant and starts the same deterministic
sequence of delays, so they retry in lockstep and stay in lockstep
indefinitely, converging on the recovering node together at each step.
Randomising the delay is what breaks the herd apart.

`delay/3` implements the "full jitter" strategy, which sleeps for a
random duration anywhere below the exponential ceiling:

```
sleep = random(0, min(cap, base * 2 ^ attempt))
```

Of the four strategies benchmarked in the reference below (no jitter,
full jitter, equal jitter, decorrelated jitter), full jitter came out
lowest on total client work at competitive completion times.

The deterministic ceiling is kept separate in `ceiling/3`, both because
it is the part worth reasoning about directly and because it lets the
randomised function be checked against a bound.

## References

Marc Brooker, ["Exponential Backoff And
Jitter"](https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter/),
AWS Architecture Blog, 2015.
""".

-export([delay/3, ceiling/3]).

-doc """
The deterministic upper bound on retry number `Attempt`.

`Attempt` is 1-based: the first retry after a failure is attempt 1 and
has a ceiling of `Base`. The doubling is clamped at 8 shifts before
`Max` is applied, which keeps the intermediate value small enough to
never become a bignum no matter how long a peer stays down.

```erlang
1> lyceum_cluster_backoff:ceiling(1, 1000, 30000).
1000
2> lyceum_cluster_backoff:ceiling(4, 1000, 30000).
8000
3> lyceum_cluster_backoff:ceiling(400, 1000, 30000).
30000
```
""".
-spec ceiling(Attempt, Base, Max) -> pos_integer() when
    Attempt :: pos_integer(),
    Base :: pos_integer(),
    Max :: pos_integer().
ceiling(Attempt, Base, Max) when
    is_integer(Attempt), Attempt >= 1, is_integer(Base), Base >= 1, is_integer(Max), Max >= 1
->
    min(Base bsl min(Attempt - 1, 8), Max).

-doc """
Milliseconds to wait before retry number `Attempt`.

Uniformly distributed over `[1, ceiling(Attempt, Base, Max)]`. The
floor of 1 rather than 0 is what keeps the result a `pos_integer()`,
and costs nothing: a caller that would have slept 0ms is not being
held back by the extra millisecond.

Being random, this can return a shorter delay than the previous
attempt did. That is the intended behaviour and not a bug, the
guarantee is on the ceiling growing, not on any individual delay.
""".
-spec delay(Attempt, Base, Max) -> pos_integer() when
    Attempt :: pos_integer(),
    Base :: pos_integer(),
    Max :: pos_integer().
delay(Attempt, Base, Max) ->
    rand:uniform(ceiling(Attempt, Base, Max)).
