#Kopia

[![Build Status](https://travis-ci.org/Jefffrey/Kopia.svg?branch=master)](https://travis-ci.org/Jefffrey/Kopia)

Kopia is a simple command-line backup system. It's extremely thin and doesn't store configurations and/or junk files.

##Setup

###Setting up a bridge

Defining a connection (namely a *bridge*) between a directory (namely a *target*) and a backup directory (namely a *destination*), is as simple as defining an alias. For UNIX systems:

```shell
alias bridge='kopia "target/" "destination/"'
```

###Directory organization

Kopia organizes backups in a human readable form:

```
destination/
    [event name]/
        [dd]-[mm]-[yyyy]_[hh]-[mm]-[ss]-[ps]/
        .../
        .../
    .../
    .../
```

The resulting directory structure is completely indipendent from Kopia and can be manually managed without risks.

##Commands

###Taking snapshots

Once a bridge is defined, you can issue commands to control the status of both the target and the destination directory. The unit for any modification is called a *snapshot*. 

You just need to remember that:

- a snapshot is a version of the target directory at some point in time
- a snapshot is always saved in a specific *event* named by you.

You can take a snapshot with:

```shell
bridge take "event name"
```

Notice that the event name should be directory-name friendly for you OS (make sure the characters you use are allowed).

###Listing snapshots

At this point to list all snapshots for a specific event:

```shell
bridge list "event name"
```

You can limit the number of results with `--max=N` and control the order with `--order=[newest|oldest]`:

```shell
bridge list "event name" --max=13 --order=oldest
bridge list "event name" --max=0
bridge list "event name" --order=newest
```

The above commands will yield:

1. the first 13 snapshots from the newest to the oldest
2. all the snapshots for the event (default)
3. all the snapshots from the oldest to the newest (default)
