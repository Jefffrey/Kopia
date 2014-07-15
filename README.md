#Kopia

[![Build Status](https://travis-ci.org/Jefffrey/Kopia.svg?branch=master)](https://travis-ci.org/Jefffrey/Kopia)

Kopia is a simple backup system. It's extremely thin and doesn't store any sort of configurations file and or junk-files.

##Targets, destinations and bridges

A *target* directory is a directory you want to take backups of. Kopia will always ask you permission to modify its contents.

A *destination* directory is the location in which you want to store the backups. This folder can also be non empty, but no guarantees are made to the existing contents, if any. 

If the specified destination doesn't exists, then it is automatically created (even parent directories).

The pair target-destination defines a *bridge*; a connection between a directory of interest and the backups location. You'll always need to specify a bridge for any operation. 

It is recommended that you set aliases for common backups:

```
alias project_a='kopia "project_a/" "backups_a/"'
alias project_b='kopia "project_b/" "backups_b/"'
```

##Snapshots and events

Once a bridge has been estabilished, you can take snaphots (specific revision of your target directory). 

Snaphosts are always organized in groups called "*events*". An event, identified by a short (directory name friendly) name. Events allow you to easily manage groups of related snapshots as a unit.

For example:

```
kopia_alias clear "event"
kopia_alias record "event" 60
```

will respectively remove all snapshots contained in the event "event" and then start a record session that will take a snaphot every minute.

##Directory organization

Kopia organizes backups into a very simple and human readable form:

```
destination/
    [event name]/
        kopia_[dd]-[mm]-[yyyy]_[hh]-[mm]-[ss]/
        .../
        .../
    .../
    .../
```

The resulting directory structure is completely indipendent from Kopia and can be manually managed without risks. Remember that Kopia also operates on the destination directory.

##Commands

You can define a bridge by specifying, in order, the target and destination, as follows:

```
kopia "target_a/" "destination_a/" [action]
kopia "target_b/" "destination_b/" [action]
```

A bridge is only conceptually defined. There's no need to initialize or configure a bridge. Once you have defined which is the target and which is the destination, you can issue any of the following commands.

For convenience, we will consider the following alias:

```
alias kopia_alias='kopia "target/" "destination/"'
```

####Takings snapshots

In order to take a single snapshot, you can issue:

```
kopia_alias take "name"
```

where `name` is the name of the event in which to place the snapshot.
