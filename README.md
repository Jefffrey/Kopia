###Kopia

Kopia is a simple and customizable backup system. It doesn't use configuration files.

####Events and snapshots

An event is a set of snapshots referred to by a short name. A snaphot is a particular backup in time. Every snapshot is defined within the event namespace.

####Bridge

Kopia always requires two initial paths:
    
    - the target path
    - the destination path

The target path is the folder you want to take snapshots of; the destination path is a folder in which you want to store the events. The destination folder is created (as well as the parents) if missing. 

A pair of `(target, destination)` is called a *bridge*. You can picture a *bridge* as a connection between two folders in which you can:

 - take backups
 - restore backups

####Commands

You can define a bridge by specifying, in order, the target and destination, as follows:

```
> kopia "target/" "destination/"
```

It is recommended that you set aliases of common backups, for example with:

```
> alias common_kopia='kopia "target/" "destination/"'
```

Everything specified later must be one of the following commands.

#####`take`

In order to take a single snapshot, you can issue:

```
> common_kopia take "name"
```

where `name` is the name of the event.
