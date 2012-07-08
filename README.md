Perforator CI
=============

Perforator CI is a continious integration server used for performance
regression testing.

It is used together with the Perforator performance unit testing tool,
which is can be found here: https://github.com/Spawnfest2012/perforator

Dependencies
----

* git
* tested only on Linux, sorry
* [npm](http://npmjs.org)

Quick tutorial
-----

Start the Perforator CI server:

```
   git clone https://github.com/Spawnfest2012/perforator2 perforator_ci
   cd perforator_ci/
   make start-clean
```

Perforator CI should be running on http://localhost:8080/

We need add a project which does some performance unit testing.

The Perforator tools has some sample performance testing modules, so let's try
it, use git@github.com:Spawnfest2012/perforator.git as your repo and keep 
the default build steps.

Do a few builds and click around, you should experience some fabulous
statistics.

Current problems
-----

* some of the statistics are sloppy, so don't call us for a refund, we know,
  we're working on it. The test duration measureument is pretty tight though.
* stuff is very fragile, be gentle.
