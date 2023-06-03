This is compiled and ran using 3Days. It also runs on native TempleOS as I have
tested it in a VM.

The Dockerfile pulls and compiles 3Days and then is run in command line mode.

A test inside TempleOS itself would be the best since everything runs in Ring-0,
so there's no CPU context switching.

If you wish to get this inside TempleOS you can either mount the first partition
of a VM disk, or you could use RedSeaGen. Using this inside TempleOS has the
added benefit of more precise timings using `GetTSC` and looking at CPU cycles.
See `::/Demo/TimeIns.HC.Z` for a demo.

