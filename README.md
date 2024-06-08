# Demonstration of stripped-down 0D (zero dependency) system written in Common Lisp

# usage
$ sbcl --script zd.lisp

# About
- minimal (not practical) example of 1 reader and 1 writer arranged in a pipeline
- send "initialize" to the reader to specify which file to open
- then, send "go" to the writer to start the run
- the writer repeatedly requests 1 character at a time from the reader until the reader sends NIL
- this looks, on the surface, to be very low level and too complicated, but,
	- SWB (SoftWare Blocks) can be composed in layers
		- the lowest layers contain systems code
		- non-systems programmers can compose SWBs at higher levels without need-to-know what is happening at lower layers
	- this technique can be extended to compile DPLs (Diagrammatic Programming Languages)
		- see further examples of this [*references to existing repos need to be included here*]
	- this shows how structured "systems" code can be programmed at low levels, but invisible to non-systems programmers
	- this technique doesn't NEED to use operating systems, threads, buffers, and, other complicated stuff (yet, this example is built to run on MacOS, etc.)
	
