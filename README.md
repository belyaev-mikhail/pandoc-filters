# pandoc-filters
My pandoc filter set

## `include.hs`

Include a single file, parsing it as markdown:

`````
```include
file.md
```
`````


Include a single file, adjusting its header levels by an offset (like in asciidoc `include`):

`````
```{ .include leveloffset=1 }
file.md
```
`````

All the headers of level 1 in `file.md` will become headers of level 2, level 3 to level 4, etc.

Include multiple files:

`````
```include
file1.md
file2.md
file3.md
```
`````

All the lines not corresponding to existing files are skipped.
