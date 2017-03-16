Goal: parse a VERY RESTRICTED subset of LaTeX into HTML.

The idea would be that the expression $f_1: A\to B$ could be parsed into <i>f</i><sub>1</sub>: <i>A</i> ->
<i>B</i>. So for now inline math only, and not too many symbols; once I can do this well, then I can expand it.

Notes about the grammar:
	- the tokenizer should interpret \word as a single token.
	- % is a comment until the end of the line.
	- {...} is a "group", another node in the parse tree.
	- ^ and _ give super- and sub-script, respectively. They look ahead by 1, unless there's a group, in which case
	  they apply it to the whole group.
	- similarly, \mathbf, \mathcal, \mathscr, etc. look ahead by 1, possibly 1 group.
	- letters are set in italic.

The first goals will probably be tokenizing, then handling ^ and _, then making letters italic, then adding
symbols. To simplify, \mathbb will have to be \mathbf (i.e. <b>...</b>) first... though there's good support for
\mathbb out there already (wiki: mathematical alphanumeric symbols).
