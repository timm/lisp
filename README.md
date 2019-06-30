<a name=top></a>
'([home](https://github.com/timm/lisp/blob/master/README.md#top) 
([&copy;2019](https://github.com/timm/lisp/blob/master/LICENSE.md) 
(["Tim Menzies"](http://menzies.us))))
<img width=1 height=25 src="https://github.com/timm/lisp/blob/master/etc/img/FFFFFF.png">
<a href="https://github.com/timm/lisp/blob/master/README.md#top">
<img src="https://raw.githubusercontent.com/timm/lisp/master/etc/img/gotlisp.png" ></a><br>
'(:site ([src](http://github.com/timm/lisp) 
([contrib](https://github.com/timm/lisp/blob/master/CONTRIBUTING.md)
([discuss](https://github.com/timm/lisp/issues))))      
&nbsp;&nbsp;:code ([lib](https://github.com/timm/lisp/tree/master/src/lib/README.md#top)
([oo](https://github.com/timm/lisp/tree/master/src/oo/README.md#top)
([sample](https://github.com/timm/lisp/tree/master/src/sample/README.md#top)))))


<font color=gray>123</font>
# Just a few LISP coding tricks


<img width=400 align=right src="https://raw.githubusercontent.com/timm/lisp/master/etc/img/haveLisp.jpg">

After decades of coding,
in 
Prolog and LISP and Smalltalk and 
Python and Lua and CoffeeScript and Gawk and  Bash and  other thing,
I found myself using the same tricks all the time; i.e.

- the same directory structures;
- the same unit testing strategy;
- the same way of handling global options;
- the same workflow (git for storage, markdown for documentation);
- the same multi-paned environment (tmux);
- the same editor (vim);
- the same shell (bash).

All of those tricks
worked in ascii terminals, were fast to boot, and worked cross-platform.

Here are those tricks, tweaked  for LISP.

## Installation

For Mac or Unix:

```
git clone http://github.com/timm/lisp
cd lisp
sh ell
```

This will install LISP (if you do not have it), and present you with
a shiny new BASH prompt:


```bash
;;;; (GOT (LISP '?)) ;v2.0 (c) 2019 <timm@ieee.org> http://git.io/gotlisp
GOT::master parent/lisp 1>
```

At this point you can:

- CTRL-D to exit 
- `cd lisp/src/someSubDirectory` and start writing code (just make sure
   your files start with [my standard header](#header)).
- `cd lisp/src/test` to run some unit tests to get a feel for this code.

### Uninstall

Just zap the directory containing this repo:

```bash
rm -rf lisp    # zaps all files
```

Then, optinally, uninstall the LISPs installed by this code (SBCL, CLISP).

## Why Use These Tools?

### (Cause (you (love (LISP)))) 


<img align=right width=400 src="http://imgs.xkcd.com/comics/lisp_cycles.png">

- "Lisp isn't a language, it's a building material." **-&nbsp;Alan&nbsp;Kay**
- "Lisp is a programmable programming language." **-&nbsp;John&nbsp;Foderaro**
- "...please don't assume Lisp is only useful for Animation and Graphics, AI, Bioinformatics, B2B and E-Commerce, Data Mining, EDA/Semiconductor applications, Expert Systems, Finance, Intelligent Agents, Knowledge Management, Mechanical CAD, Modeling and Simulation, Natural Language, Optimization, Research, Risk Analysis, Scheduling, Telecom, and Web Authoring just because these are the only things they happened to list." **-&nbsp;Kent&nbsp;Pitman** 
- "It seems to me that there have been two really clean, consistent models of programming so far: the C model and the Lisp model. These two seem points of high ground, with swampy lowlands between them. As computers have grown more powerful, the new languages being developed have been moving steadily toward the Lisp model. A popular recipe for new programming languages in the past 20 years has been to take the C model of computing and add to it, piecemeal, parts taken from the Lisp model, like runtime typing and garbage collection." **-&nbsp;Paul&nbsp;Graham**

### Cause you divide code into  lots of little files

This code uses the following directory strcture

```
lisp/
   README.md
   ell            ; create a friendly BASH environment
   got.lisp       ; simple load manager
   etc/           ; support files (e.g. vim config) 
       dotbashrc  ; called when you load `sh ell`
       dotvimrc   : uused when you call vim
       dottmux    ; used when you call tmux
   src/           ; GOT packages (1 per sub-directory)
       bias/
         about.lisp ; notes on this package.
         ...
       lib/
         about.lisp ; notes on this package.
         ...
       oo/
         about.lisp ; notes on this package.
         ...
       ...
       test/  ; place for unit tests
       ...
```

### Cause you like CLISP for brevity and SBCL for speed

All the code here runs on `sbcl` and `clisp`.

### <a name="header">Cause you like to test any file, mostly in isolation</a>

Every one of my files knows its depedancies so this
code can be loaded from any file.

To achieve this, all my code is in  `lisp/serc/\*/`\*lisp` and starts with:

```lisp
;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

```

Once that header is in place then

```lisp
(got "aa/" "bb.lisp" "cc/dd.lisp")
```

will hunt the `lisp/src/\*/\*` directories looking for your code:

- "aa/" will load all the LISP files in the sub-directory "lisp/src/aa";
- If there is only one "bb.lisp" in `lisp/src/\*/\*" then this will be loaded; 
- If there are many "dd.lisp" files, then use "cc/dd.lisp" to just
  load that file from the `cc`` directory.

Note that `got` remembers what whas loaded
(so multiple calls to load the same file will mean it gets loaded once only).

### Cause you want `README.md`s generated from LISP documentation strings

Once you run `sh ell` then the `readmes` command will pretty-print the
all the docstrings in the `\*.lisp` files, and use these to create `README.md` files
in all `lisp/lib/\*' sub-directories. 

Also, if you want other text in the `READNE.md`, add an `fyi` form into your `*.lisp` file. e.g.:

```lisp
(fyi "some text to show
mabe many lines long.

### ALso

can contain markdnown text.
```

Note that:

- The header of those `README.md` files will be taken from the first paragraph
of `lisp/README.md`.  So if you want some standard headers and navigations, add itthere.
- Each 
`lisp/lib/\*' sub-directories should have  an `about.lisp` file containing
  stuff to be listed first in the readme.


### Cause you don't want code + config all over the hard drive

All this code is held in the repo including all the config files (which are in `lisp/etc/\*`).

This means that this code can easily jump from machine to machine,

### Cause you log in to many machines via ascii terminals and all you want is a half-decent ascii IDEs

This code  contains all my ascii IDE tricks (tmux, vim, bash). Command-line rules!

## BTW, Great Books on Lisp

Start here:

- [Learn Lisp in 20 minutes](https://learnxinyminutes.com/docs/common-lisp/)
- [Land of Lisp](https://www.amazon.com/Land-Lisp-Learn-Program-Game/dp/1593272812),  Conrad Barski.  For a good time, Start here
- [Ansi Common Lisp](https://www.amazon.com/ANSI-Common-LISP-Paul-Graham/dp/0133708756/ref=pd_sbs_14_5/135-4118199-9331832?_encoding=UTF8&pd_rd_i=0133708756&pd_rd_r=cf9f4331-975b-11e9-809f-935832fab2f8&pd_rd_w=197Te&pd_rd_wg=ERVPM&pf_rd_p=588939de-d3f8-42f1-a3d8-d556eae5797d&pf_rd_r=ZRERR6MHZMGSN413SSPC&psc=1&refRID=ZRERR6MHZMGSN413SSPC),  Paul Graham. Great  reference material. Lots of smal, crytal clear, exmples

More advanced stuff:

- [Practical Common LIsp](https://www.amazon.com/Practical-Common-Lisp-Peter-Seibel/dp/1590592395/ref=pd_lpo_sbs_14_img_0?_encoding=UTF8&psc=1&refRID=ZEPN9KH9XRMKZGXAA2FE),   Peter Seibel. Great for real-world applications
- [On Lisp](https://www.amazon.com/Lisp-Advanced-Techniques-Common/dp/0130305529/ref=pd_sbs_14_5/135-4118199-9331832?_encoding=UTF8&pd_rd_i=0130305529&pd_rd_r=e79bb471-975b-11e9-b02c-7d41afbdbeec&pd_rd_w=rdmOM&pd_rd_wg=CdYXa&pf_rd_p=588939de-d3f8-42f1-a3d8-d556eae5797d&pf_rd_r=HYX8480H6JNQTKHEHAJ9&psc=1&refRID=HYX8480H6JNQTKHEHAJ9),  More Paul Graham. More advanced stuff. Same crystal clear code.

Much AI stuff (in LISP):

- [Paradigms of Artiical Intelligence](https://www.amazon.com/Paradigms-Artificial-Intelligence-Programming-Studies/dp/1558601910/ref=pd_sbs_14_2/135-4118199-9331832?_encoding=UTF8&pd_rd_i=1558601910&pd_rd_r=ba1e0b44-975b-11e9-9e03-b199d7ed47b8&pd_rd_w=DBLQX&pd_rd_wg=NmfDg&pf_rd_p=588939de-d3f8-42f1-a3d8-d556eae5797d&pf_rd_r=YKATY6HX2MYDV0NSKEJX&psc=1&refRID=YKATY6HX2MYDV0NSKEJX), Peter Norvig. Some much inference, so little code. Totally wow.


