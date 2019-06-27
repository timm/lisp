<a name=top></a>
<a href="https://github.com/timm/lisp/blob/master/README.md#top">
<img src="https://raw.githubusercontent.com/timm/lisp/master/etc/img/gotlisp.png" ></a><br>
[home](https://github.com/timm/lisp/blob/master/README.md#top) ::
[src](http://github.com/timm/lisp) ::
[contrib](https://github.com/timm/lisp/blob/master/CONTRIBUTING.md) ::
[discuss](https://github.com/timm/lisp/issues) ::
[lib](https://github.com/timm/lisp/tree/master/src/lib/README.md#top) :: 
[oo](https://github.com/timm/lisp/tree/master/src/oo/README.md#top)  :: 
[rows](https://github.com/timm/lisp/tree/master/src/rows/README.md#top) ::
[&copy; 2019](https://github.com/timm/lisp/blob/master/LICENSE.md) [Tim Menzies](http://menzies.us) 

# Just some of my LISP Coding Tricks


## Installation

For Mac or Unix:

```
cd someplace
git clone http://github.com/timm/lisp
cd lisp
sh ell
```

This will install LISP (if you do not have it), and present you with

```bash
;;;; (GOT (LISP '?)) ;v2.0 (c) 2019 <timm@ieee.org> http://git.io/gotlisp
GOT::master someplace/lisp 1>
```

At this point you can CNTRL-D to exit or `cd src/test` to run some unit
tests to get a feel for this code.


## Why Use These Tools?

### Cause you love LISP 

<img align=right src="http://lisperati.com/lisplogo_256.png">

- "Lisp isn't a language, it's a building material." - Alan Kay
- "Lisp is a programmable programming language." - John Foderaro
- "...please don't assume Lisp is only useful for Animation and Graphics, AI, Bioinformatics, B2B and E-Commerce, Data Mining, EDA/Semiconductor applications, Expert Systems, Finance, Intelligent Agents, Knowledge Management, Mechanical CAD, Modeling and Simulation, Natural Language, Optimization, Research, Risk Analysis, Scheduling, Telecom, and Web Authoring just because these are the only things they happened to list." - Kent Pitman 
- "It seems to me that there have been two really clean, consistent models of programming so far: the C model and the Lisp model. These two seem points of high ground, with swampy lowlands between them. As computers have grown more powerful, the new languages being developed have been moving steadily toward the Lisp model. A popular recipe for new programming languages in the past 20 years has been to take the C model of computing and add to it, piecemeal, parts taken from the Lisp model, like runtime typing and garbage collection." - Paul Graham

### Cause you dividing code into  lots of little files

This code uses the following directory strcture

```
/README.md
/ell            ; create a friendly BASH environment
/got.lisp       ; simple load manager
/etc/           ; support files (e.g. vim config) 
/src/           ; GOT packages (1 per sub-directory)
    bias/
    lib/
    oo/
    table/
    test/  ; place for unit tests
    ...
```

### Cause you like CLISP for brevity and SBCL for speed

All the code here runs on `sbcl` and `clisp`.

### Cause you like test any file, mostly in isolation

Every one of my files knows its depedancies so this
code can be loaded from any file.

To achieve this, all my code is in  `lib/\*/`\*lisp` and starts with:

```lisp
;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

```

Once that header is in place then

```lisp
(got "aa/" "bb.lisp" "cc/dd.lisp")
```

will hunt the `src/\*/\*` directories looking for your code:

- "aa/" will load all the LISP files in the sub-directory "src/aa";
- If there is only one "bb.lisp" in `src/\*/\*" then this will be loaded; 
- If there are many "dd.lisp" files, then use "cc/dd.lisp" to just
  load that file from the `cc`` directory.

Note that `got` remembers what whas loaded
(so multiple calls to load the same file will mean it gets loaded once only).

### Cause you want `README.md`s generated from LISP documentation strings

Once you run `sh ell` then the `readmes` command will pretty-print the
all the docstrings in the `\*.lisp` files, and use these to create `README.md` files
in all `lib/\*' sub-directories. 

Note that the header of those README.md` files will be taken from the first paragraph
of `/README.md`.  So if you want some standard headers and navigations, add itthere.

### Cause you don't want code + config all over the hard drive

All this code is held in the repo including all the config files (which are in `etc/\*`).

This means that this code can easily jump from machine to machine,

### Cause you log in to many machines via ascii terminals and all you want is a half-decent ascii IDEs

This code  contains all my ascii IDE tricks (tmux, vim, bash). Command-line rules!

### Cause you thing most IDE tools are crazy over-elaborations.

'nough said


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


