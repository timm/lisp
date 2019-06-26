<a name=top></a>
<a href="https://github.com/timm/lisp/blob/master/README.md#top">
<img src="https://raw.githubusercontent.com/timm/lisp/master/etc/img/gotlisp.png" ></a><br>
[home](https://github.com/timm/lisp/blob/master/README.md#top) ::
[src](http://github.com/timm/lisp) ::
[contrib](https://github.com/timm/lisp/blob/master/CONTRIBUTING.md) ::
[discuss](https://github.com/timm/lisp/issues) ::
[license](https://github.com/timm/lisp/blob/master/LICENSE.md) :: 
[lib](https://github.com/timm/lisp/tree/master/src/lib/README.md#top) :: 
[oo](https://github.com/timm/lisp/tree/master/src/oo/README.md#top)  :: 
[rows](https://github.com/timm/lisp/tree/master/src/rows/README.md#top)  




# Just some of my LISP Programming Tricks

<img align=right src="http://lisperati.com/lisplogo_flag2_256.png">

Table of contents

- [Introduction](#Introduction)
- [Installation](#Installation)

## Introduction

### Why Use These Tools?

- Cause you like LISP;
- Cause you like dividing your work into  lots of little files;
- Cause you like CLISP for its brevity and SBCL for its speed;
- Cause you like being able to test any file (mostly) in isolation from everything else;
- Cause you want all the code and config inside a repo (not spread all over the hard drive)
- Cause you want documentation tools that write README.md files from the LISP documentation strings.
- Cause you often log in to different machines via ascii terminals and all you want is a half-decent ascii IDE;
- Cause you thing most IDE tools are crazy over-elaborations.


### But Why Lisp? Well, Why Not?

"Lisp isn't a language, it's a building material." - Alan Kay

"Lisp is a programmable programming language." - John Foderaro

"...please don't assume Lisp is only useful for Animation and Graphics, AI, Bioinformatics, B2B and E-Commerce, Data Mining, EDA/Semiconductor applications, Expert Systems, Finance, Intelligent Agents, Knowledge Management, Mechanical CAD, Modeling and Simulation, Natural Language, Optimization, Research, Risk Analysis, Scheduling, Telecom, and Web Authoring just because these are the only things they happened to list." - Kent Pitman 

"It seems to me that there have been two really clean, consistent models of programming so far: the C model and the Lisp model. These two seem points of high ground, with swampy lowlands between them. As computers have grown more powerful, the new languages being developed have been moving steadily toward the Lisp model. A popular recipe for new programming languages in the past 20 years has been to take the C model of computing and add to it, piecemeal, parts taken from the Lisp model, like runtime typing and garbage collection." - Paul Graham

### Great Books on Lisp

Start here:

- [Learn Lisp in 20 minutes](https://learnxinyminutes.com/docs/common-lisp/)
- [Land of Lisp](https://www.amazon.com/Land-Lisp-Learn-Program-Game/dp/1593272812),  Conrad Barski.  For a good time, Start here
- [Ansi Common Lisp](https://www.amazon.com/ANSI-Common-LISP-Paul-Graham/dp/0133708756/ref=pd_sbs_14_5/135-4118199-9331832?_encoding=UTF8&pd_rd_i=0133708756&pd_rd_r=cf9f4331-975b-11e9-809f-935832fab2f8&pd_rd_w=197Te&pd_rd_wg=ERVPM&pf_rd_p=588939de-d3f8-42f1-a3d8-d556eae5797d&pf_rd_r=ZRERR6MHZMGSN413SSPC&psc=1&refRID=ZRERR6MHZMGSN413SSPC),  Paul Graham. Great  reference material. Lots of smal, crytal clear, exmples

More advanced stuff:

- [Practical Common LIsp](https://www.amazon.com/Practical-Common-Lisp-Peter-Seibel/dp/1590592395/ref=pd_lpo_sbs_14_img_0?_encoding=UTF8&psc=1&refRID=ZEPN9KH9XRMKZGXAA2FE),   Peter Seibel. Great for real-world applications
- [On Lisp](https://www.amazon.com/Lisp-Advanced-Techniques-Common/dp/0130305529/ref=pd_sbs_14_5/135-4118199-9331832?_encoding=UTF8&pd_rd_i=0130305529&pd_rd_r=e79bb471-975b-11e9-b02c-7d41afbdbeec&pd_rd_w=rdmOM&pd_rd_wg=CdYXa&pf_rd_p=588939de-d3f8-42f1-a3d8-d556eae5797d&pf_rd_r=HYX8480H6JNQTKHEHAJ9&psc=1&refRID=HYX8480H6JNQTKHEHAJ9),  More Paul Graham. More advanced stuff. Same crystal clear code.

Much AI stuff (in LISP):

- [Paradigms of Artiical Intelligence](https://www.amazon.com/Paradigms-Artificial-Intelligence-Programming-Studies/dp/1558601910/ref=pd_sbs_14_2/135-4118199-9331832?_encoding=UTF8&pd_rd_i=1558601910&pd_rd_r=ba1e0b44-975b-11e9-9e03-b199d7ed47b8&pd_rd_w=DBLQX&pd_rd_wg=NmfDg&pf_rd_p=588939de-d3f8-42f1-a3d8-d556eae5797d&pf_rd_r=YKATY6HX2MYDV0NSKEJX&psc=1&refRID=YKATY6HX2MYDV0NSKEJX), Peter Norvig. Some much inference, so little code. Totally wow.

### Software

Getting started:

- Want a 2-clicks install ? Then get [Portacle](https://shinmera.github.io/portacle/), a portable and multiplatform Common Lisp environment. It ships Emacs25, SBCL (the implementation), Quicklisp (package manager), SLIME (IDE) and Git. It’s the most straightforward way to get going !
- For other getting started advice, see [The LISP cookbook](https://lispcookbook.github.io/cl-cookbook/getting-started.html).



Optional:  Install [quicklisp](https://www.quicklisp.org/beta/)  (which comes with ([Portacle](https://shinmera.github.io/portacle/). 
  Quicklisp is more than a package manager, it is also a central repository (a dist) that ensures that all libraries build together.

FY: I have my own, lightweight package management system contolled by the `got` function.


## Installation

XXX
