%------------------------------------------------------------------------------
%       zeus_paper class
%       ================
%
%       Provides a well-defined document style for ZEUS papers.
%       Based on the standard article.cls
%
%       Author: U. Katz, University of Bonn, April 2000
%               Bug reports etc. to katz@physik.uni-bonn.de
%
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
%       Class name
%       ==========
%------------------------------------------------------------------------------
\ProvidesClass{zeus_paper}[2000/11/14 Version 1.0]
\NeedsTeXFormat{LaTeX2e}
%------------------------------------------------------------------------------
%       Declare and process options
%       ===========================
%       [zdraft]                -> ZEUS paper draft (default)
%       [zpreprint]             -> ZEUS paper preprint (DESY red report)
%       [zconf]                 -> ZEUS conference contribution
%       [znote]                 -> ZEUS note
%       [zlist]                 -> for listings of bibliography database files
%
%       [zbstdefault]           -> standard ZEUS BiBTeX style file (default)
%       [zbstdraft]             -> standard ZEUS BiBTeX style file for drafts
%       [zbstepj]               -> ZEUS BiBTeX style file for Eur.J.Phys papers
%       [zbstnp]                -> ZEUS BiBTeX style file for Nucl.Phys. papers
%       [zbstpl]                -> ZEUS BiBTeX style file for Phys.Lett. papers
%
%       all other options are passed to the article package except
%       the article options [oneside], [twocolumn], [notitlepage],
%       [10pt], [11pt], [landscape], [a5paper], [b5paper] and 
%       [excutivepaper], which are overridden by the defaults
%       ([twoside], [onecolumn], [titlepage], [12pt] and [a4paper]).
%
%       Logicals:
%       ---------
%       if@zeusdrft (true)      -> true if [zdraft] option is selected
%       if@zeusprep (false)     -> true if [zpreprint] option is selected
%       if@zeusconf (false)     -> true if [zconf] option is selected
%       if@zeusnote (false)     -> true if [znote] option is selected
%       if@zeuslist (false)     -> true if [zlist] option is selected
%
%       ifzeusbst   (true)      -> true if [zbst] option is selected
%       ifzdrftbst  (false)     -> true if [zdrftbst] option is selected
%       ifzbstepj   (false)     -> true if [zbstepj] option is selected
%       ifzbstnp    (false)     -> true if [zbstnp] option is selected
%       iflbstpl    (false)     -> true if [zbstpl] option is selected
%       iflbstpl    (false)     -> true if [zbstpl] option is selected
%
%       if@tenpt    (false)     -> true if 10pt option was selected
%       if@elept    (false)     -> true if 11pt option was selected
%------------------------------------------------------------------------------
\newif\if@zeusdrft
\newif\if@zeusprep
\newif\if@zeusconf
\newif\if@zeusnote
\newif\if@zeuslist
%
\newif\ifzeusbst
\newif\ifzdrftbst
\newif\ifzbstepj
\newif\ifzbstnp
\newif\ifzbstpl
%
\newif\if@tenpt\@tenptfalse
\newif\if@elept\@eleptfalse
%
\typeout{->Part of LaTeX4zeus, (C) U.Katz, Univ. Bonn, 14.11.00 <-^^J}
%
\DeclareOption{defaults}%
   {\@zeusdrfttrue\@zeusprepfalse\@zeusconffalse\@zeusnotefalse\@zeuslistfalse
    \zeusbstfalse\zdrftbsttrue\zbstepjfalse\zbstnpfalse\zbstplfalse}
%
\DeclareOption{oneside}%
              {\typeout{^^J***********************************
                        ^^J*  Option oneside rejected by
                        ^^J*  zeus_paper document class.
                        ^^J*  Option twoside used instead.
                        ^^J***********************************^^J}
               \PassOptionsToClass{twoside}{article}}
\DeclareOption{twoside}{\PassOptionsToClass{twoside}{article}}
\DeclareOption{twocolumn}%
              {\typeout{^^J***********************************
                        ^^J*  Option twocolumn rejected by
                        ^^J*  zeus_paper document class.
                        ^^J*  Option onecolumn used instead.
                        ^^J***********************************^^J}
               \PassOptionsToClass{onecolumn}{article}}
\DeclareOption{onecolumn}{\PassOptionsToClass{onecolumn}{article}}
\DeclareOption{notitlepage}%
              {\typeout{^^J***********************************
                        ^^J*  Option notitlepage rejected by
                        ^^J*  zeus_paper document class.
                        ^^J*  Option titlepage used instead.
                        ^^J***********************************^^J}
               \PassOptionsToClass{titlepage}{article}}
\DeclareOption{titlepage}{\PassOptionsToClass{titlepage}{article}}
\DeclareOption{10pt}{%
  \@tenpttrue
  \PassOptionsToClass{10pt}{article}}
\DeclareOption{11pt}{%
  \@elepttrue
  \PassOptionsToClass{11pt}{article}}
\DeclareOption{12pt}{%
  \PassOptionsToClass{12pt}{article}}
\DeclareOption{landscape}%
              {\typeout{^^J***********************************
                        ^^J*  Option landscape ignored by
                        ^^J*  zeus_paper document class.
                        ^^J*  Default portrait mode used.
                        ^^J***********************************^^J}}
\DeclareOption{a5paper}%
              {\typeout{^^J***********************************
                        ^^J*  Option a5paper rejected by
                        ^^J*  zeus_paper document class.
                        ^^J*  Option a4paper used instead.
                        ^^J***********************************^^J}
               \PassOptionsToClass{a4paper}{article}}
\DeclareOption{b5paper}%
              {\typeout{^^J***********************************
                        ^^J*  Option b5paper rejected by
                        ^^J*  zeus_paper document class.
                        ^^J*  Option a4paper used instead.
                        ^^J***********************************^^J}
               \PassOptionsToClass{a4paper}{article}}
\DeclareOption{executivepaper}%
              {\typeout{^^J***********************************
                        ^^J*  Option executivepaper rejected
                        ^^J*  by zeus_paper document class.
                        ^^J*  Option a4paper used instead.
                        ^^J***********************************^^J}
               \PassOptionsToClass{a4paper}{article}}
\DeclareOption{a4paper}{\PassOptionsToClass{a4paper}{article}}
%
\DeclareOption{zdraft}%
   {\@zeusdrfttrue\@zeusprepfalse\@zeusconffalse\@zeusnotefalse\@zeuslistfalse
    \zeusbstfalse\zdrftbsttrue\zbstepjfalse\zbstnpfalse\zbstplfalse
    \ExecuteOptions{12pt}
    \if@tenpt
    \typeout{^^J***********************************
             ^^J*  Option 10pt rejected by
             ^^J*  zeus_paper document class.
             ^^J*  Option 12pt used instead.
             ^^J***********************************^^J}
    \fi
    \if@elept
    \typeout{^^J***********************************
             ^^J*  Option 11pt rejected by
             ^^J*  zeus_paper document class.
             ^^J*  Option 12pt used instead.
             ^^J***********************************^^J}
    \fi}
\DeclareOption{zpreprint}%
   {\@zeusdrftfalse\@zeuspreptrue\@zeusconffalse\@zeusnotefalse\@zeuslistfalse
    \ExecuteOptions{12pt}
    \if@tenpt
    \typeout{^^J***********************************
             ^^J*  Option 10pt rejected by
             ^^J*  zeus_paper document class.
             ^^J*  Option 12pt used instead.
             ^^J***********************************^^J}
    \fi
    \if@elept
    \typeout{^^J***********************************
             ^^J*  Option 11pt rejected by
             ^^J*  zeus_paper document class.
             ^^J*  Option 12pt used instead.
             ^^J***********************************^^J}
    \fi}
\DeclareOption{zconf}%
   {\@zeusdrftfalse\@zeusprepfalse\@zeusconftrue\@zeusnotefalse\@zeuslistfalse
    \ExecuteOptions{12pt}
    \if@tenpt
    \typeout{^^J***********************************
             ^^J*  Option 10pt rejected by
             ^^J*  zeus_paper document class.
             ^^J*  Option 12pt used instead.
             ^^J***********************************^^J}
    \fi
    \if@elept
    \typeout{^^J***********************************
             ^^J*  Option 11pt rejected by
             ^^J*  zeus_paper document class.
             ^^J*  Option 12pt used instead.
             ^^J***********************************^^J}
    \fi}
\DeclareOption{znote}
   {\@zeusdrftfalse\@zeusprepfalse\@zeusconffalse\@zeusnotetrue\@zeuslistfalse
   \if@tenpt\else\if@elept\else\ExecuteOptions{12pt}\fi\fi}
\DeclareOption{zlist}
   {\@zeusdrftfalse\@zeusprepfalse\@zeusconffalse\@zeusnotefalse\@zeuslisttrue
   \if@tenpt\else\if@elept\else\ExecuteOptions{12pt}\fi\fi}
%
\DeclareOption{zbstdefault}%
   {\zeusbsttrue\zdrftbstfalse\zbstepjfalse\zbstnpfalse\zbstplfalse}
\DeclareOption{zbstdraft}%
   {\zeusbstfalse\zdrftbsttrue\zbstepjfalse\zbstnpfalse\zbstplfalse}
\DeclareOption{zbstepj}%
   {\zeusbstfalse\zdrftbstfalse\zbstepjtrue\zbstnpfalse\zbstplfalse}
\DeclareOption{zbstnp}%
   {\zeusbstfalse\zdrftbstfalse\zbstepjfalse\zbstnptrue\zbstplfalse}
\DeclareOption{zbstpl}
   {\zeusbstfalse\zdrftbstfalse\zbstepjfalse\zbstnpfalse\zbstpltrue}
%
\ExecuteOptions{defaults,a4paper,twoside,onecolumn,titlepage,final}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax
%------------------------------------------------------------------------------
%       Load the article class and pass options
%       =======================================
%------------------------------------------------------------------------------
\LoadClass{article}
%------------------------------------------------------------------------------
%       Dependencies on other packages
%       ==============================
%       -> rotating     - to rotate boxes, pictures etc.
%       -> epsfig       - to include (encapsulated) postscript files
%       -> amsmath      - to use the AMS extended math package
%                         [centertags] -> equation numbers vertically centered
%                                         in split enevironment
%                         [intlimits]  -> integral limits on bottom and top
%                                         of integral sign
%       -> amssymb      - to obtain additional math symbols in AMS fonts
%       -> amscd        - to obtain AMS flow chart utilities
%       -> array        - to obtain additional tabular functionality
%       -> multirow     - for multirow-entries in tables
%       -> supertabular - for multi-page tables
%       -> dcolumn      - decimal-point aligned colums in tables
%       -> xspace       - to add empty space after commands
%       -> axodraw      - to draw Feynman diagrams (by J.A.M.Vermaseren)
%       -> upgreek      - provide upright greek letters
%       -> color        - color support
%       -> colors       - some color definitions
%       -> calc         - enhanced calculus in LaTeX macros
%       -> ifthen       - enhance logical structures in LaTeX macros
%       -> cite         - for multiple citations like [1-4] instead of [1,2,3,4]
%       -> mcite        - for multiple citations in one reference
%------------------------------------------------------------------------------
\RequirePackage{units}
\RequirePackage{rotating}
\RequirePackage{epsfig}
\RequirePackage[centertags,intlimits]{amsmath}
\RequirePackage{amssymb,amscd}
\RequirePackage{array}
\RequirePackage{multirow}
\RequirePackage{supertabular}
\RequirePackage{dcolumn}
\RequirePackage{xspace}
%\RequirePackage{axodraw}
\RequirePackage{upgreek}
\RequirePackage{color}
%\RequirePackage{colors}
\RequirePackage{calc}
\RequirePackage{ifthen}
\RequirePackage{mcite}
\RequirePackage{cite}
\RequirePackage{caption}

\captionsetup[figure]{labelfont=bf,textfont={it}}
\captionsetup[table]{labelfont=bf,textfont={it}}

%------------------------------------------------------------------------------
%       Set page layout parameters
%       --------------------------
%       ... plain pages
%------------------------------------------------------------------------------
\pagestyle{plain}
%------------------------------------------------------------------------------
%       ... horizontal spacings
%------------------------------------------------------------------------------
\setlength{\hoffset}%
          {-1.in}
\setlength{\evensidemargin}%
          {2.cm}
\setlength{\oddsidemargin}%
          {3.cm}
\setlength{\textwidth}%
          {\paperwidth-\evensidemargin-\oddsidemargin}
\newlength{\zeusabstractwidth}%
\setlength{\zeusabstractwidth}%
          {\textwidth*9/10}
%------------------------------------------------------------------------------
%       ... vertical spacings
%------------------------------------------------------------------------------
\setlength{\topmargin}%
          {0.5cm}
\setlength{\headheight}%
          {0.0cm}
\setlength{\headsep}%
          {0.0cm}
\setlength{\footskip}%
          {1.5cm}
\newlength{\footmargin}
\setlength{\footmargin}%
          {3.0cm}
\setlength{\textheight}%
          {\paperheight-1.0in-\voffset-\topmargin-\headheight-\headsep-%
           \footskip-\footmargin}
%------------------------------------------------------------------------------
%       ... float parameters
%------------------------------------------------------------------------------
\setlength{\floatsep}%
          {6pt plus 3pt minus 1pt}
\setlength{\textfloatsep}%
          {10pt plus 5pt minus 3pt}
\renewcommand{\topfraction}%
          {0.9}
\renewcommand{\bottomfraction}%
          {0.9}
\renewcommand{\textfraction}%
          {0.1}
%------------------------------------------------------------------------------
%       ... define clearemptydoublepage
%------------------------------------------------------------------------------
\newcommand{\clearemptydoublepage}{\newpage{\pagestyle{empty}\cleardoublepage}}
%------------------------------------------------------------------------------
%       Set text format parameters (spacings etc.)
%       ------------------------------------------
%       ... interline spacing, paragraph indention etc.
%------------------------------------------------------------------------------
\setlength{\parindent}%
          {0.0cm}
\setlength{\parskip}%
          {1.ex plus0.4ex minus0.2ex}
\setlength{\overfullrule}%
          {0.0pt}
\renewcommand{\baselinestretch}%
          {1.12}
\renewcommand{\arraystretch}%
          {1.1}
\renewcommand*{\arraystretch}{1.25}
%------------------------------------------------------------------------------
%       ... section numbering depth (4: allow for sections up to i.j.k.l.)
%       ... redefine section, subsection, subsubsection, paragraph 
%           to yield proper headings
%------------------------------------------------------------------------------
\catcode`\@=11 % @ signs are now treated as letters
\setcounter{secnumdepth}{4}
\renewcommand\section{\@startsection{section}{1}{\z@}%
                                   {-3.5ex \@plus -1ex \@minus -.2ex}%
                                   {2.3ex \@plus.2ex}%
                                   {\normalfont\Large\bfseries}}
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                   {-3.25ex\@plus -1ex \@minus -.2ex}%
                                   {1.5ex \@plus .2ex}%
                                   {\normalfont\large\bfseries}}
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                                   {-3.25ex\@plus -1ex \@minus -.2ex}%
                                   {1.5ex \@plus .2ex}%
                                   {\normalfont\large\bfseries}}
\renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
                                   {3.25ex \@plus1ex \@minus.2ex}%
                                   {1.2ex \@plus .2ex}%
                                   {\normalfont\normalsize\bfseries}}
%------------------------------------------------------------------------------
%       ... adjust footnote design
%------------------------------------------------------------------------------
\newlength{\@fninsert}
\newlength{\@fnwidth}
\setlength{\@fninsert}{0.6em}
\setlength{\@fnwidth}{\textwidth-\@fninsert-0.4em}
\renewcommand{\@makefntext}[1]%
  {\noindent\makebox[\@fninsert][r]{\@makefnmark}\hfil%
  \begin{minipage}[t]{\@fnwidth}{#1}\end{minipage}}
\addtolength{\skip\footins}{2.mm}
%------------------------------------------------------------------------------
%       Provide appropriate title page layouts
%       --------------------------------------
%       ... adjust title page design:
%           - define 'abstract' and 'zeusabstract' environment
%           - define 'prepnum'
%           - define 'draftversion'
%           - define 'confname'
%           - define 'confplacedate'
%           - define 'confabsnum'
%           - define 'confsession'
%------------------------------------------------------------------------------
\renewcommand{\abstract}[1]{\gdef\@abstract{\begin{zeusabstract}%
                          \ignorespaces#1%
                          \end{zeusabstract}}}
\newenvironment{zeusabstract}%
   {\begin{center}
      {\bfseries\abstractname}\vskip2.ex
      \begin{minipage}{\zeusabstractwidth}%
      \noindent}%
   {\end{minipage}\end{center}}%
\newcommand{\prepnum}[1]{\gdef\@prepnum{#1}}
\newcommand{\draftversion}[1]{\gdef\@draftversion{#1}}
\newcommand{\confname}[1]{\gdef\@confname{#1}}
\newcommand{\confplacedate}[1]{\gdef\@confplacedate{#1}}
\newcommand{\confabsnum}[1]{\gdef\@confabsnum{#1}}
\newcommand{\confsession}[1]{\gdef\@confsession{#1}}
%------------------------------------------------------------------------------
%       ... define makezeustitle for preprints
%------------------------------------------------------------------------------
\if@zeusprep
\newcommand\makezeustitle{\begin{titlepage}%
  \let\footnotesize\small
  \let\footnoterule\relax
  \let\footnote\thanks
  \setlength{\evensidemargin}{\evensidemargin/2+\oddsidemargin/2}
  \setlength{\oddsidemargin}{\evensidemargin}
  \@ifundefined{@prepnum}{%
        \typeout{^^J ********************************
                 ^^J *  zeus_paper class warning:
                 ^^J *  prepnum undefined
                 ^^J ********************************^^J}}{%
  \hfill\begin{minipage}{\textwidth}{\large\begin{tabbing}
                        dd\kill%
                        \@prepnum\\\@date
        \end{tabbing}}\end{minipage}}
  \null\vfil
  \begin{center}%
    {\LARGE\bf \@title \par}%
    \vskip 3em%
    {\large
     \lineskip .75em%
      \begin{tabular}[t]{c}%
        \large\@author
      \end{tabular}\par}%
      \vfil\vfil%
        \@ifundefined{@abstract}{%
        \typeout{^^J ********************************
                 ^^J *  zeus_paper class warning:
                 ^^J *  abstract undefined
                 ^^J ********************************^^J}}%
        {\@abstract}
  \end{center}\par
  \@thanks
  \vfil\null
  \end{titlepage}%
  \setcounter{footnote}{0}%
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@author\@empty
  \global\let\@date\@empty
  \global\let\@title\@empty
  \global\let\@abstract\@empty
  \global\let\@prepnum\@empty
  \global\let\@confname\@empty
  \global\let\@confplacedate\@empty
  \global\let\@confabsnum\@empty
  \global\let\@confsession\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
}\fi
%------------------------------------------------------------------------------
%       ... define makezeustitle for drafts
%------------------------------------------------------------------------------
\if@zeusdrft
\newcommand\makezeustitle{\begin{titlepage}%
  \let\footnotesize\small
  \let\footnoterule\relax
  \let\footnote\thanks
  \setlength{\evensidemargin}{\evensidemargin/2+\oddsidemargin/2}
  \setlength{\oddsidemargin}{\evensidemargin}
  \null\vfil
  \begin{center}%
    {\LARGE\bf \@title \par}%
    \vskip 3em%
    {\large
     \lineskip .75em%
      \begin{tabular}[t]{c}%
        \large\@author
      \end{tabular}\par}%
      \vskip .75em%
        \@ifundefined{@draftversion}{%
        \typeout{^^J ********************************
                 ^^J *  zeus_paper class warning:
                 ^^J *  draftversion undefined
                 ^^J ********************************^^J}
         {\large Draft, Version \texttt{\char92draftversion}\par}}%
        {{\large Draft, Version \@draftversion\par}}
        {\large \@date \par}%
      \vfil\vfil%
        \@ifundefined{@abstract}{%
        \typeout{^^J ********************************
                 ^^J *  zeus_paper class warning:
                 ^^J *  abstract undefined
                 ^^J ********************************^^J}}%
        {\@abstract}
  \end{center}\par
  \@thanks
  \vfil\null
  \end{titlepage}%
  \clearemptydoublepage
  \setcounter{footnote}{0}%
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@author\@empty
  \global\let\@date\@empty
  \global\let\@title\@empty
  \global\let\@abstract\@empty
  \global\let\@prepnum\@empty
  \global\let\@confname\@empty
  \global\let\@confplacedate\@empty
  \global\let\@confabsnum\@empty
  \global\let\@confsession\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
}\fi
%------------------------------------------------------------------------------
%       ... define makezeustitle for conference contributions
%------------------------------------------------------------------------------
\if@zeusconf
\newcommand\makezeustitle{\begin{titlepage}%
  \let\footnotesize\small
  \let\footnoterule\relax
  \let\footnote\thanks
  \setlength{\evensidemargin}{\evensidemargin/2+\oddsidemargin/2}
  \setlength{\oddsidemargin}{\evensidemargin}
  \newcommand{\confheader}{%
     \begin{center}
     \normalsize
     \vspace*{-\topmargin}
     Submitted to the
     \@ifundefined{@confname}{%
     \typeout{^^J ********************************
              ^^J *  zeus_paper class warning:
              ^^J *  confname undefined
              ^^J ********************************^^J}
     {\\\bfseries Conference to be defined by %
                  \texttt{\char92confname}}}%
     {\\\bfseries\@confname}
     \@ifundefined{@confplacedate}{%
     \typeout{^^J ********************************
              ^^J *  zeus_paper class warning:
              ^^J *  confplacedate undefined
              ^^J ********************************^^J}
     {\\ Conference place and date to be defined by %
                  \texttt{\char92confplacedate}}}%
     {\\\@confplacedate}
     \end{center}
     \vskip2.mm
     \hrule}
  \newcommand{\confinfo}{%
     \begin{minipage}[t]{\textwidth}\begin{tabbing}
        dd\kill%
        \@ifundefined{@confabsnum}{%
        \typeout{^^J ********************************
                 ^^J *  zeus_paper class warning:
                 ^^J *  confabsnum undefined
                 ^^J ********************************^^J}
        {\bfseries Abstract: \texttt{\char92confabsnum}}}%
        {\bfseries Abstract: \@confabsnum}
     \end{tabbing}\end{minipage}
     \hfill
     \begin{minipage}[t]{\textwidth}{\bfseries\begin{tabbing}
        dd\kill%
        \@ifundefined{@confsession}{%
        \typeout{^^J ********************************
                 ^^J *  zeus_paper class warning:
                 ^^J *  confsesssion undefined
                 ^^J ********************************^^J}
        {Sessions: \texttt{\char92confsession}}}%
        {\@confsession}
     \end{tabbing}}\end{minipage}}
  \confheader
  \vskip2.5ex
  \confinfo
  \null\vfil
  \begin{center}%
    {\LARGE\bf \@title \par}%
    \vskip 3em%
    {\large
     \lineskip .75em%
      \begin{tabular}[t]{c}%
        \large\@author
      \end{tabular}\par}%
      {\large \@date \par}%
      \vfil\vfil%
        \@ifundefined{@abstract}{%
        \typeout{^^J ********************************
                 ^^J *  zeus_paper class warning:
                 ^^J *  abstract undefined
                 ^^J ********************************^^J}}%
        {\@abstract}
  \end{center}\par
  \@thanks
  \vfil\null
  \end{titlepage}%
  \clearemptydoublepage
  \setcounter{footnote}{0}%
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@author\@empty
  \global\let\@date\@empty
  \global\let\@title\@empty
  \global\let\@abstract\@empty
  \global\let\@prepnum\@empty
  \global\let\@confname\@empty
  \global\let\@confplacedate\@empty
  \global\let\@confabsnum\@empty
  \global\let\@confsession\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
}\fi
%------------------------------------------------------------------------------
%       ... define makezeustitle for ZEUS notes
%------------------------------------------------------------------------------
\if@zeusnote
\newcommand\makezeustitle{\begin{titlepage}%
  \let\footnotesize\small
  \let\footnoterule\relax
  \let\footnote\thanks
  \setlength{\evensidemargin}{\evensidemargin/2+\oddsidemargin/2}
  \setlength{\oddsidemargin}{\evensidemargin}
  \@ifundefined{@prepnum}{%
        \typeout{^^J ********************************
                 ^^J *  zeus_paper class warning:
                 ^^J *  prepnum undefined
                 ^^J ********************************^^J}}{%
  \hfill\begin{minipage}{\textwidth}{\large\begin{tabbing}
                        dd\kill%
                        \@prepnum\\\@date
        \end{tabbing}}\end{minipage}}
  \null\vfil
  \begin{center}%
    {\LARGE\bf \@title \par}%
    \vskip 3em%
    {\large
     \lineskip .75em%
      \begin{tabular}[t]{c}%
        \large\@author
      \end{tabular}\par}%
      \vfil\vfil%
        \@ifundefined{@abstract}{%
        \typeout{^^J ********************************
                 ^^J *  zeus_paper class warning:
                 ^^J *  abstract undefined
                 ^^J ********************************^^J}}%
        {\@abstract}
  \end{center}\par
  \@thanks
  \vfil\null
  \end{titlepage}%
  \setcounter{footnote}{0}%
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@author\@empty
  \global\let\@date\@empty
  \global\let\@title\@empty
  \global\let\@abstract\@empty
  \global\let\@prepnum\@empty
  \global\let\@confname\@empty
  \global\let\@confplacedate\@empty
  \global\let\@confabsnum\@empty
  \global\let\@confsession\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
}\fi
%------------------------------------------------------------------------------
%       ... adjust caption design
%------------------------------------------------------------------------------
\newlength{\localtextwidth}
\setlength{\localtextwidth}{\textwidth}
\newsavebox{\tmpbox}
\newlength{\@captionmargin}
\newlength{\@captionwidth}
\newlength{\@captionitemtextsep}
\setlength{\@captionmargin}{1.2cm}
\setlength{\@captionitemtextsep}{1.em plus 0.5em minus 0.6em}
\renewcommand{\@makecaption}[2]%
  {\def\baselinestretch{0.95}%
   \vspace{10.pt}
   \setlength{\@captionwidth}{\localtextwidth-\@captionmargin}
   \sbox{\tmpbox}{{\bf #1:}{\it #2}}%
   \ifthenelse{\lengthtest{\wd\tmpbox > \@captionwidth}}%
   {\centerline{\parbox[t]{\@captionwidth}%
   {\tolerance=2000\normalsize%
    {\bf #1:}\hspace{\@captionitemtextsep}{\it #2}}}}%
   {\centerline{{\bf #1:}\kern1.em{\it #2}}}}
%------------------------------------------------------------------------------
%       ... list spacings
%------------------------------------------------------------------------------
\setlength\leftmargini   {1.5em}
\setlength\leftmarginii  {1.7em}
\setlength\leftmarginiii {1.7em}
\setlength\leftmarginiv  {1.7em}
\newlength{\zeusparsepi}
\newlength{\zeustopsepi}
\newlength{\zeusitemsepi}
\newlength{\zeuspartopsepi}
\setlength{\zeusparsepi}   {\parskip*3/4}
\setlength{\zeustopsepi}   {\parskip/4}
\setlength{\zeusitemsepi}  {\parskip/4}
\setlength{\zeuspartopsepi}{0\p@ \@plus2.5\p@ \@minus\p@}
\newlength{\zeusparsepii}
\newlength{\zeustopsepii}
\newlength{\zeusitemsepii}
\newlength{\zeuspartopsepii}
\setlength{\zeusparsepii}   {\parskip*2/3}                
\setlength{\zeustopsepii}   {\parskip/5}                
\setlength{\zeusitemsepii}  {\parskip/4}                
\setlength{\zeuspartopsepii}{0\p@ \@plus2.5\p@ \@minus\p@}
\newlength{\zeusparsepiii}
\newlength{\zeustopsepiii}
\newlength{\zeusitemsepiii}
\newlength{\zeuspartopsepiii}
\setlength{\zeusparsepiii}   {\parskip/2}
\setlength{\zeustopsepiii}   {\parskip/8}
\setlength{\zeusitemsepiii}  {\parskip/4}
\setlength{\zeuspartopsepiii}{0\p@ \@plus2.5\p@ \@minus\p@}
\newlength{\zeusparsepiv}
\newlength{\zeustopsepiv}
\newlength{\zeusitemsepiv}
\newlength{\zeuspartopsepiv}
\setlength{\zeusparsepiv}   {\parskip/2}
\setlength{\zeustopsepiv}   {\parskip/10}
\setlength{\zeusitemsepiv}  {\parskip/4}
\setlength{\zeuspartopsepiv}{0\p@ \@plus2.5\p@ \@minus\p@}
\newlength{\zeusparsepv}
\newlength{\zeustopsepv}
\newlength{\zeusitemsepv}
\newlength{\zeuspartopsepv}
\setlength{\zeusparsepv}   {\parskip/2}
\setlength{\zeustopsepv}   {\parskip/12}
\setlength{\zeusitemsepv}  {\parskip/4}
\setlength{\zeuspartopsepv}{0\p@ \@plus2.5\p@ \@minus\p@}
\newlength{\zeusparsepvi}
\newlength{\zeustopsepvi}
\newlength{\zeusitemsepvi}
\newlength{\zeuspartopsepvi}
\setlength{\zeusparsepvi}   {\parskip/2}
\setlength{\zeustopsepvi}   {\parskip/16}
\setlength{\zeusitemsepvi}  {\parskip/4}
\setlength{\zeuspartopsepvi}{0\p@ \@plus2.5\p@ \@minus\p@}
\renewcommand{\@listi}%
             {\leftmargin\leftmargini
              \setlength{\parsep}{\zeusparsepi}
              \setlength{\topsep}{\zeustopsepi}
              \setlength{\itemsep}{\zeusitemsepi}
              \setlength{\partopsep}{\zeuspartopsepi}}
\let\@listI\@listi
\@listi
\renewcommand{\@listii}%
             {\leftmargin\leftmarginii
              \labelwidth\leftmarginii
              \advance\labelwidth-\labelsep
              \setlength{\parsep}{\zeusparsepii}
              \setlength{\topsep}{\zeustopsepii}
              \setlength{\itemsep}{\zeusitemsepii}
              \setlength{\partopsep}{\zeuspartopsepii}}
\renewcommand{\@listiii}%
             {\leftmargin\leftmarginiii
              \labelwidth\leftmarginiii
              \advance\labelwidth-\labelsep
              \setlength{\parsep}{\zeusparsepiii}
              \setlength{\topsep}{\zeustopsepiii}
              \setlength{\itemsep}{\zeusitemsepiii}
              \setlength{\partopsep}{\zeuspartopsepiii}}
\renewcommand{\@listiv}%
             {\leftmargin\leftmarginiv
              \labelwidth\leftmarginiv
              \advance\labelwidth-\labelsep
              \setlength{\parsep}{\zeusparsepiv}
              \setlength{\topsep}{\zeustopsepiv}
              \setlength{\itemsep}{\zeusitemsepiv}
              \setlength{\partopsep}{\zeuspartopsepiv}}
\renewcommand{\@listv}%
             {\leftmargin\leftmarginv
              \labelwidth\leftmarginv
              \advance\labelwidth-\labelsep
              \setlength{\parsep}{\zeusparsepv}
              \setlength{\topsep}{\zeustopsepv}
              \setlength{\itemsep}{\zeusitemsepv}
              \setlength{\partopsep}{\zeuspartopsepv}}
\renewcommand{\@listvi}%
             {\leftmargin\leftmarginvi
              \labelwidth\leftmarginvi
              \advance\labelwidth-\labelsep
              \setlength{\parsep}{\zeusparsepvi}
              \setlength{\topsep}{\zeustopsepvi}
              \setlength{\itemsep}{\zeusitemsepvi}
              \setlength{\partopsep}{\zeuspartopsepvi}}
\catcode`\@=12 % @ signs are no longer letters
