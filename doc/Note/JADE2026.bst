FUNCTION {hello}
{ "BibTeX bibliography style created from zeusbtxbst.bdc" top$
  "-> Part of LaTeX4zeus, © U.Katz, I.Brock, Univ. Bonn, 31.05.13 <-" top$
  "ZEUS_DRAFT (ZEUS style for drafts)" top$

  " LAB_ALPH .............. off" top$
  " SORTED ................ off" top$
  " NAME_FULL ............. off" top$
  " ATIT_LOWER ............ on" top$
  " MONTH_FULL ............ off" top$
  " JOUR_FULL ............. off" top$
  " ATIT_SUPPRESS ......... off" top$
  " MONTH_SUPPRESS ........ on" top$
  " PAREN_DATE ............ on" top$
  " DATE_FIRST ............ off" top$
  " VOLUME_SPACE .......... on" top$
  " JOUR_DEEMPH ........... on" top$
  " NUM_SUPPRESS .......... on" top$
  " BOLD_VOL .............. on" top$
  " PAGE_START_J .......... on" top$
  " PAGE_START_O .......... off" top$
  " ABBREVIATE ............ on" top$
  " NO_NAMEFMT ............ on" top$
  " SHOW_DOI .............. on" top$
  " SHOW_EPRINT ........... on" top$
  " BRACKET_EPRINT ........ off" top$
  " URL_PUNCT ............. on" top$
  " TITLE_EMPH_A .......... off" top$
  " TITLE_EMPH_B .......... off" top$
  " SHOW_TITLE_T .......... on" top$
  " NIM_PL ................ on" top$
  " NIM_NP ................ off" top$
  " NIM_EPJ ............... off" top$
  " NIM_JHEP .............. off" top$
  " CPC_PL ................ on" top$
  " CPC_NP ................ off" top$
  " MARK_CHECKED .......... on" top$
  " WARN_REPLACED ......... on" top$
  " PRINT_LABEL ........... off" top$
  " MULTI_LINE ............ off" top$
  " USE_MCITE ............. on" top$
  " MCITE_NEWLINE ......... on" top$
  " BF_SPECIALS ........... on" top$
}

ENTRY
  { address
    archive
    archivePrefix
    author
    booktitle
    chapter
    checked
    collaboration
    doi
    edition
    editor
    eprint
    howpublished
    institution
    journal
    key
    month
    note
    number
    organization
    pages
    primaryClass
    publisher
    replacedby
    replaces
    report
    reportNumber
    school
    series
    submitdate
    thesistype
    title
    type
    url
    volume
    www
    year
  }
  {}
  { label }

INTEGERS { output.state before.all mid.sentence after.sentence after.block
           continue.sentence after.newline after.sen.newline }

FUNCTION {init.state.consts}
{ #0 'before.all :=
  #1 'mid.sentence :=
  #2 'after.sentence :=
  #3 'after.block :=
  #4 'continue.sentence :=
  #5 'after.newline :=
  #6 'after.sen.newline :=
}

STRINGS { s t u }

FUNCTION {not}
{   { #0 }
    { #1 }
  if$
}

FUNCTION {and}
{   'skip$
    { pop$ #0 }
  if$
}

FUNCTION {or}
{   { pop$ #1 }
    'skip$
  if$
}

FUNCTION {add.single.period}
{ 't :=
  t #-1 #1 substring$ "." = t #-1 #3 substring$ ".{}" = or
      't
      { t add.period$ }
    if$
}

FUNCTION {remove.period}
{ 't :=
  t #-1 #1 substring$ "." = t #-1 #3 substring$ ".{}" = or
      { t #1 t text.length$ #1 - substring$ } 
      't
    if$
}

FUNCTION {capitalize.first}
{ 't :=
  t #1 #1 substring$ 'u :=
  u u purify$ =
     { u "u" change.case$ }
     { u }
   if$
   t #2 global.max$ substring$ *
}

INTEGERS { bool.capitalize }

FUNCTION {output.nonnull}
{ 's :=
  #0 'bool.capitalize :=
  output.state mid.sentence =
      { ", " * write$ }
      { output.state continue.sentence =
	  'write$
	  { output.state after.newline =
              { "," * write$ 
		newline$
		"\newblock " write$
              }
	      { #1 'bool.capitalize :=
                output.state after.block =
		  { add.period$ write$
		    newline$
		    "\newblock " write$
		  }
		  { output.state after.sen.newline =
	              { add.period$ write$ newline$ }
		      { output.state before.all =
			  'write$
			  { add.single.period " " * write$ }
			if$
		      }
                    if$
                  }
		if$
	      }
            if$
          }
        if$
      mid.sentence 'output.state :=
      }
    if$
  bool.capitalize
      { s capitalize.first }
      { s }
    if$
}

FUNCTION {output}
{ duplicate$ empty$
    'pop$
    'output.nonnull
  if$
}

FUNCTION {output.check}
{ 't :=
  duplicate$ empty$
    { pop$ "empty " t * " in " * cite$ * warning$ }
    'output.nonnull
  if$
}

FUNCTION {new.block}
{ output.state before.all =
    'skip$
    { after.block 'output.state := }
  if$
}

FUNCTION {new.line}
{ output.state before.all =
    'skip$
    { output.state after.block =
	'skip$
	{ after.newline 'output.state := }
      if$
    }
  if$
}

FUNCTION {new.sentence}
{ output.state after.block =
    'skip$
    { output.state before.all =
	'skip$
	{ after.sentence 'output.state := }
      if$
    }
  if$
}

FUNCTION {continue.output}
{ output.state before.all =
    'skip$
    { output.state after.block =
        'skip$
        { continue.sentence 'output.state := }
      if$
    }
  if$
}

FUNCTION {bbl.techrep}
{ "Technical Report" }

FUNCTION {bbl.dthesis}
{ "Diploma thesis" }

FUNCTION {bbl.mthesis}
{ "Master's thesis" }

FUNCTION {bbl.phdthesis}
{ "Ph.D. thesis" }

FUNCTION {capitalize}
{ "u" change.case$ "t" change.case$ }

FUNCTION {format.eprint.newarXivid}
{ eprint #1 #6 substring$ "arXiv:" =
    {"\href{http://arxiv.org/abs/" eprint #7 #10 substring$ * "}" *
        "{{\tt " * eprint #7 #10 substring$ * "}}" *}
    {"\href{http://arxiv.org/abs/" eprint #1 #10 substring$  * "}" *
        "{{\tt arXiv:" * eprint #1 #10 substring$ * "}}" *}
  if$
}

FUNCTION {format.eprint.oldarXivid}
{ archive empty$
    {"\href{http://arxiv.org/abs/" eprint * "}" *
        "{{\tt arXiv:" * eprint * "}}" *}
    {"\href{http://arxiv.org/abs/" archive *  "/" * eprint * "}" *
        "{{\tt arXiv:" * archive * "/" * eprint * "}}" *}
  if$
}

FUNCTION {format.eprint}
{ 
  eprint  empty$
    { "" }
    { eprint #1 #6 substring$ "arXiv:" =
      eprint #5 #1 substring$ "." =
      or  
      { format.eprint.newarXivid } 
      { format.eprint.oldarXivid }
      if$
    }
    if$
}

FUNCTION {format.doi}
{ 
  doi empty$
  { "" }
  { "\href{http://dx.doi.org/" doi * "}{doi:" * doi * "}" *}
  if$
}

FUNCTION {format.url}
{ 
  url empty$
  { "" }
  { "\href{" url * "}{" * url * "}" *}
  if$
}

FUNCTION {format.url2}
{
  url empty$
  { "" }
  { "\urlprefix\url{" swap$ * "}" * 
  www
  empty$
  { pop$ "" }
  if$
  }
  if$
}

FUNCTION {new.block.checka}
{ empty$
    'skip$
    'new.block
  if$
}

FUNCTION {new.block.checkb}
{ empty$
  swap$ empty$
  and
    'skip$
    'new.block
  if$
}

FUNCTION {new.line.checka}
{ empty$
    'skip$
    'new.line
  if$
}

FUNCTION {new.line.checkb}
{ empty$
  swap$ empty$
  and
    'skip$
    'new.line
  if$
}

FUNCTION {new.sentence.checka}
{ empty$
    'skip$
    'new.sentence
  if$
}

FUNCTION {new.sentence.checkb}
{ empty$
  swap$ empty$
  and
    'skip$
    'new.sentence
  if$
}

FUNCTION {field.or.null}
{ duplicate$ empty$
    { pop$ "" }
    'skip$
  if$
}

FUNCTION {emphasize}
{ duplicate$ empty$
    { pop$ "" }
    { "{\em " swap$ * "}" * }
  if$
}

FUNCTION {boldify}
{ duplicate$ empty$
    { pop$ "" }
    { "{\bf " swap$ * "}" * }
  if$
}

FUNCTION {mboxify}
{ duplicate$ empty$
    { pop$ "" }
    { "\mbox{" swap$ * "}" * }
  if$
}

FUNCTION {output.bibitem}
{ newline$
  "\bibitem{" write$
  cite$ write$
  "}" write$
  newline$
  checked field.or.null "2" =
    { "" }
    { checked field.or.null "1" =
        { "\mbox{}\kern-2.cm\makebox[2.cm][l]{$\square$}%" write$ newline$
          ""
        }
        { "\mbox{}\kern-2.cm\makebox[2.cm][l]{$\bullet$}%" write$ newline$
          ""
         }
      if$
    }
  if$
  before.all 'output.state :=
}

FUNCTION {fin.entry}
{ remove.period write$
  "\relax" write$
  newline$
  "\relax" write$
}

INTEGERS { nameptr namesleft numnames }

FUNCTION {format.names}
{ skip$ }

FUNCTION {format.authors}
{ author empty$
    { "" }
    { author format.names }
  if$
}

FUNCTION {format.collaboration}
{ collaboration empty$
    { "" }
    { collaboration format.names }
  if$
}



FUNCTION {format.editors}
{ editor empty$
    { "" }
    { editor format.names
      editor num.names$ #1 >
        { "eds.~" }
        { "ed.~" }
      if$
      swap$ *
    }
  if$
}

FUNCTION {format.editors.as.authors}
{ editor empty$
    { "" }
    { editor format.names
      editor num.names$ #1 >
        { "~(eds.)" }
        { "~(ed.)" }
      if$
      *
    }
  if$
}

FUNCTION {format.title}
{ title empty$
    { "" }
    { title "t" change.case$ }
  if$
}

FUNCTION {n.dashify}
{ 't :=
  ""
    { t empty$ not }
    { t #1 #1 substring$ "-" =
	{ t #1 #2 substring$ "--" = not
	    { "--" *
	      t #2 global.max$ substring$ 't :=
	    }
	    {   { t #1 #1 substring$ "-" = }
		{ "-" *
		  t #2 global.max$ substring$ 't :=
		}
	      while$
	    }
	  if$
	}
	{ t #1 #1 substring$ *
	  t #2 global.max$ substring$ 't :=
	}
      if$
    }
  while$
}

FUNCTION {format.date}
{ year empty$
    { month empty$
	{ "" }
	{ "there's a month but no year in " cite$ * warning$
	  month
	}
      if$
    }
    { month empty$
	'year
	{ month " " * year * }
      if$
    }
  if$
}

FUNCTION {format.btitle}
{ title
}

FUNCTION {tie.or.space.connect}
{ duplicate$ text.length$ #3 <
    { "~" }
    { " " }
  if$
  swap$ * *
}

FUNCTION {either.or.check}
{ empty$
    'pop$
    { "can't use both " swap$ * " fields in " * cite$ * warning$ }
  if$
}

INTEGERS{ be.save fe.save }

FUNCTION {both.empty}
{ duplicate$ empty$
  'be.save :=
  swap$
  duplicate$ empty$
  be.save + 'be.save :=
  swap$
  be.save #2 =
}

FUNCTION {first.empty}
{ swap$ duplicate$ empty$
  'fe.save :=
  swap$
  fe.save
}

FUNCTION {format.bvolume}
{ volume empty$
    { "" }
    { "Vol." volume tie.or.space.connect
      series empty$
	'skip$
        { " of series " * series emphasize * }
      if$
      "volume and number" number either.or.check
    }
  if$
}

FUNCTION {format.number.series}
{ volume empty$
    { number empty$
	{ series field.or.null }
        { "no."
	  number tie.or.space.connect
	  series empty$
	    { "there's a number but no series in " cite$ * warning$ }
            { " in series " * series * }
	  if$
	}
      if$
    }
    { "" }
  if$
}

FUNCTION {format.volume.number.series}
{ volume empty$
    { number empty$
	{ series empty$
            { "" }
            { after.newline 'output.state :=
              "in " series *
            }
          if$
        }
	{ series empty$
	    { "there's a number but no series in " cite$ * warning$ 
              mid.sentence 'output.state :=
              "No." number tie.or.space.connect
            }
            { series ", " * "No." number tie.or.space.connect * }
          if$
	}
      if$
    }
    { series empty$
	{ mid.sentence 'output.state :=
          "Vol." volume tie.or.space.connect
        }
	{ series ", " * "Vol." volume tie.or.space.connect * }
      if$
      "volume and number" number either.or.check
    }
  if$
}

FUNCTION {format.edition}
{ edition empty$
    { "" }
    { edition "1" =
        { edition "$^{\rm st}$" * }
        { edition "2" =
            { edition "$^{\rm nd}$" * }
            { edition "3" =
                { edition "$^{\rm rd}$" * }
                { edition #-2 #1 substring$ "1" =
                    { edition "$^{\rm th}$" * }
                    { edition #-1 #1 substring$
                      "1" =
                        { edition "$^{\rm st}$" * }
                        { edition #-1 #1 substring$
                          "2" =
                            { edition "$^{\rm nd}$" * }
                            { edition #-1 #1 substring$
                              "3" =
                                { edition "$^{\rm rd}$" * }
                                { edition "$^{\rm th}$" * }
                                if$
                            }
                            if$
                        }
                        if$
                    }
                    if$
                }
                if$
            }
            if$
        }
        if$
        output.state mid.sentence =
            { "l" change.case$ "~edn." * }
            { "t" change.case$ "~edn." * }
          if$
    }
    if$
}

INTEGERS { multiresult }

FUNCTION {multi.page.check}
{ 't :=
  #0 'multiresult :=
    { multiresult not
      t empty$ not
      and
    }
    { t #1 #1 substring$
      duplicate$ "-" =
      swap$ duplicate$ "," =
      swap$ "+" =
      or or
	{ #1 'multiresult := }
	{ t #2 global.max$ substring$ 't := }
      if$
    }
  while$
  multiresult
}

FUNCTION {first.page}
{ 't :=
  ""
    {  t empty$ not t #1 #1 substring$ "-" = not and }
    { t #1 #1 substring$ *
      t #2 global.max$ substring$ 't :=
    }
  while$
}

FUNCTION {format.pages}
{ pages empty$
    { "" }
    { pages multi.page.check
        { "pp.~" pages n.dashify * }
        { "p.~" pages * }
      if$
    }
  if$
}

FUNCTION {format.first.page}
{ pages empty$
    { "" }
    { "p.~" pages first.page * }
  if$
}

FUNCTION {format.vol.num.pages}
{ volume field.or.null
  number empty$
    'skip$
    { "(" number * ")" * *
      volume empty$
	{ "there's a number but no volume in " cite$ * warning$ }
	'skip$
      if$ } if$ pages empty$ 'skip$ { duplicate$ empty$ { pop$
    format.pages } { ":" * pages n.dashify * } if$ } if$ }

FUNCTION {format.vol.num.date.pages}
{ volume empty$
      { "no volume in " cite$ * warning$ "" }
      { " " volume boldify * }
    if$
  year empty$
      { "no year in " cite$ * warning$ "" }
      { "~(" year * ")" * }
    if$
  pages empty$
      { "" }
      { first.empty
          { ",~" format.first.page * }
          { ",~" pages first.page * }
        if$
      }
    if$
  swap$
  * *
}

FUNCTION {format.chapter.pages}
{ chapter empty$
    'format.pages
    { type empty$
        { "chapter" }
	{ type "l" change.case$ }
      if$
      chapter tie.or.space.connect
      pages empty$
	'skip$
	{ ", " * format.pages * }
      if$
    }
  if$
}

FUNCTION {format.in.ed.booktitle}
{ booktitle empty$
    { "" }
    { editor empty$
        { "in " booktitle }
        { "in " booktitle ", " * format.editors * }
      if$
    output.state before.all =
        { swap$ pop$ }
        { * }
      if$
    }
  if$
}

FUNCTION {format.ed.booktitle}
{ booktitle empty$
    { "" }
    { editor empty$
        { booktitle }
        { booktitle ", " * format.editors.as.authors * }
      if$
    }
  if$
}

FUNCTION {empty.misc.check}
{ author empty$ title empty$ howpublished empty$
  month empty$ year empty$ www empty$ note empty$
  and and and and and and
    { "all relevant fields are empty in " cite$ * warning$ }
    'skip$
  if$
}

FUNCTION {format.thesis.type}
{ thesistype empty$
    { type empty$
        'skip$
        { pop$ type capitalize.first }
      if$
    }
    { pop$ thesistype capitalize.first }
  if$
}

FUNCTION {format.tr.number}
{ type empty$
    { "Technical Report" }
    { type "t" change.case$ }
  if$
  report empty$
      { number empty$
          'skip$
          { number tie.or.space.connect }
        if$
      }
      { report tie.or.space.connect
        "report and number" number either.or.check
      }
    if$
}

FUNCTION {format.www}
{  
   www empty$
    { "" }
    { "\urlprefix\url{"  www * "}" * }
  if$
}

FUNCTION {format.replacedby}
{ replacedby empty$
    { "" }
    { after.sen.newline 'output.state :=
      "\newline "
      "Reference " * cite$ boldify * " is outdated. Use " *
      replacedby boldify * " instead" * 
    }
  if$
}

FUNCTION {format.report.institution.noeprint}
{ report empty$
    { eprint empty$
        { number empty$
            { "" }
            { " " number mboxify * }
          if$
        }
        { "" }
      if$
    }
    { " " report mboxify *
      eprint empty$
        'skip$
        { "" }
      if$
      "report and number" number either.or.check
    }
  if$
  duplicate$ "" =
      'skip$
      { institution empty$
	  'skip$
          { ", " * institution * }
	if$
      }   
    if$
  duplicate$ "" =
      { swap$ pop$ }
      { swap$
        type empty$
          { "Preprint" swap$ change.case$ }
          { type swap$ change.case$ }
        if$
        swap$ *
      }
    if$
}

FUNCTION {format.report.institution.eprint}
{ report empty$
    { eprint empty$
        { number empty$
            { "" }
            { " " number mboxify * }
          if$
        }
        { " " eprint mboxify * }
      if$
    }
    { " " report mboxify *
      eprint empty$
        'skip$
        { " (" * eprint mboxify * ")" * }
      if$
      "report and number" number either.or.check
    }
  if$
  duplicate$ "" =
      'skip$
      { institution empty$
	  'skip$
          { ", " * institution * }
	if$
      }   
    if$
  duplicate$ "" =
      { swap$ pop$ }
      { swap$
        type empty$
          { "Preprint" swap$ change.case$ }
          { type swap$ change.case$ }
        if$
        swap$ *
      }
    if$
}

FUNCTION {format.report.institution.eprint.thesis}
{ report empty$
    { eprint empty$
        { number empty$
            { "" }
            { " " number mboxify * }
          if$
        }
        { " " eprint mboxify * }
      if$
    }
    { " " report mboxify *
      eprint empty$
        'skip$
        { " (" * eprint mboxify * ")" * }
      if$
      "report and number" number either.or.check
    }
  if$
  duplicate$ "" =
      'skip$
      { institution empty$
	  'skip$
          { ", " * institution * }
	if$
      }   
    if$
  duplicate$ "" =
      { swap$ pop$ }
      { swap$
        " " swap$ change.case$
        swap$ *
      }
    if$
}

FUNCTION {format.note}
{ note empty$
    { "" }
    { note capitalize.first }
  if$
}

FUNCTION {format.report}
{ report empty$
    { "" }
    { type empty$
          { "Report " report mboxify * }
          { type " " * report mboxify * }
        if$
    }
  if$
}

FUNCTION {format.article.crossref}
{ key empty$
    { journal empty$
	{ "need key or journal for " cite$ * " to crossref " * crossref *
	  warning$
	  ""
	}
        { "In {\em " journal * "\/}" * }
      if$
    }
    { "In " key * }
  if$
  " \cite{" * crossref * "}" *
}

FUNCTION {format.crossref.editor}
{ editor #1 "{vv~}{ll}" format.name$
  editor num.names$ duplicate$
  #2 >
    { pop$ " et al." * }
    { #2 <
	'skip$
	{ editor #2 "{ff }{vv }{ll}{ jj}" format.name$ "others" =
            { " et al." * }
            { " and~" * editor #2 "{vv~}{ll}" format.name$ * }
	  if$
	}
      if$
    }
  if$
}

FUNCTION {format.book.crossref}
{ volume empty$
    { "empty volume in " cite$ * "'s crossref of " * crossref * warning$
      "In "
    }
    { "Vol." volume tie.or.space.connect
      " in series " *
    }
  if$
  editor empty$
  editor field.or.null author field.or.null =
  or
    { key empty$
	{ series empty$
	    { "need editor, key, or series for " cite$ * " to crossref " *
	      crossref * warning$
	      "" *
	    }
	    { "{\em " * series * "\/}" * }
	  if$
	}
	{ key * }
      if$
    }
    { format.crossref.editor * }
  if$
  " \cite{" * crossref * "}" *
}

FUNCTION {format.incoll.inproc.crossref}
{ editor empty$
  editor field.or.null author field.or.null =
  or
    { key empty$
	{ booktitle empty$
	    { "need editor, key, or booktitle for " cite$ * " to crossref " *
	      crossref * warning$
	      ""
	    }
            { "In " booktitle * }
	  if$
	}
        { "In " key * }
      if$
    }
    { "In " format.crossref.editor * }
  if$
  " \cite{" * crossref * "}" *
}

FUNCTION {article.doi}
{
% output.bibitem
  format.collaboration "collaboration" output.check
  format.authors "author" output.check  
  new.line
  format.title "title" output.check
  new.block
  crossref missing$
    { journal empty$
          { "journal empty in " cite$ *  warning$ } 
          {  "\href{http://dx.doi.org/"  doi * "}{" *  journal  * "{}" * output }
        if$
      continue.output
      format.vol.num.date.pages * "}" output

      new.block
%      format.doi output
      new.block
      format.eprint output
      format.url output
      format.www output
    }
    { format.article.crossref output.nonnull
      format.first.page output
    }
  if$   
  new.block
  format.note output
  format.replacedby output
%  fin.entry
}

FUNCTION {article.nodoi}
{% output.bibitem
  format.collaboration "collaboration" output.check
  format.authors "author" output.check
  new.line
  format.title "title" output.check
  new.block
  crossref missing$
    { journal empty$
          { "journal empty in " cite$ * warning$ }
          { journal "{}" * output }
        if$
      continue.output
      format.vol.num.date.pages output
      new.block
%      format.doi output
      new.block
      format.eprint output
      format.url output
      format.www output
    }
    { format.article.crossref output.nonnull
      format.first.page output
    }
  if$ 
 new.block
  format.note output
  format.replacedby output
%  fin.entry
}

FUNCTION {article}
{
output.bibitem 
doi   empty$
{article.nodoi }
{article.doi   } 
if$
fin.entry 
}



FUNCTION {book}
{ output.bibitem
  author empty$
    { format.editors.as.authors "author and editor" output.check }
    { format.authors output.nonnull
      crossref missing$
	{ "author and editor" editor either.or.check }
	'skip$
      if$
    }
  if$
  new.line
  format.btitle "title" output.check
  new.line
  crossref missing$
    { format.volume.number.series output
      format.edition output
      new.block
      publisher "publisher" output.check
      address output
    }
    { new.block
      format.book.crossref output.nonnull
      format.edition output
    }
  if$
  format.date "year" output.check
  new.block
  format.doi output
  new.block
  format.eprint output
  new.block
  format.url output
  format.www output
  new.block
  format.note output
  format.replacedby output
  fin.entry
}

FUNCTION {booklet}
{ output.bibitem
  author empty$
    { format.editors.as.authors output }
    { format.authors output }
  if$
  new.line
  format.btitle "title" output.check
  howpublished address new.block.checkb
  howpublished output
  address output
  year empty$
    'skip$
    { continue.output
      " (" year * ")" * output
    }
  if$
  new.line
  format.doi output
  new.block
  format.eprint output
  format.url output
  format.www output
  new.block
  format.note output
  format.replacedby output
  fin.entry
}

FUNCTION {inbook}
{ output.bibitem
  author empty$
    { format.editors.as.authors "author and editor" output.check }
    { format.authors output.nonnull
      crossref missing$
        { "author and editor" editor either.or.check }
        'skip$
      if$
    }
  if$
  new.line
  format.btitle "title" output.check
  crossref missing$
    { series new.line.checka
      format.volume.number.series output
      format.edition output
      format.chapter.pages "chapter and pages" output.check
      new.block
      publisher "publisher" output.check
      address output
    }
    { format.chapter.pages "chapter and pages" output.check
      new.block
      format.book.crossref output.nonnull
      format.edition output
    }
  if$
  format.date "year" output.check
  new.block
  format.doi output
  new.block
  format.eprint output
  format.url output
  format.www output
  new.block
  format.note output
  format.replacedby output
  fin.entry
}

FUNCTION {incollection}
{ output.bibitem
  format.authors "author" output.check
  new.line
  format.title "title" output.check
  new.line
  crossref missing$
    { format.in.ed.booktitle "booktitle" output.check
      series new.block.checka
      format.volume.number.series output
      format.edition output
      format.chapter.pages output
      new.block
      publisher "publisher" output.check
      address output
      format.date "year" output.check
    }
    { format.incoll.inproc.crossref output.nonnull
      format.chapter.pages output
    }
  if$
  new.line
  format.doi output
  new.block
  format.note output
  format.replacedby output
  fin.entry
}

FUNCTION {inproceedings}
{ output.bibitem
  format.authors "author" output.check
  new.line
  format.title "title" output.check
  new.line
  crossref missing$
    { format.ed.booktitle "booktitle" output.check
      format.volume.number.series output
      format.pages output
      new.block
      publisher output
      address output
      organization output
      continue.output
      " (" year * ")" * "year" output.check
      "l" format.report.institution.eprint
      duplicate$ empty$
          { pop$ skip$ }
          { new.block
            "Also in " swap$ * output
          }
        if$
    }
    { format.incoll.inproc.crossref output.nonnull
      format.pages output
    }
  if$
  new.line
  format.doi output
  new.block
  format.eprint output
  format.url output
  format.www output
  new.block
  format.note output
  format.replacedby output
  fin.entry
}

FUNCTION {conference} { inproceedings }

FUNCTION {manual}
{ output.bibitem
  author empty$
    { organization empty$
	{ "empty author and organization in " cite$ * warning$ }
	{ organization output.nonnull
	  address output
	}
      if$
    }
    { format.authors output.nonnull }
  if$
  new.line
  format.btitle "title" output.check
  author empty$
    { organization empty$
	{ address new.block.checka
	  address output
	}
	'skip$
      if$
    }
    { organization address new.block.checkb
      organization output
      address output
    }
  if$
  format.edition output
  format.date output
  new.line
  format.doi output
  new.block
  format.eprint output
  format.url output
  format.www output
  new.block
  format.note output
  format.replacedby output
  fin.entry
}

FUNCTION {misc}
{ output.bibitem
  format.authors output
  title howpublished new.line.checkb
  format.btitle output
  howpublished new.block.checka
  howpublished output
  format.date output
  new.line
  format.url output
  format.www output
  new.block
  format.note output
  format.replacedby output
  fin.entry
  empty.misc.check
}

FUNCTION {preprint}
{ output.bibitem
  format.authors "author" output.check
  new.line
  format.title "title" output.check
  new.block
  "t" format.report.institution.eprint "report and eprint" output.check
  format.date "year" output.check
  new.line
  format.eprint output
  format.url output
  format.www output
  new.block
  format.note output
  format.replacedby output
  fin.entry
}

FUNCTION {private}
{ output.bibitem
  format.authors "author" output.check
  type empty$
      { "private communication" output }
      { type output }
    if$
  format.date output
  new.block
  format.note output
  format.replacedby output
  fin.entry
}

FUNCTION {proceedings}
{ output.bibitem
  format.btitle "title" output.check
  series new.line.checka
  format.volume.number.series output
  format.editors output
  report empty$
      { publisher empty$
          { organization new.block.checka
            organization output
            address output
            format.date "year" output.check
          }
          { organization output
            new.block
            publisher output 
            address output
            format.date "year" output.check
          }
        if$
      }
      { publisher empty$
          { organization new.block.checka
            organization output
            address output
            format.report output
            format.date "year" output.check
          }
          { new.block
            publisher output 
            address output
            format.date "year" output.check
            organization new.block.checka
            organization output
            format.report output
          }
        if$
      }
    if$
  new.line
  format.doi output
  new.block
  format.eprint output
  format.url output
  format.www output
  new.block
  format.note output
  format.replacedby output
  fin.entry
}

FUNCTION {techreport}
{ output.bibitem
  format.authors "author" output.check
  new.line
  format.btitle "title" output.check
  new.line
  format.tr.number output.nonnull
  organization empty$
      { institution "organization and institution" output.check }
      { "organization and institution" institution either.or.check
        organization output.nonnull 
      }
    if$
  address output
  format.date "year" output.check
  new.line
  format.doi output
  new.block
  format.eprint output
  format.url output
  format.www output
  new.block
  format.note output
  format.replacedby output
  fin.entry
}

FUNCTION {thesis}
{ output.bibitem
  format.authors "author" output.check
  new.line
  format.btitle "title" output.check
  new.line
  "Thesis" format.thesis.type output.nonnull
  school "school" output.check
  address output
  "t" format.report.institution.eprint.thesis output
  format.date "year" output.check
  new.line
  format.doi output
  new.block
  format.eprint output
  format.url output
  format.www output
  new.block
  format.note output
  format.replacedby output
  fin.entry
}

FUNCTION {diplomathesis}
{ output.bibitem
  format.authors "author" output.check
  new.line
  format.btitle "title" output.check
  new.line
  bbl.dthesis format.thesis.type output.nonnull
  school "school" output.check
  address output
  "t" format.report.institution.eprint.thesis output
  format.date "year" output.check
  new.line
  format.doi output
  new.block
  format.eprint output
  format.url output
  format.www output
  new.block
  format.note output
  format.replacedby output
  fin.entry
}

FUNCTION {mastersthesis}
{ output.bibitem
  format.authors "author" output.check
  new.line
  format.btitle "title" output.check
  new.line
  bbl.mthesis format.thesis.type output.nonnull
  school "school" output.check
  address output
  "t" format.report.institution.eprint.thesis output
  format.date "year" output.check
  new.line
  format.doi output
  new.block
  format.eprint output
  format.url output
  format.www output
  new.block
  format.note output
  format.replacedby output
  fin.entry
}

FUNCTION {phdthesis}
{ output.bibitem
  format.authors "author" output.check
  new.line
  format.btitle "title" output.check
  new.line
  bbl.phdthesis format.thesis.type output.nonnull
  school "school" output.check
  address output
  "t" format.report.institution.eprint.thesis output
  format.date "year" output.check
  new.line
  format.doi output
  new.block
  format.eprint output
  format.url output
  format.www output
  new.block
  format.note output
  format.replacedby output
  fin.entry
}

FUNCTION {unpublished}
{ output.bibitem
  format.authors "author" output.check
  new.line
  title empty$
      { "title empty in " cite$ * warning$ }
      { format.btitle " (unpublished)" * output }
    if$
  www empty$
      { new.block
        format.note "note" output.check
        format.date output
      }
      { format.date output
        new.line
        format.eprint output
        format.url output
        format.www output
        new.block
        format.note output
      }
    if$
  format.replacedby output
  fin.entry
}

FUNCTION {default.type} { misc }

MACRO {jan} {"Jan.\"}
MACRO {feb} {"Feb.\"}
MACRO {mar} {"Mar.\"}
MACRO {apr} {"Apr.\"}
MACRO {may} {"May"}
MACRO {jun} {"June"}
MACRO {jul} {"July"}
MACRO {aug} {"Aug.\"}
MACRO {sep} {"Sept.\"}
MACRO {oct} {"Oct.\"}
MACRO {nov} {"Nov.\"}
MACRO {dec} {"Dec.\"}

MACRO {AcP}     {"Acta Phys."}
MACRO {AcPA}    {"Acta Phys.\ Austriaca"}
MACRO {AcPP}    {"Acta Phys.\ Pol."}
MACRO {AdP}     {"Adv.\ Phys."}
MACRO {AdNP}    {"Adv.\ Nucl.\ Phys."}
MACRO {AmJP}    {"Am.\ J.\ Phys."}
MACRO {AnPDu}   {"Ann.\ Phys.\ (Leipzig)"}
MACRO {AnPFr}   {"Ann.\ Phys.\ (Paris)"}
MACRO {AnPNY}   {"Ann.\ Phys.\ (NY)"}
MACRO {ARevNS}  {"Ann.\ Rev.\ Nucl.\ Part.\ Sci."}
MACRO {ADNDT}   {"At.\ Data Nucl.\ Data Tables"}
MACRO {AusJP}   {"Austr.\ J.\ Phys."}
MACRO {CanJP}   {"Can.\ J.\ Phys."}
MACRO {CPC}     {"Comp.\ Phys.\ Comm."}
MACRO {EJP}     {"Eur.\ Jo.\ Phys."}
MACRO {EPJ}     {"Eur.\ Phys.\ J."}
MACRO {EPL}     {"Europhys.\ Lett."}
MACRO {FBS}     {"Few Body Sys."}
MACRO {FBSSup}  {"Few Body Sys.\ Suppl."}
MACRO {FortP}   {"Fortschr.\ Phys."}
MACRO {HePA}    {"Helv.\ Phys.\ Acta"}
MACRO {IEEE}    {"IEEE Trans."}
MACRO {IJMP}    {"Int.\ J.\ Mod.\ Phys."}
MACRO {JETP}    {"Sov.\ Phys.\ JETP"}
MACRO {JETPL}   {"JETP Lett."}
MACRO {JaFi}    {"Jad.\ Fiz."}
MACRO {JHEP}    {"JHEP"}
MACRO {JMP}     {"J.\ Math.\ Phys."}
MACRO {JPFr}    {"J.\ Phys.\ (Paris)"}
MACRO {JP}      {"J.\ Phys."}
MACRO {JPSJ}    {"J.\ Phys.\ Soc.\ Jap."}
MACRO {JPSJSup} {"J.\ Phys.\ Soc.\ Jap.\ Suppl."}
MACRO {MPL}     {"Mod.\ Phys.\ Lett."}
MACRO {NCim}    {"Nuovo Cimento"}
MACRO {NDS}     {"Nucl.\ Data Sh."}
MACRO {NIM}     {"Nucl.\ Inst.\ Meth."}
MACRO {NP}      {"Nucl.\ Phys."}
MACRO {NPPS}    {"Nucl.\ Phys.\ Proc.\ Suppl."}
MACRO {PL}      {"Phys.\ Lett."}
MACRO {PRSLon}  {"Proc.\ Roy.\ Soc.\ Lond."}
MACRO {PR}      {"Phys.\ Rev."}
MACRO {PRL}     {"Phys.\ Rev.\ Lett."}
MACRO {PRep}    {"Phys.\ Rep."}
MACRO {PScr}    {"Phys.\ Scr."}
MACRO {PToday}  {"Phys.\ Today"}
MACRO {PTP}     {"Prog.\ Theor.\ Phys."}
MACRO {RMP}     {"Rev.\ Mod.\ Phys."}
MACRO {RPP}     {"Rep.\ Prog.\ Phys."}
MACRO {ScAm}    {"Sci.\ Am."}
MACRO {SSin}    {"Sci.\ Sin."}
MACRO {SovJNP}  {"Sov.\ J.\ Nucl.\ Phys."}
MACRO {SovJPN}  {"Sov.\ J.\ Part.\ Nucl."}
MACRO {SovPDo}  {"Sov.\ Phys.\ Dokl."}
MACRO {SovPUs}  {"Sov.\ Phys.\ Usp."}
MACRO {STS}     {"Bull.\ Sc.\ Tech.\ Soc.\ Aff."}
MACRO {YF}      {"Yad.\ Fiz."}
MACRO {ZfN}     {"Z.\ Naturforsch."}
MACRO {ZfP}     {"Z.\ Phys."}
	        

READ

STRINGS { longest.label }

INTEGERS { number.label longest.label.width }

FUNCTION {initialize.longest.label}
{ "" 'longest.label :=
  #1 'number.label :=
  #0 'longest.label.width :=
}

FUNCTION {longest.label.pass}
{ number.label int.to.str$ 'label :=
  number.label #1 + 'number.label :=
  label width$ longest.label.width >
    { label 'longest.label :=
      label width$ 'longest.label.width :=
    }
    'skip$
  if$
}

EXECUTE {initialize.longest.label}

ITERATE {longest.label.pass}

FUNCTION {begin.bib}
{ preamble$ empty$
    'skip$
    { preamble$ write$ newline$ }
  if$
  "\providecommand{\urlprefix}{}" write$ newline$
  "\providecommand{\etal}{et al.\xspace}" write$ newline$
  "\providecommand{\coll}{Coll.\xspace}" write$ newline$
  "\catcode`\@=11" write$ newline$
  "\def\@bibitem#1{%" write$ newline$
  "\ifmc@bstsupport" write$ newline$
  "  \mc@iftail{#1}%" write$ newline$
  "    {;\newline\ignorespaces}%" write$ newline$
  "    {\ifmc@first\else.\fi\orig@bibitem{#1}}" write$ newline$
  "  \mc@firstfalse" write$ newline$
  "\else" write$ newline$
  "  \mc@iftail{#1}%" write$ newline$
  "    {\ignorespaces}%" write$ newline$
  "    {\orig@bibitem{#1}}%" write$ newline$
  "\fi}%" write$ newline$
  "\catcode`\@=12" write$ newline$
  "\begin{mcbibliography}{"  longest.label  * "}" * write$ newline$
}

EXECUTE {hello}

EXECUTE {begin.bib}

EXECUTE {init.state.consts}

ITERATE {call.type$}

FUNCTION {end.bib}
{ newline$
  "\end{mcbibliography}" write$ newline$
}

EXECUTE {end.bib}
