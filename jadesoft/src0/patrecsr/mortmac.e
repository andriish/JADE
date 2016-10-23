%N
SPECIFY FORTNAME AS LETTER(0,5)<*ALPHA*>;
SPECIFY CONSTANT AS <*'-'|'+'|''*>(1,8)<*DIGIT*>;
SPECIFY RELOP    AS '.'(2)<*LETTER*>'.';
SPECIFY CONDITION AS <*'IF'|'UNLESS'*>;
REPLACE (*;INC (*FORTNAME*)(*ARB*);*) WITH
   (*;(*P1*)(*P2*)=(*P1*)(*P2*)+1; *)
REPLACE (*;DEC (*FORTNAME*)(*ARB*);*) WITH
   (*;(*P1*)(*P2*)=(*P1*)(*P2*)-1; *)
REPLACE (*;DUMP ((*FORTNAME*)(*RELOP*)(*CONSTANT*))(*ARB*);(*ARB*);*)
WITH (*;INC (*P1*);IF (*P1*)(*P2*)(*P3*) <* OUTPUT (*P4*); (*P5*); *>*)
REPLACE (*ABORTUSE((*FORTNAME*));*) WITH
(* REPLACE (*;ABORT((*WAIT (*CONSTANT*)*))
   (*WAIT (*CONDITION*)*)(*WAIT (*ARB*)*);*) WITH (*;(*WAIT (*P2*)*)
          (*WAIT (*P3*)*) <* (*P1*)=(*WAIT (*P1*)*); RETURN; *>*)*)
" TRANSBACK XNEW;YNEW;XOLD;YOLD;DX;DY;CS;SN; :=
  XNEW = XOLD*CS - YOLD*SN + DX; YNEW = YOLD*CS + XOLD*SN + DY; "
REPLACE (*;TRANSBACK
(*ARB*);(*ARB*);(*ARB*);(*ARB*);(*ARB*);(*ARB*);(*ARB*);(*ARB*);*)
WITH
(*;(*P1*)=(*P3*)*(*P7*)-(*P4*)*(*P8*)<*IF*> (*EXIST 5*)
  <*+(*P5*);*> <*ELSE*> <*;*>
  ;(*P2*)=(*P4*)*(*P7*)+(*P3*)*(*P8*)<*IF*> (*EXIST 6*)
  <*+(*P6*);*> <*ELSE*> <*;*> *)
" TRANSFORM XNEW;YNEW;XOLD;YOLD;DX;DY;CS;SN; :=
  XNEW = (XOLD-DX)*CS+(YOLD-DY)*SN; YNEW = (YOLD-DY)*CS-(XOLD-DX)*SN; "
REPLACE (*;TRANSFORM
(*ARB*);(*ARB*);(*ARB*);(*ARB*);(*ARB*);(*ARB*);(*ARB*);(*ARB*);*)
WITH (*
;(*P1*)=<*IF*> (*EXIST 5*) <*((*P3*)-(*P5*))*>       <*ELSE*> <*(*P3*)*>
*(*P7*)+<*IF*>   (*EXIST 6*) <*((*P4*)-(*P6*))*>     <*ELSE*> <*(*P4*)*>
*(*P8*)
;(*P2*)=<*IF*> (*EXIST 6*) <*((*P4*)-(*P6*))*>       <*ELSE*> <*(*P4*)*>
*(*P7*)-<*IF*>   (*EXIST 5*) <*((*P3*)-(*P5*))*>     <*ELSE*> <*(*P3*)*>
*(*P8*); *)
%L
