      SUBROUTINE DUMMIES
C
C    Purpose:
CD   The purpose of this module is a place-holder for all routines that
CD   are not to be implemented in the Emulator.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 5-Dec-1988
CH   History:
CH      04-SEP-90  SA   Removed JKEYBS.
CH      28-FEB-89  SA   Removed some finished escape function related entries.
CH      28-FEB-89  SA   Removed JBGBAT.
CH      28-FEB-89  SA   Removed JENBAT.
CH      08-FEB-89  SA   Added escape function related entries.
CH      08-FEB-89  SA   Removed JESCAP
CH      05-DEC-88  ATV  Defined local varibles.
CH      15-NOV-88  ATV  Remove JPICK
CH      09-NOV-88  ATV  Remove a bunch.
CH      13-OCT-88  ATV  Remove JDVISB, JVISBL
CH      10-OCT-88  ATV  Remove JISGMT, JWCLIP
CH      04-OCT-88  ATV  Remove JTTYPE, JCLEAR, JPURGE.
CH      30-AUG-88  ATV  Remove marker routine entry points.
CH      29-AUG-88  ATV  Add the remaining DI3000, MF, XPM, and ADDSYS 
CH                      entries.
CH      22-AUG-88  ATV  Remove JLSTYL
CH      17-AUG-88  ATV  Add JDEVWN, JDEVVP, JVSPAC
CH      15-AUG-88  ATV  Take out some routines.
CH      02-AUG-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      None.
C
C    Calls:
CC      None.
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER IVAL
      REAL R1, R2, R3, R4
C
C    Then common block declarations.
C
C
C    Then executable code follows
C
      ENTRY JLWIDE(IVAL)
      ENTRY JDLWID(IVAL)
      ENTRY JPEN(IVAL)
      ENTRY JDPEN(IVAL)
C      ENTRY JBGBAT(IVAL)
C      ENTRY JENBAT
C      ENTRY JWCLIP(LVAL)
      ENTRY JSETDB(IVAL)
      ENTRY JDEVWN(IVAL,R1,R2,R3,R4)
      ENTRY JDEVVP(IVAL,R1,R2,R3,R4)
      ENTRY JVSPAC(R1,R2,R3,R4)
C
C   The following are added and removed as necessary.
C
C      ENTRY J16GET 
C      ENTRY J1IGET 
      ENTRY J1TEXT 
C      ENTRY J2STRG 
      ENTRY J2TEXT 
C      ENTRY J3MARK 
C      ENTRY J3PLGN 
C      ENTRY J3RGET 
C      ENTRY J3STRG
      ENTRY J3TEXT 
C      ENTRY J4RGET 
      ENTRY JADS 
C      ENTRY JALOAD 
      ENTRY JARAST 
C      ENTRY JARSET 
      ENTRY JARST 
C      ENTRY JASAVE 
      ENTRY JATTRB 
      ENTRY JBACKG 
      ENTRY JBEAM 
      ENTRY JBSCN 
C      ENTRY JBUILD 
      ENTRY JBUTTN 
      ENTRY JCEFLG 
      ENTRY JCESUB 
      ENTRY JCESUP 
c      ENTRY JCLEAR 
      ENTRY JCLST 
c      ENTRY JCMARK 
      ENTRY JCNMRF 
      ENTRY JCNSHP 
      ENTRY JCONCT 
      ENTRY JCONPK 
C      ENTRY JCONVW 
C      ENTRY JCONWV 
      ENTRY JCOTBL 
      ENTRY JCP 
      ENTRY JCRLF 
      ENTRY JCST 
      ENTRY JCSTNM 
      ENTRY JCSTRF 
C      ENTRY JCVIEW 
      ENTRY JCYLIN 
      ENTRY JDAST 
      ENTRY JDATTR 
      ENTRY JDDETE 
      ENTRY JDEFLG 
      ENTRY JDEFNT 
      ENTRY JDEL 
      ENTRY JDELLB 
      ENTRY JDELNT 
      ENTRY JDELRA 
      ENTRY JDESUB 
      ENTRY JDESUP 
C      ENTRY JDETEC 
      ENTRY JDFATT 
      ENTRY JDHILI 
C      ENTRY JDMARK 
C      ENTRY JDPEDG 
C      ENTRY JDPIDX 
C      ENTRY JDPINT 
      ENTRY JDPKID 
      ENTRY JDSGPR 
      ENTRY JDST 
C      ENTRY JDVISB 
      ENTRY JELARC 
      ENTRY JELLIP 
      ENTRY JELSEG 
      ENTRY JEMST 
      ENTRY JEPSEG 
      ENTRY JESC 
C      ENTRY JESCAP 
      ENTRY JESCN 
      ENTRY JEVSEG 
      ENTRY JEXST 
      ENTRY JEXTNT 
      ENTRY JF2PLN 
      ENTRY JFA2 
      ENTRY JFA3 
      ENTRY JFAS2 
      ENTRY JFAS3 
      ENTRY JFATTR 
      ENTRY JFILES 
      ENTRY JFSOPN 
      ENTRY JFSXTN 
      ENTRY JFTEXT 
      ENTRY JFTOPN 
      ENTRY JFXTNT 
C      ENTRY JHCLIP 
      ENTRY JHILIT 
C      ENTRY JHITHR 
      ENTRY JHSXTN 
      ENTRY JHTEXT 
      ENTRY JHXTNT 
      ENTRY JIDISA 
      ENTRY JIEL 
C      ENTRY JIENAB 
      ENTRY JIESCP 
      ENTRY JIESEL 
      ENTRY JINAP 
      ENTRY JINESC 
      ENTRY JINLB 
      ENTRY JIQ2AL 
      ENTRY JIQ2TR 
      ENTRY JIQDDL 
C      ENTRY JIQDEV 
C      ENTRY JIQDIL 
C     ENTRY JIQDIM 
      ENTRY JIQERR 
      ENTRY JIQFMT 
      ENTRY JIQNSH 
      ENTRY JIQSHL 
C      ENTRY JIQTXT 
C      ENTRY JISGMT 
C      ENTRY JIWIND 
      ENTRY JKEYBD 
C      ENTRY JKEYBS 
C      ENTRY JLOCAT 
      ENTRY JMARGN 
C      ENTRY JMARK 
      ENTRY JMFDAT 
      ENTRY JMFFMT 
      ENTRY JMGET 
      ENTRY JMINIT 
      ENTRY JMINTR 
C      ENTRY JMODEL 
C      ENTRY JMODON 
      ENTRY JMREAD 
      ENTRY JMRECT 
      ENTRY JMSTRG 
      ENTRY JMTERM 
      ENTRY JMTEXT 
C      ENTRY JNORML 
      ENTRY JOPST 
      ENTRY JOSEP 
C      ENTRY JP3MRK 
C      ENTRY JPARAL 
C      ENTRY JPAROB 
      ENTRY JPECHO 
C      ENTRY JPEDGE 
C      ENTRY JPEROB 
C      ENTRY JPERSP 
C      ENTRY JPFSIM 
C      ENTRY JPICK 
C      ENTRY JPIDEX 
C      ENTRY JPINTR 
      ENTRY JPIPED 
      ENTRY JPKAPR 
      ENTRY JPKID 
      ENTRY JPL2 
      ENTRY JPL3 
      ENTRY JPM2 
      ENTRY JPM3 
C      ENTRY JPMARK 
C      ENTRY JPOLGN 
C      ENTRY JPR3MR 
C      ENTRY JPRMRK 
C      ENTRY JPURGE 
      ENTRY JQCATR 
      ENTRY JQCM 
      ENTRY JQCNRS 
      ENTRY JQDM 
      ENTRY JQEL 
      ENTRY JQELCT 
      ENTRY JQEP 
      ENTRY JQIDNM 
      ENTRY JQNMID 
      ENTRY JQNMST 
      ENTRY JQNMT 
      ENTRY JQNTEX 
      ENTRY JQOPST 
      ENTRY JQSTB 
      ENTRY JQSTEX 
      ENTRY JQSTNM 
      ENTRY JQSTPA 
      ENTRY JQT 
      ENTRY JQTNM 
C      ENTRY JR3MRK 
      ENTRY JR3PGN 
      ENTRY JRAST 
      ENTRY JRBFP 
C      ENTRY JRENAM 
      ENTRY JRES 
C      ENTRY JRESET 
      ENTRY JREST 
C      ENTRY JRIGHT 
C      ENTRY JRMARK 
      ENTRY JRNMID 
      ENTRY JRPLGN 
      ENTRY JRQSHP 
      ENTRY JRRECT 
      ENTRY JRSDEF 
      ENTRY JSAALL 
      ENTRY JSAPND 
      ENTRY JSASSO 
      ENTRY JSCALL 
      ENTRY JSCEFL 
      ENTRY JSCHH 
      ENTRY JSCHSP 
      ENTRY JSCHUP 
      ENTRY JSCHXP 
      ENTRY JSCM 
      ENTRY JSCNRS 
      ENTRY JSCOPY 
      ENTRY JSDASS 
      ENTRY JSDEFL 
      ENTRY JSDM 
      ENTRY JSDSFT 
      ENTRY JSEDCI 
      ENTRY JSEDFL 
      ENTRY JSEP 
      ENTRY JSEPLB 
      ENTRY JSETER 
C      ENTRY JSGPRI 
      ENTRY JSGT3 
      ENTRY JSHCLP 
      ENTRY JSHLDV 
      ENTRY JSHLDW 
      ENTRY JSHPKA 
      ENTRY JSHPRG 
      ENTRY JSICI 
      ENTRY JSIS 
      ENTRY JSISI 
      ENTRY JSLN 
      ENTRY JSLT3 
      ENTRY JSLWSC 
      ENTRY JSMK 
      ENTRY JSNGT3 
      ENTRY JSNLT3 
      ENTRY JSNMID 
      ENTRY JSPALL 
      ENTRY JSPHER 
      ENTRY JSPICK 
      ENTRY JSPKFT 
      ENTRY JSPKID 
      ENTRY JSPLCI 
      ENTRY JSPMCI 
      ENTRY JSREST 
      ENTRY JSREV 
      ENTRY JSSAVE 
      ENTRY JSTROK 
      ENTRY JSTXAL 
      ENTRY JSTXCI 
      ENTRY JSTXFN 
      ENTRY JSTXP 
      ENTRY JSTXPR 
      ENTRY JSTXTN 
c      ENTRY JSVIEW 
      ENTRY JSYSTM 
C      ENTRY JT2ALL 
      ENTRY JT2TRA 
C      ENTRY JTRANS 
      ENTRY JTRVST 
c      ENTRY JTTYPE 
      ENTRY JTX2 
      ENTRY JTX3 
C      ENTRY JUPVEC 
      ENTRY JVALUE 
C      ENTRY JVISBL 
C      ENTRY JVLOAD 
C      ENTRY JVSALL 
C      ENTRY JVSAVE 
      ENTRY JVUPLN 
C      ENTRY JVUPNT 
      ENTRY JXHRSS 
      ENTRY JXTFAS 
      ENTRY JXTPL 
C      ENTRY JYCLIP 
C      ENTRY JYON 
C
      RETURN
      END
