      INTEGER FUNCTION TRNLNM (LOGIN,LOGOUT,OUTLEN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Translate a logical name. Very VAX-specific
C-
C-   Inputs  : LOGIN:  Logical name to translate
C-   Outputs : LOGOUT: Result of translation
C-             OUTLEN: Length of LOGOUT
C-             TRNLNM: .true. = Success
C-   Controls: None
C-
C-   Created  22-NOV-1986   Jan S. Hoftun
C-   Updated  28-FEB-1990   Harrison B. Prosper
C-      Same as TRALOG but without MSGSCR.
C-   Updated  21-MAR-1990   Harrison B. Prosper
C-      Searches first LNM$PROCESS then LNM$SYSTEM
C-   Updated  28-MAR-1990   Harrison B. Prosper
C-      Increase buffer length
C-   Updated   3-AUG-1990   Harrison B. Prosper
C-      Add variant for NON-VMS machines (no translation done)
C-   Updated  3-May-1993    Herbert Greenlee
C-      Increase maximum length from 80 to 256
C-   Updated 13-May-1993    Herbert Greenlee
C-      Modify non-VMS code to do a readlink
C-   Updated   8-OCT-1993 sss
C-      The return length is a _word_ field!
C-   Updated   6-DEC-1993   R. J. Genik II
C-      Increase buffer length to 255, VMS limit.
C-      Return Integer instead of Character to TRNLNM
C-      Search table LNM$DCL_LOGICAL, which for almost all purposes is
C-      equivalent to LNM$FILE_DEV, the list of available tables. It will
C-      search the process, job, group, and all system tables, in that
C-      order, and return the first match found. Zero is returned in TRNLNM
C-      if no match was found.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) LOGIN,LOGOUT
      INTEGER OUTLEN
      CHARACTER*255  INSTRING,STRING
      INTEGER I,J,L
C&IF VAXVMS
C-
C- Get definition of LNM$_STRING for SYS$TRNLNM call.
C-
      INCLUDE '($LNMDEF)' ! module in FORSYSDEF.TLB
C-
      INTEGER TRULEN,SYS$TRNLNM
      STRUCTURE /ITMLST/
        UNION
          MAP
            INTEGER*2 BUFLEN
            INTEGER*2 CODE
            INTEGER*4 BUFADR
            INTEGER*4 RETLENADR
          ENDMAP
          MAP
            INTEGER*4 END_LIST
          ENDMAP
        ENDUNION
      END STRUCTURE
      RECORD /ITMLST/ LNMLST(2)
      integer*2 retlen
      LNMLST(1).BUFLEN=LEN(STRING)
      LNMLST(1).CODE=LNM$_STRING
      LNMLST(1).BUFADR=%LOC(STRING)
      LNMLST(1).RETLENADR=%LOC(retLEN)
      LNMLST(2).END_LIST=0
C&ELSE
C&      LOGICAL OK, D0_READLINK
C&      INTEGER TRULEN
C&ENDIF
C----------------------------------------------------------------------
C&IF VAXVMS
      L = TRULEN(LOGIN)
      CALL STR$UPCASE(INSTRING(1:L),LOGIN(1:L))
C
C
C ****  This next function doesn't act like a regular fortran function, see
C ****  VAX programmers manual, Vol 4B, pp sys-645 - sys-650 for futher
C ****  info.
C
      TRNLNM=SYS$TRNLNM(,'LNM$DCL_LOGICAL',INSTRING(1:L),,LNMLST)
C
      IF ( TRNLNM ) THEN
        outlen = retlen
        LOGOUT = STRING(1:OUTLEN)
      ELSE
        LOGOUT = LOGIN(1:L)
        OUTLEN = L
      ENDIF
      RETURN
C&ELSE
C&      CALL WORD(LOGIN,I,J,L)
C&      OUTLEN = L
C&      OK = D0_READLINK(LOGIN(I:J),LOGOUT)
C&      IF(OK)THEN
C&        TRNLNM = 1
C&        OUTLEN = TRULEN(LOGOUT)
C&      ELSE
C&        LOGOUT = LOGIN(I:J)
C&        TRNLNM = 0
C&      ENDIF
C&      RETURN
C&ENDIF
      END
