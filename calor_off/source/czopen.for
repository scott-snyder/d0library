      SUBROUTINE CZOPEN (FILNAM,INUNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Declare data input file to ZEBRA and return
C-                         unit number.
C-
C-   Inputs  : FILNAM           Name of input file
C-   Outputs : INUNIT           Input unit number for data file.
C-   Controls: None
C-
C-   Created  23-JAN-1989   Harrison B. Prosper, John Womersley
C-   MODIFIED  7-SEP-1993   Ian Adam
C-    Change option string passed to D0OPEN to XI, add CHOPT variable
C-    and call to XZRECL
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CALID.DEF'
      LOGICAL       OK      
      CHARACTER*(*) FILNAM
      INTEGER       INUNIT,ERR,L
      CHARACTER*80  MSG
      CHARACTER*2   CHOPT
      INTEGER       TRULEN,ILEN
C----------------------------------------------------------------------
C
C ****  Get a free unit number
C
      CALL GTUNIT (CALID,INUNIT,ERR)
C
C ****  Open input file and declare its existence to ZEBRA
C
      L = TRULEN(FILNAM)
      CALL D0OPEN (INUNIT,FILNAM(1:L),'XI',OK)
      IF ( OK ) THEN
        CALL XZRECL(ILEN,CHOPT)
        CALL FZFILE (INUNIT,ILEN,CHOPT)
      ELSE
        MSG = ' %CZOPEN-E-NOTFOUND: Unable to open data file: '
     &    //FILNAM(1:L)
        CALL ERRMSG('CALORIMETER','CZOPEN',
     & MSG(1:TRULEN(MSG))
     & ,'F')
      ENDIF
C
      RETURN
      END
