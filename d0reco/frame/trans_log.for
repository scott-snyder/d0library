      INTEGER FUNCTION TRANS_LOG(LOGIN,LOGOUT,OUTLEN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Translate a logical name. Very VAX-specific
C-
C-   Inputs  : LOGIN:  Logical name to translate
C-   Outputs : LOGOUT: Result of translation
C-             OUTLEN: Length of LOGOUT
C-   Controls: None
C-
C-   Created  22-NOV-1986   Jan S. Hoftun
C-   Updated  12-JAN-1990   Serban D. Protopopescu  
C-   Modified original TRALOG for offline reconstruction 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) LOGIN,LOGOUT
      INTEGER OUTLEN
C&IF VAXVMS
      INTEGER TRULEN,SYS$TRNLNM,ISTAT
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
      INTEGER*2 LNM$_STRING           ! CODE FOR NAME
      PARAMETER (LNM$_STRING=2)       ! Found in STARLET.PAS
      LNMLST(1).BUFLEN=64
      LNMLST(1).CODE=LNM$_STRING
      LNMLST(1).BUFADR=%LOC(LOGOUT)
      LNMLST(1).RETLENADR=%LOC(OUTLEN)
      LNMLST(2).END_LIST=0
C----------------------------------------------------------------------
      ISTAT=SYS$TRNLNM(,'LNM$PROCESS',LOGIN(1:TRULEN(LOGIN)),
     *      ,LNMLST)
      IF(.NOT.ISTAT) THEN
        CALL MSGSCR(ISTAT,'TRNLNM-->')
      ENDIF
      TRANS_LOG=ISTAT
C&ELSE
C&      TRANS_LOG=1
C&      OUTLEN=LEN(LOGIN)
C&      LOGOUT=LOGIN(1:OUTLEN)
C&ENDIF
      RETURN
      END
