      INTEGER FUNCTION CHKBRR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check for Begin and End Run Records, if found, write
C-                              them out
C-
C-   Returned value  : <0 = BRR or ERR found, =0 = Data record found
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-SEP-1989   Alan M. Jonckheere
C-   Updated  17-AUG-1992   Robert E. Avery  Fill CD stp flag word of 
C-                              HSTR bank in begin run record. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZD0RGZ.LINK'
      INTEGER ISTATS,LHSTR
C----------------------------------------------------------------------
C
C ****  BRR OR ERR?
      CHKBRR = 0
      IF ( MOD(IQ(LHEAD+1),10).LE.2 ) THEN
C
C ****  COPY D0RG in /GCBANK/JRUNG and dependents to /ZEBCOM/GEAN-HSTR
        CALL PATHST('GEAN')
        CALL BKHSTR(LHSTR)
        IF ( LHSTR.GT.0 ) THEN
          CALL CD_HSTR_FLG(LHSTR)
          CALL CPD0RG(IXMAIN,LHSTR,IZD0RGZ,'L')
        ENDIF
        CALL PATHRS
C
C ****  Write it out
        CALL WRZEB(ISTATS)
        CHKBRR = -1
      ENDIF
  999 RETURN
      END
