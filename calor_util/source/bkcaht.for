      SUBROUTINE BKCAHT(LCAHT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Book and fill CAHT (cal. hits header) bank
C-      NOTE: a new bank is created everytime it is called.
C-            It is up to the caller to decide whether he wants a
C-            new bank or not.
C-
C-   Created  17-JAN-1989   Serban D. Protopopescu
C-   Updated   6-MAY-1989   A.M.Jonckheere  create BKCAHT from CAHTFL
C-   Updated   2-OCT-1995   Dhiman Chakraborty   
C-                          Store CRYO-correction factors applied in CAEH
C-                          => increase NDATA  from 1 to 4. (Version = 2)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZCAHT.LINK/LIST'
      LOGICAL FIRST
      INTEGER LCAHT,IOCAHT,LHITS,GZHITS,GZHSTR
      INTEGER NDATA,NLNKS
      PARAMETER (NDATA=4)
      PARAMETER (NLNKS=4)
C
      DATA FIRST/.TRUE./
C--------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL MZFORM('CAHT','1I 3F',IOCAHT)   ! format for CAHT
        FIRST = .FALSE.
      ENDIF
C
      LHITS = GZHITS()
      IF ( LHITS.LE.0 ) CALL BKHITS(LHITS)
      LCAHT = LQ(LHITS-IZCAHT)
C
C   Create CAHT bank
C
      CALL MZBOOK(IXMAIN,LCAHT,LHITS,-IZCAHT,
     +            'CAHT',NLNKS,NLNKS-1,NDATA,IOCAHT,-1)
C
      IQ(LCAHT+1) = 2             ! version number
      LQ(LCAHT-NLNKS) = GZHSTR()  ! ref. link to latest history bank
C
  999 RETURN
      END
