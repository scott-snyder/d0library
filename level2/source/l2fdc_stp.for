      SUBROUTINE L2FDC_STP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Move downloaded level 2 STP banks under the
C-                         level 2 header 
C-
C-   Created  01-NOV-1991   D Claes - From an editted copy of 
C-                          D0$CALOR_FILTER$SOURCE:CL2_SHUNT_STP.FOR
C-                          ( As L2TRAK_STP )
C-            28-APR-1992   YI-CHENG LIU   , modify for FDC
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LAYER, L, L2FDC, LSL2H
      INTEGER LFALH,LFMAT, LFDRT, LFTCH
      INTEGER IZ2SFDC          ! Link of FDC under SL2H
      PARAMETER ( IZ2SFDC = 13 )
      INTEGER GZSFDC, GZSL2H,GZFTCH
      INTEGER GZFPDH, GZFGNH, GZFALH, GZFMAT, GZFDRT, GZFTMH
C----------------------------------------------------------------------
C
C...prepare for downloading: copy SFDC from under SFDC to under SL2H
C...drop all banks save FTMH (FTSE,FTPH,FTQU-for drift and T-zero constants) 
C...and FGEH,FALH - for the FDC geometry.
C
C...assumes SFDC is available
C...Check that SL2H bank already exits, else book it
        LSL2H = GZSL2H()
        IF (LSL2H.LE.0) CALL BKSL2H(LSL2H)
        LSFDC = GZSFDC()
        IF (LSFDC.LE.0.) THEN
          CALL ERRMSG('FDC','L2FDC_STP',          
     &          'SFDC banks not found','F')     ! SEE WHERE L2FDC_STP IS
        ELSE
          LFPDH = GZFPDH()
          CALL MZDROP(IXSTP,LFPDH,'L')    ! Drop pedestal banks
          LFGNH = GZFGNH()
          CALL MZDROP(IXSTP,LFGNH,'L')    ! Drop gain banks
          LFTCH = GZFTCH()
          CALL MZDROP(IXSTP,LFTCH,'L')    ! Drop Electronics time header
                                          ! bank
          LFMAT = GZFMAT()                
          CALL MZDROP(IXSTP,LFMAT,'L')    ! Drop material description bank
          LFDRT = GZFDRT()
          CALL MZDROP(IXSTP,LFDRT,'L')    ! Drop drift volume banks
          L2FDC = LC(LSL2H-IZ2SFDC)        ! see if already there
          IF (L2FDC.GT.0) CALL MZDROP(IXSTP,L2FDC,'L')    ! drop old data
          CALL MZCOPY(IDVSTP,LSFDC,IDVSTP,LSL2H,-IZ2SFDC,'L')
        ENDIF
  999 RETURN
      END
