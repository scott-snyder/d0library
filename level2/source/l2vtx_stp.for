      SUBROUTINE L2VTX_STP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Drop all but VPDH bank (and dependants) from SVTX
C-               and hang SVTX from SL2H bank
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-APR-1994   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZL2VT.LINK'
c Locals:
      INTEGER LSL2H,LINK,BANK,L2VTX
c Externals:
      INTEGER GZSL2H,GZSVTX
C----------------------------------------------------------------------
      LSL2H = GZSL2H()
      IF (LSL2H .EQ. 0) CALL BKSL2H(LSL2H)
      LSVTX = GZSVTX()
      IF (LSVTX .EQ. 0) CALL ERRMSG('SVTX structure non-existant',
     &  'L2VTX_STP','Cannot proceed','F')
      DO LINK = 2,IC(LSVTX-2)
        BANK = LC(LSVTX-LINK)
        IF (BANK .GT. 0) CALL MZDROP(IXSTP,BANK,'L')
      ENDDO
      L2VTX = LC(LSL2H-IZL2VT)
      IF (L2VTX .GT. 0) CALL MZDROP(IXSTP,L2VTX,'L')
      CALL MZCOPY(IDVSTP,LSVTX,IDVSTP,LSL2H,-IZL2VT,'L')
  999 RETURN
      END
