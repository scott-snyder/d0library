      SUBROUTINE FDCHFL(LKFDCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the FDCH bank depending on PATH
C-
C-   Inputs  : none 
C-   Outputs : LKFDCH = Link location of FDCH bank
C-   Controls: none
C-
C-   Created   8-MAY-1989   Jeffrey Bantly
C-   Updated  27-FEB-1990   Susan K. Blessing  to include space for
C-    rise and fall time calculations if requested
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LKFDCH
      INTEGER NFTDA,NFPDA
      INTEGER IER
      LOGICAL DO_RISE                     ! TRUE IF WANT RISE/FALL TIME CALCS
C
      CHARACTER*4 PATH
C----------------------------------------------------------------------
      CALL PATHGT(PATH)
      IF( PATH .EQ. 'GEAN' ) THEN
        IQ(LKFDCH+2)=12            ! Number of words per FTSC info->10
        IQ(LKFDCH+3)= 8            ! Number of wires per Theta sector
        IQ(LKFDCH+4)= 9            ! Number of words per FTDA info
        IQ(LKFDCH+5)=10            ! Number of FADCs per Theta sector
        IQ(LKFDCH+6)=12            ! Number of words per FPSC info->10
        IQ(LKFDCH+7)=16            ! Number of wires per Phi sector
        IQ(LKFDCH+8)= 9            ! Number of words per FPDA info
        IQ(LKFDCH+9)=16            ! Number of FADCs per Phi sector
      ELSE              ! 'RECO'  
C
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('DO_RISE',DO_RISE,IER)
        CALL EZRSET
C
        IF (DO_RISE) THEN
          NFTDA = 11
          NFPDA = 11
        ELSE
          NFTDA = 8
          NFPDA = 8
        END IF
C
        IQ(LKFDCH+2)=12            ! Number of words per FTSC info
        IQ(LKFDCH+3)= 8            ! Number of wires per Theta sector
        IQ(LKFDCH+4)= NFTDA        ! Number of words per FTDA info->8
        IQ(LKFDCH+5)=10            ! Number of FADCs per Theta sector
        IQ(LKFDCH+6)=12            ! Number of words per FPSC info
        IQ(LKFDCH+7)=16            ! Number of wires per Phi sector
        IQ(LKFDCH+8)= NFPDA        ! Number of words per FPDA info->8
        IQ(LKFDCH+9)=16            ! Number of FADCs per Phi sector
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
