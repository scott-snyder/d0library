      SUBROUTINE TLYRFL(PLANE,TRACK,WIRE,BIN,NBIN_FADC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book and fill TLYR bank
C-
C-   Inputs  : PLANE      integer   1,2,3 (anodes) or 4,5,6 (cathodes)
C-             TRACK      integer   track number
C-             WIRE       integer   in [1,256]
C-             NBIN_FADC  integer   number of bins in FADC
C-             BIN        array of NBIN_FADC integers  
C-   Outputs : none
C-   Controls: none
C-
C-   Created  15-JAN-1993   Alain PLUQUET (extracted form old TRCFAD.FOR)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRTOBN.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      INTEGER LTLYR,PLANE,TRACK,NDD,WIRE,NBIN_FADC,BIN(*)
      CALL BKTLYR(LTLYR,PLANE)
      TLPINF(NBPNT(PLANE,TRACK),PLANE,TRACK)=LTLYR
      NDD=IQ(LTLYR-1)
      IQ(LTLYR+NDD)=WIRE+1000*PLANE
      IQ(LTLYR+NDD-1)=TRACK
      CALL UCOPY(BIN,IQ(LTLYR+1),NBIN_FADC)
      END
