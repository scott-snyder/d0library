      SUBROUTINE SAMUS_L2_SIDE(ISIDE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decode trigger bits
C-
C-   Inputs  : CCTLATCH BITS 6 AND 7 (N + S)
C-   Outputs : ISIDE ( 1=>N 2=>S )  EQ 0 IMPLIES NO TRIGGER
C-                                  GE 1 IMPLIES WAS TRIGGER 
C-   Controls: 
C-
C-   Created  16-SEP-1992   O.Eroshin
C-            30-NOV-1992   T.DIEHL   SET UP TO CHECK CCTLATCH BITS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER ISIDE(2),J,I
      INTEGER IREG(6),CRATEWORDS(5),TRIG_NUM(5),CRATE_ID(5),
     $ NOTC_CARDS(5),ACTOTC(5),VERSION(5),L15OTC(5),LONGTO(5),
     $ TABTYP(5),TRGBITS(5),MERR(5),KERR(5),CCTLAT(5),OTCNUM(5,16),
     $ OTCSTAT(5,16),MGRSTAT(5),NOTCWD(5),KTABLE(5,130,2,4),ITFWD(2),
     $ TRAILWC(5),TRAILCR(5),TRAILTN(5),CCTLAT2(5)          ! GTTRGR2

C----------------------------------------------------------------------
C
      CALL GTTRGR2(IREG,CRATEWORDS,TRIG_NUM,CRATE_ID,NOTC_CARDS,
     $  ACTOTC,VERSION,L15OTC,LONGTO,TABTYP,TRGBITS,MERR,KERR,CCTLAT,
     $  CCTLAT2,OTCNUM,OTCSTAT,MGRSTAT,NOTCWD,KTABLE,ITFWD,TRAILWC,
     $  TRAILCR,TRAILTN)

C      CALL GTTRGR2(IREG,CRATEWORDS,TRIG_NUM,CRATE_ID,NOTC_CARDS,
C     $  ACTOTC,VERSION,L15OTC,LONGTO,TABTYP,TRGBITS,MERR,KERR,CCTLAT,
C     $  OTCNUM,OTCSTAT,MGRSTAT,NOTCWD,KTABLE,ITFWD,TRAILWC,
C     $  TRAILCR,TRAILTN)


C
      DO J = 1,2
        ISIDE(J) = IAND(CCTLAT(J+3),3)
      ENDDO
C
  999 RETURN
      END
