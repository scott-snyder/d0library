      SUBROUTINE BKPROC(LPROC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books bank PROC under either RECO or GEAN depending
C-                              on Path chosen by PATHST(PATH)
C-
C-   Inputs  :
C-   Outputs : LPROC       location of the booked bank in ZEBCOM.
C-   Controls:
C-
C-   Created  ??-MAY-1987   Daria Zieminska
C-   Updated  22-FEB-1989   Alan M. Jonckheere  to book under either RECO 
C-                                              or GEAN.
C-   Updated  06-DEC-1991   Nobuaki Oshima - add structural link for CATD.
C-   Updated  20-JUL-1992   Serban D. Protopopescu  add str. link for level0 
C-   Updated  9-SEP-1992    Andrew Brandt      add str. link for SCAN (Ver 3)
C-                                             +4 spares
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZPROC.LINK/LIST'
      INTEGER ND, DNS, NS
      PARAMETER( ND = 1 )
      PARAMETER( NS = 15)
      INTEGER LSUP,LPROC
C----------------------------------------------------------------------
C
      CALL PATHBK(LSUP)
C
      LPROC = LQ( LSUP - IZPROC )
      IF( LPROC .EQ. 0 ) THEN
        CALL MZBOOK(IXMAIN,LPROC,LSUP,-IZPROC,'PROC',NS,NS,ND,2,0)
        IQ(LPROC+1)=6   ! version number
      ELSE
        IF ( IQ(LPROC+1) .LT. 6) THEN
          DNS = NS - IQ( LPROC - 2 )
        CALL MZPUSH(IXCOM,LPROC,DNS,0,' ')
        IQ(LPROC+1) = 5
        ENDIF

C        IF(IQ(LPROC+1).LT.2) THEN
C          IQ(LPROC+1)=3   ! version number
C        ELSE IF(IQ(LPROC+1).EQ.2) THEN
C          CALL MZPUSH(IXCOM,LPROC,5,0,' ')
C          IQ(LPROC+1)=3   ! version number
C        ENDIF
      ENDIF
  999 RETURN
      END
