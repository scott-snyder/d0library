      SUBROUTINE BLSGEN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create and fill banks SGEN, SGBP, SGMC
C-
C-   Inputs  :
C-   Output  : SEE docs in D0$DOCS:SGEN_ZEBANKS.MEM
C-
C-   Created   2-AUG-1988   Ghita Rahal-Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSGEN.LINK'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INTEGER LSGEN,LSTPC,GZSTPC
      INTEGER IRUMIN, IRUMAX, NFORM,IER
      LOGICAL EZERR
C
C
      CALL EZGET('MINIMUM_RUN',IRUMIN,IER)
      CALL EZGET('MAXIMUM_RUN',IRUMAX,IER)
C
C ****  BOOK SGEN IN THE CORRECT PLACE OFF STPC
C
      LSTPC = LC(LSTPH-IZSTPC)
      CALL MZFORM ( 'SGEN', '2I ', NFORM )
      CALL MZBOOK ( IDVSTP, LSGEN, LSTPC, -IZSGEN,'SGEN',
     &     3, 3, 3, NFORM, 0 )
C
C
      IC ( LSGEN + 1 ) = 1              ! Bank version number
      IC ( LSGEN + 2 ) = IRUMIN         ! Minimum run validity range
      IC ( LSGEN + 3 ) = IRUMAX         ! Maximum run validity range
C
C **** Build the bank for Beam Pipe Geometry
C
      CALL BLSGBP
C
C ****  Build the bank for the Monte-Carlo Volumes
C
      CALL BLSGMC
C
      CALL EZMOVE('GNWSTP_RCP',LSGEN,3) ! Hang it off for Posterity
      IF(EZERR(IER))THEN
        CALL ERRMSG('GNWSTP','BLSGEN',
     &    'ERROR IN MOVING SRCP BANK ','W')
      ENDIF
C
  999 RETURN
      END
