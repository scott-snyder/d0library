      SUBROUTINE PRJAUX( PRUNIT, LJAUX, NJAUX, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Dump the contents of JAUX
C-
C-   Inputs  :PRUNIT = unit number to print to.
C-            LJAUX  = link to JAUX
C-            NJAUX,CFL are irrlevant as JAUX is unique for an event
C-            IFL = 0 for full printout
C-   Outputs :
C-   Controls:
C-
C-   Created  16-MAY-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:JAUX.PARAMS'           ! helpful JAUX params
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PRUNIT,IFL,NJAUX,LJAUX
      CHARACTER*(*) CFL
      INTEGER NPAR_MAX,NJT_HOT,NREPJAUX,IPAR,ICAND,IP
C----------------------------------------------------------------------
      WRITE(PRUNIT,*)'********** DUMPING JAUX BANK ****************'
  201 FORMAT(' CAND STAT IETA  ETA IPHI  PHI  ET        EMFR ETAS PHIS')
CCAND STAT IETA  ETA IPHI  PHI   ET       EMFR   ETAS    PHIS
C100:  -4  -23 -5.22  64  4.55 123.25 1.000 23.0  23.0
  202 FORMAT(' ',I3,':',2X,I2,2X,I3,1X,F5.2,2X,I2,1X,F5.2,1X,F6.2,
     & 1X,F6.2,1X,F6.2,1X,F6.2)
      IF (LJAUX .LE. 0) RETURN
C---Find some variables
      NPAR_MAX = IQ(LJAUX + 4)
      NJT_HOT  = IQ(LJAUX + 3)
      NREPJAUX = IQ(LJAUX + 2)


      DO IPAR = 1,NPAR_MAX             ! Do each parameter set
        WRITE(PRUNIT,201)
        DO ICAND = 1,NJT_HOT
          IP = (IPAR-1)*NREPJAUX*NJT_HOT + (ICAND-1)*NREPJAUX + LJAUX
          WRITE(PRUNIT,202)ICAND,IQ(IP+PJEVT),IQ(IP+PJIETA),Q(IP+PJETA),
     &     IQ(IP+PJIPHI),Q(IP+PJPHI),Q(IP+PJET),Q(IP+PJEMFR),
     &     Q(IP+PJETASIZ),Q(IP+PJPHISIZ)
        END DO
      END DO

  999 RETURN
      END
