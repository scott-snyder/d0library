      SUBROUTINE PRVLAY(PRUNIT,KVLAY,NVLAY,CFL,IFL)
C------------------------------------------------------------------
C-
C-  Print out VLAY (Hit bank for sense layer)
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LVLAY = bank address
C-  KVLAY = not used
C-  CFL   = 'ONE': print bank at KVLAY
C-          'ALL': print VLAY banks for all layers
C-          All other values act like 'ONE'
C-  IFL   = 0: no printout
C-          1 or greater: full printout (2 lines)
C-
C-  Daria Zieminska JAN.,1987
C-  Daria Zieminska May  1988 modified bank format
C-  Peter Grudberg 24-JUN-1989 use IFL and CFL
C-  P. G. 03-OCT-1989 add vers. #, spec. print for 'ALL' and IFL = 1
C-   Updated  25-SEP-1992   Peter M. Grudberg Fix for IBM compatibility
C-------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
      INTEGER PRUNIT,KVLAY,NVLAY,IFL
      INTEGER NHITS,NBOTH,NPLUS,NMIN
      INTEGER KLVLAY, NBLAYR, LAY, LAYPR
      INTEGER NVERS, I
      LOGICAL FIRST
      CHARACTER CFL*(*)
C-------------------------------------------------------------------
      IF (IFL .LE. 0) GO TO 1000
      IF ( CFL .EQ. 'ALL' .AND. IFL .EQ. 1 ) THEN
        FIRST = .TRUE.
C
C ****  Special print for IFL = 1, CFL = 'ALL'
C
        DO 50 LAY = 0, 2
          IF ( LVLAY(LAY) .NE. 0 ) THEN
            IF ( FIRST ) THEN
              FIRST = .FALSE.
              NVERS = IBITS(IQ(LVLAY(LAY)),13,5)
              WRITE (PRUNIT,4000) NVERS
 4000         FORMAT(/,'  Bank VLAY: (version',I2,')',
     &          /,12X,'--- number of hits per layer ---',/,/,
     &          '   Layer    HITS :  total   (+z)&(-z)   (+z) only',
     &          '   (-z) only',/)
            ENDIF
            IF ( .NOT. FIRST ) THEN
              WRITE (PRUNIT,4100) LAY, (IQ(LVLAY(LAY)+I), I=1,4 )
 4100         FORMAT(5X,I1,13X,I6,5X,I6,6X,I6,6X,I6)
            ENDIF
          ENDIF
   50   CONTINUE
        IF ( FIRST ) THEN
          WRITE (PRUNIT,4001)
 4001     FORMAT(/,1X,' Bank VLAY does not exist')
        ENDIF
        GOTO 1000
      ENDIF
C
C ****  detailed or selective print:
C
      IF ( CFL .EQ. 'ALL' ) THEN
        NBLAYR = 3
      ELSE
        NBLAYR = 1
      ENDIF
C
      LAY = 0
C
      DO WHILE ( LAY .LT. NBLAYR )
        IF ( CFL .EQ. 'ALL' ) THEN
          KLVLAY = LVLAY(LAY)
        ELSE
          KLVLAY = KVLAY
        ENDIF
        IF (KLVLAY .LE. 0) GO TO 20
C
        NVERS = IBITS(IQ(KLVLAY),13,5)    ! version number
        LAYPR =IQ(KLVLAY-5)    ! layer number
        NHITS =IQ(KLVLAY+1)    ! number of hits in this layer
        NBOTH =IQ(KLVLAY+2)    ! number of hits with both (+z) and (-z) data
        NPLUS =IQ(KLVLAY+3)    ! number of hits with (+z) data only
        NMIN  =IQ(KLVLAY+4)    ! number of hits with (-z) data only
        WRITE(PRUNIT,101) NVERS,LAYPR,NHITS,NBOTH,NPLUS,NMIN
C
C **** Is a loop needed?
C
   20   CONTINUE
        LAY = LAY + 1
      ENDDO
C
  101 FORMAT(/,'  Bank VLAY: (version',I2,') for Layer ',I1,/,
     $'   Number of hits in this layer/(+z)&(-z),/,
     $    (+z)only/(-z)only',4I5)
 1000 CONTINUE
      RETURN
      END
