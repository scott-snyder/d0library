      SUBROUTINE PRTRAC(PRUNIT,LBK,NBK,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print TRD geometry bank(s)
C-
C-   Inputs  : PRUNIT=  unit number for output
C-             LBK =  bank address(for the case 'SINGLE' is asked)
C-             CFL   =  'ALL'    print all geometry banks (3)
C-                      'SINGLE' print only bank with address LBK
C-             IFL   =  0  print everything
C-                      1  print only relevant min and max run numbers
C-
C-   Created  19-OCT-1987   MANSOULIE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTACH.LINK'
C
      INTEGER PRUNIT,LBK,NBK,IFL
      CHARACTER CFL *(*)
      INTEGER IBK,K,LTACH,LAUX
      IF(LTGEO.EQ.0) THEN
        WRITE(PRUNIT,1000)
        GOTO 999
      ENDIF
      LTACH=LC(LTGEO-IZTACH)
      IF(LTACH.EQ.0) THEN
        WRITE(PRUNIT,1000)
        GOTO 999
      ENDIF
      WRITE (PRUNIT,1001) IC(LTACH+1),IC(LTACH+2)
      IF( IFL.NE.0 ) GOTO 999
C
C      WRITE (PRUNIT,1002) (C(LTACH+K+2),K=1,6)
      IF(CFL.EQ.'SINGLE'.AND.(LBK.NE.0.OR.NBK.NE.0)) THEN
        IF ( LBK.EQ.0 ) THEN
          LAUX = LC(LTACH-NBK)
        ELSE
          LAUX=LBK
        ENDIF
        WRITE (PRUNIT,1003) (C(LAUX+K),K=1,6)
        LAUX=LAUX+9
        WRITE (PRUNIT,1004) (IC(LAUX+K),K=1,3)
        LAUX=LAUX+3
        WRITE (PRUNIT,1005) (C(LAUX+K),K=1,13)
      ELSE
        DO 10 IBK = 1,3
          WRITE(PRUNIT,1006) IBK
          LAUX=LC(LTACH-IBK)
          WRITE (PRUNIT,1003) (C(LAUX+K),K=1,6)
          LAUX=LAUX+9
          WRITE (PRUNIT,1004) (IC(LAUX+K),K=1,3)
          LAUX=LAUX+3
          WRITE (PRUNIT,1005) (C(LAUX+K),K=1,13)
   10   CONTINUE
      ENDIF
C
  999 RETURN
 1000 FORMAT(///,'   TRD GEOMETRY BANKS ARE ABSENT')
 1001 FORMAT(///,'  TRD GEOMETRY BANK ,  MIN RELEVANT',
     >' RUN NUMBER ',I8,'      MAX  ',I8)
 1002 FORMAT(/,'  X,Y,Z OF CENTER OF GLOBAL TRD FRAME ',3F10.3,/,
     >  '  THETA,PHI OF PRINCIPAL AXIS ',2F10.6,'      OMEGA ',F10.6)
 1003 FORMAT(/,'  X,Y,Z OF CENTER OF LAYER TRD FRAME ',3F10.3,/,
     >  '  THETA,PHI OF PRINCIPAL AXIS ',2F10.6,'      OMEGA ',F10.6)
 1004 FORMAT(/,'  NUMBER OF GRID WIRES ',I6,'     SENSE/POT WIRES ',I6,
     >  '      STRIPS ',I6)
 1005 FORMAT(/,'  INNER WINDOW.     DIST FROM AXIS ',F10.3,/,
     >  17X,'   HALF-LENGTH    ',F10.3,/,
     >  '  GRID WIRES  .     DIST FROM AXIS ',F10.3,/,
     >  17X,'   HALF-LENGTH    ',F10.3,'    PHI OF WIRE 1 ',F10.6,/,
     >  '  SENSE WIRES .     DIST FROM AXIS ',F10.3,/,
     >  17X,'   HALF-LENGTH    ',F10.3,'    PHI OF WIRE 1 ',F10.6,/,
     >  '  CATHODE     .     DIST FROM AXIS ',F10.3,/,
     >  17X,'   HALF-LENGTH (PROJECTED ONTO Z-AXIS)        ',F10.3,/,
     >  17X,'   STRIP HALF-WIDTH (ANGLE IN TRANSV PLANE)   ',F10.6,/,
     >  17X,'   ALPHA = D(PHI)/D(Z)                        ',F10.6,/,
     >  17X,'   PHI OF STRIP 1 IN MEDIAN PLANE             ',F10.6)
 1006 FORMAT(//,'  TRD GEOMETRY BANK LAYER ',I6)
      END
