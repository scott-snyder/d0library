      SUBROUTINE MH_MVOL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Routine puts out OH Mother Volume parameters
C-                         for PREGEO
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  22-JUN-1989   Norman Graf   
C-                          This subroutine is a rather laborious way
C-                          to place two mother volumes within the
C-                          volume 'MCAL'. These mother volumes are for 
C-                          the EC MH modules. 
C-                          There are ten parameters required by GEANT
C-                          for the shape of the volume. Some are 
C-                          hardcoded, others are read in from the 
C-                          file SRCP_RAW_EC.DAT.
C-                          
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL CONV,RIN,ROUT,DELZ,ZCEN,Z0
      INTEGER IRTOFF,NAME,MATNO,I,NDIV,IPT,IZ,LEN3,IFC,MOTHER,ICPY2
      INTEGER IROT,NPAR,KK
      INTEGER LSR(1000)
      REAL XX,YY,PAR(20)
      REAL RSR(500),RSR1(1000)
      EQUIVALENCE (LSR,RSR)
C
      CHARACTER*4 ACTION
      CHARACTER*18 MOTHVL(2)
      CHARACTER*7 EM_MODULES(4,2)
      CHARACTER*15 ZDVNM(4,2)
      CHARACTER*18 EMPLATE(2)
      CHARACTER*17  EMSUP(2)
      CHARACTER*32 NMSRCP
      CHARACTER*50 STRING
      INTEGER ICHARR
      CHARACTER*4 CHARR
      EQUIVALENCE (ICHARR,CHARR)
      DATA MOTHVL/'MH_MOTHER_VOLUME+Z',
     &            'MH_MOTHER_VOLUME-Z'/
C----------------------------------------------------------------------
      CALL GTSRCP('CONV_FACTOR',CONV,1)
      CALL GTSRCP('MH_MVOLS_ROT_MATRIX',IRTOFF,1)
C
      DO 200 IZ = 1,2
        CALL ADDSTR(MOTHVL(IZ),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
        CALL GTSRCP(NMSRCP,LSR,1)
        NAME = LSR(3)
        MATNO = LSR(2)
        DO 201 IFC = 4,7
          RSR(IFC) = RSR(IFC)*CONV
  201   CONTINUE
C
        CHARR= 'MCAL'
        MOTHER = ICHARR
        ACTION = 'POS '
        IROT = IRTOFF
        ICPY2 = 1
C
        NPAR = 10
        PAR(1) = 0.0    !START OF PHI DIVISIONS
        PAR(2) = 360.0  !PHI RANGE
        PAR(3) = 16.0   !NUMBER OF SIDES OF POLYGON
        PAR(4) = 2.0    !NUMBER OF CHANGES IN RADIUS
        PAR(5) = RSR(4) !Z POSITION OF FRONT FACE
        PAR(6) = RSR(5) !RADIUS OF INNER FACE
        PAR(7) = RSR(6) !RADIUS OF OUTER FACE
        PAR(8) = RSR(7) !Z OF BACK FACE      
        PAR(9) = RSR(5) ! RADIUS OF INNER FACE
        PAR(10)= RSR(6) ! RADIUS OF OUTER FACE
        XX = 0.0
        YY = 0.0
        ZCEN = 0.0
C
        STRING = '\ARRAY '//MOTHVL(IZ)
        WRITE(20,1)STRING,NAME,MATNO,
     &        MOTHER,ACTION,
     &        IROT,ICPY2,XX,YY,ZCEN,NPAR,
     &        (PAR(KK),KK=1,NPAR)
    1   FORMAT(A,/,
     &  2X,'''',A4,'''',5X,'''PGON''',5X,I2,5X,
     &  '''',A4,'''',5X,'''',A4,'''',/,
     &  I7,2X,I5,3F10.4,2X,I5,/,
     &  2X,5F10.4,/,5F10.4,/,
     &  '\END')
200   CONTINUE
C
      RETURN
      END
