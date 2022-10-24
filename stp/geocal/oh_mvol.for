      SUBROUTINE OH_MVOL
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
C-                          the EC OH modules. 
C-                          There are sixteen parameters required by GEANT
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
      DATA MOTHVL/'OH_BIG_MVOLUME+Z',
     &            'OH_BIG_MVOLUME-Z'/
C----------------------------------------------------------------------
      CALL GTSRCP('CONV_FACTOR',CONV,1)
      CALL GTSRCP_i('OH_MVOL_ROT_MATRIX',IRTOFF,1)
C
      DO 200 IZ = 1,2
        CALL ADDSTR(MOTHVL(IZ),'(1)',NMSRCP,LEN3)
     &      !Makes it into array format
        CALL GTSRCP_i(NMSRCP,LSR(1),1)
        NAME = LSR(3)
        MATNO = LSR(2)
        DO 201 IFC = 4,9
         RSR(IFC) = RSR(IFC)*CONV
 201   CONTINUE
C
        CHARR= 'MCAL'
        MOTHER = ICHARR
        ACTION = 'POS '
        IROT = IRTOFF
        ICPY2 = 1
C
        NPAR = 16
        PAR(1) = 0.0    ! START OF PHI DIVISIONS
        PAR(2) = 360.0  ! PHI RANGE
        PAR(3) = 16.0   ! NUMBER OF SIDES OF POLYGON
        PAR(4) = 4.0    ! NUMBER OF CHANGES IN RADIUS
        PAR(5) = RSR(4) ! LOW Z POSITION OF FRONT FACE 
        PAR(6) = RSR(9) ! RADIUS OF OUTER FACE
        PAR(7) = RSR(9) ! RADIUS OF OUTER FACE
        PAR(8) = RSR(5) ! HIGH Z POSITION OF FRONT FACE
        PAR(9) = RSR(8) ! RADIUS OF INNER FACE
        PAR(10)= RSR(9) ! RADIUS OF OUTER FACE
        PAR(11)= RSR(6) ! LOW Z POSITION OF BACK FACE
        PAR(12)= RSR(8) ! RADIUS OF INNER FACE
        PAR(13)= RSR(9) ! RADIUS OF OUTER FACE
        PAR(14)= RSR(7) ! HIGH Z OF BACK FACE
        PAR(15)= RSR(8) ! RADIUS OF INNER FACE
        PAR(16)= RSR(8) ! RADIUS OF INNER FACE
        IF(IZ.EQ.2) THEN
         PAR(6) = RSR(8)
         PAR(7) = RSR(8)
         PAR(15)= RSR(9)
         PAR(16)= RSR(9)
        ENDIF
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
     &  2X,8F10.4,/,8F10.4,/,
     &  '\END')
200   CONTINUE
C
      RETURN
      END
