      SUBROUTINE BLDTMH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build the time_to_position banks
C-
C-   Inputs  :  a data file called DL_VELOCITIES containing DL 
C-              velocities from hardware measurements  
C-   Outputs :  None
C-
C-   Created  17-FEB-1988   Olivier Callot
C-   Updated  14-AUG-1988   Qizhong Li-Demarteau  VELDLY changed into an array
C-   Updated  11-APR-1989   Qizhong Li-Demarteau  add DTMP and DCBD banks and 
C-                                      fill VELDLY with hardware measurements
C-   Updated  20-APR-1990   Qizhong Li-Demarteau  modified to handle
C-                                                CALIB data
C-   Updated   5-MAR-1992   Qizhong Li-Demarteau  use D0OPEN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER MPDTMW(5), MPDTMD(5)
      INTEGER LAY, SEC, ADC, IP, LDTMW(0:3), LDTMD(0:3), IDL
      INTEGER MODNUM(0:31), NDLY, HMOD, PLDTMW, PLDTMD 
      INTEGER IK1,IK2,IK3,IK4, KDL, LUN
      PARAMETER( LUN = 20 )
      INTEGER LOWRUN, HIGRUN, NWWIRE, NBWIRE, NWDELY, NBDELY
      PARAMETER( NWWIRE = 5 )
      PARAMETER( NBWIRE = 7 )
      PARAMETER( NWDELY = 4 )
      PARAMETER( NBDELY = 4 )
      LOGICAL OK
      REAL    K1, K2, K3
      REAL    TZWIR, VELWIR, TZDLY, VELDLY(8,0:31), HDVLCT(8)
      PARAMETER( TZWIR  = 200. )
      PARAMETER( VELWIR = .0040)
      PARAMETER( TZDLY  = 600. )
      CHARACTER*20  FLNAME
      DATA    FLNAME/'DL_VELOCITIES'/
      DATA    MPDTMW / 4HDTMW, 1, 1, 0, 0 /
      DATA    MPDTMD / 4HDTMD, 1, 1, 0, 0 /
      DATA    VELDLY/256*0.2214/
C  correspondence between module numbers in software and hardware 
      DATA MODNUM/42,43,8,9,11,12,14,15,16,17,18,19,20,21,22,23,
     &            25,26,27,28,29,30,31,32,33,24,36,37,38,39,40,41/
C----------------------------------------------------------------------
C
      IF ( LDTMH .EQ. 0 ) CALL BKDTMH(LDTMH)
      CALL MZFORM( 'DTMW', '4I -F', MPDTMW(5) )
      MPDTMW(4) = 4 + 32*NBWIRE*NWWIRE
      CALL MZFORM( 'DTMD', '4I -F', MPDTMD(5) )
      MPDTMD(4) = 4 + 32*NBDELY*NWDELY
C
C    velocities for DL are read in from a 
C    data file with hardware measurement
C
        CALL D0OPEN(LUN,FLNAME,'IF',OK)
        IF (.NOT. OK) THEN
          CALL ERRMSG('CDWSTP','BLDTMH',
     &    'Unable to open input file DL_VELOCITIES.DAT','W')
          GOTO 998
        ENDIF
  102 READ(LUN, 1000, END=101)
 1000 FORMAT(//)
      DO 201 KDL = 1, 8
        READ(LUN, 1001, END=101) IK1,IK2,IK3,IK4,K1,K2,K3
 1001   FORMAT(4(I4,1X),2(F8.4,1X),F12.4)
        IF (KDL .EQ. 1) HMOD = IK2
        HDVLCT(KDL) = K2
 201  CONTINUE
      DO 100 SEC = 0, 31
        IF (MODNUM(SEC) .EQ. HMOD) THEN
          CALL VZERO(VELDLY(1,SEC),8)
          DO 200 NDLY = 1, 8
            VELDLY(NDLY,SEC) = HDVLCT(NDLY) / 10. ! change into cm/ns
  200     CONTINUE
          GOTO 102
        ENDIF
 100  CONTINUE
      WRITE (71,3000) HMOD
 3000 FORMAT (' ****   Module No.',I3,' is not used in CDC')
      GOTO 102
 101  CONTINUE                          ! end of the data file
C
C ****  Book the layer banks
C
      DO 10 LAY = 0, 3
        CALL MZLIFT( IDVSTP, LDTMW(LAY), LDTMH, -(LAY+1), MPDTMW, -1)
        PLDTMW = LDTMW(LAY)
        IC( PLDTMW-5 ) = LAY
        IC( PLDTMW+1 ) = LOWRUN
        IC( PLDTMW+2 ) = HIGRUN
        IC( PLDTMW+3 ) = NWWIRE
        IC( PLDTMW+4 ) = NBWIRE
        DO 20 SEC = 0, 31
          DO 30 ADC = 0, NBWIRE-1
            IP = PLDTMW + ( SEC*NBWIRE + ADC ) * NWWIRE + 4
            C( IP+1 ) = TZWIR
            C( IP+2 ) = VELWIR
   30     CONTINUE
   20   CONTINUE
C
C       book and fill DTMP bank for sense wires 
        CALL BLDTMP(LAY,PLDTMW)
C
        CALL MZLIFT( IDVSTP, LDTMD(LAY), LDTMH, -(LAY+5), MPDTMD, -1)
        PLDTMD = LDTMD(LAY)
        IC( PLDTMD-5 ) = LAY
        IC( PLDTMD+1 ) = LOWRUN
        IC( PLDTMD+2 ) = HIGRUN
        IC( PLDTMD+3 ) = NWDELY
        IC( PLDTMD+4 ) = NBDELY
        DO 40 SEC = 0, 31
          DO 50 ADC = 0, NBDELY-1
            IP = PLDTMD + ( SEC*NBDELY + ADC ) * NWDELY + 4
            C( IP+1 ) = TZDLY
            IDL = 1
            IF (ADC.GT.1) IDL = 2
            IDL = LAY*2 + IDL
            C( IP+2 ) = (-1)**(ADC+1) * VELDLY(IDL,SEC)
   50     CONTINUE
   40   CONTINUE
C
C       book and fill DCBD bank for delay lines
        CALL BLDCBD(LAY,PLDTMD)
C
   10 CONTINUE
  999 RETURN
C
  998 WRITE(6, 2000) FLNAME
 2000 FORMAT(5X,' error opening the DL velocity file: ',A20,/
     &       '    The default values are stored in the DTMD bank')
      GOTO 101
      END
