      SUBROUTINE SACELL (NSTA,NSEC,NTUB,             NUMT,
     &                   HSHAPE1,NSPAR1,SPAR1,XPAR1,ROTM1,
     &                   HSHAPE2,NSPAR2,SPAR2,XPAR2,ROTM2,
     &                   NBUF,IBUF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns geometric parameters for the SAMUS drift
C-                         tubes
C-
C-   Inputs  : NSTA - SAMUS station number
C-             NSEC - SAMUS section number
C-             NTUB - tube number
C-             NBUF - maximum number of spacial words (in IBUF) to be filled
C-
C-   Outputs : NUMT - number of tubes for this input (must be 1 or 2)
C-             HSAPE1 - 1st tube volume shape ('TUBE')
C-             NSPAR1 - number of parameters for the 1st tube (=3)
C-             SPAR1 - volume parameters for the 1st tube
C-             XPAR1 - coordinate of the center of volume in global system
C-             ROTM1 - rotation matrix for 1st tube
C-             HSAPE2 - 2nd tube volume shape ('TUBE')
C-             NSPAR2 - number of parameters for the 2nd tube (=3)
C-             SPAR2 - volume parameters for the 2nd tube
C-             XPAR2 - coordinate of the center of volume in global system
C-             ROTM2 - rotation matrix for 2nd tube
C-             NBUF - number of spacial words (in IBUF) filled in this routine
C-             IBUF - spatial parameters
C-
C-   Controls: if tube volume doesnot exist or there are problems with SAMUS
C-             geometry or SAM_STPFILE then output parameters will be:
C-             NUMT = 0
C-             HSHAPEi = '    '
C-             NSPARi = 0
C-             others = 10000.
C-
C-   Created  13-MAY-1991   Andrei Kiryunin
C-   Updated   4-DEC-1992   Alexander Efimov  Add Ailer angles for the
C-                          SAMUS stations orientation.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER NSTA,NSEC,NTUB, NUMT,NSPAR1,NSPAR2
      REAL    SPAR1(3),SPAR2(3),XPAR1(3),XPAR2(3),ROTM1(3,3),ROTM2(3,3)
      CHARACTER*4 HSHAPE1,HSHAPE2
      INTEGER NBUF,IBUF(*)
      INTEGER SAGTUB,TUBE_TYPE(2),IOK,SAGSTA,TM,VM,SAGCON,SAGTBL,I
      REAL    RTUBE(3,2),VTUBE(3,2),CENTER(3),ANGL(3),SIZE(3),HOLE(3),
     &        RINN,ROUT,EC,ROT(3),A,B,VMOD,DLEN
C----------------------------------------------------------------------
C
C ****  Get tube coordinates in local system
C
      NUMT=SAGTUB(NSTA,NSEC,NTUB, RTUBE,VTUBE,TUBE_TYPE)
C -- problems with tube geometry banks
      IF (NUMT.NE.1.AND.NUMT.NE.2) GOTO 900
C
C ****  Get coordinates of the SAMUS station
C
      IOK=SAGSTA(NSTA,CENTER,ANGL,SIZE,HOLE)
C -- problems with station geometry banks
      IF (IOK.LT.0) GOTO 900
C
C ****  Get radius of the tube drift volume
C
      IOK=SAGCON(RINN,ROUT,EC,TM,VM)
C -- problems with constatnts banks
      IF (IOK.LT.0) GOTO 900
C
C ****  Calculate parameters for the 1st tube
C
      HSHAPE1='TUBE'
      NSPAR1=3
      SPAR1(1)=0.0
      SPAR1(2)=RINN
      IOK=SAGTBL(TUBE_TYPE(1),DLEN)
      IF (IOK.LT.0) GOTO 900
      SPAR1(3)=DLEN-EC
      DO 10 I=1,3
        XPAR1(I)=CENTER(I)+RTUBE(I,1)
   10 CONTINUE
      CALL UCOPY (VTUBE(1,1),ROT(1),3)
      B=VMOD(ROT(1),3)
      IF (B.LE.0) GOTO 900
      DO 20 I=1,3
   20 ROT(I)=ROT(I)/B
      A=VMOD(ROT(1),2)
      IF (A.LE.0) GOTO 900
      ROTM1(1,1)=-ROT(2)/A
      ROTM1(2,1)= ROT(1)/A
      ROTM1(3,1)= 0.0
      ROTM1(1,2)= ROT(1)*ROT(3)/A
      ROTM1(2,2)= ROT(2)*ROT(3)/A
      ROTM1(3,2)=-A           
      CALL UCOPY (ROT(1),ROTM1(1,3),3)
      IF (NUMT.EQ.1) GOTO 950
C
C ****  Calculate parameters for the 2nd tube
C
      HSHAPE2='TUBE'
      NSPAR2=3
      SPAR2(1)=0.0
      SPAR2(2)=RINN
      IOK=SAGTBL(TUBE_TYPE(2),DLEN)
      IF (IOK.LT.0) GOTO 900
      SPAR2(3)=DLEN-EC
      DO 30 I=1,3
        XPAR2(I)=CENTER(I)+RTUBE(I,2)
   30 CONTINUE
      CALL UCOPY (VTUBE(1,2),ROT(1),3)
      B=VMOD(ROT(1),3)
      IF (B.LE.0) GOTO 900
      DO 40 I=1,3
   40 ROT(I)=ROT(I)/B
      A=VMOD(ROT(1),2)
      IF (A.LE.0) GOTO 900
      ROTM2(1,1)=-ROT(2)/A
      ROTM2(2,1)= ROT(1)/A
      ROTM2(3,1)= 0.0
      ROTM2(1,2)= ROT(1)*ROT(3)/A
      ROTM2(2,2)= ROT(2)*ROT(3)/A
      ROTM2(3,2)=-A           
      CALL UCOPY (ROT(1),ROTM2(1,3),3)
      GOTO 999
C
C ****  Exit when problems with SAMUS geometric parameters or SAM_STPFILE
C
  900 NUMT = 0
      HSHAPE1='    '
      NSPAR1=0
      CALL VFILL (SPAR1,3,10000.)
      CALL VFILL (XPAR1,3,10000.)
      CALL VFILL (ROTM1,9,10000.)
C -- for 2nd tube
  950 HSHAPE2='    '
      NSPAR2=0
      CALL VFILL (SPAR2,3,10000.)
      CALL VFILL (XPAR2,3,10000.)
      CALL VFILL (ROTM2,9,10000.)
C
  999 RETURN
      END
