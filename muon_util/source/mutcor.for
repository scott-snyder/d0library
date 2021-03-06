      SUBROUTINE MUTCOR(IORENT,IODD,VECT,XTDV,WLEN,TCOR)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    DETERMINES TIME OF FLIGHT CORRECTION FOR MUON DRIFT CELLS
C    INPUT:IORENT=ORIENTATION
C          IODD  = EVEN/ODD CELL
C          VECT  =XYZ LOCATION OF CELL CENTER
C          XTDV  =TIME DIVISION LOCATION RELATIVE TO CELL CENTER
C          WLEN  =WIRE LENGTH
C    OUTPUT:TCOR=TIME CORRECTION
C
C    HEDIN 10-6-86
C    DH 12-87 ALLOW FOR +-IORENT
C    DH 3/88 ADD DTRES TO TIMBIN
C    DH 4/88 REMOVE T0 FROM THIS ROUTINE
C    DH 6/88 ADD EVEN/ODD CELL
C    DH 8/88 IF UNPHYSICAL PLACE XTDV AT 0
C    DH 7/90 CHANGE CALL TO VERXYZ
C    SA 7/91 NEW ARGUMENT ADDED TO VERXYZ
C    RM 11/94 WORKS FOR SSQ FILES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IORENT,I,IOR,IODD,IVER,NV,MODE,LDUMMY,IER
      REAL VECT(3),XTDV,WLEN,TCOR,VERTEX(3),CUML,ERR(3)
      REAL SPEEDL,V(3),XTD,TWIRE,XTDV1
      INTRINSIC SQRT
      DATA SPEEDL/30./      ! SPEED OF LIGHT
      CALL VERXYZ(IVER,VERTEX,NV)             ! GET VERTEX POINT
C
C     If z_vertex is 0.0 for real data, then assume SSQ data
C
      IF (VERTEX(3) .EQ. 0. .AND. IQ(LHEAD+1) .LT. 1000) THEN
        IF (IQ(LHEAD+6) .LT. 70000) THEN    ! Run 1a
          MODE = 1
        ELSE
          MODE = 2                          ! Run 1b
        ENDIF
        CALL MUGET_VERT(MODE,LDUMMY,VERTEX,ERR,IER)
      ENDIF
CC
      XTDV1=XTDV
      IF(ABS(XTDV).GT.10000.) XTDV1=0 ! CHECK IF UNPHYSICAL
CC
      IOR=IABS(IORENT)
      DO 1 I=1,3
 1    V(I)=VECT(I)
      IF(IORENT.EQ.1.OR.IORENT.EQ.3) V(2)=V(2)+XTDV1
      IF(IORENT.EQ.2.OR.IORENT.EQ.4) V(1)=V(1)+XTDV1
      CUML=SQRT((VERTEX(1)-V(1))**2+(VERTEX(2)-V(2))**2+
     A (VERTEX(3)-V(3))**2)
CC
CC   DO PATH LENGTH ALONG WIRE
CC
      XTD=XTDV1
      IF(IORENT.LT.0) XTD=-XTDV1
      IF(XTD.LT.-WLEN/2.) XTD=-WLEN/2.
      IF(XTD.GT.WLEN/2.)  XTD=WLEN/2.
      IF(IODD.EQ.0) THEN
        TWIRE=WLEN/2.+XTD            ! EVEN WIRE
      ELSE
        TWIRE=WLEN+1.+WLEN/2.-XTD    ! ODD WIRE
      ENDIF 
      TCOR=(CUML+TWIRE)/SPEEDL
      RETURN
      END
