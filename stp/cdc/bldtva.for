      SUBROUTINE BLDTVA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create and fill banks DTVH and DTVA containing side
C-                         dependent drift velocities for sense wires
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  16-SEP-1992   Domenico Pizzuto
C-   Updated   5-JAN-1993   Qizhong Li-Demarteau  use D0OPEN and fixed 
C-                                                bombs on UNIX
C-
C----------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDTVH.LINK'

      INTEGER MPDTVA (5),LAYER,LDTMW,LDTVA,PTR
      INTEGER MPDTVH (5),GZDTVH,LDTVH,GZDTMH
      INTEGER SECTOR,WIRE,SIDE,GZDTMW
      REAL DFISWV,DFOSWV,DFDVIW,DFDVOW,F (0:6,0:1)
      LOGICAL OK

C- Default inner sense wire velocity
      DATA DFISWV /39.475E-4/
C- Default outer sense wire velocity
      DATA DFOSWV /38.717E-4/
C- Default inner sense wire delta velocity
      DATA DFDVIW /-.0905E-4/
C- Default outer sense wire delta velocity
      DATA DFDVOW /-.1318E-4/
      DATA MPDTVH /0,4,4,4,3/
      DATA MPDTVA /0,2,2,450,0/

C- Push bank DTMH to accomodate additional link at -10
      LDTMH = GZDTMH ()
      CALL MZPUSH (IXSTP,LDTMH,1,0,' ')

C- Create banks DTVH hanging from DTMH via link -10.
      CALL UCTOH ('DTVH',MPDTVH(1),4,4)
      CALL MZLIFT (IDVSTP,LDTVH,LDTMH,-IZDTVH,MPDTVH,0)
C- Fill DTVH with default values
      C (LDTVH+1)  = DFISWV
      C (LDTVH+2)  = DFOSWV
      C (LDTVH+3)  = DFDVIW
      C (LDTVH+4)  = DFDVOW
C
C- Read file containing velocity deviations for inner and outer wires
C
      CALL D0OPEN(32,'VEL_DIFF','IF',OK)
      IF (.NOT. OK) THEN
        CALL ERRMSG('CDCSTP','BLDTVA',
     &    'Unable to open input file D0VCLBDIFFDB.DAT','W')
        GOTO 999
      ENDIF

C- Create banks DTVA hanging from DTVH via link -(layer+1)
      CALL UCTOH ('DTVA',MPDTVA(1),4,4)
      CALL MZFORM ('DTVA','2I -F',MPDTVA (5))
       DO 10 LAYER = 0, 3
        LDTVH = GZDTVH ()
        CALL MZLIFT (IDVSTP,LDTVA,LDTVH,-(LAYER+1),MPDTVA,0)
        IC (LDTVA+1) = 7
        IC (LDTVA+2) = 2
        PTR = LDTVA+IC (LDTVA+2)
         DO 20 SECTOR = 0, 31
          READ (32,2004) (F (WIRE,0),WIRE=0,6)
          READ (32,2004) (F (WIRE,1),WIRE=0,6)
 2004     FORMAT (T16,7(F6.4,2X))
           DO 25 WIRE = 0, 6
            DO 30 SIDE = 0, 1
C- Insert default sense wire velocity into DTVA structure
             IF (WIRE.EQ.0.OR.WIRE.EQ.6) THEN
              C (PTR+1) = (DFOSWV+(1.5-FLOAT (LAYER))*DFDVOW)*
     +                    F (WIRE,SIDE)
             ELSE
              C (PTR+1) = (DFISWV+(1.5-FLOAT (LAYER))*DFDVIW)*
     +                    F (WIRE,SIDE)
             END IF
             PTR = PTR+1
   30       CONTINUE
   25      CONTINUE
   20    CONTINUE
   10  CONTINUE

      CLOSE (32)
C
  999 RETURN
      END
