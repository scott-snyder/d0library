      SUBROUTINE BLDXXP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create and fill banks DSWP and DDLP containing 
C-                         cable length T0 corrections for sense wire and
C-                         delay line channels respectively.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-SEP-1992   Domenico Pizzuto
C-   Updated   5-JAN-1993   Qizhong Li-Demarteau  use D0OPEN 
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDSWP.LINK'
      INCLUDE 'D0$LINKS:IZDDLP.LINK'
C
      INTEGER GZDTMH,MPDSWP (5),MPDDLP (5),LDSWP,LDDLP
      INTEGER SWP,SL,MOD,DLP,PTR
      REAL TPSW (0:1,0:1,0:31),TPDL (0:1,0:31)
      LOGICAL OK
C
      DATA MPDSWP /0,0,0,131,0/
      DATA MPDDLP /0,0,0,66,0/
C----------------------------------------------------------------------
C
C- Read file containing test pulse delays
C
      CALL D0OPEN(35,'TP_DELAYS','IF',OK)
      IF (OK) THEN
        READ (35,2000) (((TPSW (SWP,SL,MOD),SWP=0,1),SL=0,1),MOD=0,31)
        READ (35,2001)  ((TPDL (DLP,MOD),DLP=0,1),MOD=0,31)
        CLOSE (35)
      ELSE
        CALL ERRMSG ('CDWSTP','BLDXXP',
     +  'Error opening test pulse delay file TPDELAYS.DAT','W')
        GOTO 999
      END IF

      LDTMH = GZDTMH ()
C- Push bank DTMH to accomodate 2 additional structural links
      CALL MZPUSH (IXSTP,LDTMH,2,0,' ')

C- Create bank DSWP hanging from DTMH via link -11
      CALL UCTOH ('DSWP',MPDSWP(1),4,4)
      CALL MZFORM ('DSWP','3I -F',MPDSWP (5))
      CALL MZLIFT (IDVSTP,LDSWP,LDTMH,-IZDSWP,MPDSWP,0)

C- Fill DSWP bank with header and sense wire test pulse delay data
      IC (LDSWP+1) = 2       ! # of superlayers per module (sector)
      IC (LDSWP+2) = 2       ! # of test pulses per superlayer
      IC (LDSWP+3) = 0       ! Flag signaling wether corrections already
                             ! applied. 0 = NO / 1 = YES
      PTR = LDSWP+3
       DO 40 MOD = 0, 31
        DO 45 SL = 0, 1
         DO 50 SWP = 0, 1
          C (PTR+1) = TPSW (SWP,SL,MOD)
          PTR = PTR+1
   50    CONTINUE
   45   CONTINUE
   40  CONTINUE

C- Create bank DDLP hanging from DTMH via link -12
      CALL UCTOH ('DDLP',MPDDLP(1),4,4)
      CALL MZFORM ('DDLP','2I -F',MPDDLP (5))
      CALL MZLIFT (IDVSTP,LDDLP,LDTMH,-IZDDLP,MPDDLP,0)
 
C- Fill DDLP bank with header and delay line test pulse delay data.
      IC (LDDLP+1) = 2       ! # of sides per delay line
      IC (LDDLP+2) = 0       ! Flag signaling whether corrections already
                             ! applied. 0 = NO / 1 = yes
      PTR = LDDLP+2
       DO 55 MOD = 0, 31
        DO 60 DLP = 0, 1                  !0 = North side
         C (PTR+1) = TPDL (DLP,MOD)       !1 = South side
         PTR = PTR+1
   60   CONTINUE
   55  CONTINUE

 2000 FORMAT (///(T10,4(F5.1,5X)))
 2001 FORMAT (////(T10,2(F5.1,5X)))

  999 RETURN
      END
