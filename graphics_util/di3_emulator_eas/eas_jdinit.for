      SUBROUTINE JDINIT (DSPNUM)
C
C    Purpose:
CD   This module is used to initialize the connection between the 
CD   application and the Evans & Sutherland.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 19-Dec-1988
CH   History:
CH      19-DEC-88  ATV  Corrected cursor definition.
CH      05-DEC-88  ATV  Defined local variables.
CH      18-NOV-88  ATV  Define a dynamic cursor.
CH      15-NOV-88  ATV  Initial entry.
C    Updated  24-MAR-2004   sss - compile with g77.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GENRL-W ,GRFPAR-W, SEGINF-R, LINATT-R
C
C    Calls:
CC      ERROR, PINIT, PATTCH, PCONN, PSNFIX, KCLAS
C
      EXTERNAL ERRHND
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER DSPNUM
C
C    Then local declarations of variables (non-common variables).
C
      CHARACTER*60 ATINFO
      REAL XCURS(4,8), BCURS(4,8)
      LOGICAL*1 XCURLN(8), BCURLN(8)
      INTEGER I
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GENRL.INC/LIST'
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      INCLUDE 'D0$INC:PROCNS.INC/LIST'
      DATA XCURS/ 0.035, 0.035, 0.0, 1.0, -0.035, -0.035, 0.0, 1.0,
     +           -0.035, 0.035, 0.0, 1.0,  0.035, -0.035, 0.0, 1.0,
     +            0.035, 0.035, 0.0, 1.0, -0.035, -0.035, 0.0, 1.0,
     +           -0.035, 0.035, 0.0, 1.0,  0.035, -0.035, 0.0, 1.0/
      DATA XCURLN/.FALSE. , .TRUE., .FALSE. , .TRUE.,
     +            .FALSE. , .TRUE., .FALSE. , .TRUE./
      DATA BCURS/ -2.0,  0.0,   0.0, 1.0,  2.0,    0.0,   0.0, 1.0,
     +             0.0, -2.0,   0.0, 1.0,  0.0,    2.0,   0.0, 1.0,
     +            -2.0,  0.0,   0.0, 1.0,  2.0,    0.0,   0.0, 1.0,
     +             0.0, -2.0,   0.0, 1.0,  0.0,    2.0,   0.0, 1.0/
      DATA BCURLN/.FALSE. , .TRUE., .FALSE. , .TRUE.,
     +            .FALSE. , .TRUE., .FALSE. , .TRUE./
C
C    Then executable code follows
C
      IF (DSPNUM .LT. 0 .OR. DSPNUM .GT. 1) THEN
         CALL ERROR ('INVALID DISPLAY DEVICE')
      ENDIF

      IF (DINIT) THEN
         CALL ERROR ('DEVICE ALREADY INITIALIZED')
      ELSEIF (SEGOPN) THEN
         CALL ERROR ('A SEGMENT IS OPEN')
       ELSE
C&IF LINUX
C&         OPEN (UNIT=1,FILE='PS390',STATUS='OLD',ERR=1000)
C&ELSE
         OPEN (UNIT=1,FILE='PS390',STATUS='OLD',ERR=1000,
     +         READONLY)
C&ENDIF
         READ (1,100,END=1010) ATINFO
  100    FORMAT(A60)
         CLOSE (UNIT=1)
         CALL PATTCH (ATINFO,ERRHND)
         CALL PINIT (ERRHND)
C.N.O.
         CALL PCONN('MEMORY_MONITOR',1,1,'FLABEL0',ERRHND)
         CALL PSNFIX(1,3,'MEMORY_MONITOR', ERRHND)
C.N.O.
C
C    DEFINE A DYNAMIC CURSOR.
C
         CALL PBEGS('CURSOR', ERRHND)
         CALL PSEBIT('VIS', 2, .TRUE.,  '"', ERRHND)
         CALL PIFBIT('"',   2, .TRUE.,  'CUD', ERRHND)
         CALL PSEBIT('CUD', 1, .FALSE., '"', ERRHND)
         CALL PIFBIT('"',   1, .TRUE.,  'XCU', ERRHND)
         CALL PIFBIT('"',   1, .FALSE., 'BCU', ERRHND)
         CALL PENDS( ERRHND )
         CALL PVCBEG('XCU', 8, .FALSE., .FALSE., 2, PVITEM, ERRHND) 
         CALL PVCLIS(8, XCURS, XCURLN, ERRHND)
         CALL PVCEND(ERRHND)
         CALL PVCBEG('BCU', 8, .FALSE., .FALSE., 2, PVITEM, ERRHND) 
         CALL PVCLIS(8, BCURS, BCURLN, ERRHND)
         CALL PVCEND(ERRHND)
         CALL PCONN('TABLETIN', 5, 1, 'CURSOR.VIS', ERRHND)
         CALL PCONN('TABLETIN', 2, 1, 'CURSOR.CUD', ERRHND)
C
C   CLEAR THE ASCII SCREEN.
C
         CALL KCLAS
C
C   INITIALIZE THE WINDOW AND VIEWPORT.
C
         DO 10 I=1,6
            IF (MOD(I,2) .EQ. 0) THEN
               UWIND(I) = 1.0
               UVIEW(I) = 1.0
            ELSE
               UWIND(I) = -1.0
               UVIEW(I) = -1.0
            ENDIF
   10    CONTINUE
         UVIEW(5) = MINTEN / 32767.0
         UVIEW(6) = DINTEN / 32767.0
         DSPDEV = DSPNUM
C
C    Segmentation overhaul initializations.
C
         NTSEG = 0
         NSEGS = 0
         NRSEG = 0
         NPRIM = 0
      ENDIF

      DINIT = .TRUE.
      RETURN
 1000 CONTINUE
      CALL ERROR ('UNABLE TO FIND PS390 FILE, CHECK LOGICAL')
 1010 CONTINUE
      CALL ERROR ('PREMATURE END OF FILE REACHED ON PS390 LOGICAL')
      RETURN
      END
