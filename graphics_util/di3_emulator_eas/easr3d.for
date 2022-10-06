      SUBROUTINE EASR3D(ICODE, NSEG, ILISTS, NREAL, RLIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-               Set up routine specific to the graphic workstation
C-               Evans and Sutherland, essentially, initializing 3d rotation,
C-               connecting and disconnecting transformations and more.
C-
C-   Inputs  :  ICODE;
C-                     1 ----> To setup the transfomation network.
C-                      NSEG and ILISTS are dummies in this case.
C-                     2 ----> To connect the segments to the transformation
C-                      network. (only if new DI3000 is chosen NSEG and ILISTS
C-                      would be dummies, otherwise they should be given.)
C-                     3 ---->To disconnect the segments from the transformation
C-                      network. (only if new DI3000 is chosen NSEG and ILISTS
C-                      would be dummies, otherwise they should be given.)
C-                     4 ----> To disconnect the transfomation network.
C-                      NSEG and ILISTS are dummies in this case.
C-                     5 ----> To remove a segment from the disply list.
C-                      NSEG and ILISTS should be given.
C-                     6 ---->To insert a segment into the disply list.
C-                      NSEG and ILISTS should be given.
C-                     7 ---->To reset the transformations to its original
C-                      configuration. (only if new DI3000 is chosen NSEG and
C-                      ILISTS would be dummies, otherwise they should be given.)
C-                     8 ----> To return the percentage of memory filled
C-                      on E&S as ILISTS(1)
C-              NSEG;
C-                      Number of segments.
C-              ILITS(NSEG);
C-                      List of segments.
C-
C-   Created  28-NOV-1989   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICODE,NSEG,ILISTS(*), NREAL
      REAL RLIST(*)
      INTEGER LISSTN(10000),IARR(20)
      INTEGER NSTREA(10000),ILIST(3)
      REAL RLEVEL,RDUM(1)
      INTEGER NI2,L,LOUT
      INTEGER NSTRT,INSTR,NRESTR,IRESP,NINTEG,NINTEG2
      INTEGER IDUM(1),NDUM,I,NSTRE,NSTR,IR,IJKL,NITGR
      DATA NRESTR,NSTR/0,0/
C
      L=6
      LOUT=6
      NI2 = 8
      IF(ICODE .GT. NI2) GOTO 99
      GOTO (1,2,3,4,5,6,7,8) ICODE
C
C       -------------------------------------------------
C
    1 CONTINUE
C
C - The action of this item is to prepare your display
C - device and prepherals for 3D drawing and manipulations.
C - The segment transformation network is connected by this item.
C
      CALL JIQDEV(1,17,IRESP)
      IF(IRESP.NE.1) THEN
        WRITE(L,*)'  THIS DEVICE DOES NOT HAVE 3D CAPABILITY'
        GOTO 99
      END IF
      CALL JDD3D(.TRUE.)
      NINTEG=0
      NREAL=0
      CALL JESCAP(30002,NINTEG,NREAL,IDUM,RDUM)
      GOTO 99
C
C       -------------------------------------------
C
    2 CONTINUE
C
C - You should use this item to connect your segments to the
C - transformation network, and therefore be able to rotate
C - and transform your segments using the knobs on the box
C - and the function keys of the keyboard. After activating
C - this command you should be able to transform your drawings
C - by turning the knobs and pressing the function keys of the
C - terminal keyborad. The specific function of each knob
C - and/or key should by now be indicted by neon lights.
C
      NREAL=0
      NSTR = 0                          ! N.O.
      CALL JIQDIL(RLEVEL)
C
          NREAL = 1
          IF(RLIST(1) .LT. 1.) RLIST(1) = 1.0E5
C
      IF(RLEVEL .EQ. -2.0 .OR. RLEVEL .GT. 0.0) THEN
        DO 105 I=1,NSEG
          NSTR=NSTR+1
          LISSTN(NSTR)=ILISTS(I)
  105   CONTINUE
C
        NSTRT=NSTR/10+1
        DO 1056 IR=1,NSTRT
          INSTR=1+(IR-1)*10
          IF(IR.EQ.NSTRT) THEN
            NSTRE=10 + NSTR- 10*NSTRT
          ELSE
            NSTRE=10
          END IF
          NSTREA(IR)=NSTRE
          CALL JESCAP(30003,NSTRE,NREAL,LISSTN(INSTR),RLIST)
 1056   CONTINUE
C--------------------
      ELSE
        CALL JESCAP(30003,1,NREAL,LISSTN(1),RLIST)
      ENDIF
C
      GOTO 99
C
C       -------------------------------------------
C
    3 CONTINUE
C
C - This command may be used to disconnect the segments from
C - the transformation network.
C
      NREAL=0
      CALL JIQDIL(RLEVEL)
      IF(RLEVEL .EQ. -2.0 .OR. RLEVEL .GT. 0.0) THEN
        NSTR=0
        DO 106 I=1,NSEG
          NSTR=NSTR+1
          LISSTN(NSTR)=ILISTS(I)
  106   CONTINUE
C
        NSTRT=NSTR/10+1
        DO 1156 IR=1,NSTRT
          INSTR=1+(IR-1)*10
          IF(IR.EQ.NSTRT) THEN
            NSTRE=10 + NSTR- 10*NSTRT
          ELSE
            NSTRE=10
          END IF
          NSTREA(IR)=NSTRE
          CALL JESCAP(30004,NSTRE,NREAL,LISSTN(INSTR),RDUM)
 1156   CONTINUE
        NRESTR=1
C--------------------
      ELSE
        CALL JESCAP(30004,NSTREA(IR),NREAL,LISSTN(1),RDUM)
        NRESTR=1
      ENDIF
C
      GOTO 99
C       -------------------------------------------------
C
    4 CONTINUE
C
C - You may use this item to disconnect the 3D transformation
C - network and therefore end a session of 3D manipulation.
C
      NREAL=0
      CALL JIQDIL(RLEVEL)
      IF(RLEVEL .EQ. -2.0 .OR. RLEVEL .GT. 0.0) THEN
        DO IR=1,NSTRT
          INSTR=1+(IR-1)*10
          CALL JESCAP(30004,NSTREA(IR),NREAL,LISSTN(INSTR),RDUM)
        ENDDO
        GOTO 99
C------------------------------------------
      ELSE
        CALL JESCAP(30004,NSTREA(IR),NREAL,LISSTN(1),RDUM)
      ENDIF
C
      NRESTR=0
      NINTEG=0
      NINTEG2=3
      NREAL=0
      CALL JESCAP(30005,NINTEG,NREAL,IDUM,RDUM)
      GOTO 99
C
C       -----------------------------------------------------
C
    5 CONTINUE
C
C - You may use this item to remove a retained segment
C - from the display list (USE WITH EMULATOR ONLY)
C
      CALL JIQDIL(RLEVEL)
      IF(RLEVEL .GT. 0.0) THEN
        WRITE(LOUT,31)
   31   FORMAT(10X, 'This item could be used with E&S emluator only')
        GOTO 99
      ENDIF
      CALL JESCAP(30010,NSEG,NDUM,ILISTS,RDUM)
      GOTO 99
C
C       -----------------------------------------------------
C
    6 CONTINUE
C
C - You may use this item to include instantly a retained segment
C - (which was previously removed) in the display list.
C - (USE WITH EMULATOR ONLY).
C
      CALL JIQDIL(RLEVEL)
      IF(RLEVEL .GT. 0.0) THEN
        WRITE(LOUT,31)
        GOTO 99
      ENDIF
      CALL JESCAP(30011,NSEG,NDUM,ILISTS,RDUM)
      GOTO 99
C
C       -------------------------------------------
C
    7 CONTINUE
C
C - This command may be used to restor
C - the transformations to the original configuration.
C
      NREAL=0
      CALL JIQDIL(RLEVEL)
      IF(RLEVEL .EQ. -2.0 .OR. RLEVEL .GT. 0.0) THEN
        NSTR=0
        DO 107 I=1,NSEG
          NSTR=NSTR+1
          LISSTN(NSTR)=ILISTS(I)
  107   CONTINUE
C
        NSTRT=NSTR/10+1
        DO 207 IR=1,NSTRT
          INSTR=1+(IR-1)*10
          IF(IR.EQ.NSTRT) THEN
            NSTRE=10 + NSTR- 10*NSTRT
          ELSE
            NSTRE=10
          END IF
          NSTREA(IR)=NSTRE
          CALL JESCAP(30006,NSTRE,NREAL,LISSTN(INSTR),RDUM)
  207   CONTINUE
        NRESTR=1
C--------------------
      ELSE
        CALL JESCAP(30006,NSTREA(IR),NREAL,LISSTN(1),RDUM)
        NRESTR=1
      ENDIF
C
      GOTO 99
C
C       -----------------------------------------------------
C
    8 CONTINUE
C
C - You may choose this item to determine the percentage of
C - memory of Evans and sutherland that is already filled.
C
      NINTEG=1
      NREAL=0
      CALL JIESCP(1,30001,NINTEG,NREAL,ILISTS,RDUM)
C      WRITE(L,*)' PERCENTAGE ALREADY FILLED ',ILISTS(1)
      GOTO 99
C
C       ---------------------------------------------------
C
   99 CONTINUE
      RETURN
      END
