      SUBROUTINE GTJPTS(LJETS,LPTRS,NSTORE,NCELL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the pointers to the cells in the jet
C-    pointed to by LJETS in array LPTRS.  The first NSTORE words of
C-    IPTRS are filled, so the user must be sure not to supply NSTORE
C-    larger than the length of LPTRS declared in the calling routine.
C-
C-   Inputs  : LJETS    [I]     JETS bank address whose cell-pointers
C-                              are sought
C-             NSTORE   [I]     Number of elements to fill in array LPTRS
C-
C-   Outputs : LPTRS    [I]     Pointers to the cells from JPTS/CJPT bank
C-                              (If NSTORE>NCELL, the last NCELL-NSTORE
C-                               elements of LPTRS will be 0)
C-             NCELL    [I]     Number of cells in JETS (see note below)
C-             IER      [I]     Error code; 0 --- OK, NCELL < NSTORE
C-                                          1 --- NCELL > NSTORE
C-                                         -1 --- Pointer to JPTS/CJPT not
C-                                                found in JETS
C-                                         -2 --- LJETS address inappropriate
C-
C-   Controls: None
C-
C-   Notes:
C-           See D0$ZEB$PROC:JPTS.ZEB for details of the JPTS bank
C-           See D0$ZEB$PROC:CJPT.ZEB for details of the CJPT bank
C-
C    Created  18-AUG-1995   Dhiman Chakraborty
C-   Updated   7-DEC-1995   Dhiman Chakraborty   
C                           IODD bug-fix
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZJPTS.LINK'
C
      INTEGER IER,NSTORE,NCELL,NFILL,NW
      INTEGER LPTRS(NSTORE)
      INTEGER II,JJ,IPTRS,IODD
C
      INTEGER LJETS,LJPTS
      INTEGER HNAME,HJPTS,HCJPT,HJETS
      INTEGER JBYT
      EXTERNAL JBYT
C----------------------------------------------------------------------
      IER = 0
      DO II = 1,NSTORE
        LPTRS(II) = 0
      ENDDO
      NCELL = 0
      IF(LJETS.LE.0) THEN
        CALL ERRMSG('GTJPTS','GTJPTS',
     &      'Illegal JETS bank address','W')
        IER = -2
        GOTO 999
      ELSE      ! Check if the address is that of a JETS bank
        CALL DCTOH(4,'JETS',HJETS)
        IF(IQ(LJETS-4).NE.HJETS) THEN
          CALL ERRMSG('GTJPTS','GTJPTS',
     &      'Inappropriate address LJETS','W')
          IER = -2
          GOTO 999
        ENDIF
      ENDIF
C
      LJPTS=LQ(LJETS-IZJPTS)
      CALL DCTOH(4,'CJPT',HCJPT)
      CALL DCTOH(4,'JPTS',HJPTS)
      HNAME = IQ(LJPTS-4)
      IF((HNAME.NE.HCJPT).AND.(HNAME.NE.HJPTS))THEN
        CALL ERRMSG('GTJPTS','GTJPTS',
     &      'JETS bank does not have pointer to JPTS OR CJPT','W')
        IER = -1
        GOTO 999
      ENDIF
      NW = IQ(LJPTS-1) - 2     ! number of words in JPTS/CJPT bank
      IF(NW.LE.0) THEN
        CALL ERRMSG('GTJPTS','GTJPTS',
     &     'Number of cells in JPTS/CJPT < 0','W')
        GOTO 999
      ENDIF
      IF(HNAME.EQ.HCJPT)THEN
        IODD = 1
        IF(JBYT(IQ(LJPTS+IQ(LJPTS-1)),1,16).GT.0)IODD = 0
        NCELL = NW*2 - IODD
        NFILL = MIN(NCELL,NSTORE)
        IF(NCELL .GT. NSTORE) THEN
          IER = 1
          CALL ERRMSG('GTJPTS','GTJPTS',
     &      'Number of cells in CJPT exceeds reserved array length','W')
        ENDIF
        IF(NW.LE.0) THEN
          CALL ERRMSG('GTJPTS','GTJPTS',
     &      'Number of words in CJPT < 0','W')
          GOTO 999
        ENDIF
        DO II = 0,NW-1
          DO JJ = 1,2
            IPTRS = (II*2)+JJ
            IF(IPTRS.LE.NFILL)
     &        LPTRS(IPTRS) = JBYT(IQ(LJPTS+3+II),((2-JJ)*16)+1,16)
          ENDDO
        ENDDO
      ELSE
        NCELL = NW
        NFILL = MIN(NCELL,NSTORE)
        IF(NCELL .GT. NSTORE) THEN
          IER = 1
          CALL ERRMSG('GTJPTS','GTJPTS',
     &      'Number of cells in JPTS exceeds reserved array length','W')
        ENDIF
        DO II = 1,NFILL
          LPTRS(II) = IQ(LJPTS+2+II)
        ENDDO
      ENDIF
C
  999 RETURN
      END
