C----------------------------------------------------------------------
      INTEGER FUNCTION T0TZFL( LDTRK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Store and return the ID  (in T0TZ) of the ZTRK,
C-                         corresponding to the DTRK.
C-
C-   Inputs       : LDTRK - pointer to the DTRK bank with specific track
C-   Outputs      : None (bank created)
C-   Return value : New T0TZ bank with corresponding ZTRK (if any) and
C-                  the ID of the track in this bank
C-   Controls:
C-
C-   Created  24-APR-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$INC:T0DREC.INC'
      INCLUDE      'D0$INC:T0DLNT2.INC'
      INCLUDE      'D0$INC:T0DLNT3.INC'
      INCLUDE      'D0$LINKS:IZT0TZ.LINK'
      INTEGER       LDTRK, GZZTRK, NZTRK, NOK, LZTRM, NTRACK
      INTEGER       ISETVN, LT0TZ1, MPT0TZ(5), GZT0TH
      LOGICAL       L_FIRST
C
      DATA          MPT0TZ / 0, 0, 0, 31, 0 /
      DATA          L_FIRST  / .TRUE. /
C
      T0TZFL = 0
      IF ( .NOT. L_T0D_ZTRK ) RETURN
      IF ( L_FIRST ) THEN
        L_FIRST = .FALSE.
        CALL UCTOH( 'T0TZ', MPT0TZ(1), 4, 4 )
        CALL MZFORM('T0TZ', '4I 2F 4I 18F', MPT0TZ(5) )
      ENDIF
C
      LZTRK = GZZTRK(0)
      IF (LZTRK .LE. 0) RETURN
C
      T0DLNT3(1) = 1
      NZTRK = 0
      LZFIT = 0
      NOK   = 0
      NTRACK = IQ(LDTRK-5)
      DO WHILE (LZTRK.GT.0)
        NZTRK = NZTRK + 1
        IF (IQ(LZTRK+3) .EQ. NTRACK) THEN
          NOK = NOK + 1
          LZFIT = LQ(LZTRK-1)
          LZTRM = LZTRK
        ENDIF
        LZTRK = LQ(LZTRK)
      ENDDO
      LZTRK = LZTRM
      IF (NOK .EQ. 1) THEN
        IF ( LZFIT .LE. 0 ) THEN
          CALL ERRMSG('T0D','T0TZFL','ZFIT bank does not exist','S')
          GO TO 999
        ENDIF
        IF ( LT0TZ .EQ. 0 ) THEN  ! First track - book from T0TH
          LT0TH = GZT0TH()
          IF (LT0TH .LE. 0) THEN
            CALL ERRMSG('T0D','T0TZFL','T0TH bank does not exist','S')
            GO TO 999
          ENDIF
          CALL MZLIFT( IXMAIN, LT0TZ, LT0TH, -IZT0TZ, MPT0TZ, 0 )
          IQ( LT0TZ-5) = 1
        ELSE                      ! Next track - book from previous
          CALL MZLIFT( IXMAIN, LT0TZ, LT0TZ, 0, MPT0TZ, 0 )
        ENDIF
        T0TZFL = IQ(LT0TZ-5)
C
C ****  Update ZTRAK tracks counter
C
        IQ( LT0TH+3 ) = IQ( LT0TH+3 ) + 1
C
C ****  Now fill the values
C
        IQ( LT0TZ   ) = ISETVN(IQ(LT0TZ),0)
        IQ( LT0TZ+1 ) = IQ( LZTRK - 5 )
        IQ( LT0TZ+2 ) = IQ( LZTRK + 2 )
        IQ( LT0TZ+3 ) = IQ( LZTRK + 3 )
        IQ( LT0TZ+4 ) = IQ( LT0TD - 5 )
        Q(  LT0TZ+5 ) = Q(  LZTRK + 6 )
        Q(  LT0TZ+6 ) = Q(  LZTRK + 8 )
        IQ( LT0TZ+7 ) = IQ( LZFIT + 3 )
        IQ( LT0TZ+8 ) = IQ( LZFIT + 4 )
        IQ( LT0TZ+9 ) = IQ( LZFIT + 6 )
        IQ( LT0TZ+10) = IQ( LZFIT + 7 )
        Q(  LT0TZ+11) = Q(  LZFIT + 8 )
        Q(  LT0TZ+12) = Q(  LZFIT + 9 )
        Q(  LT0TZ+13) = Q(  LZFIT + 10)
        Q(  LT0TZ+14) = Q(  LZFIT + 11)
        Q(  LT0TZ+15) = Q(  LZFIT + 12)
        Q(  LT0TZ+16) = Q(  LZFIT + 13)
        Q(  LT0TZ+17) = Q(  LZFIT + 14)
        Q(  LT0TZ+18) = Q(  LZFIT + 15)
        Q(  LT0TZ+19) = Q(  LZFIT + 16)
        Q(  LT0TZ+20) = Q(  LZFIT + 17)
        Q(  LT0TZ+21) = Q(  LZFIT + 18)
        Q(  LT0TZ+22) = Q(  LZFIT + 19)
        Q(  LT0TZ+23) = Q(  LZFIT + 20)
        Q(  LT0TZ+24) = Q(  LZFIT + 21)
        Q(  LT0TZ+25) = Q(  LZFIT + 22)
        Q(  LT0TZ+26) = Q(  LZFIT + 23)
        Q(  LT0TZ+27) = Q(  LZFIT + 24)
        Q(  LT0TZ+28) = Q(  LZFIT + 25)
        Q(  LT0TZ+29) = Q(  LZFIT + 34)
        Q(  LT0TZ+30) = Q(  LZFIT + 32)
        Q(  LT0TZ+31) = Q(  LZFIT + 33)
      ELSE IF (NOK .GT. 1) THEN
        CALL ERRMSG('T0D','T0TZFL','Several ZTRAKS with same ID','S')
      ELSE
        CALL ERRMSG('T0D','T0TZFL','ZTRAK has no ZFIT bank','S')
      ENDIF
C----------------------------------------------------------------------
  999 T0DLNT3(1) = 0      ! Deactivate temporary link area
      RETURN
      END
