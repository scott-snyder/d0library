      SUBROUTINE FILL_CDTK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill CDTK bank (DTRK summary)
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-OCT-1995   Ian Adam
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZDTRK.LINK'
      INCLUDE 'D0$INC:CDTK_PACKING.INC'

      INTEGER LDTRH,GZDTRH,LDTRK,LCDTK,GZDTRK,GZCDTK
      INTEGER NTRK,CDTK_POINTER,NZBANK
      REAL    THETA,PHI,X0,Y0,Z0,MIP
      INTEGER NHITXY,NHITRZ,IMIP,PKWRD1,PKWRD2
      LOGICAL MIP_OVERFLOW
      EXTERNAL NZBANK
C----------------------------------------------------------------------
      LDTRH = GZDTRH()
      IF (LDTRH.LE.0) THEN
        CALL ERRMSG('NO DTRH','FILL_CDTK','NO CDTK MADE','W')
        GOTO 999
      ENDIF

      LDTRK = LQ(LDTRH-IZDTRK)
      IF (LDTRK.LE.0) THEN
        CALL ERRMSG('NO DTRH','FILL_CDTK','NO CDTK MADE','W')
        GOTO 999
      ENDIF

      NTRK = NZBANK(IXCOM,LDTRK)
      CALL BKCDTK(NTRK,NREP,LCDTK)
      
      LDTRK = GZDTRK(0)
      LCDTK = GZCDTK()

      CDTK_POINTER = LCDTK + 3
      
      DO WHILE (LDTRK.GT.0)

        THETA   = Q( LDTRK + 9)
        PHI     = Q( LDTRK + 6)
        X0      = Q( LDTRK + 7)
        Y0      = Q( LDTRK + 8)
        Z0      = Q( LDTRK + 11)
        NHITXY  = IQ(LDTRK + 2)
        NHITRZ  = IQ(LDTRK + 5)
        MIP     = Q( LDTRK + 20)

C- PACK NHITXY,NHITRZ,MIP INTO ONE INTEGER WORD
C- NORMALIZE MIP TO MIP_CUTOFF; 22 BIT PRECISION; BIT 32 = OVERFLOW
        
        IF (MIP.GE.MIP_CUTOFF) THEN
          MIP = MIP_CUTOFF
          MIP_OVERFLOW = .TRUE.
        ELSE
          MIP_OVERFLOW = .FALSE.
        ENDIF

C- 2**22 - 1 = 4194303
 
        IMIP = INT(MIP*4194303/MIP_CUTOFF)

C- See CERN library short write-ups, page M421 for bit handling
C- utilities documentation
C- Bit assignment: 1-5 NHITXY; 6-9 NHITRZ; 10-31 MIP; 32 mip overflow

        CALL SBYT(NHITXY , PKWRD1 , 1 , 5 )
        CALL SBYT(NHITRZ , PKWRD1 , 6 , 4 )
        CALL SBYT(IMIP   , PKWRD1 , 10, 22)
        IF (MIP_OVERFLOW) THEN
          CALL SBIT1(PKWRD1,32)
        ELSE
          CALL SBIT0(PKWRD1,32)
        ENDIF

        CALL UDST_CDTK_ERRORS_PACK(LDTRK,PKWRD2)

        Q( CDTK_POINTER + 1) = THETA
        Q( CDTK_POINTER + 2) = PHI
        Q( CDTK_POINTER + 3) = X0
        Q( CDTK_POINTER + 4) = Y0  
        Q( CDTK_POINTER + 5) = Z0
        IQ(CDTK_POINTER + 6) = PKWRD1
        IQ(CDTK_POINTER + 7) = PKWRD2

        LDTRK = LQ(LDTRK)
        CDTK_POINTER = CDTK_POINTER + NREP

      ENDDO

  999 RETURN
      END
