      SUBROUTINE ZCDDEX_DST(LZTRK,DEDX,IMIP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to get the dE/dx for the ZTRK track from
C-                         DST file of D0GEANT data
C-                         (temporarily use dE/dx from a CDC track 
C-                          for the central detector track ZTRK)
C-
C-   Inputs  :
C-       LZTRK: the bank address of a central detector track ZTRK
C-   Outputs :
C-       DEDX: dE/dx (in MIP)
C-       IMIP: # of Minimum Ionizing Particles
C-
C-   Created  20-DEC-1990   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LZTRK, LDTRK, IMIP
      REAL    DEDX, NORMLZ, CUT
      DATA    NORMLZ/485.0/, CUT/1.3/
C----------------------------------------------------------------------
C
      IMIP = 0
      DEDX = 0.0
      IF (LZTRK .LE. 0) GOTO 999
      LDTRK = LQ(LZTRK - 7)            ! get CDC track address
      IF (LDTRK .GT. 0) THEN
        DEDX = Q(LDTRK + 20)        
        DEDX = DEDX / NORMLZ
        IF (DEDX .LE. 0) GOTO 999
        IF (DEDX .LE. CUT) THEN
          IMIP = 1
        ELSE
          IMIP = 2
        ENDIF
      ENDIF
C
  999 RETURN
      END


