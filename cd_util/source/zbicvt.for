
      SUBROUTINE ZBICVT(BILIPT,BILIRT,FADCDT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : make bilinear conversion for FADC data.
C-                  (The FADC data is subtracted by the pedestal value)
C-
C-   Inputs  : FADCDT : FADC data in one FADC bin 
C-             BILIPT : break point of the two linear function
C-             BILIRT : ratio between the slopes of the two linear function
C-   Outputs : FADCDT : after bilinear conversion
C-   Controls: 
C-
C-   Created   8-JUL-1989   Qizhong Li-Demarteau
C-   Updated   7-NOV-1989   Qizhong Li-Demarteau  bilinear conversion
C-                                          parameters are as inputs now 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER FADCDT, HEIGHT(0:255), N, IBREAK
      REAL    BILIRT, BILIPT
      LOGICAL FIRST
C
      SAVE  FIRST
      DATA  FIRST/.TRUE./
C----------------------------------------------------------------------
C
C  build a mapping array for bilinear conversion
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        IF (BILIRT .EQ. 0.0) BILIRT = 8.3               ! default value
        IF (BILIPT .EQ. 0.0) BILIPT = 192.0             ! default value 
        IBREAK = NINT(BILIPT)
        DO 100 N = 0, IBREAK-1
          HEIGHT(N) = N
  100   CONTINUE
        DO 200 N = IBREAK, 255
          HEIGHT(N) = NINT((N - BILIPT) * BILIRT + BILIPT)
  200   CONTINUE
      ENDIF
C
      FADCDT = HEIGHT(FADCDT)
C
  999 RETURN
      END


