C----------------------------------------------------------------------
C-
C-   Created   07-JAN-1992  Andrew G. Brandt
C-   Updated   24-SEP-1993  Andrew G. Brandt save MITOOL L0QUAL
C-   Updated   01-MAR-1994  Andrew G. Brandt update for V4,5 of JUTL
C-   Updated   02-NOV-1994  Andrew G. Brandt CW
C-   Updated   31-JUL-1995  Bob Hirosky  replace qcd_mask w/ logical array 
C-   Updated   01-SEP-1995  Bob Hirosky  Compress integer words 
C-   Updated   07-DEC-1995  Andrew G. Brandt add new MITOOL word
C-   Updated   11-MAR-1996  Andrew G. Brandt add extra vertex words
C-
C----------------------------------------------------------------------
C
C QCD_JUTL_HEAD1
C
      COMMON /QCD_JUTL_HEAD0/ FILT_MASK,FILT_MASK2,JUVERS
      COMMON /QCD_JUTL_HEAD1/QCDFILT,
     +        L0_Z(2),L0_ZF(2),MUL_INT,MITOOL,MIRUN1,L0QUAL,
     +        TRK_NV,TRK_Z(2)
C
      INTEGER  FILT_MASK,FILT_MASK2,JUVERS
      LOGICAL  QCDFILT(64)
      INTEGER  L0_ZF,MUL_INT,TRK_NV,MITOOL,MIRUN1
      REAL     L0_Z,TRK_Z,L0QUAL
      CHARACTER*255 QCD_JUTL_HEAD1_TAG
      DATA QCD_JUTL_HEAD1_TAG/'QCDFILT(64):L,
     &    L0_SLOWZ:R,L0_FASTZ:R,
     &    L0_SLOWF[-1,1]:I,L0_FASTF[-1,1]:I,MUL_INTF[-1,4]:I,
     &    MITOOL[-1,4]:I,MI_RUN1[-1,5]:I,LOQUAL:R,
     &    N_VERTEX[-1,6]:I,VTX_Z1:R,VTX_Z2:R'/
C
C QCD_JUTL_HEAD2
C
      INTEGER MAXV
      PARAMETER (MAXV=3)
      COMMON /QCD_JUTL_HEAD2/NZ,VERT_Z(MAXV),VERT_DZ(MAXV),
     &        VERT_NTRKS(MAXV),VERT_WGHT(MAXV),VERT_MTHD(MAXV)
C
      REAL VERT_Z, VERT_DZ
      INTEGER NZ
      INTEGER VERT_NTRKS,VERT_WGHT,VERT_MTHD
C
      CHARACTER*255 QCD_JUTL_HEAD2_TAG
      DATA QCD_JUTL_HEAD2_TAG/'NZ[0,3],V_Z(NZ):R,V_DZ(NZ):R,
     &     V_NTK(NZ)[-1,254]:I,V_WGHT(NZ)[-1,126]:I,
     &     V_MTHD(NZ)[-1,62]:I'/
