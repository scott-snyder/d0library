C **** COMMON BLOCK SEGINF
C
C  This is the level 3 common block used for segment information.
C  The variables consist of:
C    MSEGS          parameter      Indicating maximum size of segment 
C                                  information arrays.
C    NSEGS          integer        Number of retained segments stored.
C    SEGINF(6,MSEGS) integer       Segment information for 
C                                  retained segments.
C                                     SEGINF(1,n) = Segment name.
C                                       values 1 to 32767
C                                     SEGINF(2,n)(0:1) = Transformation
C                                       type values 0 to 2.
C                                     SEGINF(2,n)(2:2) = Hilighting
C                                       value 0 = Not hilighted
C                                       value 1 = Highlighted
C                                     SEGINF(2,n)(3:3) = Visibility
C                                       value 0 = Invisible
C                                       value 1 = Visible
C                                     SEGINF(2,n)(10:15) = Unused.
C                                     SEGINF(2,n)(16:31) = Pick ID.
C                                     SEGINF(3,n)(0:15)  = Detectability
C                                     SEGINF(3,n)(16:31) = Priority.
C                                     SEGINF(4,n) = Starting primitive 
C                                                   number.
C                                     SEGINF(5,n) = Ending primitive 
C                                                   number.
C                                     SEGINF(6,n) = Internal segment name.
C    NPRIM          integer        Number of primitives output. Valid 
C                                  values are 1 to 1048575.
C    SEGOPN         logical        Indicates the status of any segment.
C    SEGNUM         integer        Is the segment number. The values
C                                  can be:
C                                        n < 0 - no segment open.
C                                        n = 0 - temporary segment open.
C                                        n > 0 - retained segment number.
C    SEGNAM         character*4    Current segment name. Format is as 
C                                  follows:
C                                     +-+---+
C                                     |a|xxx|
C                                     +-+---+
C                                      |  |
C                                      |  +--- Segment number in base 37.
C                                      +------ The type of segment
C                                              'T' for temporary and
C                                              'R' for retained.
C    SEGSOP         logical        Flag indicating that a segment has been 
C                                  open since the JBEGIN.
C    NTSEG          integer        number used for creation of temporary 
C                                  segments.
C    NRSEG          integer        number used to create internal name 
C                                  for retained segments.
C    INST           character*8    instance name used to include the 
C                                  primitives as part of the viewing 
C                                  transformation.
C  Here are the retained segment attributes:
C    CTTYPE         integer        Transformation matrix type. Values 
C                                  between 0 and 2 implicitly.
C    DVISIB         integer        Designating whether the segment is 
C                                  visible or not. Values of 0 or 1
C    DHILIT         integer        Designating whether the segment is
C                                  high-lighted or not. Valid val 0 or 1
C    DSGPRI         integer        Segment priority (1 to 32767, zero
C                                  segment has no priority).
C    DDETEC         integer        Segment detectability (or PICK
C                                  priority). Values 1 to 32767 or zero
C                                  for no detectability.
C    DPIKID         integer        Default PICK ID.
C    CPIKID         integer        Current PICK ID.
C
      INTEGER MSEGS
      PARAMETER (MSEGS=10000)
      INTEGER SEGINF(6,MSEGS), NSEGS, DVISIB, DHILIT, DSGPRI, DDETEC
      INTEGER DPIKID, CPIKID
      INTEGER NPRIM, NRSEG
      LOGICAL SEGOPN
      INTEGER SEGNUM, NTSEG, CTTYPE
      CHARACTER*4 SEGNAM
      CHARACTER*6 INST
      LOGICAL SEGSOP, BATCH, BATSEG

      COMMON /SEGINF/ SEGOPN, SEGNUM, SEGSOP, SEGNAM, SEGINF,NRSEG
      COMMON /SEGINF/ NSEGS, DVISIB, DHILIT, DSGPRI, DDETEC, DPIKID
      COMMON /SEGINF/ BATCH, BATSEG
      COMMON /SEGINF/ CPIKID, NPRIM, NTSEG, CTTYPE, INST
