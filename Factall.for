      SUBROUTINE VUFACT(LUSTOT,NRAYS,FINGSK,FINVOL,NVMX,NZMX,NATMMX,
     1                  NATM,OPTHCK,IRAN)
C
C  Routine to calculate the element to element view factors
C  
C*****VERSION 2.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**26/10/90
C  09/07/90  Inclusion of message indicating the code is still running
C  26/10/90  Inclusion of participating gas volumes
C
C
      include 'comvar.for' 
C
      INTEGER LUSTOT,NRAYS,NVMX,NZMX,NATMMX,NATM,MXELMS,MXELMG
      REAL FINGSK(NATMMX,NVMX,NVMX,NZMX),FINVOL(NVMX,NVMX,NZMX),
     1     OPTHCK(NATMMX)
      INTEGER IELEMS,JELEMS,IATM,IELEMV,JELEMV,NRAYEL,ITYPE,INFO,IOB,
     1        ISTYPE,IKIND,INF,IX,IY,IZ,SGFRMS,SGFRME,IRAY,NOCELS,IHIT,
     2        NEXTIX,NEXTIY,NEXTIZ,NX,NY,NZ,INITIX,INITIY,INITIZ
      INTEGER*4 IRAN
      REAL SING,COSG,X,Y,Z,RAD,RANDA,RANDB,ALPHA,BETA,SP,CP,ST,CT,XRATE,
     1     YRATE,ZRATE,TEND,DTHICK,PHI,THETA,NF
      LOGICAL HIT
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  FINGSK  Array of fine grid cell gas absorptivites
C  FINVOL  Array of fine grid cell volumes
C  LUSTOT  Logical unit number for standard output
C  MXELMG  Maximum number of volume (gas) elements allowed 
C  MXELMS  Maximum number of surface elements allowed
C  NATM    Number of different atmospheres
C  NATMMX  Maximum number of different atmopsheres allowed
C  NVMX    Maximum number of vertices allowed
C  NZMX    Maximum number of z slices allowed
C  NRAYS   Ray density (number of rays fired per unit area of surface)
C  OPTHCK  Array to hold the running optical thickness down a ray
C  
C**** Arguments - output
C  NONE
C
C**** Common variables - input
C  CELLOB  Array giving the obstacle numbers in each cell
C  CELLWS  Array giving the wall segment numbers in each cell
C  CLSFZN  Array giving the surface zone numbers of each cell surface
C  CLVLZN  Array giving the volume zone numbers of each cell surface
C  COINC   Largest amount by which two values may differ and still be deemed
C          to coincide
C  ELAREA  Array giving the areas of each element
C  GHI     Array of obstacle high co-ordinates
C  GLO     Array of obstacle low co-ordinates
C  GX      Array of fine grid x co-ordinates
C  GY      Array of fine grid y co-ordinates
C  GZ      Array of fine grdi z co-ordinates
C  MXSFZN  Maximum number of surface zones allowed
C  MXWLCL  Maximum number of wall segments allowed in a cell
C  MXZNC   Maximum number of componets a zone may have (NB This must be 1)
C  NELEMS  Number of surface elements
C  NELEMV  Number of volume (gas) elements
C  NGZ     Number of z grid lines
C  NOBCMX  Maximum number of obstacles allowed in a cell
C  NOBMAX  Maximum number of obstacles allowed
C  NOBST   Array giving the number of obstacles in each cell
C  NSEG    Number of wall segments in the boundary
C  NV      Number of vertices on the boundary
C  OBSFZN  Array giving surface zone numbers of each obstacle
C  OBSTXE  Array giving the ending x cell numbers of each obstacle
C  OBSTXS  Array giving the starting x cell numbers of each obstacle
C  OBSTYE  Array giving the ending y cell numbers of each obstacle
C  OBSTYS  Array giving the starting y cell numbers of each obstacle
C  OBSTZE  Array giving the ending z cell numbers of each obstacle
C  OBSTZS  Array giving the starting z cell numbers of each obstacle
C  PI      Pi
C  SEGCEL  Array giving the cell numbers of wall segments
C  SFZN    Array giving surface zone information
C  SGSFZN  Array giving surface zone numbers of each wall segment
C  TYPE    Array giving obstacle types
C  VXWSEG  Array giving wall segment numbers at each vertex
C  WSEGXS  Array of starting x co-ordinates of wall segments
C  WSEGYS  Array of starting y co-ordinates of wall segments
C
C**** Common variables - output
C  VUFCGG  Array of gas element to gas element view factors
C  VUFCGS  Array of gas element to surface element view factors
C  VUFCSG  Array of surface element to gas element view factors
C  VUFCSS  Array of surface element to surface element view factors
C
C**** Local variables
C  ALPHA   Cone angle relative to the sending surface
C  BETA    Polar angle relative to the sending surface
C  COSG    Cosine of angle giving direction of sending surface if it is a
C          segment or direction of tangent plane if it is curved surface of
C          a cylinder
C  CP      Cosine of phi, cone angle relative to the grid
C  CT      Cosine of theta, polar angle relative to the grid
C  DTHICK  Increase in optical thickness due to passage through current cell
C  HIT     Logical flag indicating if a hit has taken place
C  IATM    Loop counter over the atmospheres
C  IELEMS  Loop counter over the surface elements
C  IELEMV  Loop counter over the volume elements
C  IHIT    Element number of the surface element which is hit
C  IKIND   Parameter passed to CONVRT indicating the kind of sending surface
C          1 for a surface perpendicular to all planes z=constant
C          2 for a surface in a plane z=constant
C          3 for a cylinder's curved surface
C  INF     Parameter passed to CONVRT giving further information about the
C          sending surface, value and meaning depends on IKIND.
C          IKIND=1: not used
C          IKIND=2: 1 - outward normal to surface is +k,
C                   2 - outward normal to surface is -k
C          IKIND=3: axial direction of cylinder, 1-x, 2-y, 3-z
C  INFO    Parameter passed to START: its value and meaning depends on ITYPE
C          ITYPE=1: 0 - sloping wall, 1 - wall along x grid line, 
C                   2 - wall along y grid line
C          ITYPE=2: 1 - in plane z=0, 2 - in plane z=ZMAX
C          ITYPE=3: axial direction of cylinder 1-x, 2-y, 3-z
C          ITYPE=4 or 5: not used
C  INITIX  x cell number of inital cell on the ray
C  INITIY  y cell number of inital cell on the ray
C  INITIZ  z cell number of inital cell on the ray
C  IOB     Obstacle number to which sending surface belongs
C  IRAN    Parameter required by random number generator
C  IRAY    Loop counter over number of rays
C  ISTYPE  Obstacle surface number of sending surface; 1-low x, 2-low z,
C          3-high z, 4-high y, 5-low y, 6-high x, 7-curved surface
C  ITYPE   Type of sending surface; 1 means wall segment; 2 means cell face;
C          3 means surface of a cylinder; 4 means surface of a box; 5 means
C          a volume zone
C  IX      x cell number of current cell on the ray
C  IY      y cell number of current cell on the ray
C  IZ      z cell number of current cell on the ray
C  JELEMS  Loop counter over the surface elements
C  JELEMV  Loop counter over the volume elements
C  NEXTIX  x cell number of next cell on ray's path
C  NEXTIY  y cell number of next cell on ray's path
C  NEXTIZ  z cell number of next cell on ray's path
C  NF      Normalising factor: 1/NRAYEL
C  NOCELS  Number of cells the ray has passed through
C  NRAYEL  Number of rays to be fired from current element
C  NX      x cell number of cell at start of ray from a volume element
C  NY      y cell number of cell at start of ray from a volume element
C  NZ      z cell number of cell at start of ray from a volume element
C  PHI     Cone angle of ray from a volume element
C  RAD     Radius of cylinder
C  RANDA   Random number used to calculate alpha
C  RANDB   Random number used to calculate beta
C  SGFRME  Segment number at end of wall containing sending surface
C  SGFRMS  Segment number at start of wall containing sending surface
C  SING    Sine of angle giving direction of sending surface if it is a
C          segment or direction of tangent plane if it is curved surface of
C          a cylinder
C  SP      Sine of phi, the cone angle relative to the grid
C  ST      Sine of theta, the polar angle relative to the grid
C  TEND    Value of the parameter t at the end of the ray in the current cell
C  THETA   Polar angle of ray from a volume element
C  X       x co-ordinate of the start of the ray (returned by START or STARTV)
C  XRATE   Rate of change of x with t along the ray
C  Y       y co-ordinate of the start of the ray (returned by START or STARTV)
C  YRATE   Rate of change of y with t along the ray
C  Z       z co-ordinate at the start of the ray (Returned by START or STARTV)
C  ZRATE   Rate of change of z with t along the ray
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  CONVRT  Convert cone and polar angles relative to the sending surface to
C          cone and polar angles relative to the grid
C  PARAMS  Set the parameters required by subroutines START, CONVRT and TRACK1
C  RAN     Random number generator - may create system dependence
C  START   Select a starting point on a surface for the current ray
C  STARTV  Select a starting point on in a volume for the current ray
C  TRACK1  Track the current ray through the next cell
C******************************************************************************

c-dal-
c      real sumto1(natmmx)
c     open(unit=7,file='nuwhere.d',status='new')
c      do 1 iatm=1,natm
c         sumto1(iatm)=0.
c1      continue
c      write (lustot,1)
c1     format (' halt at element ? ',$)
c      read*, iehalt
c      write (lustot,2)
c2     format (' halt at ray ? ',$)
c      read*, irhalt
c-dal-      
C     Set all view factor arrays to zero
c      CALL SEED(random$timeseed)
      DO 500 JELEMS=1,NELEMS
         DO 200 IELEMS=1,NELEMS
            DO 100 IATM=1,NATM
               VUFCSS(IATM,IELEMS,JELEMS)=0.
100         CONTINUE
200      CONTINUE
         DO 400 IELEMV=1,NELEMV
            DO 300 IATM=1,NATM
               VUFCGS(IATM,IELEMV,JELEMS)=0.
300         CONTINUE
400      CONTINUE
500   CONTINUE
      DO 1000 JELEMV=1,NELEMV
         DO 700 IELEMV=1,NELEMV
            DO 600 IATM=1,NATM
               VUFCGG(IATM,IELEMV,JELEMV)=0.
600         CONTINUE
700      CONTINUE
         DO 900 IELEMS=1,NELEMS
            DO 800 IATM=1,NATM
               VUFCSG(IATM,IELEMS,JELEMS)=0.
800         CONTINUE
900      CONTINUE
1000  CONTINUE

C---------------------------------------------------------------------------
C                 SENDING ELEMENT IS A SURFACE
C---------------------------------------------------------------------------
C     Fire rays from each surface element to calculate view factors:
C     VUFCSS and VUFCSG

      DO 2000 IELEMS=1,NELEMS

C****    Print message to show that the program is still working
         WRITE (LUSTOT,1050) IELEMS,NELEMS
1050     FORMAT (' PROCESSING SURFACE ELEMENT ',I3,' OF ',I3)
	 
C****    Determine the number of rays to be fired from this element
         NRAYEL=NINT(NRAYS*ELAREA(IELEMS))
         IF (NRAYEL.EQ.0) THEN
            WRITE (LUSTOT,1070) IELEMS,ELAREA(IELEMS),NRAYS
1070        FORMAT (' ELEMENT ',I3,' HAS AREA ',G11.4/
     1              ' NO RAYS ARE TO BE FIRED FROM THIS ELEMENT'/
     2              ' RAY DENSITY IS ',I6/
     3              ' SO ALL VIEW FACTORS AND EXCHANGE AREAS',
     4              ' WILL BE ZERO')
            GOTO 2000
         END IF

C****    Set the surface type and determine the parameters for 
C****    START, CONVRT and TRACK1
         ITYPE=SFZN(IELEMS,1,1)
         CALL PARAMS(ITYPE,IELEMS,SFZN,WSEGXS,WSEGYS,COINC,SEGCEL,NSEG,
     1               NV,VXWSEG,NGZ,CELLOB,TYPE,GHI,MXSFZN,MXZNC,NVMX,
     2               NZMX,NOBCMX,NOBMAX,INFO,IOB,ISTYPE,IKIND,INF,SING,
     3               COSG,INITIX,INITIY,INITIZ,SGFRMS,SGFRME)

C****    Fire the rays and track them to their ends
         DO 1600 IRAY=1,NRAYEL
c-dal-
c            if ((ielems.eq.iehalt).and.(iray.eq.irhalt)) then
c               write (lustot,3)
c3	       format (' halted, enter new element and ray halt numbers ',$)
c               read*, iehalt,irhalt
c	    end if
c-dal-

C****       Select a start point for the ray
            CALL START(LUSTOT,IELEMS,1,ITYPE,INITIX,INITIY,INITIZ,
     1                 INFO,IOB,ISTYPE,X,Y,Z,IRAN)

C****       If the sending surface is the curved surface of a cylinder the
C****       angle between the surface and the axes can only be computed
C****       now that the starting point is known
            IF (ISTYPE.EQ.7) THEN
               RAD=GHI(IOB,1)
               IF (INFO.EQ.1) THEN
                  SING=(Z-GLO(IOB,3))/RAD
                  COSG=(Y-GLO(IOB,2))/RAD
               ELSE IF (INFO.EQ.2) THEN
                  SING=(Z-GLO(IOB,3))/RAD
                  COSG=(X-GLO(IOB,1))/RAD
               ELSE
                  SING=(Y-GLO(IOB,2))/RAD
                  COSG=(X-GLO(IOB,1))/RAD
               END IF
            END IF

C****       Select a direction for the ray
C****       Firstly choose angles relative to the sending surface
            RANDA=RANDOM(IRAN)
C            CALL RANDOM(RANDA)
            RANDB=RANDOM(IRAN)
C            CALL RANDOM(RANDB)
            ALPHA=ASIN(SQRT(RANDA))
            BETA=2.*PI*RANDB
C****       Now convert these into angles relative to the axes
            CALL CONVRT(LUSTOT,ALPHA,BETA,IKIND,INF,SING,COSG,
     1                  SP,CP,ST,CT)
C****       Set the direction cosines of the ray
            XRATE=SP*CT
            YRATE=CP
            ZRATE=SP*ST

C****       Initialise the optical thickness array for this ray
            DO 1100 IATM=1,NATM
               OPTHCK(IATM)=0.
1100        CONTINUE

C****       Initialise the current cell to the initial cell on the ray
C****       and set the cell counter to zero
            IX=INITIX
	    IY=INITIY
	    IZ=INITIZ
            NOCELS=0
1200        NOCELS=NOCELS+1
C****       Track the ray through one cell at a time
            CALL TRACK1(X,Y,Z,IX,IY,IZ,ITYPE,SGFRMS,SGFRME,IOB,XRATE,
     1                  YRATE,ZRATE,LUSTOT,COINC,NVMX,NZMX,NOBMAX,
     2                  NOBCMX,MXWLCL,NGZ,GX,GY,GZ,WSEGXS,WSEGYS,CELLWS,
     3                  SGSFZN,CLSFZN,GLO,GHI,NOBST,CELLOB,TYPE,OBSTXS,
     4                  OBSTXE,OBSTYS,OBSTYE,OBSTZS,OBSTZE,OBSFZN,HIT,
     5                  IHIT,TEND,NEXTIX,NEXTIY,NEXTIZ)

C****       Update the surface to gas view factor array to account for
C****       the ray travelling through cell IX,IY,IZ
            JELEMV=CLVLZN(IX,IY,IZ)
            DO 1300 IATM=1,NATM
               DTHICK=TEND*FINGSK(IATM,IX,IY,IZ)
               VUFCSG(IATM,IELEMS,JELEMV)=VUFCSG(IATM,IELEMS,JELEMV)
     1                            +EXP(-OPTHCK(IATM))*(1.-EXP(-DTHICK))
               OPTHCK(IATM)=OPTHCK(IATM)+DTHICK
1300        CONTINUE
C****       If a surface has been hit then update the surface to surface 
C****       view factor array and go on to next ray.  If a hit has not
C****       taken place reset cell number and track through next cell
            IF (HIT) THEN
               DO 1400 IATM=1,NATM
                  VUFCSS(IATM,IELEMS,IHIT)=VUFCSS(IATM,IELEMS,IHIT)
     1                                     +EXP(-OPTHCK(IATM))
c-dal-
c      if (ihit.eq.1) then
c      write (7,1301) ielems,ihit,exp(-opthck(Iatm))
c1301  format (' ray from ',i3,' hits ',i3,' add ',g12.5)
c      sumto1(iatm)=sumto1(iatm)+exp(-opthck(iatm))
c      end if
c-dal-
1400           CONTINUE
            ELSE
               IX=NEXTIX
               IY=NEXTIY
               IZ=NEXTIZ
C****          Check that the ray has not passed through too many cells
               IF (NOCELS.GT.2*NVMAX+NZMAX) THEN
                  WRITE (LUSTOT,1500)
1500              FORMAT (' RAY HAS PASSED THROUGH TOO MANY CELLS'/
     1                    ' ERROR IN TRACKING ROUTINE'/
     2                    ' RUN ABORTED')
                  STOP
               END IF
C****          Track ray through next cell
               GOTO 1200
            END IF
1600     CONTINUE

C****    Normalise the view factors by dividing by the number of rays fired
         NF=1./FLOAT(NRAYEL)
c-dal-
c      do 1602 iatm=1,natm
c         write (7,1603) sumto1(iatm)
c1603     format (' sum to element 1 is ',g11.4)
c1602  continue	 
c      write (7,1601) nf
c1601  format (' normalise view factors, multiply by ',g11.4)
c-dal-
         DO 1900 IATM=1,NATM
            DO 1700 JELEMS=1,NELEMS
               VUFCSS(IATM,IELEMS,JELEMS)=VUFCSS(IATM,IELEMS,JELEMS)*NF
1700        CONTINUE
            DO 1800 JELEMV=1,NELEMV
               VUFCSG(IATM,IELEMS,JELEMV)=VUFCSG(IATM,IELEMS,JELEMV)*NF
1800        CONTINUE
1900     CONTINUE

c-dal-
c      do 1901 iatm=1,natm
c         write (7,1902) vufcss(iatm,ielems,1)
c1902     format (' view factor from ',i3,' to 1 for atmposhere ',i2,1x,g11.5)
c1901  continue
c-dal-
2000  CONTINUE

C---------------------------------------------------------------------------
C                 SENDING ELEMENT IS A VOLUME
C---------------------------------------------------------------------------
C     Fire rays from each volume element to calculate view factors:
C     VUFCGG and VUFCGS

C     If there is no participating gas then this section is not needed
      IF (NATM.EQ.1) GOTO 3500
      
      DO 3000 IELEMV=1,NELEMV

C****    Print message to show that the program is still working
         WRITE (LUSTOT,2050) IELEMV,NELEMV
2050     FORMAT (' PROCESSING VOLUME ELEMENT ',I3,' OF ',I3)
	 
C****    Determine the cell number at the start of this ray
         NX=VLZN(IELEMV,1)
         NY=VLZN(IELEMV,2)
         NZ=VLZN(IELEMV,3)

C****    Determine the number of rays to be fired from this element
         NRAYEL=NINT(NRAYS*FINVOL(NX,NY,NZ))
C****    If no rays are to be fired from this element give a warning message
         IF (NRAYEL.EQ.0) THEN
            WRITE (LUSTOT,2070) IELEMV,NX,NY,NZ,FINVOL(NX,NY,NZ),NRAYS
2070        FORMAT (' VOLUME ELEMENT ',I3,' IS CELL ',I3,',',I3,',',I3/
     1              ' ITS VOLUME IS ',G11.4/
     2              ' NO RAYS ARE TO BE FIRED FROM THIS ELEMENT'/
     3              ' RAY DENSITY IS ',I6/
     4              ' SO ALL VIEW FACTORS AND EXCHANGE AREAS',
     5              ' WILL BE ZERO')
            GOTO 3000
         END IF

C****    Set the parameters for TRACK1
         ITYPE=5
         SGFRMS=0
         SGFRME=0
         IOB=0

C****    Fire the rays and track them to their ends
         DO 2600 IRAY=1,NRAYEL

C****       Select a start point for the ray
            CALL STARTV(NX,NY,NZ,GX,GY,GZ,NVMX,NZMX,NOBCMX,NOBMAX,NOBST,
     1                  CELLOB,TYPE,GLO,GHI,WSEGXS,WSEGYS,CELLWS,MXWLCL,
     2                  COINC,X,Y,Z,IRAN)

C****       Select a direction for the ray
            RANDA=RANDOM(IRAN)
C            CALL RANDOM(RANDA)
            RANDB=RANDOM(IRAN)
C            CALL RANDOM(RANDB)
            PHI=ACOS(1.-2.*RANDA)
            THETA=2.*PI*RANDB
C****       Set the direction cosines of the ray
            XRATE=SIN(PHI)*COS(THETA)
            YRATE=COS(PHI)
            ZRATE=SIN(PHI)*SIN(THETA)

C****       Initialise the optical thickness array for this ray
            DO 2100 IATM=1,NATM
               OPTHCK(IATM)=0.
2100        CONTINUE

C****       Initialise the cell numbers for this ray
            IX=NX
            IY=NY
            IZ=NZ

C****       Track the ray through one cell at a time
            NOCELS=0
2200        NOCELS=NOCELS+1
            CALL TRACK1(X,Y,Z,IX,IY,IZ,ITYPE,SGFRMS,SGFRME,IOB,XRATE,
     1                  YRATE,ZRATE,LUSTOT,COINC,NVMX,NZMX,NOBMAX,
     2                  NOBCMX,MXWLCL,NGZ,GX,GY,GZ,WSEGXS,WSEGYS,CELLWS,
     3                  SGSFZN,CLSFZN,GLO,GHI,NOBST,CELLOB,TYPE,OBSTXS,
     4                  OBSTXE,OBSTYS,OBSTYE,OBSTZS,OBSTZE,OBSFZN,HIT,
     5                  IHIT,TEND,NEXTIX,NEXTIY,NEXTIZ)

C****       Update the gas to gas view factor array to account for
C****       the ray travelling through cell IX,IY,IZ
            JELEMV=CLVLZN(IX,IY,IZ)
            DO 2300 IATM=1,NATM
               DTHICK=TEND*FINGSK(IATM,IX,IY,IZ)
               VUFCGG(IATM,IELEMV,JELEMV)=VUFCGG(IATM,IELEMV,JELEMV)
     1                            +EXP(-OPTHCK(IATM))*(1.-EXP(-DTHICK))
               OPTHCK(IATM)=OPTHCK(IATM)+DTHICK
2300        CONTINUE
C****       If a surface has been hit then update the gas to surface 
C****       view factor array and go on to next ray.  If a hit has not
C****       taken place reset cell number and track through next cell
            IF (HIT) THEN
               DO 2400 IATM=1,NATM
                  VUFCGS(IATM,IELEMV,IHIT)=VUFCGS(IATM,IELEMV,IHIT)
     1                                     +EXP(-OPTHCK(IATM))
2400           CONTINUE
            ELSE
               IX=NEXTIX
               IY=NEXTIY
               IZ=NEXTIZ
C****          Check that the ray has not passed through too many cells
               IF (NOCELS.GT.2*NVMAX+NZMAX) THEN
                  WRITE (LUSTOT,2500)
2500              FORMAT (' RAY HAS PASSED THROUGH TOO MANY CELLS'/
     1                    ' ERROR IN TRACKING ROUTINE'/
     2                    ' RUN ABORTED')
                  STOP
               END IF
C****          Track ray through next cell
               GOTO 2200
            END IF
2600     CONTINUE

C****    Normalise the view factors by dividing by the number of rays fired
         NF=1./FLOAT(NRAYEL)
         DO 2900 IATM=1,NATM
            DO 2700 JELEMS=1,NELEMS
               VUFCGS(IATM,IELEMV,JELEMS)=VUFCGS(IATM,IELEMV,JELEMS)*NF
2700        CONTINUE
            DO 2800 JELEMV=1,NELEMV
               VUFCGG(IATM,IELEMV,JELEMV)=VUFCGG(IATM,IELEMV,JELEMV)*NF
2800        CONTINUE
2900     CONTINUE

3000  CONTINUE
3500  CONTINUE
	    
      RETURN
      END


      SUBROUTINE PARAMS(ITYPE,IELEMS,SFZN,WSEGXS,WSEGYS,COINC,SEGCEL,
     1                  NSEG,NV,VXWSEG,NGZ,CELLOB,TYPE,GHI,MXSFZN,MXZNC,
     2                  NVMAX,NZMAX,NOBCMX,NOBMAX,INFO,IOB,ISTYPE,IKIND,
     3                  INF,SING,COSG,IX,IY,IZ,SGFRMS,SGFRME)
C
C  Routine to calculate the values of the parameters required in VEWFAC
C  to be passed to the subroutines START, CONVRT and TRACK1
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**26/10/90
C
C
      REAL WSEGXS(8*NVMAX),WSEGYS(8*NVMAX),COINC,GHI(NOBMAX,3),SING,
     1     COSG
      INTEGER ITYPE,IELEMS,SFZN(MXSFZN,MXZNC,5),SEGCEL(8*NVMAX,2),
     1        VXWSEG(NVMAX),NGZ,CELLOB(NVMAX,NVMAX,NZMAX,NOBCMX),
     2        MXSFZN,MXZNC,NVMAX,NZMAX,NOBCMX,NOBMAX,INFO,IOB,ISTYPE,
     3        NSEG,NV,IKIND,INF,IX,IY,IZ,SGFRMS,SGFRME
      CHARACTER*1 TYPE(NOBMAX)
      REAL SEGLEN,XS,XE,YS,YE
      INTEGER ISEG,IV
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  CELLOB  Array giving the number of the obstacle in each cell
C  COINC   Largest amount by which two values may differ and still be deemed
C          to coincide
C  GHI     Array of obstacle high co-ordinates
C  IELEMS  Element number of the surface
C  ITYPE   Type of the surface 1-wall segment, 2-cell surface in plane z=0
C          or z=ZMAX, 3-obstacle surface (changed to 4 if obstacle is a box)
C  MXSFZN  Maximum number of surface zones allowed (dimension of some arrays)
C  MXZNC   Maximum number of components a fine zone is allowed to have (NB This
C          must always be 1) (dimension of some arrays)
C  NGZ     Number of z grid lines
C  NOBCMX  Maximum number of obstacles allowed in a cell (dimension of some
C          arrays)
C  NOBMAX  Maximum number of obstacles allowed (dimension of some arrays)
C  NSEG    Number of wall segments in the boundary
C  NV      Number of vertices on the boundary
C  NVMAX   Maximum number of vertices allowed (dimension of some arrays)
C  NZMAX   Maximum number of z slices allowed (dimension of some arrays)
C  SEGCEL  Array giving the x and y cell numbers of the wall segments
C  SFZN    Array containing information about the fine grid surface zones
C          See subroutine ELEMNT for more details
C  TYPE    Array giving the type of each obstacle (B-box, C-cylinder)
C  VXWSEG  Array giving the wall segment number at each vertex
C  WSEGXS  Array of starting x co-ordinates of each wall segment
C  WSEGYS  Array of starting y co-ordinates of each wall segment
C
C**** Arguments - output
C  COSG    Cosine of angle between sending surface and Cartesian axes
C  IKIND   Flag indicating orientation of a surface element
C          1-perpendicular to all planes z=const, 2-plane z=const,
C          3-curved surface of a cylinder
C  INF     Further information about orientation; if IKIND=1 INF is not used;
C          if IKIND=2 INF=1 shows normal out of surface is +k, whilst INF>1
C          shows normal out of surface is -k; if IKIND=3 INF gives the axial
C          direction of the cylinder 1-x, 2-y, 3-z
C  INFO    Further infomation about the surface type; if ITYPE=1 INFO=0 shows
C          a sloping segment, INFO=1 shows a surface along an x grid line, 
C          INFO=2 shows a surface along a y grid line;  if ITYPE=2 INFO=1 shows
C          surface in plane z=0, INFO=1 shows surface in plane z=ZMAX; if 
C          ITYPE=3 INFO gives the axial direction of the cylinder 1-x, 2-y, 3-z
C  IOB     If ITYPE=3 or 4 IOB is the number of the obstacle
C  ISTYPE  If ITYPE=3 or 4 ISTYPE is the obstacle surface number: 1-low x,
C          2-low z, 3-high z, 4-high y, 5-low y, 6-high x, 7-curved surface
C  ITYPE   Surface type (input value only changed if surface is part of a box
C          obstacle) 1-wall segment, 2-cell surface, 3-cylindrical obstacle,
C          4-box obstacle
C  IX      x cell number of surface
C  IY      y cell number of surface
C  IZ      z cell number of surface
C  SGFRME  Only used if ITYPE=1 when SGFRME is the segment number at the end of
C          the wall containing the segment under consideration
C  SGFRMS  Only used if ITYPE=1 when SGFRMS is the segment number at the start
C          of the wall containing the segment under consideration
C  SING    Sine of the angle between sending surface and the Cartesian axes
C
C**** Local variables
C  ISEG    Segment number
C  IV      Vertex number
C  SEGLEN  Segment length
C  XE      x co-ordinate at end of a segment
C  XS      x co-ordinate at start of a segment
C  YE      y co-ordinate at end of a segment
C  YS      y co-ordinate at start of a segment
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C  START  requires the following parameters to be set:
C         ITYPE, INFO, IOB, ISTYPE
C  CONVRT requires the following parameters to be set:
C         IKIND,INF,SING,COSG
C  TRACK1 requires the following parameters to be set:
C         IX,IY,IZ,ITYPE,SGFRMS,SGFRME,IOB
C  NB ITYPE and IOB are required by both START and TRACK1 
C     and so need only be set once

C******************************************************************************

C-------------------------------------------------------------------------
C                           WALL SEGMENT
C-------------------------------------------------------------------------
      IF (ITYPE.EQ.1) THEN

C        Determine segment information
         ISEG=SFZN(IELEMS,1,3)
         XS=WSEGXS(ISEG)
         XE=WSEGXS(ISEG+1)
         YS=WSEGYS(ISEG)
         YE=WSEGYS(ISEG+1)
         SEGLEN=SQRT((XE-XS)*(XE-XS)+(YE-YS)*(YE-YS))

C        Set the parameters
C        For START
         IF (ABS(XE-XS).LE.COINC) THEN
            INFO=1
         ELSE IF (ABS(YE-YS).LE.COINC) THEN
            INFO=2
         END IF
C        Ray is not from an obstacle so IOB and ISTYPE are not required
         IOB=0
         ISTYPE=0
C        For CONVRT
         IKIND=1
C        INF is not required
         INF=0
         SING=(YE-YS)/SEGLEN
         COSG=(XE-XS)/SEGLEN
C        For TRACK1
         IX=SEGCEL(ISEG,1)
         IY=SEGCEL(ISEG,2)
         IZ=SFZN(IELEMS,1,2)
         IV=0
100      IV=IV+1
         IF (IV.LT.NV) THEN
            IF (ISEG.GE.VXWSEG(IV)) GOTO 100
            SGFRMS=VXWSEG(IV-1)
            SGFRME=VXWSEG(IV)-1
	  ELSE
	    IF (ISEG.LT.VXWSEG(NV)) THEN
	       SGFRMS=VXWSEG(NV-1)
	       SGFRME=VXWSEG(NV)-1
	    ELSE
	       SGFRMS=VXWSEG(NV)
	       SGFRME=NSEG
	    END IF
	  END IF

C-------------------------------------------------------------------------
C                    CELL SURFACE IN PLANE z=0 OR z=ZMAX
C-------------------------------------------------------------------------
      ELSE IF (ITYPE.EQ.2) THEN
C        Set the parameters
C        For TRACK1
         IX=SFZN(IELEMS,1,3)
         IY=SFZN(IELEMS,1,4)
         IF (SFZN(IELEMS,1,2).GT.1) THEN
            IZ=NGZ-1
         ELSE
            IZ=1
         END IF
C        Ray is not from a segment or an obstacle so SGFRMS,SGFRME and IOB
C        are not required
         SGFRMS=0
         SGFRME=0
         IOB=0
C        For START
         IF (SFZN(IELEMS,1,2).EQ.1) THEN
            INFO=1
         ELSE
            INFO=2
         END IF
C        Ray is not from an obstacle so ISTYPE is not required
         ISTYPE=0
C        For CONVRT
         IKIND=2
         INF=INFO
C        SING and COSG are not required
         SING=1.
         COSG=1.

C-------------------------------------------------------------------------
C                       OBSTACLE SURFACE
C-------------------------------------------------------------------------
      ELSE IF (ITYPE.EQ.3) THEN
C        Set the parameters
C        For TRACK1
         IX=SFZN(IELEMS,1,2)
         IY=SFZN(IELEMS,1,3)
         IZ=SFZN(IELEMS,1,4)
C        SGFRMS and SGFRME are not required
         SGFRMS=0
         SGFRME=0
C        For START
         IOB=CELLOB(IX,IY,IZ,1)
C        NB ITYPE must be set to 4 for boxes
         IF (TYPE(IOB).EQ.'B') ITYPE=4
         IF (ITYPE.EQ.3) THEN
            INFO=NINT(GHI(IOB,2))
         ELSE
C           Obstacle is a box - INFO is not required
            INFO=0
         END IF
         ISTYPE=SFZN(IELEMS,1,5)
C        For CONVRT
         IF (ISTYPE.EQ.1) THEN
            IKIND=1
C           INF is not required
            INF=0
            SING=1.
            COSG=0.
         ELSE IF (ISTYPE.EQ.2) THEN
            IKIND=2
            INF=2
C           SING and COSG are not required
            SING=1.
            COSG=1.
         ELSE IF (ISTYPE.EQ.3) THEN
            IKIND=2
            INF=1
C           SING and COSG are not required
            SING=1.
            COSG=1.
         ELSE IF (ISTYPE.EQ.4) THEN
            IKIND=1
C           INF is not required
            INF=0
            SING=0.
            COSG=1.
         ELSE IF (ISTYPE.EQ.5) THEN
            IKIND=1
C           INF is not required
            INF=0
            SING=0.
            COSG=-1
         ELSE IF (ISTYPE.EQ.6) THEN
            IKIND=1
C           INF is not required
            INF=0
            SING=-1.
            COSG=0.
         ELSE
            IKIND=3
            INF=NINT(GHI(IOB,2))
C           SING and COSG can only be set properly when the starting
C           point of the ray has been chosen
            SING=1.
            COSG=1.
         END IF   

      END IF

      RETURN
      END
  

      SUBROUTINE STARTV(NX,NY,NZ,GX,GY,GZ,NVMAX,NZMAX,NOBCMX,NOBMAX,
     1                  NOBST,CELLOB,TYPE,GLO,GHI,WSEGXS,WSEGYS,CELLWS,
     2                  MXWLCL,COINC,X,Y,Z,IRAN)
C
C  Routine to determine a start point X,Y,Z for a ray originating in 
C  fine grid cell NX,NY,NZ
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**26/10/90
C
C
      REAL GX(NVMAX),GY(NVMAX),GZ(NVMAX),GLO(NOBMAX,3),GHI(NOBMAX,3),
     1     WSEGXS(8*NVMAX),WSEGYS(8*NVMAX),COINC,X,Y,Z
      INTEGER NX,NY,NZ,NVMAX,NZMAX,NOBCMX,NOBMAX,MXWLCL,
     1        NOBST(NVMAX,NVMAX,NZMAX),CELLOB(NVMAX,NVMAX,NZMAX,NOBCMX),
     2        CELLWS(NVMAX,NVMAX,MXWLCL)
      CHARACTER*1 TYPE(NOBMAX)
      REAL RAND1,RAND2,RAND3,RAD2,DCEN2,XS,XE,YS,YE,G,C,VALXYZ
      INTEGER IDIR,ISEG,K
      INTEGER*4 IRAN
      LOGICAL YES
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  CELLOB  Array giving the number of the obstacle in each cell
C  CELLWS  Array giving the number(s) of the segment(s) in each cell
C  COINC   Largest amount by which two values may differ and still be deemed
C          to coincide
C  GHI     Array of obstacle high co-ordinates
C  GLO     Array of obstacle low co-ordinates
C  GX      Array of fine grid x co-ordinates
C  GY      Array of fine grid y co-ordinates
C  GZ      Array of fine grid z co-ordinates
C  MXWLCL  Maximum number of wall allowed in a cell (dimension of some arrays)
C  NOBCMX  Maximum number of obstacles allowed in a cell (ditto)
C  NOBMAX  Maximum number of obstacles allowed (dimension of some arrays)
C  NOBST   Array giving the number of obstacles in each cell
C  NVMAX   Maximum number of vertices allowed (dimension of some arrays)
C  NX      x cell number
C  NY      y cell number
C  NZ      z cell number
C  NZMAX   Maximum number of z slices allowed (dimension of some arrays)
C  TYPE    Array giving the type of each obstacle (box or cylinder)
C  WSEGXS  Array giving the starting x co-ordinate of each wall segment
C  WSEGYS  Array giving the starting y co-ordinate of each wall segment
C
C**** Arguments - output
C  X       x co-ordinate of the start point
C  Y       y co-ordinate of the start point
C  Z       z co-ordinate of the start point
C
C**** Local variables
C  C       y intercept in the equation of a sloping wall
C  DCEN2   Square of the distance to the axis of a cylinder
C  G       Gradient of the equation of a sloping wall
C  IDIR    Axial direction of a cylinder (1-x, 2-y, 3-z)
C  IRAN    Integer variable needed by random number generator
C  ISEG    Wall segment number
C  K       Loop counter over wall segments
C  RAD2    Square of the radius of a cylinder
C  RAND1   Random number 1
C  RAND2   Random number 2
C  RAND3   Random number 3
C  VALXYZ  Value of the equation of a sloping wall at X,Y,Z
C  XE      x co-ordinate at the end of a wall segment
C  XS      x co-ordinate at the start of a wall segment
C  YE      y co-ordinate at the end of a wall segment
C  YS      y co-ordinate at the start of a wall segment
C  YES     Logical variable indicating if a value is in a specified range
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  TEST    Test if a value is within a specified range
C
C******************************************************************************

C     Generate 3 random numbers
100   RAND1=RANDOM(IRAN)
      RAND2=RANDOM(IRAN)
      RAND3=RANDOM(IRAN)
C100   CALL RANDOM(RAND1)
C      CALL RANDOM(RAND2)
C      CALL RANDOM(RAND3)
C     Set possible starting co-ordinates
      X=GX(NX)+RAND1*(GX(NX+1)-GX(NX))
      Y=GY(NY)+RAND2*(GY(NY+1)-GY(NY))
      Z=GZ(NZ)+RAND3*(GZ(NZ+1)-GZ(NZ))

C     Is X,Y,Z within an obstacle?
      IF (NOBST(NX,NY,NZ).GT.0) THEN
         IOB=CELLOB(NX,NY,NZ,1)
         IF (TYPE(IOB).EQ.'B') THEN
C           If X,Y,Z is within obstacle go back and try another point
            CALL TEST(X,GLO(IOB,1),GHI(IOB,1),YES)
            IF (YES) CALL TEST(Y,GLO(IOB,2),GHI(IOB,2),YES)
            IF (YES) CALL TEST(Z,GLO(IOB,3),GHI(IOB,3),YES)
            IF (YES) GOTO 100
         ELSE 
            RAD2=GHI(IOB,1)*GHI(IOB,1)
            IDIR=NINT(GHI(IOB,2))
C           If X,Y,Z is within obstacle go back and try another point
            IF (IDIR.EQ.1) THEN
               CALL TEST(X,GLO(IOB,1),GHI(IOB,3),YES)
               IF (YES) THEN
                  DCEN2=(Y-GLO(IOB,2))*(Y-GLO(IOB,2))
     1                  +(Z-GLO(IOB,3))*(Z-GLO(IOB,3))
                  IF (DCEN2.LE.RAD2) GOTO 100
               END IF
            ELSE IF (IDIR.EQ.2) THEN
               CALL TEST(Y,GLO(IOB,2),GHI(IOB,3),YES)
               IF (YES) THEN
                  DCEN2=(X-GLO(IOB,1))*(X-GLO(IOB,1))
     1                  +(Z-GLO(IOB,3))*(Z-GLO(IOB,3))
                  IF (DCEN2.LE.RAD2) GOTO 100
               END IF
            ELSE
               CALL TEST(Z,GLO(IOB,3),GHI(IOB,3),YES)
               IF (YES) THEN
                  DCEN2=(Y-GLO(IOB,2))*(Y-GLO(IOB,2))
     1                  +(X-GLO(IOB,1))*(X-GLO(IOB,1))
                  IF (DCEN2.LE.RAD2) GOTO 100
               END IF
            END IF
         END IF
      END IF

C     Is X,Y,Z in the part of the cell outside the enclosure
C     Only slpoing walls can make part of the cell outside the enclosure
      DO 200 K=1,MXWLCL
         ISEG=CELLWS(NX,NY,K)
         IF (ISEG.GT.0) THEN
            XS=WSEGXS(ISEG)
            XE=WSEGXS(ISEG+1)
            YS=WSEGYS(ISEG)
            YE=WSEGYS(ISEG+1)
            IF (ABS(XE-XS).LE.COINC) THEN
C              Wall is along an x grid line and so is not sloping
               GOTO 200
            ELSE IF (ABS(YE-YS).LE.COINC) THEN
C              Wall is along a y grid line and so is not sloping
               GOTO 200
            ELSE
C              Wall is sloping - calculate gradient and y intercept
               G=(YE-YS)/(XE-XS)
               C=(XE*YS-XS*YE)/(XE-XS)
C              Equation of wall is y-gx-c=0.  The sign of y-gx-c at points
C              not on the wall indicates which side of the wall they are on
               VALXYZ=Y-G*X-C
               IF (XE.LT.XS) THEN
C                 Left of wall is the inside of the enclosure
C                 Value>0 implies right of wall so need to try another point
                  IF (VALXYZ.GE.0.) GOTO 100
               ELSE
C                 Right of wall is the inside of the enclosure
C                 Value<0 implies left of wall so need to try another point
                  IF (VALXYZ.LE.0.) GOTO 100
               END IF
            END IF
         ELSE
C           No more walls left to test
            GOTO 300
         END IF
200   CONTINUE

300   CONTINUE

      RETURN
      END


      SUBROUTINE TRACK1(X,Y,Z,IX,IY,IZ,OTYPE,SGFRMS,SGFRME,OBFROM,
     1                  XRATE,YRATE,ZRATE,
     2                  LUSTOT,COINC,NVMAX,NZMAX,NOBMAX,NOBCMX,MXWLCL,
     3                  NGZ,GX,GY,GZ,WSEGXS,WSEGYS,CELLWS,SGSFZN,CLSFZN,
     4                  GLO,GHI,NOBST,CELLOB,TYPE,OBSTXS,OBSTXE,OBSTYS,
     5                  OBSTYE,OBSTZS,OBSTZE,OBSFZN,
     6                  HIT,IHIT,TEND,NEXTIX,NEXTIY,NEXTIZ)

C
C  Routine to track a ray through cell IX,IY,IZ and determine if it hits
C  a surface within this cell or passes on to another cell
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**24/10/90
C
C
      REAL X,Y,Z,XRATE,YRATE,ZRATE,COINC,GX(NVMAX),GY(NVMAX),GZ(NVMAX),
     1     WSEGXS(8*NVMAX),WSEGYS(8*NVMAX),GLO(NOBMAX,3),GHI(NOBMAX,3),
     2     TEND
      INTEGER IX,IY,IZ,OTYPE,SGFRMS,SGFRME,OBFROM,LUSTOT,NVMAX,NZMAX,
     1        NOBMAX,NOBCMX,MXWLCL,SGSFZN(NZMAX-1,8*NVMAX),
     2        CELLWS(NVMAX,NVMAX,MXWLCL),OBSTXS(NOBMAX),OBSTXE(NOBMAX),
     3        OBSTYS(NOBMAX),OBSTYE(NOBMAX),OBSTZS(NOBMAX),
     4        OBSTZE(NOBMAX),OBSFZN(NVMAX,NVMAX,NZMAX,7),
     5        NOBST(NVMAX,NVMAX,NZMAX),CELLOB(NVMAX,NVMAX,NZMAX,NOBCMX),
     6        CLSFZN(2,NVMAX,NVMAX),NGZ,
     7        IHIT,NEXTIX,NEXTIY,NEXTIZ,IOB,HTSURF,K,ISG,
     8        INCCLX,INCCLY,INCCLZ,INCGDX,INCGDY,INCGDZ
      CHARACTER*1 TYPE(NOBMAX),OBTYPE
      LOGICAL HIT,XEND,YEND,ZEND,YES
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  CELLOB  Array giving the numbers of each of the obstacles in each cell
C  CELLWS  Array giving the numbers of each of the wall segments in each cell
C  CLSFZN  Array giving the element numbers of the cell surfaces in the end
C          z planes (ie z=0 and z=ZMAX)
C  COINC   Largest amount by which two values may differ and still be deemed
C          to coincide
C  GHI     Array of obstacle high co-ordinates
C  GLO     Array of obstacle low co-ordinates
C  GX      Array of fine grid x co-ordinates
C  GY      Array of fine grid y co-ordinates
C  GZ      Array of fine grid z co-ordinates
C  IX      x cell number
C  IY      y cell number
C  IZ      z cell number
C  LUSTOT  Logical unit number for standard output
C  MXWLCL  Maximum number of walls allowed in a cell (dimension of some arrays)
C  NGZ     Number of z grid lines
C  NOBCMX  Maximum number of obstacles allowed in a cell (ditto)
C  NOBMAX  Maximum number of obstacles allowed (dimension of some arrays)
C  NOBST   Array giving the number of obstacles in each cell
C  NVMAX   Maximum number of vertices allowed (dimension of some arrays)
C  NZMAX   Maximum number of z slices allowed (dimension of some arrays)
C  OBFROM  The number of the obstacle from which the ray originated if it 
C          came from an obstacle (OTYPE=3 or 4) otherwise zero
C  OBSFZN  Array giving the element number of each obstacle surface
C  OBSTXE  Array of obstacle ending x cell numbers
C  OBSTXS  Array of obstacle starting x cell numbers
C  OBSTYE  Array of obstacle ending y cell numbers
C  OBSTYS  Array of obstacle starting y cell numbers
C  OBSTZE  Array of obstacle ending z cell numbers
C  OBSTZS  Array of obstacle starting z cell numbers
C  OTYPE   Type of origin of ray (1 - wall segment, 2 - cell surface,
C          3 - cylindrical obstacle surface, 4 - box obstacle surface,
C          5 - volume)
C  SGFRMS  If OTYPE=1 (ie ray originates on a boundary segment) this is the 
C          segment number at the start of the wall to which the originating 
C          segment belongs otherwise zero
C  SGFRME  If OTYPE=1 (ie ray originates on a boundary segment) this is the
C          segment number at the end of the wall to which the originating 
C          segment belongs otherwise zero
C  SGSFZN  Array giving the element number of each wall segment
C  TYPE    Array giving the types of the obstacles
C  WSEGXS  Array giving the starting x co-ordinates of each wall segment
C  WSEGYS  Array giving the starting y co-ordinates of each wall segment
C  X       x co-ordinate of entry point to this cell (changed by TRACK1)
C  XRATE   Rate of change of x with t along the ray  
C  Y       y co-ordinate of entry point to this cell (changed by TRACK1)
C  YRATE   Rate of change of y with t along the ray
C  Z       z co-ordinate of entry point to this cell (changed by TRACK1)
C  ZRATE   Rate of change of z with t along the ray
C
C**** Arguments - output
C  HIT     Logical flag indicating if a surface has been hit
C  IHIT    If HIT is true the element number of the surface that has been hit
C  NEXTIX  x cell number of next cell on ray's path
C  NEXTIY  y cell number of next cell on ray's path
C  NEXTIZ  z cell number of next cell on ray's path
C  TEND    Value of the parameter t at the end of the ray in this cell (ie at
C          the surface which is hit or at the end of the cell)
C  X       x co-ordinate of exit point from this cell (if the ray exits)
C  Y       y co-ordinate of exit point from this cell (if the ray exits)
C  Z       z co-ordinate of exit point from this cell (if the ray exits)
C
C**** Local variables
C  HTSURF  Obstacle surface number which the ray hits (1 -low x, 2 -low z,
C          3 - high z, 4 - high y, 5 - low y, 6 - high x, 7 - curved)
C  INCCLX  Increment to x cell number if ray leaves through x face
C  INCCLY  Increment to y cell number if ray leaves through y face
C  INCCLZ  Increment to z cell number if ray leaves through z face
C  INCGDX  Increment to x grid line number if ray leaves through x face
C  INCGDY  Increment to y grid line number if ray leaves through y face
C  INCGDZ  Increment to z grid line number if ray leaves through z face
C  IOB     Current obstacle number
C  ISG     Current segment number
C  K       Loop counter over the wall segments
C  OBTYPE  Type of the current obstacle (B (box) or C (cylinder))
C  XEND    Logical flag indicating if ray leaves cell by an x face
C  YEND    Logical flag indicating if ray leaves cell by an y face
C  YES     Logical flag indicating if a value lies in a specified range
C  ZEND    Logical flag indicating if ray leaves cell by an z face
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  RAYOBS  Check if ray hits a specified obstacle
C  RAYSEG  Check if ray hits a specified wall segment
C  TEST    Test if a value lies within a certain range
C  WHOLCL  Find the exit point of the ray from the whole cell (ignoring
C          all obstacles and wall segments)
C
C******************************************************************************

C     Initalise output variables
      HIT=.FALSE.
      IHIT=0
      TEND=0.
      NEXTIX=IX
      NEXTIY=IY
      NEXTIZ=IZ

C-----------------------------------------------------------------------------
C                           OBSTACLES
C-----------------------------------------------------------------------------
C     If there is an obstacle see if the ray hits it
      IF (NOBST(IX,IY,IZ).GT.0) THEN
         IOB=CELLOB(IX,IY,IZ,1)
C        If the ray originated from obstacle IOB it cannot hit it
C        so only process obstacles from which the ray did not originate
         IF (((OTYPE.NE.3).AND.(OTYPE.NE.4)).OR.(IOB.NE.OBFROM)) THEN
            OBTYPE=TYPE(IOB)
            CALL RAYOBS(X,Y,Z,IX,IY,IZ,XRATE,YRATE,ZRATE,GX,GY,GZ,IOB,
     1                  GLO,GHI,OBSTXS,OBSTXE,OBSTYS,OBSTYE,OBSTZS,
     2                  OBSTZE,OBTYPE,COINC,NVMAX,NZMAX,NOBMAX,LUSTOT,
     3                  HIT,HTSURF,TEND)
C           If the ray hit a surface determine its element number then
C           skip to the end as the ray's destiny is determined
            IF (HIT) THEN
               IHIT=OBSFZN(IX,IY,IZ,HTSURF)
               GOTO 9000
            END IF
         END IF
      END IF

C-----------------------------------------------------------------------------
C                           WALL SEGMENTS
C-----------------------------------------------------------------------------
C     The ray has not hit an obstacle, see if it hits a wall segment
      DO 100 K=1,MXWLCL
         ISG=CELLWS(IX,IY,K)
         IF (ISG.GT.0) THEN
C           This segment cannot be hit if it is part of the wall from 
C           which the ray originated
            CALL TEST(FLOAT(ISG),FLOAT(SGFRMS),FLOAT(SGFRME),YES)
            IF (.NOT.YES) THEN
               CALL RAYSEG(X,Y,Z,IX,IY,IZ,XRATE,YRATE,ZRATE,GX,GY,GZ,
     1                     ISG,WSEGXS,WSEGYS,NVMAX,NZMAX,COINC,HIT,TEND)
C              If the ray hit a surface determine its element number then
C              skip to the end as the ray's destiny is determined
               IF (HIT) THEN
                  IHIT=SGSFZN(IZ,ISG)
                  GOTO 9000
               END IF
            END IF
         ELSE
C           No more wall segments in this cell so exit from this loop
            GOTO 200
         END IF
100    CONTINUE

C-----------------------------------------------------------------------------
C                           UNOBSTRUCTED CELL
C-----------------------------------------------------------------------------
C     The ray has not hit an obstacle or a wall segment so it leaves
C     the cell through one of the planar faces of the cuboid
200   CALL WHOLCL(X,Y,Z,IX,IY,IZ,XRATE,YRATE,ZRATE,GX,GY,GZ,NVMAX,NZMAX,
     1            COINC,XEND,YEND,ZEND,TEND)

C-----------------------------------------------------------------------------
C                           CELL FACES z=0 OR z=ZMAX
C-----------------------------------------------------------------------------
C     If the ray leaves through a z face it may pass through one of the 
C     bounding planes of the enclosure ie z=0 or z=ZMAX 
C     If it does  a hit occurs
      IF (ZEND) THEN
         IF (ZRATE.GT.0) THEN
            NEXTIZ=IZ+1
         ELSE
            NEXTIZ=IZ-1
         END IF
         IF (NEXTIZ.LT.1) THEN
            HIT=.TRUE.
            IHIT=CLSFZN(1,IX,IY)
            GOTO 9000
         ELSE IF (NEXTIZ.GT.NGZ-1) THEN
            HIT=.TRUE.
            IHIT=CLSFZN(2,IX,IY)
            GOTO 9000
         END IF
      END IF

C-----------------------------------------------------------------------------
C                     NO HIT - SET NEXT CELL AND EXIT POINT
C-----------------------------------------------------------------------------
C     The ray hits nothing in the cell so must pass on to another cell
C     Determine the number of that cell and the co-ordinates of the exit point
C     from this cell which is also the entry point to the next cell

C     First set cell and grid line increments
      IF (XRATE.GT.0.) THEN
         INCCLX=+1
         INCGDX=+1
      ELSE
         INCCLX=-1
         INCGDX=0
      END IF
      IF (YRATE.GT.0.) THEN
         INCCLY=+1
         INCGDY=+1
      ELSE
         INCCLY=-1
         INCGDY=0
      END IF
      IF (ZRATE.GT.0.) THEN
         INCCLZ=+1
         INCGDZ=+1
      ELSE
         INCCLZ=-1
         INCGDZ=0
      END IF

C     Set next cell numbers and exit(entry) point
      IF (XEND.AND.YEND.AND.ZEND) THEN
C        Ray leaves at a vertex
         X=GX(IX+INCGDX)
         Y=GY(IY+INCGDY)
         Z=GZ(IZ+INCGDZ)
         NEXTIX=IX+INCCLX
         NEXTIY=IY+INCCLY
         NEXTIZ=IZ+INCCLZ
      ELSE IF (XEND.AND.YEND) THEN
C        Ray leaves at an edge
         X=GX(IX+INCGDX)
         Y=GY(IY+INCGDY)
         Z=Z+TEND*ZRATE
         NEXTIX=IX+INCCLX
         NEXTIY=IY+INCCLY
         NEXTIZ=IZ
      ELSE IF (XEND.AND.ZEND) THEN
C        Ray leaves at an edge
         X=GX(IX+INCGDX)
         Y=Y+TEND*YRATE
         Z=GZ(IZ+INCGDZ)
         NEXTIX=IX+INCCLX
         NEXTIY=IY
         NEXTIZ=IZ+INCCLZ
      ELSE IF (YEND.AND.ZEND) THEN
C        Ray leaves at an edge
         X=X+TEND*XRATE
         Y=GY(IY+INCGDY)
         Z=GZ(IZ+INCGDZ)
         NEXTIX=IX
         NEXTIY=IY+INCCLY
         NEXTIZ=IZ+INCCLZ
      ELSE IF (XEND) THEN
C        Ray leaves through an x face
         X=GX(IX+INCGDX)
         Y=Y+TEND*YRATE
         Z=Z+TEND*ZRATE
         NEXTIX=IX+INCCLX
         NEXTIY=IY
         NEXTIZ=IZ
      ELSE IF (YEND) THEN
C        Ray leaves through an y face
         X=X+TEND*XRATE
         Y=GY(IY+INCGDY)
         Z=Z+TEND*ZRATE
         NEXTIX=IX
         NEXTIY=IY+INCCLY
         NEXTIZ=IZ
      ELSE IF (ZEND) THEN
C        Ray leaves through an z face
         X=X+TEND*XRATE
         Y=Y+TEND*YRATE
         Z=GZ(IZ+INCGDZ)
         NEXTIX=IX
         NEXTIY=IY
         NEXTIZ=IZ+INCCLZ
      ELSE
C        Ray does not leave the cell at all
         WRITE (LUSTOT,1000) IX,IY,IZ
1000     FORMAT (' ERROR IN TRACK1'/
     1           ' RAY DOES NOT LEAVE CELL ',I3,',',I3,',',I3/
     2           ' ENTRY POINT ',G11.4,',',G11.4,',',G11.4/
     3           ' XRATE, YRATE, ZRATE ARE: ',3(G11.4,2X)/
     4           ' RUN ABORTED')
         STOP
      END IF

C     Exit from the routine
9000  CONTINUE

      RETURN
      END

 
      SUBROUTINE RAYOBS(X,Y,Z,IX,IY,IZ,XRATE,YRATE,ZRATE,GX,GY,GZ,
     1                  IOB,GLO,GHI,OBSTXS,OBSTXE,OBSTYS,OBSTYE,OBSTZS,
     2                  OBSTZE,OBTYPE,COINC,NVMAX,NZMAX,NOBMAX,LUSTOT,
     3                  HIT,HTSURF,TOB)
C
C  Routine to determine if the ray entering cell IX,IY,IZ at the point X,Y,Z
C  and travelling in the direction given by XRATE,YRATE,ZRATE will hit the
C  box shpaed obstacle, number IOB
C  
C*****VERSION 2.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**23/10/90
C
C
      REAL X,Y,Z,XRATE,YRATE,ZRATE,GX(NVMAX),GY(NVMAX),GZ(NZMAX),
     1     GLO(NOBMAX,3),GHI(NOBMAX,3),COINC,TOB
      INTEGER IX,IY,IZ,IOB,OBSTXS(NOBMAX),OBSTXE(NOBMAX),OBSTYS(NOBMAX),
     1        OBSTYE(NOBMAX),OBSTZS(NOBMAX),OBSTZE(NOBMAX),NVMAX,NZMAX,
     2        NOBMAX,LUSTOT,HTSURF
      LOGICAL HIT
      CHARACTER OBTYPE*1
      REAL OBXS,OBXE,OBYS,OBYE,OBZS,OBZE,TXLO,TXHI,TYLO,TYHI,TZLO,TZHI,
     1     YTXLO,ZTXLO,YTXHI,ZTXHI,ZTYLO,XTYLO,ZTYHI,XTYHI,XTZLO,YTZLO,
     2     XTZHI,YTZHI,RATE1,RATE2,RATE3,STPT1,STPT2,STPT3,CENCO1,
     3     CENCO2,OBS,OBE,A,B,C,D,PROD,TCURV,COORD3
      LOGICAL YES1,YES2 
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  COINC   Largest amount by which two values may differ and still be
C          deemed to coincide
C  GHI     Array giving the high co-ordinates of the obstacles
C  GLO     Array giving the low co-ordinates of the obstacles
C  GX      Array giving the fine grid x co-ordinates
C  GY      Array giving the fine grid y co-ordinates
C  GZ      Array giving the fine grid z co-ordinates
C  IOB     Number of the obstacle in the cell
C  IX      x cell number
C  IY      y cell number
C  IZ      z cell number
C  LUSTOT  Logical unit number for standard output
C  NOBMAX  Maximum number of obstacles allowed (dimension of some arrays)
C  NVMAX   Maximum number of vertices allowed (dimension of some arrays)
C  NZMAX   Maximum number of z slices allowed (dimension of some arrays)
C  OBSTXE  Array holding the ending x cell numbers of the obstacles
C  OBSTXS  Array holding the starting x cell numbers of the obstacles
C  OBSTYE  Array holding the ending y cell numbers of the obstacles
C  OBSTYS  Array holding the starting y cell numbers of the obstacles
C  OBSTZE  Array holding the ending z cell numbers of the obstacles
C  OBSTZS  Array holding the starting z cell numbers of the obstacles
C  OBTYPE  Character variable with value B or C showing the type of obstacle
C          (ie box or cylinder)
C  X       x co-ordinate of entry point of ray to the cell
C  XRATE   Rate at which x changes with t along the ray
C  Y       y co-ordinate of entry point of ray to cell
C  YRATE   Rate at which y changes with t along the ray
C  Z       z co-ordinate of entry point of ray to cell
C  ZRATE   Rate at which z changes with t along the ray
C
C**** Arguments - output
C  HIT     Logical flag indicating if the obstacle is hit
C  HTSURF  Obstacle surface number which is hit (if a hit takes place)
C          1-low x, 2-low z, 3-high z, 4-high y, 5-low y, 6-high x
C  TOB     Value of the parameter t at the obstacle (if the obstacle is hit)
C
C**** Local variables
C  A       Coefficient of t**2 in the quadratic to be solved to find the value
C          of t when the ray hits the curved surface of a cylinder
C  B       Coefficient of t in the quadratic to be solved to find the value
C          of t when the ray hits the curved surface of a cylinder
C  C       Constant in the quadratic to be solved to find the value of t when 
C          the ray hits the curved surface of a cylinder
C  CENCO1  Value of first non-axial co-ordinate at centre of plane face
C  CENCO2  Value of second non-axial co-ordinate at centre of plane face
C  COORD3  Value of the axial co-ordinate when the ray hits the curved surface
C          of a cylinder
C  D       Discriminant (b*b-4ac) of the quadratic to be solved to find the
C          value of t when the ray hits the curved surface of a cylinder
C  D2CEN   Square of the distance from a point to the axis of a cylinder
C  IDIR    Integer indicating the direction of the axis of a cylinder
C          1-parallel to x axis, 2-parallel to y axis, 3-parallel to z axis
C  OBE     Ending axial co-ordinate of the obstacle in this cell
C  OBS     Starting axial co-ordinate of the obstacle in this cell
C  OBXE    Ending x co-ordinate of the obstacle in this cell
C  OBXS    Starting x co-ordinate of the obstacle in this cell
C  OBYE    Ending y co-ordinate of the obstacle in this cell
C  OBYS    Starting y co-ordinate of the obstacle in this cell
C  OBZE    Ending z co-ordinate of the obstacle in this cell
C  OBZS    Starting z co-ordinate of the obstacle in this cell
C  PROD    The product of the roots of the quadratic to be solved to find the
C          value of t when the ray hits the curved surface of a cylinder
C  RAD2    Square of the radius of a cylinder
C  RATE1   Rate of change with t of first non-axial co-ordinate
C  RATE2   Rate of change with t of second non-axial co-ordinate
C  RATE3   Rate of change with t of axial co-ordinate
C  STPT1   Value of first non-axial co-ordinate on entry to the cell
C  STPT2   Value of second non-axial co-ordinate on entry to the cell
C  STPT3   Value of axial co-ordinate on entry to the cell
C  TCURV   Value of t at the curved surface of a cylinder
C  TXHI    Value of t at the high x surface of the obstacle
C  TXLO    Value of t at the low x surface of the obstacle
C  TYHI    Value of t at the high y surface of the obstacle
C  TYLO    Value of t at the low y surface of the obstacle
C  TZHI    Value of t at the high z surface of the obstacle
C  TZLO    Value of t at the low z surface of the obstacle
C  XTYHI   Value of x at TYHI
C  XTYLO   Value of x at TYLO
C  XTZHI   Value of x at TZHI
C  XTZLO   Value of x at TZLO
C  YES1    Logical flag indicating if a co-ordinate is within obstacle
C  YES2    Logical flag indicating if a co-ordinate is within obstacle
C  YTXHI   Value of y at TXHI
C  YTXLO   Value of y at TXLO
C  YTZHI   Value of y at TZHI
C  YTZLO   Value of y at TZLO
C  ZTXHI   Value of z at TXHI
C  ZTXLO   Value of z at TXLO
C  ZTYHI   Value of z at TYHI
C  ZTYLO   Value of z at TYLO
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  TEST    Test if a value lies between 2 others
C
C******************************************************************************
C  Note: throughout this routine the equation of the ray is taken in 
C  parametric form to be (x+xrate*t,y+yrate*t,z+zrate*t) where the direction
C  cosines xrate, yrate, zrate are given in terms of the cone and polar angles
C  phi and theta by: 
C     xrate=sin(phi)cos(theta), yrate=cos(theta), zrate=sin(phi)sin(theta)
C******************************************************************************

C     Determine limits within this cell of the obstacle
C     NB If the obstacle is a cylinder only the limits in the axial
C     direction are required
      IF (OBTYPE.EQ.'B') THEN
         OBXS=MAX(GX(IX),GLO(IOB,1))
         OBXE=MIN(GX(IX+1),GHI(IOB,1))
         OBYS=MAX(GY(IY),GLO(IOB,2))
         OBYE=MIN(GY(IY+1),GHI(IOB,2))
         OBZS=MAX(GZ(IZ),GLO(IOB,3))
         OBZE=MIN(GZ(IZ+1),GHI(IOB,3))
      ELSE
C        For cylinders set the axial direction and the radius squared as well
C        as the axial limits
C23456789012345678901234567890123456789012345678901234567890123456789012
         IDIR=NINT(GHI(IOB,2))
         RAD2=GHI(IOB,1)*GHI(IOB,1)
         IF (IDIR.EQ.1) THEN
	    OBXS=MAX(GX(IX),GLO(IOB,1))
	    OBXE=MIN(GX(IX+1),GHI(IOB,3))
	 ELSE IF (IDIR.EQ.2) THEN
	    OBYS=MAX(GY(IY),GLO(IOB,2))
	    OBYE=MIN(GY(IY+1),GHI(IOB,3))
	 ELSE
	    OBZS=MAX(GZ(IZ),GLO(IOB,3))
	    OBZE=MIN(GZ(IZ+1),GHI(IOB,3))
	 END IF
      END IF

C     A hit has not yet been found so set HIT to false
      HIT=.FALSE.

C     The geometry is such that if a ray hits one surface it cannot
C     hit another, so once a hit has been found we can skip over the 
C     remaining checks.  
C     The structure of what follows is:
C        See if ray hits an x end (if there are any)
C            If xrate>0 only low x end can be hit
C            If xrate<0 only high x end can be hit
C            If xrate=0 no x end can be hit
C        See if ray hits a y end (if there are any)
C            If yrate>0 only low y end can be hit
C            If yrate<0 only high y end can be hit
C            If yrate=0 no x end can be hit
C        See if ray hits a y end (if there are any)
C            If zrate>0 only low z end can be hit
C            If zrate<0 only high z end can be hit
C            If zrate=0 no z end can be hit
C        See if ray hits the curved surface (if there is one)
C        Set parameters and return

C-----------------------------------------------------------
C                  Is an x end hit?
C-----------------------------------------------------------
C     If the obstacle is a cylinder it only has x ends if x is
C     the axial direction - ie IDIR=1.  If this is not the case
C     skip down to see if a y end is hit.
      IF ((OBTYPE.EQ.'C').AND.(IDIR.NE.1)) GOTO 2000
C     The obstacle has x ends so see if one of them is hit
      IF (XRATE.GT.0.) THEN
C        Ray can only hit low x end - is low x end there to be hit?
         IF (OBSTXS(IOB).EQ.IX) THEN
C           Low x end is there - has ray entered cell beyond it?
            IF (X.LE.OBXS+COINC) THEN
C              Ray has entered before low x end
C              Find t when ray reaches plane of low x end
               TXLO=(OBXS-X)/XRATE
               TXLO=MAX(0.,TXLO)
C              Determine y and z co-ordinates at this point on ray 
C              and see if they lie within the low x face
               YTXLO=Y+TXLO*YRATE
               ZTXLO=Z+TXLO*ZRATE
               IF (OBTYPE.EQ.'B') THEN
                  CALL TEST(YTXLO,OBYS,OBYE,YES1)
                  CALL TEST(ZTXLO,OBZS,OBZE,YES2)
                  HIT=YES1.AND.YES2
               ELSE
                  D2CEN=(YTXLO-GLO(IOB,2))**2+(ZTXLO-GLO(IOB,3))**2
                  IF (D2CEN.LE.RAD2) HIT=.TRUE.
               END IF
               IF (HIT) THEN
C                 A hit takes place - set parameters and skip to end
                  HTSURF=1
                  TOB=TXLO
                  GOTO 9000
               END IF
            END IF
         END IF

      ELSE IF (XRATE.LT.0.) THEN
C        Ray can only hit high x end - is high x end there to be hit?
         IF (OBSTXE(IOB).EQ.IX) THEN
C           High x end is there - has ray entered cell before it?
            IF (X.GE.OBXE-COINC) THEN
C              Ray has entered after high x end
C              Find t when ray reaches plane of high x end
               TXHI=(OBXE-X)/XRATE
               TXHI=MAX(0.,TXHI)
C              Determine y and z co-ordinates at this point on ray 
C              and see if they lie within the high x face
               YTXHI=Y+TXHI*YRATE
               ZTXHI=Z+TXHI*ZRATE
               IF (OBTYPE.EQ.'B') THEN
                  CALL TEST(YTXHI,OBYS,OBYE,YES1)
                  CALL TEST(ZTXHI,OBZS,OBZE,YES2)
                  HIT=YES1.AND.YES2
               ELSE
                  D2CEN=(YTXHI-GLO(IOB,2))**2+(ZTXHI-GLO(IOB,3))**2
                  IF (D2CEN.LE.RAD2) HIT=.TRUE.
               END IF
               IF (HIT) THEN
C                 A hit takes place - set parameters and skip to end
                  HTSURF=6
                  TOB=TXHI
                  GOTO 9000
               END IF
            END IF
         END IF

      ELSE
C        xrate=0 so no x end can be hit
         CONTINUE

      END IF

C-----------------------------------------------------------
C                  Is a y end hit?
C-----------------------------------------------------------
C     If the obstacle is a cylinder it only has y ends if y is
C     the axial direction - ie IDIR=2.  If this is not the case
C     skip down to see if a z end is hit.
2000  IF ((OBTYPE.EQ.'C').AND.(IDIR.NE.2)) GOTO 3000
C     The obstacle has y ends so see if one of them is hit
      IF (YRATE.GT.0.) THEN
C        Ray can only hit low y end - is low y end there to be hit?
         IF (OBSTYS(IOB).EQ.IY) THEN
C           Low y end is there - has ray entered cell beyond it?
            IF (Y.LE.OBYS+COINC) THEN
C              Ray has entered before low y end
C              Find t when ray reaches plane of low y end
               TYLO=(OBYS-Y)/YRATE
               TYLO=MAX(0.,TYLO)
C              Determine x and z co-ordinates at this point on ray 
C              and see if they lie within the low y face
               XTYLO=X+TYLO*XRATE
               ZTYLO=Z+TYLO*ZRATE
               IF (OBTYPE.EQ.'B') THEN
                  CALL TEST(XTYLO,OBXS,OBXE,YES1)
                  CALL TEST(ZTYLO,OBZS,OBZE,YES2)
                  HIT=YES1.AND.YES2
               ELSE
                  D2CEN=(XTYLO-GLO(IOB,1))**2+(ZTYLO-GLO(IOB,3))**2
                  IF (D2CEN.LE.RAD2) HIT=.TRUE.
               END IF
               IF (HIT) THEN
C                 A hit takes place - set parameters and skip to end
                  HTSURF=5
                  TOB=TYLO
                  GOTO 9000
               END IF
            END IF
         END IF

      ELSE IF (YRATE.LT.0.) THEN
C        Ray can only hit high y end - is high y end there to be hit?
         IF (OBSTYE(IOB).EQ.IY) THEN
C           High y end is there - has ray entered cell before it?
            IF (Y.GE.OBYE-COINC) THEN
C              Ray has entered after high y end
C              Find t when ray reaches plane of high y end
               TYHI=(OBYE-Y)/YRATE
               TYHI=MAX(0.,TYHI)
C              Determine x and z co-ordinates at this point on ray 
C              and see if they lie within the high y face
               XTYHI=X+TYHI*XRATE
               ZTYHI=Z+TYHI*ZRATE
               IF (OBTYPE.EQ.'B') THEN
                  CALL TEST(XTYHI,OBXS,OBXE,YES1)
                  CALL TEST(ZTYHI,OBZS,OBZE,YES2)
                  HIT=YES1.AND.YES2
               ELSE
                  D2CEN=(XTYHI-GLO(IOB,1))**2+(ZTYHI-GLO(IOB,3))**2
                  IF (D2CEN.LE.RAD2) HIT=.TRUE.
               END IF
               IF (HIT) THEN
C                 A hit takes place - set parameters and skip to end
                  HTSURF=4
                  TOB=TYHI
                  GOTO 9000
               END IF
            END IF
         END IF

      ELSE
C        yrate=0 so no y end can be hit
         CONTINUE

      END IF

C-----------------------------------------------------------
C                  Is a z end hit?
C-----------------------------------------------------------
C     If the obstacle is a cylinder it only has z ends if z is
C     the axial direction - ie IDIR=3.  If this is not the case
C     skip down to see if the curved surface is hit.
3000  IF ((OBTYPE.EQ.'C').AND.(IDIR.NE.3)) GOTO 4000
C     The obstacle has z ends so see if one of them is hit
      IF (ZRATE.GT.0.) THEN
C        Ray can only hit low z end - is low z end there to be hit?
         IF (OBSTZS(IOB).EQ.IZ) THEN
C           Low z end is there - has ray entered cell beyond it?
            IF (Z.LE.OBZS+COINC) THEN
C              Ray has entered before low z end
C              Find t when ray reaches plane of low z end
               TZLO=(OBZS-Z)/ZRATE
               TZLO=MAX(0.,TZLO)
C              Determine x and y co-ordinates at this point on ray 
C              and see if they lie within the low z face
               YTZLO=Y+TZLO*YRATE
               XTZLO=X+TZLO*XRATE
               IF (OBTYPE.EQ.'B') THEN
                  CALL TEST(YTZLO,OBYS,OBYE,YES1)
                  CALL TEST(XTZLO,OBXS,OBXE,YES2)
                  HIT=YES1.AND.YES2
               ELSE
                  D2CEN=(YTZLO-GLO(IOB,2))**2+(XTZLO-GLO(IOB,1))**2
                  IF (D2CEN.LE.RAD2) HIT=.TRUE.
               END IF
               IF (HIT) THEN
C                 A hit takes place - set parameters and skip to end
                  HTSURF=2
                  TOB=TZLO
                  GOTO 9000
               END IF
            END IF
         END IF

      ELSE IF (ZRATE.LT.0.) THEN
C        Ray can only hit high z end - is high z end there to be hit?
         IF (OBSTZE(IOB).EQ.IZ) THEN
C           High z end is there - has ray entered cell before it?
            IF (Z.GE.OBZE-COINC) THEN
C              Ray has entered after high z end
C              Find t when ray reaches plane of high z end
               TZHI=(OBZE-Z)/ZRATE
               TZHI=MAX(0.,TZHI)
C              Determine x and y co-ordinates at this point on ray 
C              and see if they lie within the high z face
               YTZHI=Y+TZHI*YRATE
               XTZHI=X+TZHI*XRATE
               IF (OBTYPE.EQ.'B') THEN
                  CALL TEST(YTZHI,OBYS,OBYE,YES1)
                  CALL TEST(XTZHI,OBXS,OBXE,YES2)
                  HIT=YES1.AND.YES2
               ELSE
                  D2CEN=(YTZHI-GLO(IOB,2))**2+(XTZHI-GLO(IOB,1))**2
                  IF (D2CEN.LE.RAD2) HIT=.TRUE.
               END IF
               IF (HIT) THEN
C                 A hit takes place - set parameters and skip to end
                  HTSURF=3
                  TOB=TZHI
                  GOTO 9000
               END IF
            END IF
         END IF

      ELSE
C        zrate=0 so no z end can be hit
         CONTINUE

      END IF

C-----------------------------------------------------------
C                  Is curved surface hit?
C-----------------------------------------------------------
C     If the obstacle is a box it does not have a curved surface
4000  IF (OBTYPE.EQ.'B') GOTO 9000
C     The obstacle has a curved surface so see if it is hit
C     Set the axial and non-axial rates, start points, etc
      IF (IDIR.EQ.1) THEN
         RATE1=YRATE
         RATE2=ZRATE
         RATE3=XRATE
         STPT1=Y
         STPT2=Z
         STPT3=X
         CENCO1=GLO(IOB,2)
         CENCO2=GLO(IOB,3)
         OBS=OBXS
         OBE=OBXE
      ELSE IF (IDIR.EQ.2) THEN
         RATE1=ZRATE
         RATE2=XRATE
         RATE3=YRATE
         STPT1=Z
         STPT2=X
         STPT3=Y
         CENCO1=GLO(IOB,3)
         CENCO2=GLO(IOB,1)
         OBS=OBYS
         OBE=OBYE
      ELSE 
         RATE1=XRATE
         RATE2=YRATE
         RATE3=ZRATE
         STPT1=X
         STPT2=Y
         STPT3=Z
         CENCO1=GLO(IOB,1)
         CENCO2=GLO(IOB,2)
         OBS=OBZS
         OBE=OBZE
      END IF

C     Set the coefficients of the quadratic equation for t
      A=RATE1*RATE1+RATE2*RATE2
      B=2.*((STPT1-CENCO1)*RATE1+(STPT2-CENCO2)*RATE2)
      C=(STPT1-CENCO1)**2+(STPT2-CENCO2)**2-RAD2
      D=B*B-4*A*C

C     If entry point is within the extended cylinder then the
C     ray cannot hit the curved surface only an end
      IF (C.LT.0.) GOTO 9000
      
C     If ray is parallel to the axis of the cylinder it cannot hit
C     the curved surface - when this happens A=0
      IF (A.EQ.0.) GOTO 9000

C     If the roots of the quadratic are complex the ray will never
C     hit the cylinder in any cell
      IF (D.LT.0.) GOTO 9000

C     The product of the roots of the quadratic is C/A, A>0 and we
C     have already checked that C is non-negative so 
C     both roots of the quadratic have the same sign.  
C     TCURV is the smaller root (NB A>0)
      TCURV=(-B-SQRT(D))/(2.*A)
      IF (TCURV.GE.0) THEN
C        Ray does hit the cylinder - see if it does so in this cell
         COORD3=STPT3+TCURV*RATE3
         CALL TEST(COORD3,OBS,OBE,HIT)
         IF (HIT) THEN
            HTSURF=7
            TOB=TCURV
         END IF
      END IF

C-----------------------------------------------------------
C                No more surfaces to hit
C-----------------------------------------------------------

C     Set parameters if a hit has not occurred
9000  IF (.NOT.HIT) THEN
         HTSURF=0
         TOB=1.E22
      END IF

      RETURN
      END


      SUBROUTINE RAYSEG(X,Y,Z,IX,IY,IZ,XRATE,YRATE,ZRATE,GX,GY,GZ,
     1                  ISG,WSEGXS,WSEGYS,NVMAX,NZMAX,COINC,HIT,TWALL)
C
C  Routine to determine if the ray entering cell IX,IY,IZ at the point X,Y,Z 
C  with direction cosines XRATE,YRATE,ZRATE hits wall segment ISG
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**24/10/90
C
C
      REAL X,Y,Z,XRATE,YRATE,ZRATE,GX(NVMAX),GY(NVMAX),GZ(NZMAX),COINC,
     1     WSEGXS(8*NVMAX),WSEGYS(8*NVMAX),TWALL
      INTEGER IX,IY,IZ,ISG,NVMAX,NZMAX
      LOGICAL HIT
      REAL XS,XE,YS,YE,XTWALL,YTWALL,ZTWALL,G,C,WLRATE,WLIN
      INTEGER IWLTYP
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  COINC   Largest amount by which two values may differ and still be deemed
C          to coincide
C  GX      Array of fine grid x co-ordinates
C  GY      Array of fine grid y co-ordinates
C  GZ      Array of fine grid z co-ordinates
C  ISG     Number of wall segment
C  IX      x cell number
C  IY      y cell number
C  IZ      z cell number
C  NVMAX   Maximum number of vertices allowed (dimension of some arrays)
C  NZMAX   Maximum number of z slices allowed (dimension of some arrays)
C  WSEGXS  Array of starting x co-ordinates of the wall segments
C  WSEGYS  Array of strating y co-ordinates of the wall segments
C  X       x co-ordinate of entry point to this cell
C  XRATE   Rate of change of x with t along the ray
C  Y       y co-ordinate of entry point to this cell
C  YRATE   Rate of change of y with t along the ray
C  Z       z co-ordinate of entry point to this cell
C  ZRATE   Rate of change of z with t along the ray
C
C**** Arguments - output
C  HIT     Logical variable indicating if a hit has taken place
C  TWALL   Value of the parameter t at the wall which is hit
C
C**** Local variables
C  C       y-intercept in equation of the wall segment
C  G       Gradient in equation of the wall segment
C  IWLTYP  Wall type indicator: 1-low x, 2-high x, 3-low y, 4- high y,
C          5- sloping
C  WLIN    Value of y-g*x at entry point X,Y
C  WRATE   Value of y-g*x at point XRATE,YRATE
C  XE      Ending x co-ordinate of the wall segment
C  XS      Starting x co-ordinate of the wall segment
C  XTWALL  x co-ordinate when t=TWALL
C  YE      Ending y co-ordinate of the wall segment
C  YS      Starting y co-ordinate of the wall segment
C  YTWALL  y co-ordinate when t=TWALL
C  ZTWALL  z co-ordinate when t=TWALL
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  TEST    Test if a value lies between two others
C
C******************************************************************************
C
C  Throughout this routine use is made of the parametric form of the equation
C  of the ray.  The general point on the ray has co-ordinates:
C        (x+t*xrate,y+t*yrate,z+t*zrate)
C  where (x,y,z) is the entry point to the cell and xrate,yrate,zrate are
C  the direction cosines calculated from the cone and polar angles phi and
C  theta by
C        xrate=sin(phi)cos(theta), yrate=cos(phi), zrate=sin(phi)sin(theta)
C  If wall segment ISG does not lie along a grid line (ie it is sloping) then
C  its equation is given in Cartesian form by:
C             y = gx + c
C  The gradient g is given by (ye-ys)/(xe-xs) and the y-intercept c is given
C  by c=(xe*ys-xs*ye)/(xe-xs) where (xs,ys), (xe,ye) are the start and end
C  points of the segment.
C
C******************************************************************************

C     The wall has not yet been hit so set HIT to false
      HIT=.FALSE.

C     Determine the start and end co-ordinates of the segment
      XS=WSEGXS(ISG)
      XE=WSEGXS(ISG+1)
      YS=WSEGYS(ISG)
      YE=WSEGYS(ISG+1)

C     Determine the wall type
      IF (ABS(XE-XS).LE.COINC) THEN
C        Wall is along an x grid line
         IF (ABS(XS-GX(IX)).LE.COINC) THEN
C           Wall is along low x grid line
            IWLTYP=1
         ELSE
C           Wall is along high x grid line
            IWLTYP=2
         END IF
      ELSE IF (ABS(YE-YS).LE.COINC) THEN
C        Wall is along a y grid line
         IF (ABS(YS-GY(IY)).LE.COINC) THEN
C           Wall is along low y grid line
            IWLTYP=3
         ELSE
C           Wall is along high y grid line
            IWLTYP=4
         END IF
      ELSE
C        Wall is not along a grid line ie it is sloping
         IWLTYP=5
      END IF

C     Determine the wall of t when the wall is reached (if it is)
      IF (IWLTYP.EQ.1) THEN

C        Wall is along low x grid line so if XRATE>0 ray misses the wall
         IF (XRATE.LT.0.) THEN
            TWALL=(GX(IX)-X)/XRATE
C           See if y at this value of t lies in the cell
            YTWALL=Y+TWALL*YRATE
            CALL TEST(YTWALL,GY(IY),GY(IY+1),HIT)
C           If y at t=TWALL is in the cell check if z at t=TWALL is in cell
            IF (HIT) THEN
               ZTWALL=Z+TWALL*ZRATE
               CALL TEST(ZTWALL,GZ(IZ),GZ(IZ+1),HIT)
            END IF
         END IF

      ELSE IF (IWLTYP.EQ.2) THEN

C        Wall is along high x grid line so if XRATE<0 ray misses the wall
         IF (XRATE.GT.0.) THEN
            TWALL=(GX(IX+1)-X)/XRATE
C           See if y at this value of t lies in the cell
            YTWALL=Y+TWALL*YRATE
            CALL TEST(YTWALL,GY(IY),GY(IY+1),HIT)
C           If y at t=TWALL is in the cell check if z at t=TWALL is in cell
            IF (HIT) THEN
               ZTWALL=Z+TWALL*ZRATE
               CALL TEST(ZTWALL,GZ(IZ),GZ(IZ+1),HIT)
            END IF
         END IF

      ELSE IF (IWLTYP.EQ.3) THEN

C        Wall is along low y grid line so if YRATE>0 ray misses the wall
         IF (YRATE.LT.0.) THEN
            TWALL=(GY(IY)-Y)/YRATE
C           See if x at this value of t lies in the cell
            XTWALL=X+TWALL*XRATE
            CALL TEST(XTWALL,GX(IX),GX(IX+1),HIT)
C           If x at t=TWALL is in the cell check if z at t=TWALL is in cell
            IF (HIT) THEN
               ZTWALL=Z+TWALL*ZRATE
               CALL TEST(ZTWALL,GZ(IZ),GZ(IZ+1),HIT)
            END IF
         END IF

      ELSE IF (IWLTYP.EQ.4) THEN

C        Wall is along high y grid line so if YRATE<0 ray misses the wall
         IF (YRATE.GT.0.) THEN
            TWALL=(GY(IY+1)-Y)/YRATE
C           See if x at this value of t lies in the cell
            XTWALL=X+TWALL*XRATE
            CALL TEST(XTWALL,GX(IX),GX(IX+1),HIT)
C           If x at t=TWALL is in the cell check if z at t=TWALL is in cell
            IF (HIT) THEN
               ZTWALL=Z+TWALL*ZRATE
               CALL TEST(ZTWALL,GZ(IZ),GZ(IZ+1),HIT)
            END IF
         END IF

      ELSE

C        Wall is not along a grid line ie it is sloping
C        Calculate gradient and y-intercept of equation of wall
         G=(YE-YS)/(XE-XS)
         C=(XE*YS-XS*YE)/(XE-XS)
C        Determine if the ray is parallel to the wall if it is it cannot hit it
         WLRATE=YRATE-G*XRATE
         IF (ABS(WLRATE).GT.COINC) THEN
C           Ray is not parallel to the wall
            WLIN=Y-G*X
            TWALL=(C-WLIN)/WLRATE
C	    If TWALL<0 then the ray is moving away from the wall so cannot hit
            IF (TWALL.GT.0.) THEN
C              See if x at TWALL is within the segment
               XTWALL=X+TWALL*XRATE
               CALL TEST(XTWALL,XS,XE,HIT)
C              If x at t=TWALL is within the segment y will be also
C              See if z at t=TWALL is within the cell
               IF (HIT) THEN
                  ZTWALL=Z+TWALL*ZRATE
                  CALL TEST(ZTWALL,GZ(IZ),GZ(IZ+1),HIT)
               END IF
            END IF
         END IF

      END IF

C     If a hit has not occured set TWALL to infinity (or near enough!!)
      IF (.NOT.HIT) TWALL=1.E22
      
      RETURN
      END


      SUBROUTINE WHOLCL(X,Y,Z,IX,IY,IZ,XRATE,YRATE,ZRATE,GX,GY,GZ,
     1                  NVMAX,NZMAX,COINC,XEND,YEND,ZEND,TEND)
C
C  Routine to calculate the value of the parameter t (TEND) when the ray 
C  which entered cell (IX,IY,IZ) at the point (X,Y,Z) leaves the cell
C  (assuming it is a cuboid).  Logical flags XEND, YEND and ZEND are set
C  to indicate which kind of face(s) of the cell contain the exit point.
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**19/10/90
C
C
C
      LOGICAL XEND,YEND,ZEND
      INTEGER IX,IY,IZ,NVMAX,NZMAX
      REAL X,Y,Z,XRATE,YRATE,ZRATE,GX(NVMAX),GY(NVMAX),GZ(NVMAX),TEND
      REAL COINC,TX,TY,TZ
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  COINC   Largest amount by which two values may differ and still be 
C          deemed to coincide
C  GX      Array of fine grid x co-ordinates
C  GY      Array of fine grid y co-ordinates
C  GZ      Array of fine grid z co-ordinates
C  IX      Fine grid x cell number
C  IY      Fine grid y cell number
C  IZ      Fine grid z cell number
C  NVMAX   Maximum number of vertices allowed (dimension of some arrays)
C  NZMAX   Maximum number of z slices allowed (dimension of some arrays)
C  X       Fine grid x co-ordinate of entry point
C  XRATE   Rate at which x changes with t along the ray
C  Y       Fine grid y co-ordinate of entry point
C  YRATE   Rate at which y changes with t along the ray
C  Z       Fine grid z co-ordinate of entry point
C  ZRATE   Rate at which z changes with t along the ray
C
C**** Arguments - output
C  TEND    Value of parameter t at exit point of cell
C  XEND    Logical flag indicating if ray leaves by an x face
C  YEND    Logical flag indicating if ray leaves via a y face
C  ZEND    Logical flag indicating if ray leaves via a z face
C
C**** Local variables
C  TX      Value of t at x end of cell
C  TY      Value of t at y end of cell
C  TZ      Value of t at z end of cell
C
C**** Local Arrays
C  NONE
C  
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C     Determine TX - note this depends on which way the ray is going
C     ie if x is increasing, decreasing or staying the same along the ray
      IF (XRATE.GT.0) THEN
         TX=(GX(IX+1)-X)/XRATE
      ELSE IF (XRATE.LT.0) THEN
         TX=(GX(IX)-X)/XRATE
      ELSE
         TX=1.E21
      END IF

C     Determine TY - note this depends on which way the ray is going
C     ie if y is increasing, decreasing or staying the same along the ray
      IF (YRATE.GT.0) THEN
         TY=(GY(IY+1)-Y)/YRATE
      ELSE IF (YRATE.LT.0) THEN
         TY=(GY(IY)-Y)/YRATE
      ELSE
         TY=1.E21
      END IF

C     Determine TZ - note this depends on which way the ray is going
C     ie if z is increasing, decreasing or staying the same along the ray
      IF (ZRATE.GT.0) THEN
         TZ=(GZ(IZ+1)-Z)/ZRATE
      ELSE IF (ZRATE.LT.0) THEN
         TZ=(GZ(IZ)-Z)/ZRATE
      ELSE
         TZ=1.E21
      END IF

C     Set TEND to be the smallest of TX,TY,TZ
      TEND=MIN(TX,TY,TZ)

C     Set the flags XEND,YEND,ZEND appropriately
      XEND=.FALSE.
      YEND=.FALSE.
      ZEND=.FALSE.
      IF (ABS(TX-TY).LE.COINC) THEN

C        TX=TY
         IF (ABS(TX-TZ).LE.COINC) THEN
C           TX=TY=TZ
            XEND=.TRUE.
            YEND=.TRUE.
            ZEND=.TRUE.
         ELSE IF (TX.LT.TZ) THEN
C           TX=TY<TZ
            XEND=.TRUE.
            YEND=.TRUE.
         ELSE
C           TZ<TX=TY
            ZEND=.TRUE.
         END IF

      ELSE IF (TX.LT.TY) THEN

C        TX<TY
         IF (ABS(TX-TZ).LE.COINC) THEN
C           TX=TZ<TY
            XEND=.TRUE.
            ZEND=.TRUE.
         ELSE IF (TX.LT.TZ) THEN
C           TX<TY and TX<TZ
            XEND=.TRUE.
         ELSE
C           TZ<TX<TY
            ZEND=.TRUE.
         END IF

      ELSE

C        TY<TX
         IF (ABS(TY-TZ).LE.COINC) THEN
C           TY=TZ<TX
            YEND=.TRUE.
            ZEND=.TRUE.
         ELSE IF (TY.LT.TZ) THEN
C           TY<TX and TY<TZ
            YEND=.TRUE.
         ELSE
C           TZ<TY<TX
            ZEND=.TRUE.
         END IF

      END IF

      RETURN
      END

      SUBROUTINE START(LUSTOT,IZONE,ICP,ITYPE,IX,IY,IZ,INFO,IOB,
     1                 ISTYPE,X,Y,Z,IRAN)
C
C  Routine to randomly select a start point on surface IZONE
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**18/06/90
C
C
      include 'comvar.for'
C
      INTEGER LUSTOT,IZONE,ICP,ITYPE,IX,IY,IZ,INFO,IOB,ISTYPE
      REAL X,Y,Z
      INTEGER*4 IRAN
      INTEGER ISEG,INOB
      REAL RANDN1,RANDN2,RAD,D2,XS,XE,YS,YE,DX,DY,OBXS,OBXE,OBYS,OBYE,
     1     OBZS,OBZE,R
      LOGICAL YES,YES1,YES2
C   
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  ICP     Component number of the sending surface (this is now always 1)
C  INFO    Further information required by START; if ITYPE=1 (boundary segment)
C          INFO=1 means segment along x grid line and INFO=2 means segment
C          along y grid line; if ITYPE=2 (cell surface) then INFO=1 means the
C          plane z=0 and INFO=2 means the plane z=zmax; if ITYPE=3 (cylindrical
C          obstacle surface) INFO gives the direction of the cylinder (1 for x,
C          2 for y and 3 for z).
C  IOB     If ITYPE=3 or 4 IOB gives the number of the sending obstacle
C  ISTYPE  If ITYPE= 3 or 4 ISTYPE gives the obstacle surface number
C  ITYPE   Type of the sending surface: ITYPE=1 means a boundary segment;
C          ITYPE=2 means a cell surface;  ITYPE=3 means a cylindrical obstacle;
C          ITYPE=4 means a box obstacle
C  IX      x cell number containing sending surface
C  IY      y cell number containing sending surface
C  IZ      z cell number containing sending surface
C  IZONE   Sending surface zone number
C  LUSTOT  Logical unit number for standard output
C
C**** Arguments - output
C  X       x co-ordinate of starting point
C  Y       y co-ordinate of starting point
C  Z       z co-ordinate of starting point
C
C**** Common variables - input
C  CELLOB  Array giving the number of the obstacle in each cell
C  CLINOT  Array giving in/out status of the cell surfaces
C  COINC   Largest amount by which 2 values are allowed to differ and still be
C          deemed to coincide
C  GHI     Array of obstacle high co-ordinates
C  GLO     Array of obstacle low co-ordinates
C  GX      Array of x grid line co-ordinates
C  GY      Array of y grid line co-ordinates
C  GZ      Array of z grid line co-ordinates
C  NGZ     Number of z grid line co-ordinates
C  PI      Pi
C  SFZN    Information about surface elements
C  SGINOT  Array giving in/out status of boundary segments
C  WSEGXS  Array giving x co-ordinate of start of each segment
C  WSEGYS  Array giving y co-ordinate of start of each segment
C
C**** Common variables - output
C  NONE
C
C**** Local variables
C  DX      Change in x along a boundary segment
C  DY      Change in y along a boundary segment
C  D2      Square of distance of a point from axis of cylinder
C  INOB    Number of obstacle in current cell
C  IRAN    Integer value needed for random number generator
C          N.B. IRAN may be machine dependent
C  ISEG    Number of current boundary segment
C  OBXE    Ending x co-ordinate of obstacle in current cell
C  OBXS    Starting x co-ordinate of obstacle in current cell
C  OBYE    Ending y co-ordinate of obstacle in current cell
C  OBYS    Starting y co-ordinate of obstacle in current cell
C  OBZE    Ending z co-ordinate of obstacle in current cell
C  OBZS    Starting z co-ordinate of obstacle in current cell
C  R       Distance from centre of a point on a circular face
C  RAD     Radius of a cylindrical obstacle
C  RANDN1  First random number
C  RANDN2  Second random number
C  XE      Ending x co-ordinate of current wall segment
C  XS      Starting x co-ordinate of current wall segment
C  YE      Ending y co-ordinate of current wall segment
C  YES     Logical flag returned by INSURF
C  YES1    Logical flag returned by TEST
C  YES2    Logical flag returned by TEST
C  YS      Starting y co-ordinate of current wall segment
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  INSURF  Determine if a point lies within the cell surface of a particular
C          cell
C  RAN     Random number generator - may be system specific
C  TEST    Determine if a particular value lies between two given values
C******************************************************************************

C     Determine first random numbers
C      CALL SEED(rnd$timeseed) 
      RANDN1=RANDOM(IRAN)
      RANDN2=RANDOM(IRAN)

C      CALL RANDOM(RANDN1)
C      CALL RANDOM(RANDN2)
            IPPP=IPPP+1
            If(IPPP.LT.100) Write(*,9168) RANDN1,RANDN2   
            If(IPPP.EQ.100) Pause
9168        FORMAT(2x,'Random number =', F8.4,3x,F8.4)    
      
      
      
      
C     Finding a start point will depend on the type of surface

      IF (ITYPE.EQ.1) THEN

C        ***************************************************
C        *          SURFACE IS A BOUNDARY SEGEMNT          *
C        ***************************************************

C        Determine the segment number
         ISEG=SFZN(IZONE,ICP,3)

C        Finding a start point will depend on whether the segment is 
C        whole or whether it is partially obscured

	 IF (SGINOT(ISEG,IZ).EQ.1) THEN

C           +++++++++++++++++++++++++++++++++++++++++++
C           +          Segment is unobscured          +
C           +++++++++++++++++++++++++++++++++++++++++++
	    X=WSEGXS(ISEG)+RANDN1*(WSEGXS(ISEG+1)-WSEGXS(ISEG))
	    Y=WSEGYS(ISEG)+RANDN1*(WSEGYS(ISEG+1)-WSEGYS(ISEG))
	    Z=GZ(IZ)+RANDN2*(GZ(IZ+1)-GZ(IZ))

	 ELSE IF (SGINOT(ISEG,IZ).EQ.2) THEN

C           +++++++++++++++++++++++++++++++++++++++++++
C           +       Segment obscured by cylinder      +
C           +++++++++++++++++++++++++++++++++++++++++++
C           Find the number of the obstacle obscuring this face
            INOB=CELLOB(IX,IY,IZ,1)
C           Compute possible start point
10	    X=WSEGXS(ISEG)+RANDN1*(WSEGXS(ISEG+1)-WSEGXS(ISEG))
	    Y=WSEGYS(ISEG)+RANDN1*(WSEGYS(ISEG+1)-WSEGYS(ISEG))
	    Z=GZ(IZ)+RANDN2*(GZ(IZ+1)-GZ(IZ))
C           See if possible start point lies within obscured part of face
	    RAD=GHI(INOB,1)
C           If the boundary segment is along an x grid line then distance
C           squared to centre is (y-yc)**2+(z-zc)**2 but if the segment is
C           along a y grid line it is (x-xc)**2+(z-zc)**2
	    IF (INFO.EQ.1) THEN
	       D2=(Y-GLO(INOB,2))*(Y-GLO(INOB,2))
     1            +(Z-GLO(INOB,3))*(Z-GLO(INOB,3))
            ELSE
	       D2=(X-GLO(INOB,1))*(X-GLO(INOB,1))
     1	          +(Z-GLO(INOB,3))*(Z-GLO(INOB,3))
            END IF
C           See if the possible start point is too close to the centre of the 
C           obscured circle - if it is get 2 new random numbers and start again
	    IF (D2.LE.RAD*RAD) THEN
	       RANDN1=RANDOM(IRAN)
	       RANDN2=RANDOM(IRAN) 
C             CALL RANDOM(RANDN1)
C             CALL RANDOM(RANDN2)
	       GOTO 10
	    END IF

         ELSE IF (SGINOT(ISEG,IZ).EQ.3) THEN

C           +++++++++++++++++++++++++++++++++++++++++++
C           +       Segment is obscured by a box      +
C           +++++++++++++++++++++++++++++++++++++++++++
C           Find the number of the obstacle which is obscuring the surface
            INOB=CELLOB(IX,IY,IZ,1)
C           Determine the start and end co-ordinates of the segment
            XS=WSEGXS(ISEG)
            XE=WSEGXS(ISEG+1)
            YS=WSEGYS(ISEG)
            YE=WSEGYS(ISEG+1)
            DX=XE-XS
            DY=YE-YS
C           Check that the segment lies along an x or y grid line
            DISP=MIN(ABS(DX),ABS(DY))
            IF (DISP.GT.COINC) THEN
               WRITE (LUSTOT,12) ISEG,INOB,IX,IY,IZ,XS,YS,XE,YE
12             FORMAT (' BOUNDARY SEGMENT ',I2,' IS OBSCURED BY A BOX'/
     1                 ' (THE BOX IS OBSTACLE ',I2,')'/
     2                 ' IN CELL ',3(I2,2X)/
     3                 ' START CO-ORDINATES OF THIS SEGMENT ARE ',
     4                 G11.4,',',G11.4/
     5                 ' END CO-ORDINATES OF THIS SEGMENT ARE ',
     6                 G11.4,',',G11.4/
     7                 ' THIS SEGMENT IS NOT PARALLEL TO X OR Y AXIS'/
     8                 ' HENCE IT CANNOT BE OBSCURED BY A BOX'/
     9                 ' RUN ABORTED')
               STOP
            END IF
C           Determine the start and end co-ordinates of the obstacle in
C           the current cell
            OBXS=MAX(GX(IX),GLO(INOB,1))
            OBXE=MIN(GX(IX+1),GHI(INOB,1))
            OBYS=MAX(GY(IY),GLO(INOB,2))
            OBYE=MIN(GY(IY+1),GHI(INOB,2))
            OBZS=MAX(GZ(IZ),GLO(INOB,3))
            OBZE=MIN(GZ(IZ+1),GHI(INOB,3))
            IF (ABS(DX).LE.COINC) THEN
C              Find possible starting co-ordinates for wall along x grid line
               X=XS
13             Y=YS+RANDN1*(YE-YS)
               Z=GZ(IZ)+RANDN2*(GZ(IZ+1)-GZ(IZ))
C              Check whether (X,Y,Z) is in obscured part of the segment
               CALL TEST(Y,OBYS,OBYE,YES1)
               CALL TEST(Z,OBZS,OBZE,YES2)
               IF (YES1.AND.YES2) THEN
C                 (X,Y,Z) is in obscured part, get 2 new random numbers 
C                 and start again
                  RANDN1=RANDOM(IRAN)
                  RANDN2=RANDOM(IRAN)
C                   CALL RANDOM(RANDN1)
C                   CALL RANDOM(RANDN2)
                  GOTO 13
               END IF
            ELSE IF (ABS(DY).LE.COINC) THEN
C              Find possible starting co-ordinates for wall along x grid line
               Y=YS
15             X=XS+RANDN1*(XE-XS)
               Z=GZ(IZ)+RANDN2*(GZ(IZ+1)-GZ(IZ))
C              Check whether (X,Y,Z) is within obscured part of segment
               CALL TEST(X,OBXS,OBXE,YES1)
               CALL TEST(Z,OBZS,OBZE,YES2)
               IF (YES1.AND.YES2) THEN
C                 (X,Y,Z) is in the obscured point - get 2 new random numbers
C                 and start again
                  RANDN1=RANDOM(IRAN)
                  RANDN2=RANDOM(IRAN) 
C                  CALL RANDOM(RANDN1)
C                  CALL RANDOM(RANDN2)
                  GOTO 15
               END IF
            END IF

         ELSE IF (SGINOT(ISEG,IZ).EQ.4) THEN

C           +++++++++++++++++++++++++++++++++++++++++++
C           +       Segment is totally obscured       +
C           +++++++++++++++++++++++++++++++++++++++++++
            WRITE (LUSTOT,17) ISEG,IZ
17          FORMAT (' SEGMENT ',I3,' IN SLICE ',I2,' IS TOTALLY',
     1              ' OBSCURED'/
     2              ' NO RAYS CAN COME FROM THIS SURFACE'/
     3              ' SUBROUTINE START SHOULD NOT HAVE BEEN CALLED'/
     4              ' RUN ABORTED')
            STOP

	 ELSE

C           +++++++++++++++++++++++++++++++++++++++++++
C           +   Segment has illegal value of SGINOT   +
C           +++++++++++++++++++++++++++++++++++++++++++

            WRITE (LUSTOT,18) ISEG,IZ
18          FORMAT (' ILLEGAL VALUE OF SGINOT FIR SEGMENT ',I3/
     1              ' IN SLICE ',I2/
     2              ' RUN ABORTED')
	    STOP
	 END IF

      ELSE IF (ITYPE.EQ.2) THEN

C        ***************************************************
C        *          SURFACE IS A BOUNDARY CELL FACE        *
C        ***************************************************

C        Finding a start point will depend on whether the cell surface is 
C        wholly inside the enclosure and whether or not it is partially
C        obscured by an obstacle

         IF (CLINOT(INFO,IX,IY).EQ.2) THEN

C           +++++++++++++++++++++++++++++++++++++++++++
C           +          Surface is unobscured          +
C           +++++++++++++++++++++++++++++++++++++++++++
C           Compute start point
	    X=GX(IX)+RANDN1*(GX(IX+1)-GX(IX))
	    Y=GY(IY)+RANDN2*(GY(IY+1)-GY(IY))
	    Z=0.
C           Check that z co-ordinate is correct
	    IF (INFO.EQ.2) Z=GZ(NGZ)

	 ELSE IF ((CLINOT(INFO,IX,IY).EQ.5)
     1             .OR.(CLINOT(INFO,IX,IY).EQ.3)) THEN

C           +++++++++++++++++++++++++++++++++++++++++++
C           +    Surface is obscured by a cylinder    +
C           +++++++++++++++++++++++++++++++++++++++++++
C           Determine the cylinder number
            INOB=CELLOB(IX,IY,IZ,1)
C           Compute a possible starting point with correct z co-ordinate
	    Z=0.
	    IF (INFO.EQ.2) Z=GZ(NGZ)
20	    X=GX(IX)+RANDN1*(GX(IX+1)-GX(IX))
	    Y=GY(IY)+RANDN2*(GY(IY+1)-GY(IY))
C           Check if (X,Y,Z) is in obscured part of surface
	    RAD=GHI(INOB,1)
	    D2=(X-GLO(INOB,1))*(X-GLO(INOB,1))
     1	       +(Y-GLO(INOB,2))*(Y-GLO(INOB,2))
C           If possible start point is too close to the centre of the circle
C           get 2 new random numbers and start again
	    IF (D2.LE.RAD*RAD) THEN
	       RANDN1=RANDOM(IRAN)
	       RANDN2=RANDOM(IRAN)
C             CALL RANDOM(RANDN1)
C             CALL RANDOM(RANDN2)
	       GOTO 20
	    END IF
C           If the surface is only partly inside the enclosure we must check
C           that (X,Y,Z) is inside the enclosure
            IF (CLINOT(INFO,IX,IY).EQ.3) THEN
               CALL INSURF(LUSTOT,IX,IY,X,Y,YES)
C              If (X,Y,Z) is outside get 2 new random numbers and start again
               IF (.NOT.YES) THEN
                  RANDN1=RANDOM(IRAN)
                  RANDN2=RANDOM(IRAN) 
C                   CALL RANDOM(RANDN1)
C                   CALL RANDOM(RANDN2)
                  GOTO 20
               END IF
            END IF

         ELSE IF ((CLINOT(INFO,IX,IY).EQ.4)
     1            .OR.(CLINOT(INFO,IX,IY).EQ.6)) THEN

C           +++++++++++++++++++++++++++++++++++++++++++
C           +       Surface is obscured by a box      +
C           +++++++++++++++++++++++++++++++++++++++++++
C           Determine the number of the obscuring box
            INOB=CELLOB(IX,IY,IZ,1)
C           find the start and end co-ordinates of the box within this cell
            OBXS=MAX(GX(IX),GLO(INOB,1))
            OBXE=MIN(GX(IX+1),GHI(INOB,1))
            OBYS=MAX(GY(IY),GLO(INOB,2))
            OBYE=MIN(GY(IY+1),GHI(INOB,2))
C           Compute a possible starting point with correct z co-ordinate
            Z=0.
            IF (INFO.EQ.2) Z=GZ(NGZ)
30          X=GX(IX)+RANDN1*(GX(IX+1)-GX(IX))
            Y=GY(IY)+RANDN2*(GY(IY+1)-GY(IY))
C           Check if (X,Y,Z) is in the obscured part of the surface
            CALL TEST(X,OBXS,OBXE,YES1)
            CALL TEST(Y,OBYS,OBYE,YES2)
C           If (X,Y,Z) is in the obscured part get 2 new random numbers and
C           start again
            IF (YES1.AND.YES2) THEN
              RANDN1=RANDOM(IRAN)
              RANDN2=RANDOM(IRAN)
C               CALL RANDOM(RANDN1)
C               CALL RANDOM(RANDN2)
               GOTO 30
            END IF
C           If the surface is only partly inside the enclosure we must check
C           that (X,Y,Z) is inside
            IF (CLINOT(INFO,IX,IY).EQ.4) THEN
               CALL INSURF(LUSTOT,IX,IY,X,Y,YES)
C              If (X,Y,Z) is outside get 2 new random numbers and try again
               IF (.NOT.YES) THEN
                  RANDN1=RANDOM(IRAN)
                  RANDN2=RANDOM(IRAN)
C                   CALL RANDOM(RANDN1)
C                   CALL RANDOM(RANDN2)
                  GOTO 30
               END IF
            END IF

         ELSE IF (CLINOT(INFO,IX,IY).EQ.1) THEN

C           ++++++++++++++++++++++++++++++++++++++++++++++++
C           + Surface is unobscured but only partly inside +
C           ++++++++++++++++++++++++++++++++++++++++++++++++
C           Compute a possible starting point with correct z co-ordinate
            Z=0.
            IF (INFO.EQ.2) Z=GZ(NGZ)
40          X=GX(IX)+RANDN1*(GX(IX+1)-GX(IX))
            Y=GY(IY)+RANDN2*(GY(IY+1)-GY(IY))
C           Check that (X,Y,Z) is inside
            CALL INSURF(LUSTOT,IX,IY,X,Y,YES)
C           If (X,Y,Z) is outside get 2 new random numbers and start again
            IF (.NOT.YES) THEN
              RANDN1=RANDOM(IRAN)
              RANDN2=RANDOM(IRAN)
C               CALL RANDOM(RANDN1)
C               CALL RANDOM(RANDN2)
               GOTO 40
            END IF

         ELSE IF (CLINOT(INFO,IX,IY).EQ.7) THEN

C           +++++++++++++++++++++++++++++++++++++++++++
C           +     Surface is wholly obscured          +
C           +++++++++++++++++++++++++++++++++++++++++++
            WRITE (LUSTOT,45) IX,IY,INFO
45          FORMAT (' CELL SURFACE FOR CELL ',I2,',',I2,' IN PLANE ',I2/
     1              ' IS ENTIRELY OBSCURED'/
     2              ' NO RAYS CAN BE FIRED FROM HERE'/
     3              ' START SHOULD NOT HAVE BEEN CALLED'/
     4              ' RUN ABORTED')
            STOP

	 ELSE

C           +++++++++++++++++++++++++++++++++++++++++++
C           +       CLINOT has an illegal value       +
C           +++++++++++++++++++++++++++++++++++++++++++
            WRITE (LUSTOT,47) IX,IY,INFO
47          FORMAT (' CLINOT HAS AN ILLEGAL VALUE IN START',/,
     1              ' FOR CELL SURFACE ',I2,',',I2/,
     2              ' IN PLANE GIVEN BY INFO=',I1/,
     3              ' RUN ABORTED')
	    STOP
	 END IF

      ELSE

C        ***************************************************
C        *          SURFACE IS AN OBSTACLE SURFACE         *
C        ***************************************************

C        Determine the starting co-ordinates of the obstacle in this cell
         OBXS=MAX(GX(IX),GLO(IOB,1))
	 OBYS=MAX(GY(IY),GLO(IOB,2))      
	 OBZS=MAX(GZ(IZ),GLO(IOB,3))

	 IF (ITYPE.EQ.3) THEN
C           *************************************************
C           *             Obstacle is a cylinder            *
C           *************************************************

C           Determine ending co-ordinates of cylinder in this cell
C           Only one of these values will be used according to the direction
C           of the cylinder
            OBXE=MIN(GX(IX+1),GHI(IOB,3))
	    OBYE=MIN(GY(IY+1),GHI(IOB,3))
	    OBZE=MIN(GZ(IZ+1),GHI(IOB,3))
C           Set cylinder radius
            RAD=GHI(IOB,1)

C           Finding a starting point depends on the type of surface
	    IF (ISTYPE.EQ.7) THEN
C              ++++++++++++++++++++++++++++++++++++++++++++++
C              +       Surface is the curved surface        +
C              ++++++++++++++++++++++++++++++++++++++++++++++
	       IF (INFO.EQ.1) THEN
C                *** Cylinder parallel x axis ***
                  X=OBXS+RANDN1*(OBXE-OBXS)
		  Y=GLO(IOB,2)+RAD*COS(2.*PI*RANDN2)
		  Z=GLO(IOB,3)+RAD*SIN(2.*PI*RANDN2)
	       ELSE IF (INFO.EQ.2) THEN
C                 *** Cylinder parallel y axis ***
                  Y=OBYS+RANDN1*(OBYE-OBYS)
		  X=GLO(IOB,1)+RAD*COS(2.*PI*RANDN2)
		  Z=GLO(IOB,3)+RAD*SIN(2.*PI*RANDN2)
	       ELSE
C                 *** Cylinder parallel z axis
                  Z=OBZS+RANDN1*(OBZE-OBZS)
		  X=GLO(IOB,1)+RAD*COS(2.*PI*RANDN2)
		  Y=GLO(IOB,2)+RAD*SIN(2.*PI*RANDN2)
	       END IF
	    ELSE IF (ISTYPE.EQ.1) THEN
C              ++++++++++++++++++++++++++++++++++++++++++++++
C              +         Surface is the low x end           +
C              ++++++++++++++++++++++++++++++++++++++++++++++
               X=GLO(IOB,1)
	       R=RAD*SQRT(RANDN1)
	       Y=GLO(IOB,2)+R*COS(2.*PI*RANDN2)
	       Z=GLO(IOB,3)+R*SIN(2.*PI*RANDN2)
	    ELSE IF (ISTYPE.EQ.2) THEN
C              ++++++++++++++++++++++++++++++++++++++++++++++
C              +          Surface is the low z end          +
C              ++++++++++++++++++++++++++++++++++++++++++++++
               Z=GLO(IOB,3)
	       R=RAD*SQRT(RANDN1)
	       X=GLO(IOB,1)+R*COS(2.*PI*RANDN2)
	       Y=GLO(IOB,2)+R*SIN(2.*PI*RANDN2)
	    ELSE IF (ISTYPE.EQ.3) THEN
C              ++++++++++++++++++++++++++++++++++++++++++++++
C              +         Surface is the high z end          +
C              ++++++++++++++++++++++++++++++++++++++++++++++
               Z=GHI(IOB,3)
	       R=RAD*SQRT(RANDN1)
	       X=GLO(IOB,1)+R*COS(2.*PI*RANDN2)
	       Y=GLO(IOB,2)+R*SIN(2.*PI*RANDN2)
	    ELSE IF (ISTYPE.EQ.4) THEN
C              ++++++++++++++++++++++++++++++++++++++++++++++
C              +         Surface is the high y end          +
C              ++++++++++++++++++++++++++++++++++++++++++++++
               Y=GHI(IOB,3)
	       R=RAD*SQRT(RANDN1)
	       X=GLO(IOB,1)+R*COS(2.*PI*RANDN2)
	       Z=GLO(IOB,3)+R*SIN(2.*PI*RANDN2)
	    ELSE IF (ISTYPE.EQ.5) THEN
C              ++++++++++++++++++++++++++++++++++++++++++++++
C              +         Surface is the low y end           +
C              ++++++++++++++++++++++++++++++++++++++++++++++
               Y=GLO(IOB,2)
	       R=RAD*SQRT(RANDN1)
	       X=GLO(IOB,1)+R*COS(2.*PI*RANDN2)
	       Z=GLO(IOB,3)+R*SIN(2.*PI*RANDN2)
	    ELSE
C              ++++++++++++++++++++++++++++++++++++++++++++++
C              +         Surface is the high x end          +
C              ++++++++++++++++++++++++++++++++++++++++++++++
               X=GHI(IOB,3)
	       R=RAD*SQRT(RANDN1)
	       Y=GLO(IOB,2)+R*COS(2.*PI*RANDN2)
	       Z=GLO(IOB,3)+R*SIN(2.*PI*RANDN2)
	    END IF

         ELSE

C           *************************************************
C           *               Obstacle is a box               *
C           *************************************************

C           Determine end co-ordinates of the obstacle in this cell
            OBXE=MIN(GX(IX+1),GHI(IOB,1))
	    OBYE=MIN(GY(IY+1),GHI(IOB,2))
	    OBZE=MIN(GZ(IZ+1),GHI(IOB,3))
	    IF (ISTYPE.EQ.1) THEN
C              ++++++++++++++++++++++++++++++++++++++++++++++
C              +         Surface is the low x end           +
C              ++++++++++++++++++++++++++++++++++++++++++++++
               X=GLO(IOB,1)
	       Y=OBYS+RANDN1*(OBYE-OBYS)
	       Z=OBZS+RANDN2*(OBZE-OBZS)
	    ELSE IF (ISTYPE.EQ.2) THEN
C              ++++++++++++++++++++++++++++++++++++++++++++++
C              +         Surface is the low z end           +
C              ++++++++++++++++++++++++++++++++++++++++++++++
               Z=GLO(IOB,3)
	       X=OBXS+RANDN1*(OBXE-OBXS)
	       Y=OBYS+RANDN2*(OBYE-OBYS)
	    ELSE IF (ISTYPE.EQ.3) THEN
C              ++++++++++++++++++++++++++++++++++++++++++++++
C              +         Surface is the high z end          +
C              ++++++++++++++++++++++++++++++++++++++++++++++
               Z=GHI(IOB,3)
	       X=OBXS+RANDN1*(OBXE-OBXS)
	       Y=OBYS+RANDN2*(OBYE-OBYS)
	    ELSE IF (ISTYPE.EQ.4) THEN
C              ++++++++++++++++++++++++++++++++++++++++++++++
C              +         Surface is the high y end          +
C              ++++++++++++++++++++++++++++++++++++++++++++++
               Y=GHI(IOB,2)
	       X=OBXS+RANDN1*(OBXE-OBXS)
	       Z=OBZS+RANDN2*(OBZE-OBZS)
	    ELSE IF (ISTYPE.EQ.5) THEN
C              ++++++++++++++++++++++++++++++++++++++++++++++
C              +         Surface is the low y end           +
C              ++++++++++++++++++++++++++++++++++++++++++++++
               Y=GLO(IOB,2)
	       X=OBXS+RANDN1*(OBXE-OBXS)
	       Z=OBZS+RANDN2*(OBZE-OBZS)
	    ELSE
C              ++++++++++++++++++++++++++++++++++++++++++++++
C              +         Surface is the high x end          +
C              ++++++++++++++++++++++++++++++++++++++++++++++
               X=GHI(IOB,1)
	       Y=OBYS+RANDN1*(OBYE-OBYS)
	       Z=OBZS+RANDN2*(OBZE-OBZS)
	    END IF

	 END IF

      END IF	 

      RETURN
      END

      SUBROUTINE TEST(A,B,C,YES)
C
C  Routine to test if the value A lies between the values B and C
C  If it does the logical variable YES is set to true otherwise it is false
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**13/06/90
C
C
      include 'comvar.for'
C
      REAL A,B,C,T
      LOGICAL YES
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  A       The value to be tested
C  B       One end of the test interval
C  C       Other end of the test interval
C
C**** Arguments - output
C  YES     Logical variable set to true if A lies between B and C and set to
C          false otherwise
C
C**** Common variables - input
C  NONE
C
C**** Common variables - output
C  NONE
C
C**** Local variables
C  T       Value used to determine position of A relative to B and C
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************


      YES=.FALSE.
      T=(A-B)*(A-C)
      IF (T.LE.0.) YES=.TRUE.

      RETURN
      END
      

      SUBROUTINE INSURF(LUSTOT,IX,IY,X,Y,YES)
C
C  Routine to determine whether the point (X,Y) lies within the cell surface
C  of cell (IX,IY) where there is a sloping wall in the cell so that the face
C  is partially outside the enclosure
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**28/03/90
C
C
      include 'comvar.for'
C
      REAL X,Y,XS,XE,YS,YE,DX,DY,DISP,GRAD,YINT,TEST
      INTEGER LUSTOT,IX,IY,IWALL,ISEG
      LOGICAL YES
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  IX      X cell number of cell to be considered
C  IY      Y cell number of cell to be considered
C  LUSTOT  Logical unit number for standard output
C  X       X co-ordinate of point to be considered
C  Y       Y co-ordinate of point to be considered
C
C**** Arguments - output
C  YES     Logical flag showing that point is inside if value is TRUE
C
C**** Common variables - input
C  CELLWS  Array of wall segment numbers in each cell
C  COINC   Values differing by less than COINC are regarded as coinciding
C  MXWLCL  Maximum permitted number of wall segments per cell
C  WSEGXS  Array of the x co-ord. at the start of each wall segment
C  WSEGYS  Array of the y co-ord. at the start of each wall segment
C
C**** Common variables - output
C  NONE
C
C**** Local variables
C  DX      Change in x co-ordinate along a wall segment
C  DY      Change in y co-ordinate along a wall segment
C  DISP    Smaller of DX and DY (in absolute values)
C  GRAD    Gradient of the wall segment
C  ISEG    Number of the current wall segment
C  IWALL   Loop counter of number of walls in the cell
C  TEST    Value which determines on which side of the wall point (X,Y) is
C  XS      X co-ordinate at the start of the current wall segment
C  XE      X co-ordinate at the end of the current wall segment
C  YS      Y co-ordinate at the start of the current wall segment
C  YE      Y co-ordinate at the end of the current wall segment
C  YINT    Y intercept of the equation of the wall
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C     Find the sloping wall by checking each wall in turn
      DO 100 IWALL=1,MXWLCL
C        Determine current segment number
         ISEG=CELLWS(IX,IY,IWALL)
C        If ISEG=0 there are no more sloping walls so one has not been found
         IF (ISEG.EQ.0) GOTO 200
C        Find start and end co-ordinates of the wall
         XS=WSEGXS(ISEG)
         XE=WSEGXS(ISEG+1)
         YS=WSEGYS(ISEG)
         YE=WSEGYS(ISEG+1)
         DX=XE-XS
         DY=YE-YS
         DISP=MIN(ABS(DX),ABS(DY))
C        If DISP>0 this is the sloping wall
         IF (DISP.GT.COINC) GOTO 300
100   CONTINUE
C     If we reach here no sloping wall has been found
200   WRITE (LUSTOT,250) IX,IY
250   FORMAT (' CELL ',I3,',',I3,' CONTAINS NO SLOPING WALL'/
     1        ' BUT IS ONLY PARTLY INSIDE THE ENCLOSURE'/
     2        ' THIS IS IMPOSSIBLE'/
     3        ' RUN ABORTED')
      STOP
C     Come to here when the sloping wall has been found
C     Determine the equation of the sloping wall
300   GRAD=DY/DX
      YINT=YS-GRAD*XS
C     Determine the value of the test parameter
      TEST=(Y-GRAD*X-YINT)*DX
      IF (TEST.GT.0.) THEN
         YES=.TRUE.
      ELSE
         YES=.FALSE.
      END IF

      RETURN
      END

      

      SUBROUTINE CONVRT(LUSTOT,ALPHA,BETA,IKIND,INFO,SING,COSG,
     1                  SINPHI,COSPHI,SINTH,COSTH)
C
C  Routine to convert the cone and polar angles of a ray measured relative to
C  the surface from which the ray originates into cone and polar angles 
C  relative to the grid.
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**19/06/90
C
C
      include 'comvar.for'
C
      INTEGER LUSTOT,IKIND,INFO
      REAL ALPHA,BETA,SING,COSG,SINPHI,COSPHI,SINTH,COSTH
      REAL SINA,COSA,SINB,COSB
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  ALPHA   Cone angle of ray relative to sending surface
C  BETA    Polar angle of the ray relative to sending surface
C  COSG    Cosine of angle giving direction of surface if it is perpendicular
C          to all planes z=constant (IKIND=1) or direction of tangent if it is
C          the curved surface of a cylinder (IKIND=3).  If IKIND=2 COSG is not
C          required
C  IKIND   Parameter indicating the type of the sending surface
C          IKIND=1 surface is perpendicular to all planes z=constant
C          IKIND=2 surface is in a plnae z=constant
C          IKIND=3 surface is curved surface of a cylinder
C  INFO    If IKIND=1 INFO is not used
C          If IKIND=2 INFO=1 means normal out of sending surface is +k
C                     whilst INFO>1 means outward normal is -k
C          If IKIND=3 INFO gives direction of cylinder (1 for x, 2 for y,
C                     3 for z)
C  LUSTOT  Logical unit number for standard output
C  SING    Sine of angle giving direction of sending surface (refer to COSG)
C
C**** Arguments - output
C  COSPHI  Cosine of the cone angle relative to the grid
C  COSTH   Cosine of the polar angle relative to the grid
C  SINPHI  Sine of the cone angle relative to the grid 
C  SINTH   Sine of the polar angle relative to the grid
C
C**** Common variables - input
C  COINC   Largest amount by which 2 values may differ and still be deemed to 
C          coincide
C
C**** Common variables - output
C  NONE
C
C**** Local variables
C  COSA    Cosine of alpha
C  COSB    Cosine of beta
C  SINA    Sine of alpha
C  SINB    Sine of beta
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C     Calculate sin and cos of alpha and beta
      SINA=SIN(ALPHA)
      COSA=COS(ALPHA)
      SINB=SIN(BETA)
      COSB=COS(BETA)

C     Calculate sin and cos of phi and theta according to the type of surface
      IF (IKIND.EQ.1) THEN
	 COSPHI=SINA*COSB*SING+COSA*COSG
	 SINPHI=SQRT(1.-COSPHI*COSPHI)
	 IF (ABS(SINPHI).LE.COINC) THEN
	    SINTH=0.
	    COSTH=1.
	    GOTO 100
	 END IF
	 COSTH=(SINA*COSB*COSG-COSA*SING)/SINPHI
	 SINTH=(SINA*SINB)/SINPHI
      ELSE IF (IKIND.EQ.2) THEN
         IF (INFO.EQ.1) THEN
	    SIGN=+1.
	 ELSE  
	    SIGN=-1.
	 END IF
	 COSPHI=SINA*SINB
	 SINPHI=SQRT(1.-COSPHI*COSPHI)
	 IF (ABS(SINPHI).LE.COINC) THEN
	    SINTH=0.
	    COSTH=1.
	    GOTO 100
	 END IF
         COSTH=SINA*COSB/SINPHI
	 SINTH=SIGN*COSA/SINPHI
      ELSE IF (IKIND.EQ.3) THEN
         IF (INFO.EQ.1) THEN
	    COSPHI=COSA*COSG-SINA*COSB*SING
            SINPHI=SQRT(1.-COSPHI*COSPHI)
	    IF (ABS(SINPHI).LE.COINC) THEN
	       SINTH=0.
	       COSTH=1.
	       GOTO 100
	    END IF
	    COSTH=SINA*SINB/SINPHI
	    SINTH=(COSA*SING+SINA*COSB*COSG)/SINPHI
	 ELSE IF (INFO.EQ.2) THEN
	    COSPHI=SINA*SINB
            SINPHI=SQRT(1.-COSPHI*COSPHI)
	    IF (ABS(SINPHI).LE.COINC) THEN
	       SINTH=0.
	       COSTH=1.
	       GOTO 100
	    END IF
	    COSTH=(COSA*COSG-SINA*COSB*SING)/SINPHI
	    SINTH=(COSA*SING+SINA*COSB*COSG)/SINPHI
	 ELSE
	    COSPHI=COSA*SING+SINA*COSB*COSG
            SINPHI=SQRT(1.-COSPHI*COSPHI)
	    IF (ABS(SINPHI).LE.COINC) THEN
	       SINTH=0.
	       COSTH=1.
	       GOTO 100
	    END IF
	    COSTH=(COSA*COSG-SINA*COSB*SING)/SINPHI
	    SINTH=SINA*SINB/SINPHI
	 END IF
      ELSE
         WRITE (LUSTOT,50) IKIND
50       FORMAT (' IN CONVRT THE VALUE OF IKIND IS ',I3/
     1           ' THE ONLY PERMITTED VALUES ARE 1, 2 AND 3'/
     2           ' RUN ABORTED')
	 STOP
      END IF

100   CONTINUE

      RETURN
      END

      SUBROUTINE CRSEXF(LUSTOT,NATM,NSURF,NVOL,NZONES,NVZONE,ACTZON,
     1                  ACTVZN,ZNAREA,FINGSK,FINVOL,NATMMX,NVMX,NZMX,
     2                  MXELMS,MXELMG,MXZONE,MXVZON,VLAREA,VUSS,VUSG,
     3                  VUGG,VUGS,DEAELM,DEAELV,DEASS,DEASG,DEAGG,DEAGS,
     4                  CVAREA,IEXOPT,EXFSS,EXFSG,EXFGG,EXFGS,LURSLT,
     5                  EMISS)
C
C  Routine to process an atmosphere at a time the fine grid view factors
C  that have been produced by VUFACT and turn them in coarse grid direct
C  exchange areas through the use of exchange area algebra 
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      include 'comvar.for'
C
      PARAMETER (MXSIZE=MXZN+MXVZN)
      INTEGER NATMMX,NVMX,NZMX,MXELMS,MXELMG,MXZONE,MXVZON,NATM,NSURF,
     1        NVOL,LUSTOT,NZONES,NVZONE,ACTZON(MXZONE),ACTVZN(MXVZON),
     2        IEXOPT,LURSLT
      REAL FINGSK(NATMMX,NVMX,NVMX,NZMX),FINVOL(NVMX,NVMX,NZMX),
     1     VUSS(MXELMS,MXELMS),VUSG(MXELMS,MXELMG),VUGG(MXELMG,MXELMG),
     2     VUGS(MXELMG,MXELMS),DEAELM(MXELMS),DEAELV(MXELMG),
     3     VLAREA(MXELMG),DEASS(MXZONE,MXZONE),DEASG(MXZONE,MXVZON),
     4     DEAGG(MXVZON,MXVZON),DEAGS(MXVZON,MXZONE),ZNAREA(MXZONE),
     5     CVAREA(NATMMX,MXVZON),EXFSS(MXZONE,MXZONE),
     6     EXFSG(MXZONE,MXVZON),EXFGG(MXVZON,MXVZON),
     7     EXFGS(MXVZON,MXZONE),EMISS(MXZN)
      INTEGER IATM,IELEM,JELEM,IELMV,JELMV,IX,IY,IZ,ISURF,JSURF,IVOL,
     1        JVOL,ACTGAS(MXVZN),ICOMP,NCOMPS,IELEMV
      REAL X(MXSIZE,MXSIZE),R(MXSIZE,MXSIZE),D(MXSIZE),R1(MXZN,MXZN),
     1     TEMP(MXZN,MXZN),TEMPV(MXZN)
      LOGICAL LEXOPT(3)
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  ACTVZN  Array of actual volume zone numbers (ie those with non-zero volume)
C  ACTZON  Array of actual surface zone numbers (ie those with non-zero area)
C  CVAREA  Array to hold the "notional" areas for each atmosphere of the 
C          volume zones ("notional" area = 4*absorptivity*volume)
C  DEAELM  Array to be passed to CRSDEA as work space
C  DEAELV  Array to be passed to CRSDEA as work space
C  DEAGG   Array to be returned by CRSDEA of coarse gas to gas deas
C  DEAGS   Array to be returned by CRSDEA of coarse gas to surface deas
C  DEASG   Array to be returned by CRSDEA of coarse surface to gas deas
C  DEASS   Array to be returned by CRSDEA of coarse surface to surface deas
C  EMISS   Array of surface zone emissivities
C  FINGSK  Array of fine grid gas absorptivities
C  FINVOL  Array of fine grid cell volumes
C  IEXOPT  Flag indicating which exchange factor option has been chosen
C          1=view factors, 2=deas, 3=teas, 4=all
C  LURSLT  Logical unit number for results (passed to outexf)
C  LUSTOT  Logical unit number for standard output
C  MXELMG  Maximum number of fine grid cells allowed (dimension of some arrays)
C  MXELMS  Maximum number of fine grid surface elements allowed (dimension
C          of some arrays)
C  MXVZON  Maximum number of coarse volume zones allowed (dimension of some
C          arrays)
C  MXZONE  Maximum number of coarse surface zones allowed (dimension of some
C          arrays)
C  NATM    Number of different atmospheres (ie number of exchange area sets)
C  NATMMX  Maximum number of atmospheres allowed (dimension of some arrays)
C  NSURF   Number of coarse grid surface zones
C  NVMX    Maximum number of vertices allowed (dimension of some arrays)
C  NVOL    Number of coarse grid volume zones
C  NVZONE  Number of actual volume zones
C  NZMX    Maximum number of z slices allowed (dimension of some arrays)
C  NZONES  Number of actual surface zones
C  VLAREA  Array holding the "notional" area of the fine grid cells for the 
C          current atmosphere ("notional" area is 4kV where k is the gas
C          absorptivity and V is the volume)
C  VUGG    Array to hold the fine gas to gas view factors for the current 
C          atmosphere
C  VUGS    Array to hold the fine gas to surface view factors for the current
C          atmosphere
C  VUSG    Array to hold the fine surface to gas view factors for the current
C          atmosphere
C  VUSS    Array to hold the fine surface to surface view factors for the 
C          current atmosphere
C  ZNAREA  Array of surface zone areas
C
C**** Arguments - output
C  CVAREA  Array of "notional" areas of the coarse volume zones
C
C**** Common variables - input
C  ELAREA  Array of fine grid surface zone areas
C  NELEMS  Number of fine grid surface zones
C  NELEMV  Number of fine grid volume zones
C  SFZONE  Component information of the coarse grid surface zones
C          SFZONE(I,1) - number of components of coarse surface zone I
C          SFZONE(I,J) - element number of component J-1 of coarse surface
C                        zone I
C  VLZN    Array giving the fine grid cell numbers of each fine grid volume
C          element
C  VLZONE  Component information of the coarse grid volume zones
C          VLZONE(I,1) - number of components of coarse volume zone I
C          VLZONE(I,J) - element number of component J-1 of coarse volume
C                        zone I
C  VUFCGG  Fine grid gas to gas view factors
C  VUFCGS  Fine grid gas to surface view factors
C  VUFCSG  Fine grid surface to gas view factors
C  VUFCSS  Fine grid surface to surface view factors
C  
C**** Common variables - output
C  VUFCGG  Coarse grid gas to gas deas
C  VUFCGS  Coarse grid gas to surface deas
C  VUFCSG  Coarse grid surface to gas deas
C  VUFCSS  Coarse grid surface to surface deas
C  
C**** Local variables
C  IATM    Loop counter over the atmospheres
C  ICOMP   Loop counter over the components of a coarse volume zone
C  IELEM   Loop counter over sending surface elements
C  IELEMV  Volume element number of current component of a coarse volume zone
C  IELMV   Loop counter over sending volume elements
C  IOPT    Loop counter over exchange factor options
C  ISURF   Loop counter over sending surface zones
C  IVOL    Loop counter over siending volume zones
C  IX      x fine grid cell number of current volume element
C  IY      y fine grid cell number of current volume element
C  IZ      z fine grid cell number of current volume element
C  JELEM   Loop counter over receiving surface elements
C  JELMV   Loop counter over receiving volume elements
C  JSURF   Loop counter over receiving surface zones
C  JVOL    Loop counter over receiving volume zones
C  LEXOPT  Array of logical flags indicating whether a particular kind of
C          exchange factor is to be calculated
C  NCOMPS  Number of components of current coarse volume zone
C
C**** Local Arrays
C  ACTGAS  Working space array to be passed to SMOOTH
C  D       Working space array to be passed to SMOOTH
C  R       Working space array to be passed to SMOOTH
C  R1      Working space array passed to TEACRS
C  TEMP    Working space array passed to TEACRS
C  TEMPV   Working space array passed to TEACRS
C  X       Working space array to be passed to SMOOTH
C
C**** Functions and Subroutines
C  CRSDEA  Compute coarse direct exchange areas from fine view factors
C          for a single atmosphere
C  OUTEXF  Output exchange factors
C  SMOOTH  Smooth the deas computed by the Monte Carlo method so that they
C          are symmetric and satisfy conservation constraints
C  TEACRS  Compute coarse grid TEAs from DEAs
C  VFCRS   Compute the coarse view factors from the coarse deas
C
C******************************************************************************

C**** Set the "notional" area array to zero
      DO 20 IVOL=1,NVZONE
         DO 10 IATM=1,NATM
	    CVAREA(IATM,IVOL)=0.
10       CONTINUE
20    CONTINUE

C**** Calculate zone to zone deas from element to element view factors
C**** for one atmosphere at a time
      DO 1500 IATM=1,NATM
C****    Load view factors in VUSS,VUSG,VUGG,VUGS for this atmosphere
         DO 300 JELEM=1,NELEMS
            DO 100 IELEM=1,NELEMS
               VUSS(IELEM,JELEM)=VUFCSS(IATM,IELEM,JELEM)
100         CONTINUE
            DO 200 IELMV=1,NELEMV
               VUGS(IELMV,JELEM)=VUFCGS(IATM,IELMV,JELEM)
200         CONTINUE
300      CONTINUE
         DO 600 JELMV=1,NELEMV
            DO 400 IELMV=1,NELEMV
               VUGG(IELMV,JELMV)=VUFCGG(IATM,IELMV,JELMV)
400         CONTINUE
            DO 500 IELEM=1,NELEMS
               VUSG(IELEM,JELMV)=VUFCSG(IATM,IELEM,JELMV)
500         CONTINUE
600      CONTINUE

C****    Compute the array of "notional" areas for this atmopshere
         DO 700 IELMV=1,NELEMV
            IX=VLZN(IELMV,1)
            IY=VLZN(IELMV,2)
            IZ=VLZN(IELMV,3)
            VLAREA(IELMV)=4.*FINGSK(IATM,IX,IY,IZ)*FINVOL(IX,IY,IZ)
700      CONTINUE

c-dal-
c      write (*,6005) 
c6005  format (' view factor sums')
c      do 604 isurf=1,nelems
c         sums=0.
c    	 sumg=0.
c	 do 601 jsurf=1,nelems
c	    sums=sums+vuss(isurf,jsurf)
c601	 continue
c         do 602 jvol=1,nelemv
c	    sumg=sumg+vusg(isurf,jvol)
c602	 continue
c         write (*,603) iatm,isurf,sums,sumg,sums+sumg
c603	 format (' Atmosphere ',i2,' Surface ',i3,' Sums ',g12.5,
c     1           ' Sumg ',g12.5,' Total ',g12.5)
c604   continue
c      do 608 ivol=1,nelemv
c         sums=0.
c	 sumg=0.
c	 do 605 jvol=1,nelemv
c	    sumg=sumg+vugg(ivol,jvol)
c605	 continue
c         do 606 jsurf=1,nelems
c	    sums=sums+vugs(ivol,jsurf)
c606	 continue
c         write (*,607) iatm,ivol,sumg,sums,sums+sumg
c607	 format (' Atmosphere ',i2,' Volume ',i3,' Sumg ',g12.5,
c     1           ' Sums ',g12.5,' Total ',g12.5)
c608   continue
c-dal-     
         
C****    Call CRSDEA to compute the coarse deas
         CALL CRSDEA(NSURF,NVOL,SFZONE,VLZONE,NELEMS,NELEMV,ELAREA,
     1               VLAREA,VUSS,VUSG,VUGG,VUGS,MXZN,MXCP,MXVZN,MXVZNC,
     2               MXELEM,MXELMV,DEAELM,DEAELV,DEASS,DEASG,DEAGG,
     3               DEAGS)

C****    Compute the array of "notional" areas of the coarse volume zones
C****    for this atmosphere
         DO 750 IVOL=1,NVZONE
	    CVAREA(IATM,IVOL)=0.
	    NCOMPS=VLZONE(IVOL,1)
	    DO 730 ICOMP=1,NCOMPS
	       IELEMV=VLZONE(IVOL,ICOMP+1)
	       IX=VLZN(IELEMV,1)
	       IY=VLZN(IELEMV,2)
	       IZ=VLZN(IELEMV,3)
	       CVAREA(IATM,IVOL)=CVAREA(IATM,IVOL)
     1                        +4.*FINGSK(IATM,IX,IY,IZ)*FINVOL(IX,IY,IZ)
730         CONTINUE
750      CONTINUE

C****    Smooth the DEAs
         CALL SMOOTH(LUSTOT,IATM,DEASS,DEASG,DEAGG,DEAGS,NSURF,NVOL,
     1               NZONES,NVZONE,ACTZON,ACTVZN,ACTGAS,ZNAREA,CVAREA,
     2               MXZN,MXVZN,MXSIZE,NATMMX,X,R,D)
c-dal-
c     write (*,7005)
c7005  format (' DEA sums')
c      do 704 isurf=1,nsurf
c         isurf=51
c        sums=0.
c	 sumg=0.
c	 do 701 jsurf=1,nsurf
c	    sums=sums+deass(isurf,jsurf)
c701	 continue
c        do 702 jvol=1,nvol
c	    sumg=sumg+deasg(isurf,jvol)
c702	 continue
c         write (*,703) iatm,isurf,sums,sumg,sums+sumgc
c703	 format (' Atmosphere ',i2,' Surface ',i3,' Sums ',g12.5,
c     1           ' Sumg ',g12.5,' Total ',g12.5)
c704   continue
c      do 708 ivol=1,nvol
c         sums=0.
c	 sumg=0.
c	 do 705 jvol=1,nvol
c	    sumg=sumg+deagg(ivol,jvol)
c705	 continue
c          do 706 jsurf=1,nsurf
c	    sums=sums+deags(ivol,jsurf)
c706	 continue
c         write (*,707) iatm,ivol,sumg,sums,sums+sumg
c707	 format (' Atmosphere ',i2,' Volume ',i3,' Sumg ',g12.5,
c      1           ' Sums ',g12.5,' Total ',g12.5)
c708   continue
c-dal-     

C****    Calculate the requested exchange factors from the deas
C****    and output the values
         IF (IEXOPT.LE.3) THEN
	    DO 780 IOPT=1,3
	       LEXOPT(IOPT)=.FALSE.
780         CONTINUE
            LEXOPT(IEXOPT)=.TRUE.
         ELSE
	    DO 790 IOPT=1,3
	       LEXOPT(IOPT)=.TRUE.
790	    CONTINUE
         END IF
	 IF (LEXOPT(1)) THEN
C****       View factors have been requested
            CALL VFCRS(IATM,DEASS,DEASG,DEAGG,DEAGS,NSURF,NVOL,NZONES,
     1                 NVZONE,ACTZON,ACTVZN,ZNAREA,CVAREA,MXZONE,MXVZON,
     2                 NATMMX,EXFSS,EXFSG,EXFGG,EXFGS)
            CALL OUTEXF(LUSTOT,IATM,1,LURSLT,EXFSS,EXFSG,EXFGG,EXFGS,
     1                  ACTZON,ACTVZN,NZONES,NVZONE,MXZONE,MXVZON)
         END IF
	 IF (LEXOPT(2)) THEN
C****       Direct exchange areas have been requested
            CALL OUTEXF(LUSTOT,IATM,2,LURSLT,DEASS,DEASG,DEAGG,DEAGS,
     1                  ACTZON,ACTVZN,NZONES,NVZONE,MXZONE,MXVZON)	 
         END IF
	 IF (LEXOPT(3)) THEN
C****	    Total exchange areas have been requested
            CALL TEACRS(DEASS,DEASG,DEAGG,DEAGS,IATM,NSURF,NVOL,NZONES,
     1                  NVZONE,ACTZON,ACTVZN,ZNAREA,EMISS,LUSTOT,R1,
     2                  TEMP,TEMPV,MXZN,MXVZN,EXFSS,EXFSG,EXFGG,EXFGS)
            CALL OUTEXF(LUSTOT,IATM,3,LURSLT,EXFSS,EXFSG,EXFGG,EXFGS,
     1                  ACTZON,ACTVZN,NZONES,NVZONE,MXZONE,MXVZON)
         END IF

c-dal-
c      if (iexopt.eq.2) goto 1500
c      write (*,8005)
c8005  format (' EXCHANGE FACTOR sums')
c      do 804 isurf=1,nsurf
c         sums=0.
c	 sumg=0.
c	 do 801 jsurf=1,nsurf
c	    sums=sums+exfss(isurf,jsurf)
c801	 continue
c         do 802 jvol=1,nvol
c	    sumg=sumg+exfsg(isurf,jvol)
c802	 continue
c         write (*,803) iatm,isurf,sums,sumg,sums+sumg
c803	 format (' Atmosphere ',i2,' Surface ',i3,' Sums ',g12.5,
c     1           ' Sumg ',g12.5,' Total ',g12.5)
c804   continue
c      do 808 ivol=1,nvol
c         sums=0.
c	 sumg=0.
c	 do 805 jvol=1,nvol
c	    sumg=sumg+exfgg(ivol,jvol)
c805	 continue
c         do 806 jsurf=1,nsurf
c	    sums=sums+exfgs(ivol,jsurf)
c806	 continue
c         write (*,807) iatm,ivol,sumg,sums,sums+sumg
c807	 format (' Atmosphere ',i2,' Volume ',i3,' Sumg ',g12.5,
c     1           ' Sums ',g12.5,' Total ',g12.5)
c808   continue
c-dal-     
         
C****    Load results from CRSDEA back into master VUFC arrays
         DO 1000 JSURF=1,NSURF
            DO 800 ISURF=1,NSURF
               VUFCSS(IATM,ISURF,JSURF)=DEASS(ISURF,JSURF)
800         CONTINUE
            DO 900 IVOL=1,NVOL
               VUFCGS(IATM,IVOL,JSURF)=DEAGS(IVOL,JSURF)
900         CONTINUE
1000     CONTINUE
         DO 1300 JVOL=1,NVOL
            DO 1100 IVOL=1,NVOL
               VUFCGG(IATM,IVOL,JVOL)=DEAGG(IVOL,JVOL)
1100        CONTINUE
            DO 1200 ISURF=1,NSURF
               VUFCSG(IATM,ISURF,JVOL)=DEASG(ISURF,JVOL)
1200        CONTINUE
1300     CONTINUE

1500  CONTINUE

C**** Close results file before returning to main module
      CLOSE(LURSLT)
      
      RETURN
      END
           
      SUBROUTINE CRSDEA(NSURF,NVOL,SFZONE,VLZONE,NELEMS,NELEMV,ELAREA,
     1                  VLAREA,VUSS,VUSG,VUGG,VUGS,MXZN,MXCP,MXVZN,
     2                  MXVZNC,MXELEM,MXELMV,DEAELM,DEAELV,DEASS,DEASG,
     3                  DEAGG,DEAGS)
C
C  Routine to calculate the coarse grid surface zone direct exchange areas
C  from the fine grid view factors using exchange area algebra
C  
C*****VERSION 1.1********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/11/90
C
C
      INTEGER MXZN,MXCP,MXVZN,MXVZNC,MXELEM,MXELMV,
     1        NSURF,NVOL,NELEMS,NELEMV,SFZONE(MXZN,MXCP),
     2        VLZONE(MXVZN,MXVZNC)
      REAL ELAREA(MXELEM),VLAREA(MXELMV),DEAELM(MXELEM),DEAELV(MXELMV),
     1     VUSS(MXELEM,MXELEM),VUSG(MXELEM,MXELMV),VUGG(MXELMV,MXELMV),
     2     VUGS(MXELMV,MXELEM),DEASS(MXZN,MXZN),DEASG(MXZN,MXVZN),
     3     DEAGG(MXVZN,MXVZN),DEAGS(MXVZN,MXZN)
      INTEGER NCOMPS,ISURF,JSURF,ICOMP,JCOMP,IELEM,JELEM,ITOELM,ITOELV,
     1        IVOL,JVOL,IELMV,JELMV
C
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  DEAELM  1 dimensional temporary storage array
C  DEAELV  1 dimensional temporary storage array
C  ELAREA  Array of element areas
C  MXCP    Maximum number of components a zone is allowed to have
C  MXELEM  Maximum number of elements allowed
C  MXELMV  Maximum number of volume elements allowed
C  MXVZN   Maximum number of volume zones allowed
C  MXVZNC  Maximum number of components a volume zone is allowed to have
C  MXZN    Maximum number of zones allowed
C  NELEMS  Number of elements
C  NELEMV  Number of volume elements
C  NSURF   Number of surface zones
C  NVOL    Number of volume zones
C  SFZONE  Array of surface zone data (number & list of components)
C          - SFZONE(I,1) number of components of surface zone I
C          - SFZONE(I,J) element number of component J-1 of surface zone I
C  VLAREA  Array of "notional" areas of volume elements
C  VLZONE  Array of volume zone data (number & list of components)
C          - VLZONE(I,1) number of components of volume zone I
C          - VLZONE(I,J) element number of component J-1 of volume zone I
C  VUGG    Array of elemental gas to gas view factors
C  VUGS    Array of elemental gas to volume view factors
C  VUSG    Array of elemental surface to gas view factors
C  VUSS    Array of elemental surface to surface view factors
C
C**** Arguments - output
C  DEAGG   Array of coarse gas to gas deas
C  DEAGS   Array of coarse gas to surface deas
C  DEASG   Array of coarse surface to gas deas
C  DEASS   Array of coarse surface to surface deas
C
C**** Local variables
C  ICOMP    Loop counter on component number
C  IELEM    Loop counter on sending surface element number
C  IELMV    Loop counter on sending volume element number
C  ISURF    Loop counter on surface zone number
C  ITOELM   Loop counter on receiving surface element number
C  ITOELV   Loop counter on receiving volume element number
C  IVOL     Loop counter on sending volume zone number
C  JCOMP    Loop counter on component number
C  JELEM    Loop counter on element number
C  JELMV    Loop counter on receiving volume element number
C  JSURF    Loop counter on surface zone number
C  JVOL     Loop counter on receiving volume zone number
C  NCOMPS   Number of components in a surface zone
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C**** Set all DEA arrays to zero
      DO 300 JSURF=1,NSURF
         DO 100 ISURF=1,NSURF
            DEASS(ISURF,JSURF)=0.
100      CONTINUE
         DO 200 IVOL=1,NVOL
	    DEAGS(IVOL,JSURF)=0.
200      CONTINUE
300   CONTINUE
      DO 600 JVOL=1,NVOL
         DO 400 IVOL=1,NVOL
	    DEAGG(IVOL,JVOL)=0.
400	 CONTINUE
         DO 500 ISURF=1,NSURF
	    DEASG(ISURF,JVOL)=0.
500	 CONTINUE
600   CONTINUE

C**** Process each surface zone as the sender
      DO 2000 ISURF=1,NSURF
C****    Determine number of components of this surface zone
C****    If it has no components proceed to the next zone
         NCOMPS=SFZONE(ISURF,1)
         IF (NCOMPS.EQ.0) GOTO 2000
C****    There are some components so set arrays DEAELM,DEAELV to zero
         DO 1100 ITOELM=1,NELEMS
            DEAELM(ITOELM)=0.
1100     CONTINUE
         DO 1200 ITOELV=1,NELEMV
	    DEAELV(ITOELV)=0.
1200	 CONTINUE

C****    Sum the component DEAs to each element to get the zone to element DEAs
         DO 1500 ICOMP=1,NCOMPS
            IELEM=SFZONE(ISURF,ICOMP+1)
            DO 1300 ITOELM=1,NELEMS
               DEAELM(ITOELM)=DEAELM(ITOELM)
     1                        +VUSS(IELEM,ITOELM)*ELAREA(IELEM)
1300        CONTINUE
            DO 1400 ITOELV=1,NELEMV
	       DEAELV(ITOELV)=DEAELV(ITOELV)
     1                        +VUSG(IELEM,ITOELV)*ELAREA(IELEM)
1400        CONTINUE
1500     CONTINUE
C****    Sum the zone to element DEAs for all the components which make up
C****    each receiving surface zone
         DO 1700 JSURF=1,NSURF
C****       Determine the number of components in the receiving zone
C****       If there are no components proceed to next receiving zone
            NCOMPS=SFZONE(JSURF,1)
            DO 1600 JCOMP=1,NCOMPS
               JELEM=SFZONE(JSURF,JCOMP+1)
               DEASS(ISURF,JSURF)=DEASS(ISURF,JSURF)+DEAELM(JELEM)
1600        CONTINUE
1700     CONTINUE
           
C****    Sum the zone to element DEAs for all the components which make up
C****    each receiving volume zone
         DO 1900 JVOL=1,NVOL
C****	    Determine the number of components in the receiving zone
C****       If there are no components proceed to the next zone
            NCOMPS=VLZONE(JVOL,1)
	    DO 1800 JCOMP=1,NCOMPS
	       JELMV=VLZONE(JVOL,JCOMP+1)
	       DEASG(ISURF,JVOL)=DEASG(ISURF,JVOL)+DEAELV(JELMV)
1800	    CONTINUE
1900     CONTINUE
 
2000  CONTINUE

C**** Process each volume zone as the sender
      DO 3000 IVOL=1,NVOL
C****    Determine number of components of this volume zone
C****    If it has no components proceed to the next zone
         NCOMPS=VLZONE(IVOL,1)
         IF (NCOMPS.EQ.0) GOTO 3000
C****    There are some components so set arrays DEAELM,DEAELV to zero
         DO 2100 ITOELV=1,NELEMV
	    DEAELV(ITOELV)=0.
2100	 CONTINUE
         DO 2200 ITOELM=1,NELEMS
            DEAELM(ITOELM)=0.
2200     CONTINUE

C****    Sum the component DEAs to each element to get the zone to element DEAs
         DO 2500 ICOMP=1,NCOMPS
            IELMV=VLZONE(IVOL,ICOMP+1)
            DO 2300 ITOELV=1,NELEMV
               DEAELV(ITOELV)=DEAELV(ITOELV)
     1                        +VUGG(IELMV,ITOELV)*VLAREA(IELMV)
2300        CONTINUE
            DO 2400 ITOELM=1,NELEMS
	       DEAELM(ITOELM)=DEAELM(ITOELM)
     1                        +VUGS(IELMV,ITOELM)*VLAREA(IELMV)
2400        CONTINUE
2500     CONTINUE
C****    Sum the zone to element DEAs for all the components which make up
C****    each receiving volume zone
         DO 2700 JVOL=1,NVOL
C****       Determine the number of components in the receiving zone
C****       If there are no components proceed to next receiving zone
            NCOMPS=VLZONE(JVOL,1)
            DO 2600 JCOMP=1,NCOMPS
               JELMV=VLZONE(JVOL,JCOMP+1)
               DEAGG(IVOL,JVOL)=DEAGG(IVOL,JVOL)+DEAELV(JELMV)
2600        CONTINUE
2700     CONTINUE
C****    Sum the zone to element DEAs for all the components which make up
C****    each receiving surface zone
         DO 2900 JSURF=1,NSURF
C****	    Determine the number of components in the receiving zone
C****       If there are no components proceed to the next zone
            NCOMPS=SFZONE(JSURF,1)
	    DO 2800 JCOMP=1,NCOMPS
	       JELEM=SFZONE(JSURF,JCOMP+1)
	       DEAGS(IVOL,JSURF)=DEAGS(IVOL,JSURF)+DEAELM(JELEM)
2800	    CONTINUE
2900     CONTINUE
3000  CONTINUE

      RETURN
      END

      SUBROUTINE SMOOTH(LUSTOT,IATM,DEASS,DEASG,DEAGG,DEAGS,NSURF,NVOL,
     1                  NZONES,NVZONE,ACTZON,ACTVZN,ACTGAS,ZNAREA,
     2                  CVAREA,MXZONE,MXVZON,MXSIZE,NATMMX,X,R,D)
C
C  Routine to "smooth" the direct exchange areas resulting from the Monte
C  Carlo method.  First the DEA arrays are made symmetric.  They then no longer
C  have the row sum property so a constrained minimisation approach as given
C  in Larsen & Howell, J Heat Transfer 108:239-242(1986) is taken
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**01/05/90
C
C
      INTEGER LUSTOT,IATM,NSURF,NVOL,NZONES,NVZONE,ACTZON(MXZONE),
     1        ACTVZN(MXVZON),ACTGAS(MXVZON),MXZONE,MXVZON,MXSIZE,
     2        NATMMX
      REAL DEASS(MXZONE,MXZONE),DEASG(MXZONE,MXVZON),
     1     DEAGG(MXVZON,MXVZON),DEAGS(MXVZON,MXZONE),
     2     ZNAREA(MXZONE),CVAREA(NATMMX,MXVZON),X(MXSIZE,MXSIZE),
     3     R(MXSIZE,MXSIZE),D(MXSIZE)
      INTEGER I,IPIVOT,IROW,ISURF,ISZONE,IVOL,IVZONE,J,JCOL,JSURF,
     1        JSZONE,JVOL,JVZONE,NACTGZ,NSIZEX
      REAL FACT,HOLD,SUM,SUMI
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  ACTGAS  Array to be used as work space to hold the actual gas zone 
C          numbers for this atmosphere (ie those with non-zero volume and
C          non-zero absorptivity)
C  ACTVZN  Array giving the numbers of the actual volume zones (ie those
C          with non-zero volume)
C  ACTZON  Array giving the numbers of the actual surface zones (ie those
C          with non-zero area)
C  CVAREA  Array of the "notional" areas of the volume zones
C  D       Storage vector used to hold the rhs in equation for Lagrange 
C          multipliers and the the Lagrange multipliers themselves
C  DEAGG   Array of gas to gas direct exchange areas
C  DEAGS   Array of gas to surface direct exchange areas
C  DEAGS   Array of surface to surface direct exchange areas
C  DEASS   Array of surface to surface direct exchange areas
C  IATM    Atmosphere number
C  LUSTOT  Logical unit number for standard output
C  MXSIZE  Maximum number of zones in total (surface plus volume)
C  MXVZON  Maximum number of volume zones
C  MXZONE  Maximum number of surface zones
C  NATMMX  Maximum number of atmospheres allowed
C  NSURF   Number of surface zones
C  NVOL    Number of volume zones
C  NVZONE  Number of actual volume zones
C  NZONES  Number of actual surface zones
C  R       Storage array used to hold coefficients in equation to be solved
C          for Lagrange multipliers
C  X       Array of work space used in Larsen & Howell method (corresponds
C          to matrix X in their paper)
C  ZNAREA  Array of surface zone areas
C
C**** Arguments - output
C  DEAGG   Array of smoothed gas to gas direct exchange areas
C  DEAGS   Array of smoothed gas to surface direct exchange areas
C  DEASG   Array of smoothed surface to gas direct exchange areas
C  DEASS   Array of smoothed surface to surface direct exchange areas
C  
C  
C**** Local variables
C  FACT    Factor (multiplier) used in the Gaussian elimination
C  HOLD    Temporary storage
C  I       Loop counter for row number (actual zones)
C  IPIVOT  Number of pivotal row
C  IROW    Loop counter on row number (actual zones)
C  ISURF   Loop counter on surface zone number or surface zone number of
C          actual zone number I
C  ISZONE  Loop counter over surface zones
C  IVOL    Volume zone number of actual zone number I
C  IVZONE  Loop counter over volume zones
C  J       Loop counter on column number (actual zones)
C  JCOL    Loop counter on column number (actual zones)
C  JSURF   Loop counter on surface zone number or surface zone number of
C          actual zone number J
C  JSZONE  Loop counter over surface zones
C  JVOL    Volume zone number of actual zone number J
C  JVZONE  Loop counter over volume zones
C  NACTGZ  Number of actual gas zones for this atmosphere
C  NSIZEX  Size of the matrix X
C  SUM     Sum used in back substitution of Gaussian elimination
C  SUMI    Sum of row of weights or of row of deas
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C**** Make the DEAs symmetric
      DO 200 ISURF=1,NSURF-1
         DO 100 JSURF=ISURF+1,NSURF
            DEASS(ISURF,JSURF)=0.5*(DEASS(ISURF,JSURF)
     1                              +DEASS(JSURF,ISURF))
            DEASS(JSURF,ISURF)=DEASS(ISURF,JSURF)
100      CONTINUE
200   CONTINUE
      IF (IATM.GT.1) THEN
C****    The first atmosphere is clear so no gas DEAs are involved
         DO 400 IVOL=1,NVOL-1
            DO 300 JVOL=IVOL+1,NVOL
               DEAGG(IVOL,JVOL)=0.5*(DEAGG(IVOL,JVOL)+DEAGG(JVOL,IVOL))
C23/7/98       Correction - line below added to make the new DEAGG 
C              matrix symmetric
               DEAGG(JVOL,IVOL)=DEAGG(IVOL,JVOL)
300         CONTINUE
400      CONTINUE
         DO 600 ISURF=1,NSURF
            DO 500 IVOL=1,NVOL
               DEASG(ISURF,IVOL)=0.5*(DEASG(ISURF,IVOL)
     1                                +DEAGS(IVOL,ISURF))
               DEAGS(IVOL,ISURF)=DEASG(ISURF,IVOL)
500         CONTINUE
600      CONTINUE
      END IF

C**** Determine the actual gas zones for this atmosphere
      NACTGZ=0
      DO 800 IVZONE=1,NVZONE
         ACTGAS(IVZONE)=0
800   CONTINUE
C**** For the first atmosphere the gas is clear ie no actual zones
      IF (IATM.GT.1) THEN
         DO 900 IVZONE=1,NVZONE
            IVOL=ACTVZN(IVZONE)
            IF (CVAREA(IATM,IVOL).GT.0.) THEN
               NACTGZ=NACTGZ+1
               ACTGAS(NACTGZ)=IVOL
            END IF
900      CONTINUE
      END IF

C**** Set size of matrix X
      NSIZEX=NZONES+NACTGZ

C**** Load the DEAs into X
      DO 1200 ISZONE=1,NZONES
         ISURF=ACTZON(ISZONE)
         DO 1100 JSZONE=1,NZONES
            JSURF=ACTZON(JSZONE)
            X(ISZONE,JSZONE)=DEASS(ISURF,JSURF)
1100     CONTINUE
1200  CONTINUE
      DO 1400 IVZONE=1,NACTGZ
         IVOL=ACTGAS(IVZONE)
         DO 1300 JVZONE=1,NACTGZ
            JVOL=ACTGAS(JVZONE)
            X(NZONES+IVZONE,NZONES+JVZONE)=DEAGG(IVOL,JVOL)
1300     CONTINUE
1400  CONTINUE
      DO 1600 ISZONE=1,NZONES
         ISURF=ACTZON(ISZONE)
         DO 1500 JVZONE=1,NACTGZ
            JVOL=ACTGAS(JVZONE)
            X(ISZONE,NZONES+JVZONE)=DEASG(ISURF,JVOL)
            X(NZONES+JVZONE,ISZONE)=DEAGS(JVOL,ISURF)
1500     CONTINUE
1600  CONTINUE

C**** Set the matrix R (The weights are the squares of the elements of X)
      DO 1800 I=1,NSIZEX
         SUMI=0.
         DO 1700 J=1,NSIZEX
            R(I,J)=X(I,J)*X(I,J)
            SUMI=SUMI+R(I,J)
1700     CONTINUE
         R(I,I)=R(I,I)+SUMI
1800  CONTINUE

C**** Set the right hand side vector D
      DO 2000 ISZONE=1,NZONES
         SUMI=0.
         DO 1900 J=1,NSIZEX
            SUMI=SUMI+X(ISZONE,J)
1900     CONTINUE
         ISURF=ACTZON(ISZONE)
         D(ISZONE)=ZNAREA(ISURF)-SUMI
2000  CONTINUE
      DO 2200 IVZONE=1,NACTGZ
         SUMI=0.
         DO 2100 J=1,NSIZEX
            SUMI=SUMI+X(NZONES+IVZONE,J)
2100     CONTINUE
         IVOL=ACTGAS(IVZONE)
         D(NZONES+IVZONE)=CVAREA(IATM,IVOL)-SUMI
2200  CONTINUE

C**** Solve the simultaneous linear equations R.lam=D by using
C**** Gaussian elimination with partial pivoting - the solution
C**** vector lam is stored in D
      DO 2700 J=1,NSIZEX-1
C****    Eliminate a column at a time; first find the pivot for the column
         IPIVOT=J
         DO 2300 IROW=J+1,NSIZEX
            IF (ABS(R(IROW,J)).GT.ABS(R(IPIVOT,J))) IPIVOT=IROW
2300     CONTINUE
C****    If the pivot row is not row J swap rows in R and D
         IF (IPIVOT.NE.J) THEN
            DO 2400 JCOL=J,NSIZEX
               HOLD=R(J,JCOL)
               R(J,JCOL)=R(IPIVOT,JCOL)
               R(IPIVOT,JCOL)=HOLD
2400        CONTINUE
            HOLD=D(J)
            D(J)=D(IPIVOT)
            D(IPIVOT)=HOLD
         END IF
C****    Check that the matrix is not singular
         IF (R(J,J).EQ.0.) THEN
            WRITE (LUSTOT,3800)
            GOTO 3900
         END IF
C****    Eliminate column J below the diagonal
         DO 2600 IROW=J+1,NSIZEX
            FACT=R(IROW,J)/R(J,J)
            DO 2500 JCOL=J+1,NSIZEX
               R(IROW,JCOL)=R(IROW,JCOL)-FACT*R(J,JCOL)
2500        CONTINUE
            D(IROW)=D(IROW)-FACT*D(J)
2600     CONTINUE
2700  CONTINUE
C**** Now that elimination is complete check for simgularity
      IF (R(NSIZEX,NSIZEX).EQ.0.) THEN
         WRITE (LUSTOT,3800)
         GOTO 3900
      END IF

C**** Use back substitution to find the solution vector lam (the 
C**** Lagrange multipliers) storing it in D
      D(NSIZEX)=D(NSIZEX)/R(NSIZEX,NSIZEX)
      DO 2900 I=NSIZEX-1,1,-1
         SUM=0.
         DO 2800 JCOL=I+1,NSIZEX
            SUM=SUM+R(I,JCOL)*D(JCOL)
2800     CONTINUE
         D(I)=(D(I)-SUM)/R(I,I)
2900  CONTINUE

C***  Now correct the matrix X so that its rows have the correct sums
      DO 3100 J=1,NSIZEX
         DO 3000 I=1,NSIZEX
            X(I,J)=X(I,J)+X(I,J)*X(I,J)*(D(I)+D(J))
3000     CONTINUE
3100  CONTINUE

C**** Load the corrected values back from X into the DEA arrays
      DO 3400 I=1,NZONES
         ISURF=ACTZON(I)
         DO 3200 J=1,NZONES
            JSURF=ACTZON(J)
            DEASS(ISURF,JSURF)=X(I,J)
3200     CONTINUE
         DO 3300 J=NZONES+1,NSIZEX
            JVOL=ACTGAS(J-NZONES)
            DEASG(ISURF,JVOL)=X(I,J)
3300     CONTINUE
3400  CONTINUE
      DO 3700 I=NZONES+1,NSIZEX
         IVOL=ACTGAS(I-NZONES)
         DO 3500 J=1,NZONES
            JSURF=ACTZON(J)
            DEAGS(IVOL,JSURF)=X(I,J)
3500     CONTINUE
         DO 3600 J=NZONES+1,NSIZEX
            JVOL=ACTGAS(J-NZONES)
            DEAGG(IVOL,JVOL)=X(I,J)
3600     CONTINUE
3700  CONTINUE

3800  FORMAT (' IN SMOOTHING THE DEAs THE MATRIX R IS SINGULAR'/
     1        ' THE PROGRAM WILL CONTINUE WITH UNSMOOTHED DEAs'//)
3900  CONTINUE

      RETURN
      END

      SUBROUTINE VFCRS(IATM,DEASS,DEASG,DEAGG,DEAGS,NSURF,NVOL,NZONES,
     1                 NVZONE,ACTZON,ACTVZN,ZNAREA,CVAREA,MXZONE,MXVZON,
     2                 NATMMX,VFCSS,VFCSG,VFCGG,VFCGS)
C
C  Routine to calculate the coarse grid view factors from the coarse grid
C  direct exchange areas
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**22/11/90
C
C
      INTEGER IATM,NSURF,NVOL,NZONES,NVZONE,ACTZON(MXZONE),
     1        ACTVZN(MXVZON),MXZONE,MXVZON,NATMMX
      REAL DEASS(MXZONE,MXZONE),DEASG(MXZONE,MXVZON),
     1     DEAGG(MXVZON,MXVZON),DEAGS(MXVZON,MXZONE),
     2     ZNAREA(MXZONE),CVAREA(NATMMX,MXVZON),
     3     VFCSS(MXZONE,MXZONE),VFCSG(MXZONE,MXVZON),
     4     VFCGG(MXVZON,MXVZON),VFCGS(MXVZON,MXZONE)
      INTEGER I,J,ISURF,JSURF,IVOL,JVOL
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  ACTVZN  Array of actual volume zone numbers (those with non-zero volume)
C  ACTZON  Array of actual surface zone numbers (those with non-zero area)
C  CVAREA  Array of notional volume zone areas for each atmosphere
C  DEAGG   Array of gas to gas direct exchange areas
C  DEAGS   Array of gas to surface direct exchange areas
C  DESSG   Array of surface to gas direct exchange areas
C  DEASS   Array of surface to surface direct exchange areas
C  IATM    Number of current atmosphere
C  MXVZON  Maximum number of volume zones allowed
C  MXZONE  Maximum number of surface zones allowed
C  NATMMX  Maximum number of atmospheres allowed
C  NSURF   Number of surface zones
C  NVOL    Number of volume zones 
C  NVZONE  Number of actual volume zones
C  NZONES  Number of actual surface zones
C  VFCGG   Array to hold gas to gas view factors
C  VFCGS   Array to hold gas to surface view factors
C  VFCSG   Array to hold surface to gas view factors
C  VFCSS   Array to hold surface to surface view factors
C  ZNAREA  Array of surface zone areas
C
C**** Arguments - output
C  VFCGG   Array of gas to gas view factors
C  VFCGS   Array of gas to surface view factors
C  VFCSG   Array of surface to gas view factors
C  VFCSS   Array of surface to surface view factors
C
C**** Local variables
C  I       Loop counter over sending zone
C  ISURF   Sending surface zone number
C  IVOL    Sending volume zone number
C  J       Loop counter over receiving zone
C  JSURF   Receiving zone number
C  JVOL    Receiving volume number
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C**** Set all vfc arrays to zero
      DO 300 J=1,NSURF
         DO 100 I=1,NSURF
            VFCSS(I,J)=0.
100      CONTINUE
         DO 200 I=1,NVOL
            VFCGS(I,J)=0.
200      CONTINUE
300   CONTINUE
      DO 600 J=1,NVOL
         DO 400 I=1,NVOL
            VFCGG(I,J)=0.
400      CONTINUE
         DO 500 I=1,NSURF
            VFCSG(I,J)=0.
500      CONTINUE
600   CONTINUE

C**** Compute view factors by dividing deas by sending zone area
      DO 1300 I=1,NZONES
         ISURF=ACTZON(I)
         DO 1100 J=1,NZONES
            JSURF=ACTZON(J)
            VFCSS(ISURF,JSURF)=DEASS(ISURF,JSURF)/ZNAREA(ISURF)
1100     CONTINUE
         DO 1200 J=1,NVZONE
            JVOL=ACTVZN(J)
            VFCSG(ISURF,JVOL)=DEASG(ISURF,JVOL)/ZNAREA(ISURF)
1200     CONTINUE
1300  CONTINUE
      DO 1600 I=1,NVZONE
         IVOL=ACTVZN(I)
C****    Check that the "notional" area of this volume zone is non-zero
         IF (CVAREA(IATM,IVOL).EQ.0) GOTO 1600
         DO 1400 J=1,NVZONE
            JVOL=ACTVZN(J)
            VFCGG(IVOL,JVOL)=DEAGG(IVOL,JVOL)/CVAREA(IATM,IVOL)
1400     CONTINUE
         DO 1500 J=1,NZONES
            JSURF=ACTZON(J)
            VFCGS(IVOL,JSURF)=DEAGS(IVOL,JSURF)/CVAREA(IATM,IVOL)
1500     CONTINUE
1600  CONTINUE

      RETURN
      END
 
 
      SUBROUTINE TEACRS(DEASS,DEASG,DEAGG,DEAGS,IATM,NSURF,NVOL,NZONES,
     1                  NVZONE,ACTZON,ACTVZN,ZNAREA,EMISS,LUSTOT,R,TEMP,
     2                  TEMPV,MXZN,MXVZN,TEASS,TEASG,TEAGG,TEAGS)
C
C  Routine to convert the direct exchange areas into total exchange areas
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**23/11/90
C
C
      INTEGER NSURF,NVOL,NZONES,NVZONE,ACTZON(MXZN),ACTVZN(MXVZN),
     1        LUSTOT,MXZN,MXVZN,IATM
      REAL DEASS(MXZN,MXZN),DEASG(MXZN,MXVZN),DEAGG(MXVZN,MXVZN),
     1     DEAGS(MXVZN,MXZN),ZNAREA(MXZN),EMISS(MXZN),R(MXZN,MXZN),
     2     TEMP(MXZN,MXZN),TEMPV(MXZN),TEASS(MXZN,MXZN),
     3     TEASG(MXZN,MXVZN),TEAGG(MXVZN,MXVZN),TEAGS(MXVZN,MXZN)
      INTEGER I,IFROM,IPIVOT,IROW,ISURF,ITO,J,JCOL,JSURF,K,KSURF,
     1        IVOL,JVOL
      REAL EPSI,EPSJ,FACT,HOLD,RHO,SUM,AREAI
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  ACTVZN  Array of actual volume zone numbers (ie those with non-zero volume)
C  ACTZON  Array of actual surface zone numbers (ie those with non-zero area)
C  DEAGG   Array of gas to gas direct exchange areas
C  DEAGS   Array of gas to surface direct exchange areas
C  DEASG   Array of surface to gas direct exchange areas
C  DEASS   Array of surface to surface direct exchange areas
C  EMISS   Array of surface zone emissivities
C  IATM    Number of the atmosphere
C  LUSTOT  Logical unit number for standard output
C  MXVZN   Maximum number of volume zones allowed
C  MXZN    Maximum number of surface zones allowed
C  NSURF   Number of surface zones
C  NVOL    Number of volume zones
C  NVZONE  Number of actual volume zones
C  NZONES  Number of actual surface zones
C  R       Storage array used to hold the matrix (Da-ssDrho)**(-1)
C  TEAGG   Array to hold gas to gas total exchange areas
C  TEAGS   Array to hold gas to surface total exchange areas
C  TEASG   Array to hold surface to gas total exchange areas
C  TEASS   Array to hold surface to surface total exchange areas
C  TEMP    Storage array used in calculation of R
C  TEMPV   Storage vector used in calculation of R
C  ZNAREA  Array of surface zone areas
C
C**** Arguments - output
C  TEAGG   Array of gas to gas total exchange areas
C  TEAGS   Array of gas to surface total exchange areas
C  TEASG   Array of surface to gas total exchange areas
C  TEASS   Array of surface to surface total exchange areas
C
C**** Local variables
C  AREAI   Area of surface zone ISURF
C  EPSI    Emissivity of surface ISURF
C  EPSJ    Emissivity of surface JSURF
C  FACT    Factor (multiplier used in the computation of matrix R
C  HOLD    Temporary storage
C  I       Loop counter on row number
C  IFROM   Loop counter on sending surface zone
C  IPIVOT  Number of the pivotal row
C  IROW    Loop counter on row number
C  ISURF   Loop counter on surface zone number or surface zone number of
C          actual surface zone I
C  ITO     Loop counter on receiving zone number
C  IVOL    Loop counter on volume zone number
C  J       Loop counter on column number
C  JCOL    Loop counter on column number
C  JSURF   Surface zone number of actual surface zone J
C  JVOL    Loop counter on volume zone number
C  K       Loop counter in matrix multiplication
C  KSURF   Surface zone number of actual surface zone K
C  NZONES  Number of surface zones
C  RHO     Surface reflectivity
C  SUM     Sum of various quantities
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************
C
C     The formula for the computation of the total exchange areas are
C
C          SS = D   D R(ss)D
C                eps a      eps
C
C          SG = D   D R(sg)
C                eps a
C
C          GG = (gg) + (gs)D   R(sg)
C                           rho
C
C                 t
C          GS = SG
C
C                            -1
C     where R = (D -(ss)D   )
C                 a      rho
C
C    D denotes a diagonal matrix and the subscript indicates the quantity
C    to be found on the diagonal
C
C******************************************************************************

C**** Set all TEA arrays to zero
      DO 20 ITO=1,NSURF
         DO 10 IFROM=1,NSURF
            TEASS(IFROM,ITO)=0.
10       CONTINUE
         DO 15 IFROM=1,NVOL
            TEAGS(IFROM,ITO)=0.
15       CONTINUE
20    CONTINUE
      DO 40 ITO=1,NVOL
         DO 30 IFROM=1,NVOL
            TEAGG(IFROM,ITO)=0.
30       CONTINUE
         DO 35 IFROM=1,NSURF
            TEASG(IFROM,ITO)=0.
35       CONTINUE
40    CONTINUE

C**** The first stage is to find R, the inverse of Da-ssDrho
C**** Calculate Da-ssDrho and store in TEMP and set R to the identity
       DO 60 I=1,NZONES
          ISURF=ACTZON(I)
          DO 50 J=1,NZONES
             JSURF=ACTZON(J)
             RHO=1.-EMISS(JSURF)
             TEMP(I,J)=-DEASS(ISURF,JSURF)*RHO
             R(I,J)=0.
50       CONTINUE
         TEMP(I,I)=ZNAREA(ISURF)+TEMP(I,I)
         R(I,I)=1.
60    CONTINUE
C**** Reduce TEMP to an upper triangular matrix using Gauss elimination
C**** with pivoting with matrix R as the right hand side
      DO 140 J=1,NZONES-1
C****    Find the pivot row for column J
         IPIVOT=J
         DO 90 IROW=J+1,NZONES
            IF (ABS(TEMP(IROW,J)).GT.ABS(TEMP(IPIVOT,J))) IPIVOT=IROW
90       CONTINUE
C****    If row J is not the pivot swap rows J and IPIVOT
         IF (IPIVOT.EQ.J) GOTO 110
         DO 100 JCOL=1,NZONES
C****       Swap rows in matrix TEMP
            HOLD=TEMP(J,JCOL)
            TEMP(J,JCOL)=TEMP(IPIVOT,JCOL)
            TEMP(IPIVOT,JCOL)=HOLD
C****       Swap rows in matrix R
            HOLD=R(J,JCOL)
            R(J,JCOL)=R(IPIVOT,JCOL)
            R(IPIVOT,JCOL)=HOLD
100      CONTINUE
C****    Check that TEMP is not singular
110      IF (TEMP(J,J).EQ.0.) GOTO 500
C****    Use row J to eliminate column J below the diagonal
         DO 130 IROW=J+1,NZONES
            FACT=TEMP(IROW,J)/TEMP(J,J)
            DO 120 JCOL=1,NZONES
               TEMP(IROW,JCOL)=TEMP(IROW,JCOL)-FACT*TEMP(J,JCOL)
C****          Do the same to R as is done to TEMP
               R(IROW,JCOL)=R(IROW,JCOL)-FACT*R(J,JCOL)
120         CONTINUE
130      CONTINUE
140   CONTINUE
C**** Check that TEMP is not singular
      IF (TEMP(NZONES,NZONES).EQ.0.) GOTO 500
C**** TEMP has been made upper triangular, do back substitution to
C**** find the inverse of the original matrix, storing it in R
      DO 180 JCOL=1,NZONES
C****    Back substitution for column JCOL of R
C***     Solution temporaorily held in TEMPV
         TEMPV(NZONES)=R(NZONES,JCOL)/TEMP(NZONES,NZONES)
         DO 160 I=NZONES-1,1,-1
            SUM=0.
            DO 150 J=I+1,NZONES
               SUM=SUM+TEMP(I,J)*TEMPV(J)
150         CONTINUE
            TEMPV(I)=(R(I,JCOL)-SUM)/TEMP(I,I)
160      CONTINUE
C****    Now load the solution from TEMPV into column JCOL of R
         DO 170 I=1,NZONES
            R(I,JCOL)=TEMPV(I)
170      CONTINUE
180   CONTINUE

C**** R now contains the inverse of Da-ssDrho
C**** First calculate TEASS - find Rss
      DO 210 I=1,NZONES
         ISURF=ACTZON(I)
         DO 200 J=1,NZONES
            JSURF=ACTZON(J)
            SUM=0.
            DO 190 K=1,NZONES
               KSURF=ACTZON(K)
               SUM=SUM+R(I,K)*DEASS(KSURF,JSURF)
190         CONTINUE
            TEASS(ISURF,JSURF)=SUM
200      CONTINUE
210   CONTINUE
C**** Premultiply Rss by DepsDa and postmultiply by Deps
      DO 230 I=1,NZONES
         ISURF=ACTZON(I)
         EPSI=EMISS(ISURF)
         DO 220 J=1,NZONES
            JSURF=ACTZON(J)
            EPSJ=EMISS(JSURF)
            TEASS(ISURF,JSURF)=EPSI*ZNAREA(ISURF)*TEASS(ISURF,JSURF)
     1                         *EPSJ
220      CONTINUE
230   CONTINUE

C**** Other TEAs are not required if this is atmosphere 1
      IF (IATM.EQ.1) GOTO 490
      
C**** Calculate Rsg and store the result in TEMP
      DO 260 I=1,NZONES
         ISURF=ACTZON(I)
         DO 250 J=1,NVZONE
            JVOL=ACTVZN(J)
            SUM=0.
            DO 240 K=1,NZONES
               KSURF=ACTZON(K)
C23/7/98  Correction to error in this line
C         Index for matrix R should be I and K not ISURF and KSURF.
               SUM=SUM+R(I,K)*DEASG(KSURF,JVOL)
240         CONTINUE
            TEMP(ISURF,JVOL)=SUM
250      CONTINUE
260   CONTINUE

C**** Set TEASG by premultiplying Rsg by DepsDa
C**** then TEAGS is the transpose of TEAGS
      DO 280 I=1,NZONES
         ISURF=ACTZON(I)
         EPSI=EMISS(ISURF)
         AREAI=ZNAREA(ISURF)
         DO 270 J=1,NVZONE
            JVOL=ACTVZN(J)
            TEASG(ISURF,JVOL)=EPSI*AREAI*TEMP(ISURF,JVOL)
            TEAGS(JVOL,ISURF)=TEASG(ISURF,JVOL)
270      CONTINUE
280   CONTINUE

C**** Premultiply Rsg by gsDpho and add gg to get TEAGG
      DO 310 I=1,NVZONE
         IVOL=ACTVZN(I)
         DO 300 J=1,NVZONE
            JVOL=ACTVZN(J)
            SUM=0.
            DO 290 K=1,NZONES
               KSURF=ACTZON(K)
               RHO=1.-EMISS(KSURF)
               SUM=SUM+DEAGS(IVOL,KSURF)*RHO*TEMP(KSURF,JVOL)
290         CONTINUE
            TEAGG(IVOL,JVOL)=SUM+DEAGG(IVOL,JVOL)
300      CONTINUE
310   CONTINUE

C**** Skip down here if the gas is clear
490   CONTINUE

      RETURN

C     If the matrix Da-ssDrho is singular R cannot be found
C     Print an error message and stop the program
500   WRITE (LUSTOT,510)
510   FORMAT (' MATRIX INVERSION TO FIND TOTAL EXCHANGE AREAS FAILS'/
     1        ' DUE TO SINGULARITY OF MATRIX'/
     2        ' RUN ABORTED')
      STOP

      END

