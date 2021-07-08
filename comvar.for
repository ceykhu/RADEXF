


C**** File of parameter values and COMMON blocks for RADEXF

C----- P A R A M E T E R S -----
C-------------------------------

C**** FINE GRID GEOMETRY PARAMETERS
C**** Max. number of vertices is NVMAX-1
C**** Max. number of z slices is NZMAX-1
C**** Max. number of obstacles is NOBMAX
C**** Max. number of wall in a cell is MXWLCL
      PARAMETER (NVMAX=8,NZMAX=5,NOBMAX=4,MXWLCL=4)

C**** FINE GRID ZONING PARAMETERS
C**** Max. number of fine grid surface zones (elements) is MXELEM
C**** Max. number of fine grid volume zones (elements) is MXELMV
      PARAMETER (MXELEM=80,MXELMV=20)

C**** COARSE GRID ZONING PARAMETERS
C**** Max. number of coarse grid surface zones is MXZN
C**** Max. number of coarse grid volume zones is MXVZN
C**** Max. number of components of a coarse grid surface zone is MXCP
C**** Max. number of components of a coarse grid volume zone is MXZNC
      PARAMETER (MXZN=72,MXVZN=20,MXCP=6,MXVZNC=4)

C**** ATMOSPHERE PARAMETERS
C**** Max. number of atmospheres allowed is NATMAX
      PARAMETER (NATMAX=12)
      
C**** DO NOT ALTER THESE PARAMETERS
      PARAMETER (NOBCMX=1,MXSFZN=MXELEM,MXZNC=1)

C--------------------------------------
C----- C O M M O N   B L O C K S  -----
C--------------------------------------

      INTEGER NV
      REAL VX(NVMAX),VY(NVMAX)
      COMMON / GEOMRY / VX,VY,NV

      REAL GX(NVMAX),GY(NVMAX),AREA(2,NVMAX,NVMAX),
     1     AREASG(8*NVMAX,NZMAX-1)
      INTEGER VXGRID(NVMAX),VYGRID(NVMAX),NGX,NGY
      COMMON / GRIDS / GX,GY,NGX,NGY,VXGRID,VYGRID,AREA,AREASG

      INTEGER NGZ
      REAL GZ(NZMAX)
      COMMON / ZGRID / NGZ,GZ      

      REAL GLO(NOBMAX,3),GHI(NOBMAX,3)
      INTEGER OBSTXS(NOBMAX),OBSTXE(NOBMAX),OBSTYS(NOBMAX),
     1        OBSTYE(NOBMAX),OBSTZS(NOBMAX),OBSTZE(NOBMAX),
     2        CELLOB(NVMAX,NVMAX,NZMAX,NOBCMX),
     3        NOBST(NVMAX,NVMAX,NZMAX),NOB
      LOGICAL OBINOT(NOBMAX,NVMAX,NVMAX,7)
      CHARACTER*1 TYPE(NOBMAX)
      COMMON / OBST / NOB,GLO,GHI,OBSTXS,OBSTXE,OBSTYS,OBSTYE,OBSTZS,
     1                OBSTZE,CELLOB,NOBST,TYPE,OBINOT

      REAL WSEGXS(8*NVMAX),WSEGYS(8*NVMAX)
      INTEGER CELLWS(NVMAX,NVMAX,MXWLCL),VXWSEG(NVMAX),
     1        SEGCEL(8*NVMAX,2),NSEG,
     2        CLINOT(2,NVMAX,NVMAX),SGINOT(8*NVMAX,NZMAX)
      COMMON / WALL / WSEGXS,WSEGYS,NSEG,CELLWS,VXWSEG,SEGCEL,CLINOT,
     1                SGINOT

      REAL COINC,PI
      COMMON / RUNCON / COINC,PI
      
      INTEGER SGSFZN(NZMAX-1,8*NVMAX),CLSFZN(2,NVMAX,NVMAX),
     1        NSFZCP(MXSFZN),SFZN(MXSFZN,MXZNC,5),NZONE,
     2        OBSFZN(NVMAX,NVMAX,NZMAX,7),
     3        CLVLZN(NVMAX,NVMAX,NZMAX),VLZN(MXELMV,3)
      COMMON / ZONES / SGSFZN,CLSFZN,NSFZCP,SFZN,NZONE,OBSFZN,
     1                 CLVLZN,VLZN

      INTEGER NELEMS,NELEMV
      REAL ELAREA(MXELEM)
      COMMON / ELEMS / NELEMS,ELAREA,NELEMV

      LOGICAL COARSE(NVMAX),COARSZ(NZMAX)
      INTEGER SFZONE(MXZN,MXCP),VLZONE(MXVZN,MXVZNC)
      COMMON / CRSGD / COARSE,COARSZ,SFZONE,VLZONE

      REAL VUFCSS(NATMAX,MXELEM,MXELEM),VUFCSG(NATMAX,MXELEM,MXELMV),
     1     VUFCGG(NATMAX,MXELMV,MXELMV),VUFCGS(NATMAX,MXELMV,MXELEM)
      COMMON / RADFAC / VUFCSS,VUFCSG,VUFCGG,VUFCGS
