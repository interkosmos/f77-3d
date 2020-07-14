C     ******************************************************************
C
C     F77-3D: A BASIC RAYCASTING ENGINE IN FORTRAN 77 FOR MS-DOS.
C
C     COMPILE WITH OPEN WATCOM FORTRAN 77:
C     > WFL386 F773D.FOR /L=DOS4G
C
C     AUTHOR:  PHILIPP ENGEL
C     LICENCE: ISC
C
C     ******************************************************************
C
*$pragma aux delay "*_" parm (value)
C
C     ******************************************************************
      PROGRAM MAIN
      EXTERNAL RAYCST, ROTATE, SETPIX, SETMOD
      LOGICAL  GETCHR, MOVE
      CHARACTER*1 CHR
      INTEGER     CODE, X
      LOGICAL     CTRL, MOVED
      REAL        DIST, DIRX, DIRY, PLANEX, PLANEY, POSX, POSY

      POSX   =  2.0
      POSY   =  3.0
      DIRX   = -1.0
      DIRY   =  0.0
      PLANEX =  0.0
      PLANEY =  0.66

      CALL SETMOD(1)

      MOVED = .TRUE.

      DO
        IF (GETCHR(CHR, CODE, CTRL)) THEN
          SELECT CASE (CODE)
C
C           ACTION: QUIT (Q).
C
            CASE (16)
              EXIT
C
C           ACTION: QUIT (CTRL + C).
C
            CASE (46)
              IF (CTRL) THEN
                EXIT
              END IF
C
C           ACTION: MOVE UP.
C
            CASE (72)
              MOVED = MOVE(POSX, POSY, DIRX, DIRY, 0.1)
C
C           ACTION: MOVE LEFT.
C
            CASE (75)
              CALL ROTATE(DIRX, DIRY, 0.05)
              CALL ROTATE(PLANEX, PLANEY, 0.05)
              MOVED = .TRUE.
C
C           ACTION: MOVE RIGHT.
C
            CASE (77)
              CALL ROTATE(DIRX, DIRY, -0.05)
              CALL ROTATE(PLANEX, PLANEY, -0.05)
              MOVED = .TRUE.
C
C           ACTION: MOVE DOWN.
C
            CASE (80)
              MOVED = MOVE(POSX, POSY, DIRX, DIRY, -0.1)
          END SELECT
        END IF

        IF (MOVED) THEN
          DO X = 0, 319
            CALL RAYCST(X, POSX, POSY, DIRX, DIRY, PLANEX, PLANEY, DIST)
          END DO
          MOVED = .FALSE.
        END IF

C        CALL DELAY(20)
      END DO

      CALL SETMOD(0)
      STOP
      END
C
C     ******************************************************************
C
      LOGICAL FUNCTION GETCHR(CHR, CODE, CTRL)
C
C     RETURNS .TRUE. IF KEY HAS BEEN PRESSED, ELSE .FALSE. THIS
C     FUNCTION CHECKS BIOS INTERRUPT 16H TO GET CHAR AND SCAN CODE.
C
C     ARGUMENTS:
C
C       CHR     -   THE CHARACTER OF THE KEY.
C       CODE    -   THE SCAN CODE OF THE KEY.
C       CTRL    -   FLAG FOR CTRL KEY PRESSED.
C
      INCLUDE 'DOS.FI'
      INTEGER INT, MASK
      PARAMETER (INT='16'X, MASK='40'X)
      CHARACTER*1 CHR
      INTEGER     CODE
      LOGICAL     CTRL

      GETCHR = .FALSE.
      CTRL   = .FALSE.

      AH = '1'X
      CALL FINTR(INT, REGS)

      IF (IAND(EFLAGS, MASK) .EQ. 0) THEN
        GETCHR = .TRUE.
        AH = '0'X
        CALL FINTR(INT, REGS)
        CHR = CHAR(AL)
        CODE = AH

        AH = '2'X
        CALL FINTR(INT, REGS)
        IF (IAND(ISHFT('1'X, 2), AL)) CTRL = .TRUE.
      END IF
      END
C
C     ******************************************************************
C
      LOGICAL FUNCTION MOVE(X, Y, DIRX, DIRY, SPEED)
      INTEGER MW, MH
      PARAMETER (MW=12, MH=12)
      REAL X, Y, DIRX, DIRY, SPEED
      REAL NX, NY
      INTEGER MAP(MW, MH)
      COMMON /WORLD/ MAP

      MOVE = .FALSE.

      NX = X + DIRX * SPEED
      NY = Y + DIRY * SPEED

      IF (NX .LE. 1 .OR. NX .GE. MW .OR. NY .LE. 1 .OR. NY .GE. MH)
     &  RETURN

      IF (MAP(INT(NX) + 1, INT(Y) + 1) .EQ. 0) THEN
        X = NX
        MOVE = .TRUE.
      END IF

      IF (MAP(INT(X) + 1, INT(NY) + 1) .EQ. 0) THEN
        Y = NY
        MOVE = .TRUE.
      END IF
      END

C
C     ******************************************************************
C
      SUBROUTINE SETMOD(MODE)
C
C     SETS THE VIDEO MODE USING BIOS INTERRUPT 13H.
C
C     ARGUMENTS:
C
C       MODE    -   THE SCREEN MODE TO SET:
C
C                       0   -   DEFAULT TEXT MODE (3H).
C                       1   -   VGA, 320X200, 256 COLOURS (13H).
C
      INCLUDE 'DOS.FI'
      INTEGER MODE
      INTEGER INT
      PARAMETER (INT='10'X)
      AH = '0'X
      SELECT CASE (MODE)
        CASE (1)
          AL = '13'X
        CASE DEFAULT
          AL = '3'X
      END SELECT
      CALL FINTR(INT, REGS)
      END
C
C     ******************************************************************
C
      SUBROUTINE SETPIX(X, Y, COLOR)
C
C     DRAWS A SINGLE PIXEL IN GIVEN COLOR.
C
C     ARGUMENTS:
C
C       X       -   THE COLUMN (0 ... 320).
C       Y       -   THE ROW    (0 ... 200).
C       C       -   THE COLOUR (0 ... 255).
C
      INTEGER   SCRSZE
      PARAMETER (SCRSZE=320 * 200)
      INTEGER   X, Y, COLOR
      INTEGER*1 SCREEN(:)
      INTEGER   ISTAT, OFFSET
      SAVE      SCREEN

      IF (.NOT. ALLOCATED(SCREEN)) THEN
        ALLOCATE (SCREEN(0:SCRSZE - 1), LOCATION='A0000'X, STAT=ISTAT)
        IF (ISTAT .NE. 0) STOP 'Error: Could not access video memeory.'
      END IF

      OFFSET = ISHFT(Y, 8) + ISHFT(Y, 6) + X
      SCREEN(OFFSET) = COLOR
      END
C
C     ******************************************************************
C
      SUBROUTINE RAYCST(X, POSX, POSY, DIRX, DIRY, PLANEX, PLANEY, DIST)
C
C     CASTS A RAY IN A GIVEN DIRECTION X AND RENDERS SCENE.
C
C     ALGORITHM IS TAKEN FROM:
C
C         http://lodev.org/cgtutor/raycasting.html
C
      EXTERNAL SETPIX
      INTEGER MW, MH, W, H, W2, H2
      PARAMETER (MW=12, MH=12, W=320, H=200, W2=W / 2, H2=H / 2)
      INTEGER   X
      REAL      POSX, POSY, DIRX, DIRY, PLANEX, PLANEY, DIST
      INTEGER   C, LENGTH, MAPX, MAPY, SIDE, STEPX, STEPY, WALL
      INTEGER   Y, Y1, Y2
      INTEGER   MAP(MW, MH)
      REAL      CAMX, DDISTX, DDISTY, RAYX, RAYY, SIDEX, SIDEY
      REAL      WALLX, WDIST
      COMMON /WORLD/ MAP

      CAMX = 2 * X / REAL(W) - 1
      RAYX = DIRX + PLANEX * CAMX
      RAYY = DIRY + PLANEY * CAMX

      MAPX = INT(POSX)
      MAPY = INT(POSY)

      DDISTX = ABS(1 / RAYX)
      DDISTY = ABS(1 / RAYY)

      IF (RAYX .LT. 0) THEN
        STEPX = -1
        SIDEX = (POSX - MAPX) * DDISTX
      ELSE
        STEPX = 1
        SIDEX = (MAPX + 1.0 - POSX) * DDISTX
      END IF

      IF (RAYY .LT. 0) THEN
        STEPY = -1
        SIDEY = (POSY - MAPY) * DDISTY
      ELSE
        STEPY = 1
        SIDEY = (MAPY + 1.0 - POSY) * DDISTY
      END IF

      WALL = 0

      DO
        IF (SIDEX .LT. SIDEY) THEN
          SIDEX = SIDEX + DDISTX
          MAPX  = MAPX + STEPX
          SIDE  = 0
        ELSE
          SIDEY = SIDEY + DDISTY
          MAPY  = MAPY + STEPY
          SIDE  = 1
        END IF

        IF (MAP(MAPX + 1, MAPY + 1) .GT. 0) THEN
          WALL = MAP(MAPX + 1, MAPY + 1)
          EXIT
        END IF
      END DO

      IF (WALL .EQ. 0) RETURN

      IF (SIDE .EQ. 0) THEN
        WDIST = (MAPX - POSX + (1 - STEPX) / 2) / RAYX
        WALLX = POSY + WDIST * RAYY
      ELSE
        WDIST = (MAPY - POSY + (1 - STEPY) / 2) / RAYY
        WALLX = POSX + WDIST * RAYX
      END IF

      IF (X .EQ. W2) DIST = WDIST

      WALLX  = WALLX - INT(WALLX)
      LENGTH = INT(H / WDIST) / 2

      Y1 = -LENGTH + H2
      Y2 =  LENGTH + H2

      DO Y = 0, H - 1
        C = 0
        IF (Y .GE. Y1 .AND. Y .LE. Y2) C = WALL
        IF (SIDE .EQ. 1 .AND. C .GT. 0) C = C + 144
        IF (Y .GT. Y2) C = 20
        CALL SETPIX(X, Y, C)
      END DO
      END
C
C     ******************************************************************
C
      SUBROUTINE ROTATE(X, Y, ANGLE)
      REAL X, Y, ANGLE
      REAL OLDX

      OLDX = X
      X =    X * COS(ANGLE) - Y * SIN(ANGLE)
      Y = OLDX * SIN(ANGLE) + Y * COS(ANGLE)
      END
C
C     ******************************************************************
C
      BLOCK DATA
      INTEGER   MAP(12, 12)
      COMMON /WORLD/  MAP
      DATA MAP /38,40,40,43,40,43,40,43,40,43,40,38,
     &          38,00,00,00,00,00,00,00,00,00,00,38,
     &          38,00,00,00,00,00,00,00,00,00,00,31,
     &          44,00,00,32,00,00,00,00,00,00,00,38,
     &          38,00,31,48,00,00,00,00,00,00,00,38,
     &          44,00,00,00,38,38,37,00,00,00,00,31,
     &          38,00,00,00,38,00,00,00,00,00,00,38,
     &          38,00,00,00,00,00,00,00,00,00,00,38,
     &          38,00,00,00,00,00,00,00,00,33,00,31,
     &          38,00,00,00,00,00,00,00,33,33,00,38,
     &          38,00,00,00,00,00,00,00,00,00,00,38,
     &          38,43,43,43,43,43,43,43,43,43,43,38/
      END
C     ******************************************************************
