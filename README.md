# F77-3D
A very basic (and slow) raycasting engine, written in FORTRAN 77 for MS-DOS. The
program uses BIOS interrupts for input handling and video memory access.

![f77-3d](f773d.gif)

## Compilation
Due to the limitations of ANSI FORTRAN 77 language standard, the program must be
compiled with the (free) [Open Watcom FORTRAN 77](http://www.openwatcom.org/)
and linked with the DOS/4G 32-bit DOS extender.

Build and run the executable with:

```
> wfl386 src\f773d.for /l=dos4g
> f773d.exe
```

## Licence
ISC
