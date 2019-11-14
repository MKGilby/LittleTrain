ASHScript 1.1 revision 0
BlockSize width 32 height 32
NewImage name "notrotated" widthb 113 heightb 1
NewImage name "rotated" widthb 55 heightb 4
LoadImage name "sprites0" filename "sprites0.tga"
LoadImage name "sprites1" filename "sprites1.tga"
CopyPartialImage source "sprites0" leftb 0 top 0 target "notrotated" leftb 0 topb 0 widthb 23 heightb 1
CopyPartialImage source "sprites0" leftb 23 top 0 target "rotated" leftb 0 topb 0 widthb 2 heightb 1
CopyPartialImage source "sprites0" leftb 25 top 0 target "notrotated" leftb 23 topb 0 widthb 11 heightb 1
CopyPartialImage source "sprites0" leftb 36 top 0 target "rotated" leftb 2 topb 0 widthb 2 heightb 1
CopyPartialImage source "sprites0" leftb 38 top 0 target "notrotated" leftb 34 topb 0 widthb 9 heightb 1
CopyPartialImage source "sprites0" leftb 47 top 0 target "rotated" leftb 4 topb 0 widthb 9 heightb 1
CopyPartialImage source "sprites0" leftb 56 top 0 target "notrotated" leftb 43 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites0" leftb 57 top 0 target "rotated" leftb 13 topb 0 widthb 3 heightb 1
CopyPartialImage source "sprites0" leftb 60 top 0 target "notrotated" leftb 44 topb 0 widthb 18 heightb 1
CopyPartialImage source "sprites0" leftb 78 top 0 target "rotated" leftb 16 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites0" leftb 79 top 0 target "notrotated" leftb 62 topb 0 widthb 12 heightb 1
CopyPartialImage source "sprites0" leftb 91 top 0 target "rotated" leftb 17 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites0" leftb 92 top 0 target "notrotated" leftb 74 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites0" leftb 93 top 0 target "rotated" leftb 18 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites0" leftb 94 top 0 target "notrotated" leftb 75 topb 0 widthb 3 heightb 1
CopyPartialImage source "sprites0" leftb 97 top 0 target "rotated" leftb 19 topb 0 widthb 3 heightb 1
CopyPartialImage source "sprites0" leftb 100 top 0 target "notrotated" leftb 78 topb 0 widthb 2 heightb 1
CopyPartialImage source "sprites0" leftb 102 top 0 target "rotated" leftb 22 topb 0 widthb 2 heightb 1
CopyPartialImage source "sprites0" leftb 104 top 0 target "notrotated" leftb 80 topb 0 widthb 8 heightb 1
CopyPartialImage source "sprites0" leftb 112 top 0 target "rotated" leftb 24 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites0" leftb 113 top 0 target "notrotated" leftb 88 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites0" leftb 114 top 0 target "rotated" leftb 25 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites1" leftb 0 top 0 target "rotated" leftb 26 topb 0 widthb 2 heightb 1
CopyPartialImage source "sprites1" leftb 2 top 0 target "notrotated" leftb 89 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites1" leftb 3 top 0 target "rotated" leftb 28 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites1" leftb 4 top 0 target "notrotated" leftb 90 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites1" leftb 5 top 0 target "rotated" leftb 29 topb 0 widthb 2 heightb 1
CopyPartialImage source "sprites1" leftb 7 top 0 target "notrotated" leftb 91 topb 0 widthb 8 heightb 1
CopyPartialImage source "sprites0" leftb 47 top 0 target "rotated" leftb 31 top 0 widthb 1 heightb 1
CopyPartialImage source "sprites0" leftb 48 top 0 target "rotated" leftb 32 top 0 widthb 1 heightb 1
CopyPartialImage source "sprites0" leftb 49 top 0 target "rotated" leftb 33 top 0 widthb 1 heightb 1
CopyPartialImage source "sprites0" leftb 53 top 0 target "rotated" leftb 34 top 0 widthb 1 heightb 1
CopyPartialImage source "sprites0" leftb 50 top 0 target "rotated" leftb 35 top 0 widthb 1 heightb 1
CopyPartialImage source "sprites0" leftb 51 top 0 target "rotated" leftb 36 top 0 widthb 1 heightb 1
CopyPartialImage source "sprites0" leftb 52 top 0 target "rotated" leftb 37 top 0 widthb 1 heightb 1
CopyPartialImage source "sprites0" leftb 54 top 0 target "rotated" leftb 38 top 0 widthb 1 heightb 1
CopyPartialImage source "rotated" leftb 31 top 16 target "rotated" leftb 32 top 16 widthb 1 height 16
CopyPartialImage source "rotated" leftb 31 top 16 target "rotated" leftb 33 top 16 widthb 2 height 16
CopyPartialImage source "rotated" leftb 31 top 16 target "rotated" leftb 35 top 16 widthb 4 height 16
LoadImage name "conga" filename "conga.tga"
NewImage name "present" width 32 height 32
NewImage name "wrapper" width 32 height 32
CopyPartialImage source "conga" left 0 top 0 target "present" left 0 top 0 widthb 1 heightb 1
CopyPartialImage source "conga" leftb 1 top 0 target "wrapper" left 0 top 0 widthb 1 heightb 1
RecolorRGB name "present" r 255 g 0 b 0
RecolorRGB name "wrapper" r 0 g 255 b 0
CopyImage source "wrapper" target "present" colorkeyrgb 0 0 0
CopyPartialImage source "present" left 0 top 0 target "notrotated" leftb 99 top 0 widthb 1 heightb 1
CopyPartialImage source "conga" left 0 top 0 target "present" left 0 top 0 widthb 1 heightb 1
CopyPartialImage source "conga" leftb 1 top 0 target "wrapper" left 0 top 0 widthb 1 heightb 1
RecolorRGB name "present" r 255 g 0 b 0
RecolorRGB name "wrapper" r 0 g 64 b 255
CopyImage source "wrapper" target "present" colorkeyrgb 0 0 0
CopyPartialImage source "present" left 0 top 0 target "notrotated" leftb 100 top 0 widthb 1 heightb 1
CopyPartialImage source "conga" left 0 top 0 target "present" left 0 top 0 widthb 1 heightb 1
CopyPartialImage source "conga" leftb 1 top 0 target "wrapper" left 0 top 0 widthb 1 heightb 1
RecolorRGB name "present" r 255 g 255 b 0
RecolorRGB name "wrapper" r 255 g 0 b 0
CopyImage source "wrapper" target "present" colorkeyrgb 0 0 0
CopyPartialImage source "present" left 0 top 0 target "notrotated" leftb 101 top 0 widthb 1 heightb 1
CopyPartialImage source "conga" left 0 top 0 target "present" left 0 top 0 widthb 1 heightb 1
CopyPartialImage source "conga" leftb 1 top 0 target "wrapper" left 0 top 0 widthb 1 heightb 1
RecolorRGB name "present" r 255 g 255 b 0
RecolorRGB name "wrapper" r 0 g 64 b 255
CopyImage source "wrapper" target "present" colorkeyrgb 0 0 0
CopyPartialImage source "present" left 0 top 0 target "notrotated" leftb 102 top 0 widthb 1 heightb 1
CopyPartialImage source "conga" left 0 top 0 target "present" left 0 top 0 widthb 1 heightb 1
CopyPartialImage source "conga" leftb 1 top 0 target "wrapper" left 0 top 0 widthb 1 heightb 1
RecolorRGB name "present" r 0 g 64 b 255
RecolorRGB name "wrapper" r 255 g 0 b 255
CopyImage source "wrapper" target "present" colorkeyrgb 0 0 0
CopyPartialImage source "present" left 0 top 0 target "notrotated" leftb 103 top 0 widthb 1 heightb 1
CopyPartialImage source "conga" left 0 top 0 target "present" left 0 top 0 widthb 1 heightb 1
CopyPartialImage source "conga" leftb 1 top 0 target "wrapper" left 0 top 0 widthb 1 heightb 1
RecolorRGB name "present" r 0 g 255 b 0
RecolorRGB name "wrapper" r 255 g 0 b 0
CopyImage source "wrapper" target "present" colorkeyrgb 0 0 0
CopyPartialImage source "present" left 0 top 0 target "notrotated" leftb 104 top 0 widthb 1 heightb 1
CopyPartialImage source "conga" left 0 top 0 target "present" left 0 top 0 widthb 1 heightb 1
CopyPartialImage source "conga" leftb 1 top 0 target "wrapper" left 0 top 0 widthb 1 heightb 1
RecolorRGB name "present" r 0 g 255 b 0
RecolorRGB name "wrapper" r 255 g 0 b 255
CopyImage source "wrapper" target "present" colorkeyrgb 0 0 0
CopyPartialImage source "present" left 0 top 0 target "notrotated" leftb 105 top 0 widthb 1 heightb 1
CopyPartialImage source "conga" left 0 top 0 target "present" left 0 top 0 widthb 1 heightb 1
CopyPartialImage source "conga" leftb 1 top 0 target "wrapper" left 0 top 0 widthb 1 heightb 1
RecolorRGB name "present" r 255 g 0 b 255
RecolorRGB name "wrapper" r 255 g 255 b 0
CopyImage source "wrapper" target "present" colorkeyrgb 0 0 0
CopyPartialImage source "present" left 0 top 0 target "notrotated" leftb 106 top 0 widthb 1 heightb 1
FreeImage name "wrapper"
FreeImage name "present"
LoadImage name "congafnt" filename "congafnt.tga"
NewImage name "fonttmp" width 144 height 21
NewImage name "wagon" widthb 2 heightb 1
CopyPartialImage source "conga" leftb 2 top 0 target "wagon" left 0 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga" leftb 2 top 0 target "wagon" leftb 1 top 0 widthb 1 heightb 1 transform "H"
CopyImage source "congafnt" target "fonttmp"
RecolorRGB name "fonttmp" r 255 g 0 b 0
CopyPartialImage source "fonttmp" left 0 top 0 target "wagon" left 2 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 12 top 0 target "wagon" left 50 top 2 width 12 height 21
CopyImage source "congafnt" target "fonttmp"
RecolorRGB name "fonttmp" r 0 g 255 b 0
CopyPartialImage source "fonttmp" left 12 top 0 target "wagon" left 18 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 0 top 0 target "wagon" left 34 top 2 width 12 height 21
CopyPartialImageRepeat count 2 deltaleftb 1 deltatop 0 source "wagon" left 0 top 0 target "rotated" leftb 39 topb 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga" leftb 2 top 0 target "wagon" left 0 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga" leftb 2 top 0 target "wagon" leftb 1 top 0 widthb 1 heightb 1 transform "H"
CopyImage source "congafnt" target "fonttmp"
RecolorRGB name "fonttmp" r 0 g 64 b 255
CopyPartialImage source "fonttmp" left 24 top 0 target "wagon" left 2 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 36 top 0 target "wagon" left 50 top 2 width 12 height 21
CopyImage source "congafnt" target "fonttmp"
RecolorRGB name "fonttmp" r 255 g 255 b 0
CopyPartialImage source "fonttmp" left 36 top 0 target "wagon" left 18 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 24 top 0 target "wagon" left 34 top 2 width 12 height 21
CopyPartialImageRepeat count 2 deltaleftb 1 deltatop 0 source "wagon" left 0 top 0 target "rotated" leftb 41 topb 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga" leftb 2 top 0 target "wagon" left 0 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga" leftb 2 top 0 target "wagon" leftb 1 top 0 widthb 1 heightb 1 transform "H"
CopyImage source "congafnt" target "fonttmp"
RecolorRGB name "fonttmp" r 255 g 0 b 255
CopyPartialImage source "fonttmp" left 48 top 0 target "wagon" left 2 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 60 top 0 target "wagon" left 50 top 2 width 12 height 21
CopyImage source "congafnt" target "fonttmp"
RecolorRGB name "fonttmp" r 0 g 255 b 255
CopyPartialImage source "fonttmp" left 60 top 0 target "wagon" left 18 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 48 top 0 target "wagon" left 34 top 2 width 12 height 21
CopyPartialImageRepeat count 2 deltaleftb 1 deltatop 0 source "wagon" left 0 top 0 target "rotated" leftb 43 topb 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga" leftb 2 top 0 target "wagon" left 0 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga" leftb 2 top 0 target "wagon" leftb 1 top 0 widthb 1 heightb 1 transform "H"
CopyImage source "congafnt" target "fonttmp"
RecolorRGB name "fonttmp" r 255 g 0 b 0
CopyPartialImage source "fonttmp" left 72 top 0 target "wagon" left 2 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 84 top 0 target "wagon" left 50 top 2 width 12 height 21
CopyImage source "congafnt" target "fonttmp"
RecolorRGB name "fonttmp" r 0 g 255 b 0
CopyPartialImage source "fonttmp" left 84 top 0 target "wagon" left 18 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 72 top 0 target "wagon" left 34 top 2 width 12 height 21
CopyPartialImageRepeat count 2 deltaleftb 1 deltatop 0 source "wagon" left 0 top 0 target "rotated" leftb 45 topb 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga" leftb 2 top 0 target "wagon" left 0 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga" leftb 2 top 0 target "wagon" leftb 1 top 0 widthb 1 heightb 1 transform "H"
CopyImage source "congafnt" target "fonttmp"
RecolorRGB name "fonttmp" r 0 g 64 b 255
CopyPartialImage source "fonttmp" left 96 top 0 target "wagon" left 2 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 60 top 0 target "wagon" left 50 top 2 width 12 height 21
CopyImage source "congafnt" target "fonttmp"
RecolorRGB name "fonttmp" r 255 g 255 b 0
CopyPartialImage source "fonttmp" left 60 top 0 target "wagon" left 18 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 96 top 0 target "wagon" left 34 top 2 width 12 height 21
CopyPartialImageRepeat count 2 deltaleftb 1 deltatop 0 source "wagon" left 0 top 0 target "rotated" leftb 47 topb 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga" leftb 2 top 0 target "wagon" left 0 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga" leftb 2 top 0 target "wagon" leftb 1 top 0 widthb 1 heightb 1 transform "H"
CopyImage source "congafnt" target "fonttmp"
RecolorRGB name "fonttmp" r 255 g 0 b 255
CopyPartialImage source "fonttmp" left 72 top 0 target "wagon" left 2 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 108 top 0 target "wagon" left 50 top 2 width 12 height 21
CopyImage source "congafnt" target "fonttmp"
RecolorRGB name "fonttmp" r 0 g 255 b 255
CopyPartialImage source "fonttmp" left 108 top 0 target "wagon" left 18 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 72 top 0 target "wagon" left 34 top 2 width 12 height 21
CopyPartialImageRepeat count 2 deltaleftb 1 deltatop 0 source "wagon" left 0 top 0 target "rotated" leftb 49 topb 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga" leftb 2 top 0 target "wagon" left 0 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga" leftb 2 top 0 target "wagon" leftb 1 top 0 widthb 1 heightb 1 transform "H"
CopyImage source "congafnt" target "fonttmp"
RecolorRGB name "fonttmp" r 255 g 0 b 0
CopyPartialImage source "fonttmp" left 12 top 0 target "wagon" left 2 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 24 top 0 target "wagon" left 50 top 2 width 12 height 21
CopyImage source "congafnt" target "fonttmp"
RecolorRGB name "fonttmp" r 0 g 255 b 0
CopyPartialImage source "fonttmp" left 24 top 0 target "wagon" left 18 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 12 top 0 target "wagon" left 34 top 2 width 12 height 21
CopyPartialImageRepeat count 2 deltaleftb 1 deltatop 0 source "wagon" left 0 top 0 target "rotated" leftb 51 topb 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga" leftb 2 top 0 target "wagon" left 0 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga" leftb 2 top 0 target "wagon" leftb 1 top 0 widthb 1 heightb 1 transform "H"
CopyImage source "congafnt" target "fonttmp"
RecolorRGB name "fonttmp" r 0 g 64 b 255
CopyPartialImage source "fonttmp" left 120 top 0 target "wagon" left 2 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 132 top 0 target "wagon" left 50 top 2 width 12 height 21
CopyImage source "congafnt" target "fonttmp"
RecolorRGB name "fonttmp" r 255 g 255 b 0
CopyPartialImage source "fonttmp" left 132 top 0 target "wagon" left 18 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 120 top 0 target "wagon" left 34 top 2 width 12 height 21
CopyPartialImageRepeat count 2 deltaleftb 1 deltatop 0 source "wagon" left 0 top 0 target "rotated" leftb 53 topb 0 widthb 1 heightb 1 transform "H"
FreeImage name "wagon"
FreeImage name "fonttmp"
FreeImage name "congafnt"
FreeImage name "conga"
CopyPartialImage source "notrotated" leftb 43 top 0 target "notrotated" leftb 107 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "notrotated" leftb 45 top 0 target "notrotated" leftb 108 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "notrotated" leftb 43 top 0 target "notrotated" leftb 109 top 0 widthb 1 heightb 1 transform "HR"
CopyPartialImage source "notrotated" leftb 45 top 0 target "notrotated" leftb 110 top 0 widthb 1 heightb 1 transform "HR"
CopyPartialImage source "notrotated" leftb 43 top 0 target "notrotated" leftb 111 top 0 widthb 1 heightb 1 transform "Hr"
CopyPartialImage source "notrotated" leftb 45 top 0 target "notrotated" leftb 112 top 0 widthb 1 heightb 1 transform "Hr"
FreeImage name "sprites0"
FreeImage name "sprites1"
CopyPartialImageRepeat count 55 deltaleftb 1 deltatop 0 source "rotated" leftb 0 top 0 target "rotated" leftb 0 topb 1 widthb 1 heightb 1 transform "H"
CopyPartialImageRepeat count 55 deltaleftb 1 deltatop 0 source "rotated" leftb 0 top 0 target "rotated" leftb 0 topb 2 widthb 1 heightb 1 transform "HR"
CopyPartialImageRepeat count 55 deltaleftb 1 deltatop 0 source "rotated" leftb 0 top 0 target "rotated" leftb 0 topb 3 widthb 1 heightb 1 transform "Hr"
Animation Name "#" image "notrotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 0 0
Animation Name "!" image "notrotated" widthb 1 heightb 1 Framecount 7 Framedelay 6 loopdelay 0 paused Framesb 68 0 65 0 66 0 63 0 64 0 62 0 67 0
Animation Name "%" image "rotated" widthb 1 heightb 1 Framecount 8 Framedelay 6 loopdelay 0 looped Framesb 4 0 5 0 6 0 10 0 7 0 8 0 9 0 11 0
Animation Name "%L" image "rotated" widthb 1 heightb 1 Framecount 8 Framedelay 6 loopdelay 0 looped Framesb 4 1 5 1 6 1 10 1 7 1 8 1 9 1 11 1
Animation Name "%U" image "rotated" widthb 1 heightb 1 Framecount 8 Framedelay 6 loopdelay 0 looped Framesb 4 2 5 2 6 2 10 2 7 2 8 2 9 2 11 2
Animation Name "%D" image "rotated" widthb 1 heightb 1 Framecount 8 Framedelay 6 loopdelay 0 looped Framesb 4 3 5 3 6 3 10 3 7 3 8 3 9 3 11 3
Animation Name "*" image "notrotated" widthb 1 heightb 1 Framecount 8 Framedelay 6 loopdelay 0 looped Framesb 55 0 49 0 56 0 50 0 51 0 52 0 53 0 54 0
Animation Name "a" image "notrotated" widthb 1 heightb 1 Framecount 6 Framedelay 6 loopdelay 0 looped randomstart Framesb 75 0 74 0 76 0 78 0 77 0 79 0
Animation Name "b" image "notrotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 22 0
Animation Name "c" image "notrotated" widthb 1 heightb 1 Framecount 9 Framedelay 6 loopdelay 0 looped Framesb 80 0 81 0 82 0 83 0 84 0 85 0 86 0 87 0 80 0
Animation Name "d" image "notrotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 89 0
Animation Name "e" image "notrotated" widthb 1 heightb 1 Framecount 5 Framedelay 6 loopdelay 0 looped Framesb 8 0 9 0 10 0 11 0 8 0
Animation Name "f" image "notrotated" widthb 1 heightb 1 Framecount 4 Framedelay 6 loopdelay 0 looped randomstart pingpong Framesb 3 0 4 0 2 0 1 0
Animation Name "g" image "notrotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 21 0
Animation Name "h" image "notrotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 5 0
Animation Name "i" image "notrotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 88 0
Animation Name "j" image "notrotated" widthb 1 heightb 1 Framecount 5 Framedelay 6 loopdelay 0 looped randomstart pingpong Framesb 57 0 58 0 59 0 60 0 61 0
Animation Name "k" image "notrotated" widthb 1 heightb 1 Framecount 8 Framedelay 6 loopdelay 0 looped randomstart Framesb 23 0 24 0 25 0 26 0 27 0 28 0 29 0 30 0
Animation Name "l" image "notrotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 12 0
Animation Name "m" image "notrotated" widthb 1 heightb 1 Framecount 10 Framedelay 6 loopdelay 0 looped randomstart Framesb 69 0 70 0 71 0 71 0 70 0 69 0 72 0 73 0 73 0 72 0
Animation Name "n" image "notrotated" widthb 1 heightb 1 Framecount 8 Framedelay 6 loopdelay 0 looped randomstart Framesb 13 0 14 0 15 0 16 0 17 0 18 0 19 0 20 0
Animation Name "o" image "notrotated" widthb 1 heightb 1 Framecount 4 Framedelay 10 loopdelay 0 looped Framesb 31 0 32 0 31 0 33 0
Animation Name "p" image "notrotated" widthb 1 heightb 1 Framecount 8 Framedelay 6 loopdelay 0 looped randomstart Framesb 91 0 92 0 93 0 94 0 95 0 96 0 97 0 98 0
Animation Name "q" image "notrotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 90 0
Animation Name "r" image "notrotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 44 0
Animation Name "A" image "rotated" widthb 1 heightb 1 Framecount 6 Framedelay 6 loopdelay 0 looped randomstart Framesb 19 0 18 0 20 0 22 0 21 0 23 0
Animation Name "AL" image "rotated" widthb 1 heightb 1 Framecount 6 Framedelay 6 loopdelay 0 looped randomstart Framesb 19 1 18 1 20 1 22 1 21 1 23 1
Animation Name "AU" image "rotated" widthb 1 heightb 1 Framecount 6 Framedelay 6 loopdelay 0 looped randomstart Framesb 19 2 18 2 20 2 22 2 21 2 23 2
Animation Name "AD" image "rotated" widthb 1 heightb 1 Framecount 6 Framedelay 6 loopdelay 0 looped randomstart Framesb 19 3 18 3 20 3 22 3 21 3 23 3
Animation Name "B" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 27 0
Animation Name "BL" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 27 1
Animation Name "BU" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 27 2
Animation Name "BD" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 27 3
Animation Name "C" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 13 0
Animation Name "CL" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 13 1
Animation Name "CU" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 13 2
Animation Name "CD" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 13 3
Animation Name "D" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 26 0
Animation Name "DL" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 26 1
Animation Name "DU" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 26 2
Animation Name "DD" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 26 3
Animation Name "E" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 3 0
Animation Name "EL" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 3 1
Animation Name "EU" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 3 2
Animation Name "ED" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 3 3
Animation Name "F" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 17 0
Animation Name "FL" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 17 1
Animation Name "FU" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 17 2
Animation Name "FD" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 17 3
Animation Name "G" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 16 0
Animation Name "GL" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 16 1
Animation Name "GU" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 16 2
Animation Name "GD" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 16 3
Animation Name "H" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 12 0
Animation Name "HL" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 12 1
Animation Name "HU" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 12 2
Animation Name "HD" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 12 3
Animation Name "I" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 25 0
Animation Name "IL" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 25 1
Animation Name "IU" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 25 2
Animation Name "ID" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 25 3
Animation Name "J" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 24 0
Animation Name "JL" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 24 1
Animation Name "JU" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 24 2
Animation Name "JD" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 24 3
Animation Name "K" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 29 0
Animation Name "KL" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 29 1
Animation Name "KU" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 29 2
Animation Name "KD" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 29 3
Animation Name "L" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 1 0
Animation Name "LL" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 1 1
Animation Name "LU" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 1 2
Animation Name "LD" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 1 3
Animation Name "M" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 28 0
Animation Name "ML" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 28 1
Animation Name "MU" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 28 2
Animation Name "MD" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 28 3
Animation Name "N" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 0 0
Animation Name "NL" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 0 1
Animation Name "NU" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 0 2
Animation Name "ND" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 0 3
Animation Name "O" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 2 0
Animation Name "OL" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 2 1
Animation Name "OU" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 2 2
Animation Name "OD" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 2 3
Animation Name "P" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 30 0
Animation Name "PL" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 30 1
Animation Name "PU" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 30 2
Animation Name "PD" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 30 3
Animation Name "Q" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 14 0
Animation Name "QL" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 14 1
Animation Name "QU" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 14 2
Animation Name "QD" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 14 3
Animation Name "R" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 15 0
Animation Name "RL" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 15 1
Animation Name "RU" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 15 2
Animation Name "RD" image "rotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 15 3
Animation Name "c1" image "notrotated" widthb 1 heightb 1 Framecount 6 Framedelay 1 loopdelay 0 Framesb 43 0 45 0 46 0 48 0 47 0 34 0
Animation Name "c2" image "notrotated" widthb 1 heightb 1 Framecount 8 Framedelay 6 loopdelay 0 looped Framesb 36 0 38 0 37 0 39 0 40 0 35 0 41 0 42 0
Animation Name "-" image "notrotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 7 0
Animation Name "=" image "notrotated" widthb 1 heightb 1 Framecount 1 Framedelay 0 loopdelay 0 Framesb 6 0
Animation Name "$" image "rotated" widthb 1 heightb 1 FrameCount 8 FrameDelay 6 looped Framesb 31 0 32 0 33 0 34 0 35 0 36 0 37 0 38 0
Animation Name "$L" image "rotated" widthb 1 heightb 1 FrameCount 8 FrameDelay 6 looped Framesb 31 1 32 1 33 1 34 1 35 1 36 1 37 1 38 1
Animation Name "$U" image "rotated" widthb 1 heightb 1 FrameCount 8 FrameDelay 6 looped Framesb 31 2 32 2 33 2 34 2 35 2 36 2 37 2 38 2
Animation Name "$D" image "rotated" widthb 1 heightb 1 FrameCount 8 FrameDelay 6 looped Framesb 31 3 32 3 33 3 34 3 35 3 36 3 37 3 38 3
Animation name "s" Image "notrotated" widthb 1 heightb 1 framecount 1 framesb 99 0
Animation name "t" Image "notrotated" widthb 1 heightb 1 framecount 1 framesb 100 0
Animation name "u" Image "notrotated" widthb 1 heightb 1 framecount 1 framesb 101 0
Animation name "v" Image "notrotated" widthb 1 heightb 1 framecount 1 framesb 102 0
Animation name "w" Image "notrotated" widthb 1 heightb 1 framecount 1 framesb 103 0
Animation name "x" Image "notrotated" widthb 1 heightb 1 framecount 1 framesb 104 0
Animation name "y" Image "notrotated" widthb 1 heightb 1 framecount 1 framesb 105 0
Animation name "z" Image "notrotated" widthb 1 heightb 1 framecount 1 framesb 106 0
Animation name "S" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 39 0 40 0
Animation name "SL" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 39 1 40 1
Animation name "SU" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 39 2 40 2
Animation name "SD" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 39 3 40 3
Animation name "T" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 41 0 42 0
Animation name "TL" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 41 1 42 1
Animation name "TU" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 41 2 42 2
Animation name "TD" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 41 3 42 3
Animation name "U" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 43 0 44 0
Animation name "UL" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 43 1 44 1
Animation name "UU" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 43 2 44 2
Animation name "UD" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 43 3 44 3
Animation name "V" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 45 0 46 0
Animation name "VL" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 45 1 46 1
Animation name "VU" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 45 2 46 2
Animation name "VD" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 45 3 46 3
Animation name "W" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 47 0 48 0
Animation name "WL" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 47 1 48 1
Animation name "WU" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 47 2 48 2
Animation name "WD" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 47 3 48 3
Animation name "X" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 49 0 50 0
Animation name "XL" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 49 1 50 1
Animation name "XU" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 49 2 50 2
Animation name "XD" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 49 3 50 3
Animation name "Y" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 51 0 52 0
Animation name "YL" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 51 1 52 1
Animation name "YU" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 51 2 52 2
Animation name "YD" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 51 3 52 3
Animation name "Z" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 53 0 54 0
Animation name "ZL" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 53 1 54 1
Animation name "ZU" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 53 2 54 2
Animation name "ZD" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb 53 3 54 3
Animation name "c1L" image "notrotated" widthb 1 heightb 1 FrameCount 6 Framedelay 1 Framesb 107 0 108 0 46 0 48 0 47 0 34 0
Animation name "c1U" image "notrotated" widthb 1 heightb 1 FrameCount 6 Framedelay 1 Framesb 109 0 110 0 46 0 48 0 47 0 34 0
Animation name "c1D" image "notrotated" widthb 1 heightb 1 FrameCount 6 Framedelay 1 Framesb 111 0 112 0 46 0 48 0 47 0 34 0
