ASHScript 1.2 revision 4
sub type ImageScript name "Images"
BlockSize width 32 height 32
NewImage name "notrotated" widthb 113 heightb 1
NewImage name "rotated" widthb 55 heightb 4
LoadImage name "sprites0.tga" filename "sprites0.tga"
RearrangeTilesV2H name "sprites0.tga" width 32 height 32
LoadImage name "sprites1.tga" filename "sprites1.tga"
RearrangeTilesV2H name "sprites1.tga" width 32 height 32
CopyPartialImage source "sprites0.tga" leftb 0 top 0 target "notrotated" leftb 0 topb 0 widthb 23 heightb 1
CopyPartialImage source "sprites0.tga" leftb 23 top 0 target "rotated" leftb 0 topb 0 widthb 2 heightb 1
CopyPartialImage source "sprites0.tga" leftb 25 top 0 target "notrotated" leftb 23 topb 0 widthb 11 heightb 1
CopyPartialImage source "sprites0.tga" leftb 36 top 0 target "rotated" leftb 2 topb 0 widthb 2 heightb 1
CopyPartialImage source "sprites0.tga" leftb 38 top 0 target "notrotated" leftb 34 topb 0 widthb 9 heightb 1
CopyPartialImage source "sprites0.tga" leftb 47 top 0 target "rotated" leftb 4 topb 0 widthb 9 heightb 1
CopyPartialImage source "sprites0.tga" leftb 56 top 0 target "notrotated" leftb 43 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites0.tga" leftb 57 top 0 target "rotated" leftb 13 topb 0 widthb 3 heightb 1
CopyPartialImage source "sprites0.tga" leftb 60 top 0 target "notrotated" leftb 44 topb 0 widthb 18 heightb 1
CopyPartialImage source "sprites0.tga" leftb 78 top 0 target "rotated" leftb 16 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites0.tga" leftb 79 top 0 target "notrotated" leftb 62 topb 0 widthb 12 heightb 1
CopyPartialImage source "sprites0.tga" leftb 91 top 0 target "rotated" leftb 17 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites0.tga" leftb 92 top 0 target "notrotated" leftb 74 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites0.tga" leftb 93 top 0 target "rotated" leftb 18 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites0.tga" leftb 94 top 0 target "notrotated" leftb 75 topb 0 widthb 3 heightb 1
CopyPartialImage source "sprites0.tga" leftb 97 top 0 target "rotated" leftb 19 topb 0 widthb 3 heightb 1
CopyPartialImage source "sprites0.tga" leftb 100 top 0 target "notrotated" leftb 78 topb 0 widthb 2 heightb 1
CopyPartialImage source "sprites0.tga" leftb 102 top 0 target "rotated" leftb 22 topb 0 widthb 2 heightb 1
CopyPartialImage source "sprites0.tga" leftb 104 top 0 target "notrotated" leftb 80 topb 0 widthb 8 heightb 1
CopyPartialImage source "sprites0.tga" leftb 112 top 0 target "rotated" leftb 24 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites0.tga" leftb 113 top 0 target "notrotated" leftb 88 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites0.tga" leftb 114 top 0 target "rotated" leftb 25 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites1.tga" leftb 0 top 0 target "rotated" leftb 26 topb 0 widthb 2 heightb 1
CopyPartialImage source "sprites1.tga" leftb 2 top 0 target "notrotated" leftb 89 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites1.tga" leftb 3 top 0 target "rotated" leftb 28 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites1.tga" leftb 4 top 0 target "notrotated" leftb 90 topb 0 widthb 1 heightb 1
CopyPartialImage source "sprites1.tga" leftb 5 top 0 target "rotated" leftb 29 topb 0 widthb 2 heightb 1
CopyPartialImage source "sprites1.tga" leftb 7 top 0 target "notrotated" leftb 91 topb 0 widthb 8 heightb 1
CopyPartialImage source "sprites0.tga" leftb 47 top 0target "rotated" leftb 31 top 0 widthb 1 heightb 1
CopyPartialImage source "sprites0.tga" leftb 48 top 0target "rotated" leftb 32 top 0 widthb 1 heightb 1
CopyPartialImage source "sprites0.tga" leftb 49 top 0target "rotated" leftb 33 top 0 widthb 1 heightb 1
CopyPartialImage source "sprites0.tga" leftb 53 top 0target "rotated" leftb 34 top 0 widthb 1 heightb 1
CopyPartialImage source "sprites0.tga" leftb 50 top 0target "rotated" leftb 35 top 0 widthb 1 heightb 1
CopyPartialImage source "sprites0.tga" leftb 51 top 0target "rotated" leftb 36 top 0 widthb 1 heightb 1
CopyPartialImage source "sprites0.tga" leftb 52 top 0target "rotated" leftb 37 top 0 widthb 1 heightb 1
CopyPartialImage source "sprites0.tga" leftb 54 top 0target "rotated" leftb 38 top 0 widthb 1 heightb 1
CopyPartialImage source "rotated" leftb 31 top 16 target "rotated" leftb 32 top 16 widthb 1 height 16
CopyPartialImage source "rotated" leftb 31 top 16 target "rotated" leftb 33 top 16 widthb 2 height 16
CopyPartialImage source "rotated" leftb 31 top 16 target "rotated" leftb 35 top 16 widthb 4 height 16
LoadImage name "conga.tga" filename "conga.tga"
NewImage name "present" width 32 height 32
NewImage name "wrapper" width 32 height 32
CopyPartialImage source "conga.tga" left 0 top 0 target "present" left 0 top 0 widthb 1 heightb 1
CopyPartialImage source "conga.tga" leftb 1 top 0 target "wrapper" left 0 top 0 widthb 1 heightb 1
RecolorRGB name "present" r 255 g 0 b 0
RecolorRGB name "wrapper" r 0 g 255 b 0
CopyImage source "wrapper" target "present" colorkeyrgb 0 0 0
CopyPartialImage source "present" left 0 top 0 target "notrotated" leftb 99 top 0 widthb 1 heightb 1
CopyPartialImage source "conga.tga" left 0 top 0 target "present" left 0 top 0 widthb 1 heightb 1
CopyPartialImage source "conga.tga" leftb 1 top 0 target "wrapper" left 0 top 0 widthb 1 heightb 1
RecolorRGB name "present" r 255 g 0 b 0
RecolorRGB name "wrapper" r 0 g 64 b 255
CopyImage source "wrapper" target "present" colorkeyrgb 0 0 0
CopyPartialImage source "present" left 0 top 0 target "notrotated" leftb 100 top 0 widthb 1 heightb 1
CopyPartialImage source "conga.tga" left 0 top 0 target "present" left 0 top 0 widthb 1 heightb 1
CopyPartialImage source "conga.tga" leftb 1 top 0 target "wrapper" left 0 top 0 widthb 1 heightb 1
RecolorRGB name "present" r 255 g 255 b 0
RecolorRGB name "wrapper" r 255 g 0 b 0
CopyImage source "wrapper" target "present" colorkeyrgb 0 0 0
CopyPartialImage source "present" left 0 top 0 target "notrotated" leftb 101 top 0 widthb 1 heightb 1
CopyPartialImage source "conga.tga" left 0 top 0 target "present" left 0 top 0 widthb 1 heightb 1
CopyPartialImage source "conga.tga" leftb 1 top 0 target "wrapper" left 0 top 0 widthb 1 heightb 1
RecolorRGB name "present" r 255 g 255 b 0
RecolorRGB name "wrapper" r 0 g 64 b 255
CopyImage source "wrapper" target "present" colorkeyrgb 0 0 0
CopyPartialImage source "present" left 0 top 0 target "notrotated" leftb 102 top 0 widthb 1 heightb 1
CopyPartialImage source "conga.tga" left 0 top 0 target "present" left 0 top 0 widthb 1 heightb 1
CopyPartialImage source "conga.tga" leftb 1 top 0 target "wrapper" left 0 top 0 widthb 1 heightb 1
RecolorRGB name "present" r 0 g 64 b 255
RecolorRGB name "wrapper" r 255 g 0 b 255
CopyImage source "wrapper" target "present" colorkeyrgb 0 0 0
CopyPartialImage source "present" left 0 top 0 target "notrotated" leftb 103 top 0 widthb 1 heightb 1
CopyPartialImage source "conga.tga" left 0 top 0 target "present" left 0 top 0 widthb 1 heightb 1
CopyPartialImage source "conga.tga" leftb 1 top 0 target "wrapper" left 0 top 0 widthb 1 heightb 1
RecolorRGB name "present" r 0 g 255 b 0
RecolorRGB name "wrapper" r 255 g 0 b 0
CopyImage source "wrapper" target "present" colorkeyrgb 0 0 0
CopyPartialImage source "present" left 0 top 0 target "notrotated" leftb 104 top 0 widthb 1 heightb 1
CopyPartialImage source "conga.tga" left 0 top 0 target "present" left 0 top 0 widthb 1 heightb 1
CopyPartialImage source "conga.tga" leftb 1 top 0 target "wrapper" left 0 top 0 widthb 1 heightb 1
RecolorRGB name "present" r 0 g 255 b 0
RecolorRGB name "wrapper" r 255 g 0 b 255
CopyImage source "wrapper" target "present" colorkeyrgb 0 0 0
CopyPartialImage source "present" left 0 top 0 target "notrotated" leftb 105 top 0 widthb 1 heightb 1
CopyPartialImage source "conga.tga" left 0 top 0 target "present" left 0 top 0 widthb 1 heightb 1
CopyPartialImage source "conga.tga" leftb 1 top 0 target "wrapper" left 0 top 0 widthb 1 heightb 1
RecolorRGB name "present" r 255 g 0 b 255
RecolorRGB name "wrapper" r 255 g 255 b 0
CopyImage source "wrapper" target "present" colorkeyrgb 0 0 0
CopyPartialImage source "present" left 0 top 0 target "notrotated" leftb 106 top 0 widthb 1 heightb 1
FreeImage name "wrapper"
FreeImage name "present"
LoadImage name "congafnt.tga" filename "congafnt.tga"
NewImage name "fonttmp" width 144 height 21
NewImage name "wagon" widthb 2 heightb 1
CopyPartialImage source "conga.tga" leftb 2 top 0 target "wagon" left 0 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga.tga" leftb 2 top 0 target "wagon" leftb 1 top 0 widthb 1 heightb 1 transform "H"
CopyImage source "congafnt.tga" target "fonttmp"
RecolorRGB name "fonttmp" r 255 g 0 b 0
CopyPartialImage source "fonttmp" left 0 top 0 target "wagon" left 2 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 12 top 0 target "wagon" left 50 top 2 width 12 height 21
CopyImage source "congafnt.tga" target "fonttmp"
RecolorRGB name "fonttmp" r 0 g 255 b 0
CopyPartialImage source "fonttmp" left 12 top 0 target "wagon" left 18 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 0 top 0 target "wagon" left 34 top 2 width 12 height 21
CopyPartialImageRepeat count 2 deltaleftb 1 deltatop 0 source "wagon" left 0 top 0 target "rotated" leftb 39 topb 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga.tga" leftb 2 top 0 target "wagon" left 0 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga.tga" leftb 2 top 0 target "wagon" leftb 1 top 0 widthb 1 heightb 1 transform "H"
CopyImage source "congafnt.tga" target "fonttmp"
RecolorRGB name "fonttmp" r 0 g 64 b 255
CopyPartialImage source "fonttmp" left 24 top 0 target "wagon" left 2 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 36 top 0 target "wagon" left 50 top 2 width 12 height 21
CopyImage source "congafnt.tga" target "fonttmp"
RecolorRGB name "fonttmp" r 255 g 255 b 0
CopyPartialImage source "fonttmp" left 36 top 0 target "wagon" left 18 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 24 top 0 target "wagon" left 34 top 2 width 12 height 21
CopyPartialImageRepeat count 2 deltaleftb 1 deltatop 0 source "wagon" left 0 top 0 target "rotated" leftb 41 topb 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga.tga" leftb 2 top 0 target "wagon" left 0 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga.tga" leftb 2 top 0 target "wagon" leftb 1 top 0 widthb 1 heightb 1 transform "H"
CopyImage source "congafnt.tga" target "fonttmp"
RecolorRGB name "fonttmp" r 255 g 0 b 255
CopyPartialImage source "fonttmp" left 48 top 0 target "wagon" left 2 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 60 top 0 target "wagon" left 50 top 2 width 12 height 21
CopyImage source "congafnt.tga" target "fonttmp"
RecolorRGB name "fonttmp" r 0 g 255 b 255
CopyPartialImage source "fonttmp" left 60 top 0 target "wagon" left 18 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 48 top 0 target "wagon" left 34 top 2 width 12 height 21
CopyPartialImageRepeat count 2 deltaleftb 1 deltatop 0 source "wagon" left 0 top 0 target "rotated" leftb 43 topb 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga.tga" leftb 2 top 0 target "wagon" left 0 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga.tga" leftb 2 top 0 target "wagon" leftb 1 top 0 widthb 1 heightb 1 transform "H"
CopyImage source "congafnt.tga" target "fonttmp"
RecolorRGB name "fonttmp" r 255 g 0 b 0
CopyPartialImage source "fonttmp" left 72 top 0 target "wagon" left 2 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 84 top 0 target "wagon" left 50 top 2 width 12 height 21
CopyImage source "congafnt.tga" target "fonttmp"
RecolorRGB name "fonttmp" r 0 g 255 b 0
CopyPartialImage source "fonttmp" left 84 top 0 target "wagon" left 18 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 72 top 0 target "wagon" left 34 top 2 width 12 height 21
CopyPartialImageRepeat count 2 deltaleftb 1 deltatop 0 source "wagon" left 0 top 0 target "rotated" leftb 45 topb 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga.tga" leftb 2 top 0 target "wagon" left 0 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga.tga" leftb 2 top 0 target "wagon" leftb 1 top 0 widthb 1 heightb 1 transform "H"
CopyImage source "congafnt.tga" target "fonttmp"
RecolorRGB name "fonttmp" r 0 g 64 b 255
CopyPartialImage source "fonttmp" left 96 top 0 target "wagon" left 2 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 60 top 0 target "wagon" left 50 top 2 width 12 height 21
CopyImage source "congafnt.tga" target "fonttmp"
RecolorRGB name "fonttmp" r 255 g 255 b 0
CopyPartialImage source "fonttmp" left 60 top 0 target "wagon" left 18 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 96 top 0 target "wagon" left 34 top 2 width 12 height 21
CopyPartialImageRepeat count 2 deltaleftb 1 deltatop 0 source "wagon" left 0 top 0 target "rotated" leftb 47 topb 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga.tga" leftb 2 top 0 target "wagon" left 0 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga.tga" leftb 2 top 0 target "wagon" leftb 1 top 0 widthb 1 heightb 1 transform "H"
CopyImage source "congafnt.tga" target "fonttmp"
RecolorRGB name "fonttmp" r 255 g 0 b 255
CopyPartialImage source "fonttmp" left 72 top 0 target "wagon" left 2 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 108 top 0 target "wagon" left 50 top 2 width 12 height 21
CopyImage source "congafnt.tga" target "fonttmp"
RecolorRGB name "fonttmp" r 0 g 255 b 255
CopyPartialImage source "fonttmp" left 108 top 0 target "wagon" left 18 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 72 top 0 target "wagon" left 34 top 2 width 12 height 21
CopyPartialImageRepeat count 2 deltaleftb 1 deltatop 0 source "wagon" left 0 top 0 target "rotated" leftb 49 topb 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga.tga" leftb 2 top 0 target "wagon" left 0 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga.tga" leftb 2 top 0 target "wagon" leftb 1 top 0 widthb 1 heightb 1 transform "H"
CopyImage source "congafnt.tga" target "fonttmp"
RecolorRGB name "fonttmp" r 255 g 0 b 0
CopyPartialImage source "fonttmp" left 12 top 0 target "wagon" left 2 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 24 top 0 target "wagon" left 50 top 2 width 12 height 21
CopyImage source "congafnt.tga" target "fonttmp"
RecolorRGB name "fonttmp" r 0 g 255 b 0
CopyPartialImage source "fonttmp" left 24 top 0 target "wagon" left 18 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 12 top 0 target "wagon" left 34 top 2 width 12 height 21
CopyPartialImageRepeat count 2 deltaleftb 1 deltatop 0 source "wagon" left 0 top 0 target "rotated" leftb 51 topb 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga.tga" leftb 2 top 0 target "wagon" left 0 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "conga.tga" leftb 2 top 0 target "wagon" leftb 1 top 0 widthb 1 heightb 1 transform "H"
CopyImage source "congafnt.tga" target "fonttmp"
RecolorRGB name "fonttmp" r 0 g 64 b 255
CopyPartialImage source "fonttmp" left 120 top 0 target "wagon" left 2 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 132 top 0 target "wagon" left 50 top 2 width 12 height 21
CopyImage source "congafnt.tga" target "fonttmp"
RecolorRGB name "fonttmp" r 255 g 255 b 0
CopyPartialImage source "fonttmp" left 132 top 0 target "wagon" left 18 top 2 width 12 height 21
CopyPartialImage source "fonttmp" left 120 top 0 target "wagon" left 34 top 2 width 12 height 21
CopyPartialImageRepeat count 2 deltaleftb 1 deltatop 0 source "wagon" left 0 top 0 target "rotated" leftb 53 topb 0 widthb 1 heightb 1 transform "H"
FreeImage name "wagon"
FreeImage name "fonttmp"
FreeImage name "congafnt.tga"
FreeImage name "conga.tga"
CopyPartialImage source "notrotated" leftb 43 top 0 target "notrotated" leftb 107 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "notrotated" leftb 45 top 0 target "notrotated" leftb 108 top 0 widthb 1 heightb 1 transform "H"
CopyPartialImage source "notrotated" leftb 43 top 0 target "notrotated" leftb 109 top 0 widthb 1 heightb 1 transform "HR"
CopyPartialImage source "notrotated" leftb 45 top 0 target "notrotated" leftb 110 top 0 widthb 1 heightb 1 transform "HR"
CopyPartialImage source "notrotated" leftb 43 top 0 target "notrotated" leftb 111 top 0 widthb 1 heightb 1 transform "Hr"
CopyPartialImage source "notrotated" leftb 45 top 0 target "notrotated" leftb 112 top 0 widthb 1 heightb 1 transform "Hr"
FreeImage name "sprites0.tga"
FreeImage name "sprites1.tga"
CopyPartialImageRepeat count 55 deltaleftb 1 deltatop 0 source "rotated" leftb 0 top 0 target "rotated" leftb 0 topb 1 widthb 1 heightb 1 transform "H"
CopyPartialImageRepeat count 55 deltaleftb 1 deltatop 0 source "rotated" leftb 0 top 0 target "rotated" leftb 0 topb 2 widthb 1 heightb 1 transform "HR"
CopyPartialImageRepeat count 55 deltaleftb 1 deltatop 0 source "rotated" leftb 0 top 0 target "rotated" leftb 0 topb 3 widthb 1 heightb 1 transform "Hr"
