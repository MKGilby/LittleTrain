camera {
   location  <0,0,-11>         /* 100=1m */
   direction <0, 0,  1.5>
   up        <0, 1,  0>
   right     <4/3, 0,  0>
   look_at   <0, 0, 0>
}

#include "colors.inc"
#include "shapes.inc"

#declare py1=
   pigment {
      gradient y
      color_map {
                  [0.0000, color red .1 green .6 blue .9 ]
                  [1.0000, color red .3 green .4 blue .7 ]
                }
      scale <5,1,1>
      //rotate z*30
   }

#declare py2=
   pigment {
      crackle form <1,2,0> solid
      color_map {
                  [0.00, Black ]
                  [1.00, White ]
                }
      scale <.1,.1,.1>
      //rotate z*30
   }

#declare py3=
   pigment {
      gradient y
      color_map {
                  [0.00, Gray10 ]
                  [1.00, Gray90 ]
                }
      scale <1,.8,1>
      translate -.03*y
      //rotate z*30
   }

box { <-3,-3,-0.1> <3,3,0>
   pigment {
      average
      pigment_map {
         [1 py1]
         [1 py2]
         [1 py3]
      }
      translate y*0.4
      scale <5,5,1>
   }
   finish { ambient .5 }
   scale <2,2,1>
}

#declare colg=color red 1 green .5 blue .1;

#declare p1= 
   pigment {
      gradient y
      color_map {
                  [0.0000, color red 1 green 1 blue 1 ]
                  [1.0000, color red .1 green .2 blue .3 ]
                }
      scale <1,.7,1>
      translate y*-.03
      //rotate z*30
   }  
 
#declare p2= 
   pigment {
      crackle form <1,2,0> solid
      color_map {
                  [0.00, Gray10 ]
                  [1.00, White ]
                } 
      scale <.1,.1,.1>
      //rotate z*30
   }  
 
#declare p3= 
   pigment {
      gradient y
      color_map {
                  [0.00, Gray50 ]
                  [1.00, White ]
                } 
      scale <1,.7,1>
      translate -.03*y
      //rotate z*30
   }  

text { ttf "coopbl.ttf", "?", 1, 0
   translate <Text_Width("coopbl.ttf", "?", 1, 0)*-0.5,-.3,-1.1>
   pigment {
      average
      pigment_map {
//         [2 p1] 
         [1 p2]
         [2 p3]
      }
      scale <1,1.2,1>
      translate -.3*y
   } 
   scale <4.2, 4.2, .1>
//   scale <2,1,1>
   finish { ambient .9 }
} 

light_source { <0,30,-30> color White*1 }
