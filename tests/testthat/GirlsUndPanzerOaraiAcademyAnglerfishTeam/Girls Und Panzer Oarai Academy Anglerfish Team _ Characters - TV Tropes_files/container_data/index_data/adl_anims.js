adl.animations={_all:{params:{dur:{func:"anim",label:"Duration (seconds)",input:"number",value:1,step:.1},ease:{func:"anim",label:"Easing",input:"select",options:{ease:"Ease",linear:"Linear","ease-in-out":"Ease In & Out","ease-in":"Ease In","ease-out":"Ease Out"}},delay:{func:"anim",label:"Delay (seconds)",input:"number",value:0,step:.1}}},bounce:{style:{transformOrigin:"center bottom"},keyframes:{"from, 20%, 53%, 80%, to":{animationTimingFunction:"cubic-bezier(0.215, 0.610, 0.355, 1.000)",transform:"translate3d(0,0,0)"},"40%, 43%":{animationTimingFunction:"cubic-bezier(0.755, 0.050, 0.855, 0.060)",transform:"translate3d(0, -30px, 0)"},"70%":{animationTimingFunction:"cubic-bezier(0.755, 0.050, 0.855, 0.060)",transform:"translate3d(0, -15px, 0)"},"90%":{transform:"translate3d(0,-4px,0)"}}},flash:{keyframes:{"from, 50%, to":{opacity:1},"25%, 75%":{opacity:0}}},pulse:{keyframes:{from:{transform:"scale3d(1, 1, 1)"},"50%":{transform:"scale3d(1.05, 1.05, 1.05)"},to:{transform:"scale3d(1, 1, 1)"}}},shake:{keyframes:{"from, to":{transform:"translate3d(0, 0, 0)"},"10%, 30%, 50%, 70%, 90%":{transform:"translate3d(-10px, 0, 0)"},"20%, 40%, 60%, 80%":{transform:"translate3d(10px, 0, 0)"}}},headshake:{style:{animationTimingFunction:"ease-in-out"},keyframes:{"0%":{transform:"translateX(0)"},"6.5%":{transform:"translateX(-6px) rotateY(-9deg)"},"18.5%":{transform:"translateX(5px) rotateY(7deg)"},"31.5%":{transform:"translateX(-3px) rotateY(-5deg)"},"43.5%":{transform:"translateX(2px) rotateY(3deg)"},"50%":{transform:"translateX(0)"}}},tada:{keyframes:{from:{transform:"scale3d(1, 1, 1)"},"10%, 20%":{transform:"scale3d(.9, .9, .9) rotate3d(0, 0, 1, -3deg)"},"30%, 50%, 70%, 90%":{transform:"scale3d(1.1, 1.1, 1.1) rotate3d(0, 0, 1, 3deg)"},"40%, 60%, 80%":{transform:"scale3d(1.1, 1.1, 1.1) rotate3d(0, 0, 1, -3deg)"},to:{transform:"scale3d(1, 1, 1)"}}},wobble:{keyframes:{from:{transform:"none"},"15%":{transform:"translate3d(-25%, 0, 0) rotate3d(0, 0, 1, -5deg)"},"30%":{transform:"translate3d(20%, 0, 0) rotate3d(0, 0, 1, 3deg)"},"45%":{transform:"translate3d(-15%, 0, 0) rotate3d(0, 0, 1, -3deg)"},"60%":{transform:"translate3d(10%, 0, 0) rotate3d(0, 0, 1, 2deg)"},"75%":{transform:"translate3d(-5%, 0, 0) rotate3d(0, 0, 1, -1deg)"},to:{transform:"none"}}},jello:{style:{transformOrigin:"center"},keyframes:{"from, 11.1%, to":{transform:"none"},"22.2%":{transform:"skew(-12.5deg,-12.5deg)"},"33.3%":{transform:"skewX(6.25deg,6.25deg)"},"44.4%":{transform:"skewX(-3.125deg,-3.125deg)"},"55.5%":{transform:"skewX(1.5625deg,1.5625deg)"},"66.6%":{transform:"skewX(-0.78125deg,-0.78125deg)"},"77.7%":{transform:"skewX(0.390625deg,0.390625deg)"},"88.8%":{transform:"skewX(-0.1953125deg,-0.1953125deg)"}}},slideIn:{params:{varx:{func:"anim",label:"From",input:"select",options:{left:"Left",right:"Right",top:"Top",bottom:"Bottom"}},vary:{func:"anim",label:"Distance",input:"number",value:"1000px",units:{"%":"%",px:"px"}}},vars:{left:{_A:"-_Y",_B:0},right:{_A:"_Y",_B:0},top:{_A:0,_B:"-_Y"},bottom:{_A:0,_B:"_Y"}},keyframes:{from:{visibilty:"visible",transform:"translate(_A, _B)"},to:{visibilty:"visible",transform:"translate(0, 0)"}}},slideOut:{params:{varx:{func:"anim",label:"To",input:"select",options:{left:"Left",right:"Right",top:"Top",bottom:"Bottom"}},vary:{func:"anim",label:"Distance",input:"number",value:"1000px",units:{"%":"%",px:"px"}}},vars:{left:{_A:"-_Y",_B:0},right:{_A:"_Y",_B:0},top:{_A:0,_B:"-_Y"},bottom:{_A:0,_B:"_Y"}},keyframes:{from:{visibilty:"visible",transform:"translate(0,0)"},to:{visibilty:"hidden",transform:"translate(_A, _B)"}}},fadeIn:{params:{varz:{func:"anim",label:"Start opacity",input:"range",min:0,max:1,step:.01,value:"0"},varx:{func:"anim",label:"Direction",input:"select",options:{normal:"None",down:"Down",up:"Up",left:"Left",right:"Right"}},vary:{func:"anim",label:"Distance",input:"number",value:"100%",units:{"%":"%",px:"px"}}},vars:{normal:{_A:"0, 0"},down:{_A:"0, _Y"},up:{_A:"0, -_Y"},left:{_A:"-_Y, 0"},right:{_A:"_Y,0"}},keyframes:{from:{opacity:"_Z",transform:"translate(_A)"},to:{transform:"translate(0,0)"}}},fadeOut:{params:{varz:{func:"anim",label:"End opacity",input:"range",min:0,max:1,step:.01,value:"0"},varx:{func:"anim",label:"Direction",input:"select",options:{normal:"None",down:"Down",up:"Up",left:"Left",right:"Right"}},vary:{func:"anim",label:"Distance",input:"number",value:"100%",units:{"%":"%",px:"px"}}},vars:{normal:{_A:"0, 0"},down:{_A:"0, _Y"},up:{_A:"0, -_Y,"},left:{_A:"-_Y, 0"},right:{_A:"_Y, 0"}},keyframes:{from:{transform:"translate(0,0)"},to:{opacity:"_Z",transform:"translate(_A)"}}},bounceIn:{keyframes:{"from, 20%, 40%, 60%, 80%, to":{animationTimingFunction:"cubic-bezier(0.215, 0.610, 0.355, 1.000)"},"0%":{opacity:0,transform:"scale(.3, .3)"},"20%":{transform:"scale(1.1, 1.1)"},"40%":{transform:"scale(.9, .9)"},"60%":{opacity:1,transform:"scale(1.03, 1.03)"},"80%":{transform:"scale(.97, .97)"},to:{opacity:1,transform:"scale(1, 1)"}}},bounceInFrom:{params:{varx:{func:"anim",label:"From",input:"select",options:{left:"Left",right:"Right",top:"Top",bottom:"Bottom"}}},vars:{left:{_A:"-3000px, 0, 0",_B:"25px,0, 0",_C:"-10px, 0, 0",_D:"5px, 0, 0"},right:{_A:"3000px, 0, 0",_B:"-25px,0, 0",_C:"10px, 0, 0",_D:"-5px, 0, 0"},top:{_A:"0, -3000px, 0",_B:"0, 25px, 0",_C:"0, -10px, 0",_D:"0, 5px, 0"},bottom:{_A:"0, 3000px, 0",_B:"0, -25px, 0",_C:"0, 10px, 0",_D:"0, -5px, 0"}},keyframes:{"from, 60%, 75%, 90%, to":{animationTimingFunction:"cubic-bezier(0.215, 0.610, 0.355, 1.000)"},"0%":{opacity:0,transform:"translate3d(_A)"},"60%":{opacity:1,transform:"translate3d(_B)"},"75%":{transform:"translate3d(_C)"},"90%":{transform:"translate3d(_D)"},to:{transform:"none"}}},bounceOut:{keyframes:{"20%":{transform:"scale(.9, .9)"},"50%, 55%":{opacity:1,transform:"scale(1.1, 1.1)"},to:{opacity:0,transform:"scale(.3, .3)"}}},bounceOutTo:{params:{varx:{func:"anim",label:"To",input:"select",options:{left:"Left",right:"Right",top:"Top",bottom:"Bottom"}}},vars:{left:{_A:"5px, 0, 0",_B:"-10px,0, 0",_C:"25px,0, 0",_D:"-2000px, 0, 0"},right:{_A:"-5px, 0, 0",_B:"10px,0, 0",_C:"-25px,0, 0",_D:"2000px, 0, 0"},top:{_A:"0, 5px, 0",_B:"0, -10px, 0",_C:"0, 25px, 0",_D:"0, -2000px, 0"},bottom:{_A:"0, -5px, 0",_B:"0, 10px, 0",_C:"0,-25px, 0",_D:"0, 2000px, 0"}},keyframes:{"from, 20%, 40%, 60%, to":{animationTimingFunction:"cubic-bezier(0.215, 0.610, 0.355, 1.000)"},"20%":{transform:"translate3d(_A)"},"40%":{transform:"translate3d(_B)"},"60%":{opacity:1,transform:"translate3d(_C)"},to:{opacity:0,transform:"translate3d(_D)"}}},scale:{params:{varx:{func:"anim",label:"Direction",input:"select",options:{from:"From",to:"To"}},vary:{func:"anim",label:"Size",input:"number",value:"0.5",step:"0.5"}},vars:{to:{_A:"1, 1",_B:"_Y, _Y"},from:{_B:"1, 1",_A:"_Y, _Y"}},keyframes:{from:{transform:"scale(_A)"},to:{transform:"scale(_B)"}}},zoomIn:{keyframes:{from:{opacity:0,transform:"scale(.3, .3)"},"50%":{}}},zoomOut:{params:{},keyframes:{from:{},"50%":{opacity:0,transform:"scale(.3, .3)"},to:{opacity:0}}},swing:{style:{transformOrigin:"top center",backfaceVisibility:"hidden",transform:"translateZ(0) scale(1.0, 1.0)"},keyframes:{"20%":{transform:"rotate(15deg)"},"40%":{transform:"rotate(-10deg)"},"60%":{transform:"rotate(5deg)"},"80%":{transform:"rotate(-5deg)"},to:{transform:"rotate(0deg)"}}},rubberband:{style:{backfaceVisibility:"hidden",transform:"translateZ(0) scale(1.0, 1.0)"},keyframes:{from:{transform:"scale(1, 1)"},"30%":{transform:"scale(0.85, 0.85)"},"40%":{transform:"scale(1.15, 1.15)"},"50%":{transform:"scale(0.85, 0.85)"},"65%":{transform:"scale(1.05, 1.05)"},"75%":{transform:"scale(0.95, .95)"},to:{transform:"scale(1, 1)"}}},lightspeedIn:{params:{varx:{func:"anim",label:"From",input:"select",options:{left:"Left",right:"Right"}}},ease:"ease-out",vars:{right:{_A:"1000px",_B:"-20deg",_C:"-15deg",_D:"5deg"},left:{_A:"-1000px",_B:"20deg",_C:"15deg",_D:"-5deg"}},keyframes:{from:{transform:"translate(_A, 0) skewX(_B)",filter:"blur(3px)",opacity:0},"30%":{transform:"skewX(_C)",opacity:1},"50%":{transform:"skewX(_D)",opacity:1},to:{transform:"translate(0,0) skewX(0)",filter:"blur(0px)",opacity:1}}},flipInY:{keyframes:{from:{transform:"perspective(1800px) rotate3d(1, 0, 0, 90deg)","transform-origin":"50% 0%","transform-style":"preserve-3d",opacity:0,"animation-timing-function":"ease-in"},to:{transform:"perspective(1800px)","transform-style":"preserve-3d"}},style:{"backface-visibility":"visible !important"}},flipOutY:{keyframes:{from:{transform:"perspective(1000000px)","transform-style":"preserve-3d"},to:{transform:"perspective(1000000px) rotate3d(1, 0, 0, 90deg) translate(0,5px)","transform-origin":"50% 100%","transform-style":"preserve-3d",opacity:0}},style:{"backface-visibility":"visible !important"}}};