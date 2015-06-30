;(use comparse)
;(include "rascl-parser.scm")
;(import rascl-parser)

(define dc00 "background : 0xa7b7b0, foreground : 0x000000 , font_family : Bitstream Vera Sans")
(define dc01 (conc "\n" dc00 "\n"))
(define dc02 (conc "\n" dc00))
(define dc03 (conc dc00 "\n"))
(define dc04 (conc "\n\n\n" dc00 "\n\n"))

(define dc10 "background : 0xa7b7b0
foreground : 0x000000
font_family : Bitstream Vera Sans")
(define dc11 (conc "\n" dc10 "\n"))
(define dc12 (conc "\n" dc10))
(define dc13 (conc dc10 "\n"))
(define dc14 (conc "\n\n\n" dc10 "\n\n"))

(define dict000 (conc "{" dc00 "}"))
(define dict001 (conc "{ " dc00 "   }"))
(define dict002 (conc "{  " dc00 "}"))
(define dict003 (conc "{" dc00 " }"))
(define dict004 (conc "{\n" dc00 "}\n"))
(define dict005 (conc "\n{\n" dc00 "}\n"))
(define dict006 (conc "{\n" dc00 "}"))
(define dict007 (conc "\n\n {\n" dc00 " \n }\n\n\n"))

(define dict100 (conc "{" dc10 "}"))
(define dict101 (conc "{ " dc10 "   }"))
(define dict102 (conc "{  " dc10 "}"))
(define dict103 (conc "{" dc10 " }"))
(define dict104 (conc "{\n" dc10 "}\n"))
(define dict105 (conc "\n{\n" dc10 "}\n"))
(define dict106 (conc "{\n" dc10 "}"))
(define dict107 (conc "\n\n {\n" dc10 " \n }\n\n\n"))

(define dict010 (conc "{" dc01 "}"))
(define dict011 (conc "{ " dc01 "   }"))
(define dict012 (conc "{  " dc01 "}"))
(define dict013 (conc "{" dc01 " }"))
(define dict014 (conc "{\n" dc01 "}\n"))
(define dict015 (conc "\n{\n" dc01 "}\n"))
(define dict016 (conc "{\n" dc01 "}"))
(define dict017 (conc "\n\n {\n" dc01 " \n }\n\n\n"))

(define dict110 (conc "{" dc11 "}"))
(define dict111 (conc "{ " dc11 "   }"))
(define dict112 (conc "{  " dc11 "}"))
(define dict113 (conc "{" dc11 " }"))
(define dict114 (conc "{\n" dc11 "}\n"))
(define dict115 (conc "\n{\n" dc11 "}\n"))
(define dict116 (conc "{\n" dc11 "}"))
(define dict117 (conc "\n\n {\n" dc11 " \n }\n\n\n"))

(define dict020 (conc "{" dc02 "}"))
(define dict021 (conc "{ " dc02 "   }"))
(define dict022 (conc "{  " dc02 "}"))
(define dict023 (conc "{" dc02 " }"))
(define dict024 (conc "{\n" dc02 "}\n"))
(define dict025 (conc "\n{\n" dc02 "}\n"))
(define dict026 (conc "{\n" dc02 "}"))
(define dict027 (conc "\n\n {\n" dc02 " \n }\n\n\n"))

(define dict120 (conc "{" dc12 "}"))
(define dict121 (conc "{ " dc12 "   }"))
(define dict122 (conc "{  " dc12 "}"))
(define dict123 (conc "{" dc12 " }"))
(define dict124 (conc "{\n" dc12 "}\n"))
(define dict125 (conc "\n{\n" dc12 "}\n"))
(define dict126 (conc "{\n" dc12 "}"))
(define dict127 (conc "\n\n {\n" dc12 " \n }\n\n\n"))

(define dict030 (conc "{" dc03 "}"))
(define dict031 (conc "{ " dc03 "   }"))
(define dict032 (conc "{  " dc03 "}"))
(define dict033 (conc "{" dc03 " }"))
(define dict034 (conc "{\n" dc03 "}\n"))
(define dict035 (conc "\n{\n" dc03 "}\n"))
(define dict036 (conc "{\n" dc03 "}"))
(define dict037 (conc "\n\n {\n" dc03 " \n }\n\n\n"))

(define dict130 (conc "{" dc13 "}"))
(define dict131 (conc "{ " dc13 "   }"))
(define dict132 (conc "{  " dc13 "}"))
(define dict133 (conc "{" dc13 " }"))
(define dict134 (conc "{\n" dc13 "}\n"))
(define dict135 (conc "\n{\n" dc13 "}\n"))
(define dict136 (conc "{\n" dc13 "}"))
(define dict137 (conc "\n\n {\n" dc13 " \n }\n\n\n"))

(define dict040 (conc "{" dc04 "}"))
(define dict041 (conc "{ " dc04 "   }"))
(define dict042 (conc "{  " dc04 "}"))
(define dict043 (conc "{" dc04 " }"))
(define dict044 (conc "{\n" dc04 "}\n"))
(define dict045 (conc "\n{\n" dc04 "}\n"))
(define dict046 (conc "{\n" dc04 "}"))
(define dict047 (conc "\n\n {\n" dc04 " \n }\n\n\n"))

(define dict140 (conc "{" dc14 "}"))
(define dict141 (conc "{ " dc14 "   }"))
(define dict142 (conc "{  " dc14 "}"))
(define dict143 (conc "{" dc14 " }"))
(define dict144 (conc "{\n" dc14 "}\n"))
(define dict145 (conc "\n{\n" dc14 "}\n"))
(define dict146 (conc "{\n" dc14 "}"))
(define dict147 (conc "\n\n {\n" dc14 " \n }\n\n\n"))