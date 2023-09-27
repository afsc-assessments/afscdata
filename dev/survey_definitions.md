# Survey definitions 

  99906 = EBS slope
  99904 = AI all 
  99903 = GOA
  99902 = NBS
  99901 = EBS standard - don't use
  99900 = EBS+NW

  survey definition 47 = GOA        area_id
    type = "stratum"                10:550
    type = "region"                 99903
    type = "INPFC By Depth"         c(911:914,921:924,931:934,941:944,951:954)
    type = "inpfc"                  seq(919,959,10)
    type = "depth" - all            991:994
    type = "area" - regulatory      803:805

  52 = AI
    type = "stratum"                c(211:214,221:224,311:314,321:324,411:414,421:424,511:513,521:523,594,611:614,621:624,711,712,721,722,793,794)
    type = "region"                 99904
    type = "depth" - all            991:994
    type = "subarea"                c(299,799,3499,5699)
    type = "subarea by depth"       c(291:294,791:794,3491:3494,5691:5694)
    type = "regulatory area"        c(810,820)
    type = "regulatory by depth"    c(811:814,821:824)

  78 = EBS slope
    design_year = 2002
    type = "stratum"                c()  


  98 = EBS slope
    design_year = 2002
    type = "stratum"                c()

  143 = NBS
    type = stratum                  70:81
    type = region                   99902