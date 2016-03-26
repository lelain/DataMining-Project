data donnees;
  infile "\\univ-lyon1\enseignement\homes\l\p1513678\Mes documents\Doc\Data mining\PB4\viruses.dat" ; 
  input var1 var2 var3 var4 var5 var6 var7 var8 var9 var10 var11 var12 var13 var14 var15 var16 var17 var18 ;
run;

proc print data=donnees; run;
