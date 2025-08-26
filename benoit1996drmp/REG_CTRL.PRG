   /* --------------------------------------------------------------------
    |  This file  analyzes the war data from                             |
    |  Erich Weede's 1984 article "Democracy and War Involvment"         |
    |                                            JCR vol 28 no 4         |
    |                                                                    |
    |  Uses negative binomial model with control variables to test       |
    |  robustness of the bivariate relationships.                        |
    |                                                                    |
    |                                       --Ken Benoit                 |
    --------------------------------------------------------------------- */

/* **************************************
   *                                    *
   *      Initialization and misc       *
   *                                    *
   ************************************** */

library count;
filen = "weede";


output file = reg_ctrl.out reset;
countset;
__miss = 1;


proc(0) = nb(dep,ind);
    local i, d, b, bg, g, s2, vc, llik;
    _cnstart=0;
    @__altnam = ind;@
    {bg,vc,llik} = negbin("weede",stof(dep),stof(ind),0);
    g=bg[rows(bg)];
    s2=1+exp(g);
    call countprt(bg,vc,llik);
    "       sigma^2 =" s2;
    "DEP: ";; dep;; "    IND: ";; ind;?;?;
  endp;


proc(0) = nb3(dep,ind);
    local i, d, b, bg, g, s2, vc, llik;
    {bg,vc,llik} = negbin("weede3",stof(dep),stof(ind),0);
    g=bg[rows(bg)];
    s2=1+exp(g);
    call countprt(bg,vc,llik);
    "       sigma^2 = " s2 " * * * Without U.S. !";?;
  endp;


" ****************************************************************** ";
" ********************        REG_CTRL.PRG      ******************** ";
" *            Regression models with control variables            * ";
" ****************************************************************** ";?;?;

?;?;"Associations";

call ols("weede","poldem65","lecnpc70 ecintdep lpopln70");
call ols("weede","lecnpc70","poldem65 ecintdep");
call ols("weede","ecintdep","poldem65");
?;?;

?;?;"Equations (1) - (3) in Table of Neg Bin Regrs with Controls";
"-----------------------------------------------------------";?;
call nb("ssal6080","poldem65 lpopln70 lmilwp70");
call nb("ssal6080","poldem65 lpopln70 lmilwp70 ecintdep");
call nb("ssal6080","poldem65 lpopln70 lmilwp70 ecintdep lecnpc70");

?;?;"Equations (4) - (6) in Table of Neg Bin Regrs with Controls";
"-----------------------------------------------------------";?;
call nb("ssal6080","fh73 lpopln70 lmilwp70");
call nb("ssal6080","fh73 lpopln70 lmilwp70 ecintdep");
call nb("ssal6080","fh73 lpopln70 lmilwp70 ecintdep lecnpc70");

/* not used
call nb("butterw","poldem65 lpopln70 lmilwp70");
call nb("butterw","poldem65 lpopln70 lmilwp70 ecintdep");
call nb("butterw","poldem65 lpopln70 lmilwp70 ecintdep lecnpc70");

call nb("butterw","fh73 lpopln70 lmilwp70");
call nb("butterw","fh73 lpopln70 lmilwp70 ecintdep");
call nb("butterw","fh73 lpopln70 lmilwp70 ecintdep lecnpc70"); */

?;?;"Correlations among independent variables";
call subdatv("weede",0);
?;"n = ";; rows(packr(poldem65~fh73~lpopln70~lmilwp70~ecintdep~lecnpc70));
"  poldem    fh73      lpopln70   lmilwp70  ecintdep  lecnpc70";
corrx(packr(poldem65~fh73~lpopln70~lmilwp70~ecintdep~lecnpc70));

output file = off;

